;;; agent-shell-usage-limits.el --- Show Claude Code usage limits in agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tomohiro Miyasaka

;; Author: Tomohiro Miyasaka <tomohiro.miyasaka@analytical-engines.com>
;; URL: https://github.com/analytical-engines/agent-shell-usage-limits
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.89.0"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude Code tells its front ends about the usage limits of a Claude
;; subscription (the 5-hour and 7-day windows that `/usage' shows) with
;; the Agent SDK's `rate_limit_event'.  claude-agent-acp 0.42 and later,
;; the ACP adapter agent-shell runs Claude Code through, forward each of
;; those as a `usage_update' notification whose `_meta' carries the
;; event's data as `_claude/rateLimit'.  agent-shell 0.89 and later hand
;; that `_meta' to the `usage-update' event of `agent-shell-subscribe-to'.
;;
;; This package subscribes to that event in agent-shell buffers connected
;; to Claude Code and shows the windows in the mode line, for example
;;
;;   " 5h:10%[4:26] 7d:39%[4d/13h] "
;;
;; The event only comes when a value changes, so the last values
;; received are kept and shown until the next change.
;;
;; Setup:
;;
;;   (require 'agent-shell-usage-limits)
;;   (agent-shell-usage-limits-mode 1)
;;
;; Details: `M-x agent-shell-usage-limits-describe' shows every window
;; received, with reset times, in a posframe popover when posframe is
;; installed and workable, otherwise in the echo area.  Holding mouse-1
;; on the mode line display shows the same popover.
;;
;; Customization (`M-x customize-group RET agent-shell-usage-limits'):
;;
;;   `agent-shell-usage-limits-mode-line-windows'  windows to show
;;   `agent-shell-usage-limits-format'             format of one window
;;   `agent-shell-usage-limits-separator'          between windows
;;   `agent-shell-usage-limits-mode-line-format'   around all windows
;;   `agent-shell-usage-limits-window-labels'      window key -> label
;;   `agent-shell-usage-limits-time-format-short'  time left, under a day
;;   `agent-shell-usage-limits-time-format-long'   time left, a day or more
;;   `agent-shell-usage-limits-reset-time-format'  reset time of day
;;   `agent-shell-usage-limits-warning-threshold'  percent for `warning'
;;   `agent-shell-usage-limits-error-threshold'    percent for `error'
;;   `agent-shell-usage-limits-placement'          where to show
;;   `agent-shell-usage-limits-agent-identifiers'  which shells to watch
;;   `agent-shell-usage-limits-account-labels'     account -> label
;;
;; Several accounts:
;;
;; The values belong to a Claude account.  A shell's account is the
;; Claude Code config directory its agent runs with (CLAUDE_CONFIG_DIR,
;; or ~/.claude when unset), and one set of values is kept per account.
;; A shell's mode line and details show its own account only;
;; `M-x agent-shell-usage-limits-list' lists every account in a table,
;; one row each.  `agent-shell-usage-limits-make-claude-code-config'
;; makes an agent config for another account, and direnv-style per
;; project environments work too, as long as the shell is started with
;; that environment in effect.
;;
;; Using the values elsewhere:
;;
;;   (agent-shell-usage-limits-utilization 'five_hour)        => 7
;;   (agent-shell-usage-limits-resets-at 'seven_day)          => 1793588400.0
;;   (agent-shell-usage-limits-seconds-until-reset 'five_hour) => 12000.0
;;   (agent-shell-usage-limits-account-list)  => ("/home/me/.claude" ...)
;;
;; All three return nil until a value has been received.  They take an
;; optional account; by default they read the current buffer's account
;; in a watched shell and the account last updated elsewhere.  Functions
;; on `agent-shell-usage-limits-update-functions' are called with the
;; `_claude/rateLimit' alist each time a new one arrives, in the shell
;; that received it, for example to warn when the 5-hour window passes
;; 80 percent:
;;
;;   (add-hook 'agent-shell-usage-limits-update-functions
;;             (lambda (_rate-limit)
;;               (when (>= (or (agent-shell-usage-limits-utilization 'five_hour) 0) 80)
;;                 (message "Claude Code: 5-hour window at %d%%"
;;                          (agent-shell-usage-limits-utilization 'five_hour)))))
;;
;; Limitations:
;;
;; - Only Claude Code is supported.  Other agents do not report usage
;;   limits over ACP yet.
;; - Accounts are told apart by config directory only.  Two directories
;;   logged into the same account count as two accounts.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'format-spec)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-anthropic-make-claude-client "agent-shell-anthropic")
(defvar agent-shell-mode-hook)
(defvar agent-shell--state)
(defvar agent-shell-anthropic-claude-environment)

(defgroup agent-shell-usage-limits nil
  "Show Claude Code usage limits in agent-shell mode lines."
  :group 'agent-shell
  :prefix "agent-shell-usage-limits-")

;;;; Customization

(defcustom agent-shell-usage-limits-agent-identifiers '(claude-code)
  "Agent identifiers whose shells are watched.
Each is the `:identifier' of an agent config (see
`agent-shell-make-agent-config').  Nil watches every agent-shell buffer;
updates without a `_claude/rateLimit' are ignored in any case."
  :type '(choice (const :tag "Every agent" nil)
                 (repeat symbol)))

(defcustom agent-shell-usage-limits-window-labels
  '((five_hour . "5h")
    (seven_day . "7d")
    (seven_day_overage_included . "Fable"))
  "Window keys of `unifiedWindows' and the labels shown for them.
The order is the order of `agent-shell-usage-limits-details'.  Windows
not listed here are kept and shown under their key name.

`seven_day_overage_included' is the weekly window of the models that a
plan includes at a limited share (Claude Code calls it the Fable limit).
Anthropic does not put the model name in the key, so if the model
changes, only the label needs to change."
  :type '(alist :key-type symbol :value-type string))

(defcustom agent-shell-usage-limits-mode-line-windows '(five_hour seven_day)
  "Window keys shown in the mode line, in this order.
A window that has not been received yet is shown with \"-\" values.
Add `seven_day_overage_included' to also show the Fable window."
  :type '(repeat symbol))

(defcustom agent-shell-usage-limits-format "%l:%u%%[%r]"
  "Format of one window in the mode line, in `format-spec' syntax.
%l  label (`agent-shell-usage-limits-window-labels')
%k  window key, such as five_hour
%u  utilization as a whole percent, without the percent sign
%r  time left until the reset
%t  time of the reset (`agent-shell-usage-limits-reset-time-format')
%%  a percent sign
Values not received yet are \"-\".  Width specs such as %3u work."
  :type 'string)

(defcustom agent-shell-usage-limits-separator " "
  "String put between windows in the mode line."
  :type 'string)

(defcustom agent-shell-usage-limits-mode-line-format " %w "
  "Format of the whole mode line display, in `format-spec' syntax.
%w is the windows of `agent-shell-usage-limits-mode-line-windows', each
rendered with `agent-shell-usage-limits-format' and joined with
`agent-shell-usage-limits-separator'.
%a is the label of the buffer's account (see
`agent-shell-usage-limits-account-label'), for example \" %a %w \" when
shells of several accounts are open."
  :type 'string)

(defcustom agent-shell-usage-limits-account-labels nil
  "Labels for accounts, as an alist of (CONFIG-DIR . LABEL).
CONFIG-DIR is the Claude Code config directory that identifies the
account, as `agent-shell-usage-limits-account' returns it, such as
\"/home/me/.claude-work\".  Accounts not listed are labelled with their
abbreviated directory, such as \"~/.claude-work\"."
  :type '(alist :key-type string :value-type string))

(defcustom agent-shell-usage-limits-time-format-short "%h:%02m"
  "Format of the time left when it is under 24 hours (`format-spec').
%h hours, %m minutes."
  :type 'string)

(defcustom agent-shell-usage-limits-time-format-long "%dd/%Hh"
  "Format of the time left when it is 24 hours or more (`format-spec').
%d days, %H hours within the day (0 to 23), %m minutes, %h total hours."
  :type 'string)

(defcustom agent-shell-usage-limits-reset-time-format '("%H:%M" . "%m/%d %H:%M")
  "Formats of a reset time, in `format-time-string' syntax.
The car is used when the reset is today, the cdr on any other day."
  :type '(cons (string :tag "Today") (string :tag "Another day")))

(defcustom agent-shell-usage-limits-warning-threshold 80
  "Utilization in percent from which the display uses the `warning' face."
  :type 'number)

(defcustom agent-shell-usage-limits-error-threshold 95
  "Utilization in percent from which the display uses the `error' face."
  :type 'number)

(defcustom agent-shell-usage-limits-placement 'misc-info
  "Where `agent-shell-usage-limits-mode' puts the display.
`misc-info' adds it to the buffer-local `mode-line-misc-info' of watched
shells only, the way agent-shell adds its own mode line text, and leaves
`mode-line-format' alone.  `front' puts it at the front of the default
`mode-line-format', `misc' into `global-mode-string', and `none' leaves
the placement to you: put (:eval (agent-shell-usage-limits-string))
wherever you like.  Toggle the mode off and on to apply a change."
  :type '(choice (const :tag "Watched shells only" misc-info)
                 (const :tag "Front of the default mode line" front)
                 (const :tag "global-mode-string" misc)
                 (const :tag "Placed by hand" none)))

(defvar agent-shell-usage-limits-update-functions nil
  "Functions called each time a new rate limit arrives.
Each is called with the `_claude/rateLimit' alist as its only argument,
in the shell buffer that received it.  The accessors such as
`agent-shell-usage-limits-utilization' already return the new values of
that buffer's account when they run, and
`agent-shell-usage-limits-account' returns the account.")

;;;; Values received

;; The values belong to a Claude account.  An account is identified by the
;; Claude Code config directory its shell runs with (CLAUDE_CONFIG_DIR),
;; and `agent-shell-usage-limits-accounts' keeps one set of values per
;; account.  The three variables below hold the values of one account:
;; the account last updated, or, inside
;; `agent-shell-usage-limits--with-account', the account given to it.

(defvar agent-shell-usage-limits-rate-limit nil
  "The last `_claude/rateLimit' alist received from Claude Code.")

(defvar agent-shell-usage-limits-updated-at nil
  "When `agent-shell-usage-limits-rate-limit' was received, as `float-time'.")

(defvar agent-shell-usage-limits-windows nil
  "The last value received for each window.
An alist of (KEY . (:window ALIST :received TIME)) where ALIST is the
window's entry of `unifiedWindows' and TIME its `float-time' of arrival.
A per-model window such as the Fable one arrives only with updates from
that model's sessions, so windows are merged one by one instead of
replacing the whole set; an update from another model keeps it.")

(defvar agent-shell-usage-limits-accounts nil
  "The values received for each account.
An alist of (ACCOUNT . (:rate-limit RATE-LIMIT :updated-at TIME :windows
WINDOWS)), where ACCOUNT is a config directory as returned by
`agent-shell-usage-limits-account' and the rest are the values of
`agent-shell-usage-limits-rate-limit', `agent-shell-usage-limits-updated-at'
and `agent-shell-usage-limits-windows' for that account.")

(defvar agent-shell-usage-limits--bound-account nil
  "The account bound by the innermost `agent-shell-usage-limits--with-account'.")

(defmacro agent-shell-usage-limits--with-account (account &rest body)
  "Run BODY with the value variables bound to the values of ACCOUNT.
An account with nothing received yet has them all nil."
  (declare (indent 1) (debug t))
  (let ((entry (make-symbol "entry")))
    `(let* ((agent-shell-usage-limits--bound-account ,account)
            (,entry (alist-get agent-shell-usage-limits--bound-account
                               agent-shell-usage-limits-accounts nil nil #'equal))
            (agent-shell-usage-limits-rate-limit (plist-get ,entry :rate-limit))
            (agent-shell-usage-limits-updated-at (plist-get ,entry :updated-at))
            (agent-shell-usage-limits-windows (plist-get ,entry :windows)))
       ,@body)))

(defun agent-shell-usage-limits--account-from-environment (environment)
  "Return the config directory that Claude Code sees with ENVIRONMENT.
ENVIRONMENT is a list of \"VAR=value\" strings put in front of
`process-environment', the way acp.el starts the agent.  The directory
is CLAUDE_CONFIG_DIR when set and \"~/.claude\" otherwise, expanded but
with symbolic links left as they are, so two directories that link to
one count as two accounts.  For example:

  (agent-shell-usage-limits--account-from-environment
   \\='(\"CLAUDE_CONFIG_DIR=~/.claude-work/\"))
  => \"/home/me/.claude-work\""
  (let* ((process-environment (append environment process-environment))
         (dir (getenv "CLAUDE_CONFIG_DIR")))
    (directory-file-name
     (expand-file-name (if (and dir (not (string-empty-p dir))) dir "~/.claude")))))

(defvar-local agent-shell-usage-limits--account nil
  "The account of this shell, once `agent-shell-usage-limits-account' found it.
Kept so that later changes to `process-environment', such as direnv
switching it when another buffer is selected, do not move the shell to
another account.")

(defun agent-shell-usage-limits-account (&optional buffer)
  "Return the account of the agent-shell BUFFER, the current buffer by default.
The account is the Claude Code config directory the agent was started
with; see `agent-shell-usage-limits--account-from-environment'.  It is
found once per buffer and then kept."
  (with-current-buffer (or buffer (current-buffer))
    (or agent-shell-usage-limits--account
        (setq agent-shell-usage-limits--account
              (agent-shell-usage-limits--account-from-environment
               ;; agent-shell keeps the ACP client in its buffer state, and
               ;; acp.el keeps the variables it puts in front of
               ;; `process-environment' when starting the agent.
               (map-elt (map-elt (bound-and-true-p agent-shell--state) :client)
                        :environment-variables))))))

(defun agent-shell-usage-limits-account-label (account)
  "Return the label of ACCOUNT for display.
From `agent-shell-usage-limits-account-labels', or else the abbreviated
directory, such as \"~/.claude-work\"."
  (or (cdr (assoc account agent-shell-usage-limits-account-labels))
      (abbreviate-file-name account)))

(defun agent-shell-usage-limits-account-list ()
  "Return the accounts that values have been received for, sorted."
  (sort (mapcar #'car agent-shell-usage-limits-accounts) #'string<))

(cl-defun agent-shell-usage-limits-make-claude-code-config (&key name config-dir)
  "Return a Claude Code agent config for the account in CONFIG-DIR.
The config is agent-shell's own Claude Code config with its buffer and
mode line names set to NAME and CLAUDE_CONFIG_DIR set to CONFIG-DIR for
the agent it starts, so shells made from it use that account.  Add it
to `agent-shell-agent-configs' as a function, for example:

  (add-to-list \\='agent-shell-agent-configs
               (lambda ()
                 (agent-shell-usage-limits-make-claude-code-config
                  :name \"Claude (work)\" :config-dir \"~/.claude-work\")))

The identifier stays `claude-code', so the config is watched like the
original.  Claude Code must be logged in for CONFIG-DIR: on macOS the
login lives in the Keychain, one entry per config directory, so run
CLAUDE_CONFIG_DIR=<dir> claude auth login once."
  (unless (and name config-dir)
    (error "Both :name and :config-dir are required"))
  (let ((config (agent-shell-anthropic-make-claude-code-config))
        (variable (concat "CLAUDE_CONFIG_DIR=" (expand-file-name config-dir))))
    (setf (alist-get :buffer-name config) name
          (alist-get :mode-line-name config) name
          (alist-get :client-maker config)
          (lambda (buffer)
            ;; The client maker reads the variables from this customizable
            ;; variable when it starts the agent; bind it for this account.
            (let ((agent-shell-anthropic-claude-environment
                   (cons variable agent-shell-anthropic-claude-environment)))
              (agent-shell-anthropic-make-claude-client :buffer buffer))))
    config))

(defun agent-shell-usage-limits--store (account rate-limit time)
  "Store RATE-LIMIT as received for ACCOUNT at TIME.
Also make it the account last updated, whose values the value
variables hold outside `agent-shell-usage-limits--with-account'."
  (let ((agent-shell-usage-limits-windows
         (plist-get (alist-get account agent-shell-usage-limits-accounts nil nil #'equal)
                    :windows)))
    (agent-shell-usage-limits--remember-windows rate-limit time)
    (setf (alist-get account agent-shell-usage-limits-accounts nil nil #'equal)
          (list :rate-limit rate-limit :updated-at time
                :windows agent-shell-usage-limits-windows)))
  (let ((entry (alist-get account agent-shell-usage-limits-accounts nil nil #'equal)))
    (setq agent-shell-usage-limits-rate-limit rate-limit
          agent-shell-usage-limits-updated-at time
          agent-shell-usage-limits-windows (plist-get entry :windows))))

(defun agent-shell-usage-limits--display-account ()
  "Return the account whose values the current buffer shows.
The buffer's own account in a watched shell, else nil for the account
last updated."
  (and (agent-shell-usage-limits--watched-buffer-p)
       (agent-shell-usage-limits-account)))

(defun agent-shell-usage-limits--remember-windows (rate-limit time)
  "Store each window of RATE-LIMIT's `unifiedWindows' as received at TIME."
  (dolist (entry (map-elt rate-limit 'unifiedWindows))
    (setf (alist-get (car entry) agent-shell-usage-limits-windows)
          ;; Copy: agent-shell may reuse the containers it hands out.
          (list :window (copy-tree (cdr entry)) :received time))))

(defun agent-shell-usage-limits--window-key (window)
  "Return the `unifiedWindows' key for WINDOW.
WINDOW is a key such as `five_hour', or one of the aliases `five-hour'
and `seven-day'."
  (pcase window
    ('five-hour 'five_hour)
    ('seven-day 'seven_day)
    (_ window)))

(defun agent-shell-usage-limits--window (window)
  "Return the alist last received for WINDOW, or nil if none yet."
  (plist-get (alist-get (agent-shell-usage-limits--window-key window)
                        agent-shell-usage-limits-windows)
             :window))

(defmacro agent-shell-usage-limits--for-account (account &rest body)
  "Run BODY with the values of ACCOUNT, or of the shown account when nil.
When ACCOUNT is nil and an enclosing `agent-shell-usage-limits--with-account'
has bound one, that one is kept.  Otherwise the shown account is the
current buffer's in a watched shell, and the account last updated
anywhere else."
  (declare (indent 1) (debug t))
  (let ((acct (make-symbol "account"))
        (fn (make-symbol "body")))
    `(let ((,acct ,account)
           (,fn (lambda () ,@body)))
       (unless (or ,acct agent-shell-usage-limits--bound-account)
         (setq ,acct (agent-shell-usage-limits--display-account)))
       (if (and ,acct (not (equal ,acct agent-shell-usage-limits--bound-account)))
           (agent-shell-usage-limits--with-account ,acct (funcall ,fn))
         (funcall ,fn)))))

(defun agent-shell-usage-limits-window-received (window &optional account)
  "Return when WINDOW was last received, as `float-time', or nil if never.
ACCOUNT as for `agent-shell-usage-limits-utilization'."
  (agent-shell-usage-limits--for-account account
    (plist-get (alist-get (agent-shell-usage-limits--window-key window)
                          agent-shell-usage-limits-windows)
               :received)))

(defun agent-shell-usage-limits-window-keys (&optional account)
  "Return the keys of every window received so far, in display order.
The order of `agent-shell-usage-limits-window-labels' first, then the
windows not listed there, oldest first.  ACCOUNT as for
`agent-shell-usage-limits-utilization'."
  (agent-shell-usage-limits--for-account account
    (let ((known (mapcar #'car agent-shell-usage-limits-window-labels))
          (received (mapcar #'car agent-shell-usage-limits-windows)))
      (append (seq-filter (lambda (key) (memq key received)) known)
              (seq-remove (lambda (key) (memq key known)) (reverse received))))))

(defun agent-shell-usage-limits-window-label (window)
  "Return the label of WINDOW, or its key name when it has no label."
  (let ((key (agent-shell-usage-limits--window-key window)))
    (or (alist-get key agent-shell-usage-limits-window-labels)
        (symbol-name key))))

(defun agent-shell-usage-limits-utilization (window &optional account)
  "Return the utilization of WINDOW as a whole percent, or nil if none yet.
For example 7 for a utilization of 0.07.  The values are those of
ACCOUNT (see `agent-shell-usage-limits-account'); by default of the
current buffer's account in a watched shell, else of the account last
updated."
  (agent-shell-usage-limits--for-account account
    (when-let* ((util (map-elt (agent-shell-usage-limits--window window) 'utilization)))
      (round (* 100 util)))))

(defun agent-shell-usage-limits-resets-at (window &optional account)
  "Return when WINDOW resets, as `float-time', or nil if none yet.
ACCOUNT as for `agent-shell-usage-limits-utilization'."
  (agent-shell-usage-limits--for-account account
    (let ((resets (map-elt (agent-shell-usage-limits--window window) 'resetsAt)))
      (and (numberp resets) (float resets)))))

(defun agent-shell-usage-limits-seconds-until-reset (window &optional account)
  "Return the seconds left until WINDOW resets, or nil if none yet.
Zero once the reset time has passed.  ACCOUNT as for
`agent-shell-usage-limits-utilization'."
  (when-let* ((resets (agent-shell-usage-limits-resets-at window account)))
    (max 0 (- resets (float-time)))))

;;;; Receiving from agent-shell

(defvar-local agent-shell-usage-limits--subscription nil
  "Token of this buffer's `usage-update' subscription, for unsubscribing.")

(defun agent-shell-usage-limits--on-usage-update (event &optional account)
  "Keep the `_claude/rateLimit' carried by the `usage-update' EVENT, if any.
The values are stored for ACCOUNT, by default the account of the
current buffer (see `agent-shell-usage-limits-account')."
  (when-let* ((rate-limit (map-nested-elt event '(:data :meta _claude/rateLimit))))
    (let ((account (or account (agent-shell-usage-limits-account))))
      (agent-shell-usage-limits--store account rate-limit (float-time))
      (force-mode-line-update t)
      (agent-shell-usage-limits--with-account account
        (run-hook-with-args 'agent-shell-usage-limits-update-functions rate-limit)))))

(defun agent-shell-usage-limits--watched-buffer-p ()
  "Return non-nil if the current buffer is a shell of a watched agent.
See `agent-shell-usage-limits-agent-identifiers'."
  (and (derived-mode-p 'agent-shell-mode)
       (or (null agent-shell-usage-limits-agent-identifiers)
           (not (fboundp 'agent-shell-get-config))
           (memq (map-elt (agent-shell-get-config (current-buffer)) :identifier)
                 agent-shell-usage-limits-agent-identifiers))))

(defun agent-shell-usage-limits--subscribe ()
  "Subscribe to `usage-update' in the current buffer if it is watched.
Does nothing when already subscribed."
  (when (and (agent-shell-usage-limits--watched-buffer-p)
             (fboundp 'agent-shell-subscribe-to)
             (null agent-shell-usage-limits--subscription))
    ;; Settle the account now, while the environment is the one the
    ;; agent is started with.
    (agent-shell-usage-limits-account)
    (let ((buffer (current-buffer)))
      (setq agent-shell-usage-limits--subscription
            (agent-shell-subscribe-to
             :shell-buffer buffer
             :event 'usage-update
             :on-event (lambda (event)
                         ;; Store under this shell's account, whatever
                         ;; buffer is current when the event arrives.
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (agent-shell-usage-limits--on-usage-update event)))))))))

(defun agent-shell-usage-limits--unsubscribe ()
  "Drop the current buffer's `usage-update' subscription, if any."
  (when (and agent-shell-usage-limits--subscription
             (fboundp 'agent-shell-unsubscribe))
    (agent-shell-unsubscribe :subscription agent-shell-usage-limits--subscription)
    (setq agent-shell-usage-limits--subscription nil)))

;;;; Rendering

(defun agent-shell-usage-limits--format-duration (seconds)
  "Format SECONDS as time left, or \"-\" when SECONDS is nil.
For example \"4:26\" for 15960, or \"4d/13h\" for 392400."
  (if (null seconds)
      "-"
    (let* ((hours (floor seconds 3600))
           (minutes (floor (mod seconds 3600) 60)))
      (format-spec (if (>= hours 24)
                       agent-shell-usage-limits-time-format-long
                     agent-shell-usage-limits-time-format-short)
                   `((?d . ,(number-to-string (/ hours 24)))
                     (?H . ,(number-to-string (% hours 24)))
                     (?h . ,(number-to-string hours))
                     (?m . ,(number-to-string minutes)))))))

(defun agent-shell-usage-limits--format-reset-time (time)
  "Format the reset TIME, a `float-time', or \"-\" when TIME is nil.
Uses the car of `agent-shell-usage-limits-reset-time-format' when TIME is
today and the cdr otherwise."
  (if (null time)
      "-"
    (format-time-string
     (if (equal (format-time-string "%F" time) (format-time-string "%F"))
         (car agent-shell-usage-limits-reset-time-format)
       (cdr agent-shell-usage-limits-reset-time-format))
     time)))

(defun agent-shell-usage-limits--render-window (window placeholder)
  "Render WINDOW with `agent-shell-usage-limits-format'.
With PLACEHOLDER non-nil, put asterisks where the numbers would be."
  (format-spec agent-shell-usage-limits-format
               `((?l . ,(agent-shell-usage-limits-window-label window))
                 (?k . ,(symbol-name (agent-shell-usage-limits--window-key window)))
                 (?u . ,(cond (placeholder "**")
                              ((agent-shell-usage-limits-utilization window)
                               (number-to-string
                                (agent-shell-usage-limits-utilization window)))
                              (t "-")))
                 (?r . ,(if placeholder
                            (format-spec agent-shell-usage-limits-time-format-short
                                         '((?d . "*") (?H . "*") (?h . "*") (?m . "**")))
                          (agent-shell-usage-limits--format-duration
                           (agent-shell-usage-limits-seconds-until-reset window))))
                 (?t . ,(if placeholder
                            "**:**"
                          (agent-shell-usage-limits--format-reset-time
                           (agent-shell-usage-limits-resets-at window)))))))

(defun agent-shell-usage-limits--render (placeholder)
  "Render the mode line text from the customized formats.
With PLACEHOLDER non-nil, put asterisks where the numbers would be."
  (format-spec agent-shell-usage-limits-mode-line-format
               `((?w . ,(mapconcat (lambda (window)
                                     (agent-shell-usage-limits--render-window window placeholder))
                                   agent-shell-usage-limits-mode-line-windows
                                   agent-shell-usage-limits-separator))
                 (?a . ,(let ((account (or agent-shell-usage-limits--bound-account
                                           (agent-shell-usage-limits--display-account))))
                          (if account
                              (agent-shell-usage-limits-account-label account)
                            "-"))))))

(defun agent-shell-usage-limits--face ()
  "Return the face for the mode line text, from the highest shown utilization.
Nil below `agent-shell-usage-limits-warning-threshold', so the text takes
the mode line's own colors."
  (let ((worst (apply #'max 0
                      (mapcar (lambda (window)
                                (or (agent-shell-usage-limits-utilization window) 0))
                              agent-shell-usage-limits-mode-line-windows))))
    (cond ((>= worst agent-shell-usage-limits-error-threshold) 'error)
          ((>= worst agent-shell-usage-limits-warning-threshold) 'warning)
          (t nil))))

(defun agent-shell-usage-limits-details (&optional account)
  "Return a multi-line description of the windows of one account.
Each line has the label, utilization, reset time and time left.  A window
older than the latest update of its account by more than a minute gets
its own time of arrival, and one whose reset time has passed says so.

ACCOUNT as for `agent-shell-usage-limits-utilization': by default the
current buffer's account in a watched shell, else the account last
updated.  When values of other accounts have been received, the
account's label is in the title and a last line counts the others; see
`agent-shell-usage-limits-list' for all of them."
  (let* ((accounts (agent-shell-usage-limits-account-list))
         (shown (or account
                    (agent-shell-usage-limits--display-account)
                    (and agent-shell-usage-limits-rate-limit
                         (car (seq-find (lambda (entry)
                                          (eq (plist-get (cdr entry) :rate-limit)
                                              agent-shell-usage-limits-rate-limit))
                                        agent-shell-usage-limits-accounts)))))
         (others (length (remove shown accounts))))
    (agent-shell-usage-limits--for-account shown
      (concat
       (agent-shell-usage-limits--details-section
        (if (and shown (> others 0))
            (format "Claude Code usage limits: %s"
                    (agent-shell-usage-limits-account-label shown))
          "Claude Code usage limits"))
       (when (> others 0)
         (format "\n%d other account%s: M-x agent-shell-usage-limits-list"
                 others (if (= others 1) "" "s")))))))

(defun agent-shell-usage-limits--details-section (title)
  "Return the details of the windows in the value variables, under TITLE."
  (let ((keys (agent-shell-usage-limits-window-keys)))
    (if (null keys)
        (format "%s: waiting for the first update" title)
      (let* ((width (apply #'max
                           (mapcar (lambda (key)
                                     (string-width (agent-shell-usage-limits-window-label key)))
                                   keys)))
             (line
              (lambda (key)
                (let* ((util (agent-shell-usage-limits-utilization key))
                       (left (agent-shell-usage-limits-seconds-until-reset key))
                       (received (agent-shell-usage-limits-window-received key))
                       (stale (and received agent-shell-usage-limits-updated-at
                                   (> (- agent-shell-usage-limits-updated-at received) 60))))
                  (concat
                   (format "%s  %4s  resets %-12s %s"
                           (string-pad (agent-shell-usage-limits-window-label key) width)
                           (if util (format "%d%%" util) "-")
                           (agent-shell-usage-limits--format-reset-time
                            (agent-shell-usage-limits-resets-at key))
                           (if (and left (zerop left))
                               "(reset passed)"
                             (format "(in %s)" (agent-shell-usage-limits--format-duration left))))
                   (when stale
                     (format "  as of %s" (format-time-string "%m/%d %H:%M" received))))))))
        (concat (format "%s (received %s)\n"
                        title
                        (format-time-string "%H:%M:%S" agent-shell-usage-limits-updated-at))
                (mapconcat line keys "\n"))))))

;;;; Describing

;; posframe is loaded only when `agent-shell-usage-limits-describe' runs.
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-poshandler-window-bottom-right-corner "posframe")

(defconst agent-shell-usage-limits--posframe-buffer " *agent-shell-usage-limits*"
  "Buffer of the popover shown by `agent-shell-usage-limits-describe'.")

(defun agent-shell-usage-limits--hide-posframe ()
  "Hide the popover and remove this function from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'agent-shell-usage-limits--hide-posframe)
  (when (fboundp 'posframe-hide)
    (posframe-hide agent-shell-usage-limits--posframe-buffer)))

(defun agent-shell-usage-limits--popover-p ()
  "Return non-nil if a posframe popover works on the selected frame.
That is any graphical frame, and a terminal frame on Emacs 31 or later."
  (and (require 'posframe nil t)
       (posframe-workable-p)))

(defun agent-shell-usage-limits-describe (&optional event)
  "Show the details of every usage limit window received so far.
Shows a popover above the right end of the mode line when posframe is
workable, otherwise the text in the echo area.  The popover closes at
the next command.  When called from the mode line by mouse (EVENT
non-nil), the popover goes to the clicked window and closes when the
button is released."
  (interactive (list (and (mouse-event-p last-input-event) last-input-event)))
  (let ((text (agent-shell-usage-limits-details))
        (window (or (and event (posn-window (event-start event)))
                    (selected-window))))
    (if (and (window-live-p window)
             (with-selected-window window (agent-shell-usage-limits--popover-p)))
        (with-selected-window window
          (let ((graphic (display-graphic-p)))
            (posframe-show agent-shell-usage-limits--posframe-buffer
                           ;; Pad every line, and add blank lines above and
                           ;; below, so the margins are even.
                           :string (mapconcat (lambda (line) (concat "  " line "  "))
                                              (append '("") (split-string text "\n") '(""))
                                              "\n")
                           :poshandler #'posframe-poshandler-window-bottom-right-corner
                           ;; A thin border on graphical frames.  On a
                           ;; terminal the border is drawn with characters
                           ;; that get cut at the right edge and overlap
                           ;; the mode line, so use a background instead.
                           :internal-border-width (if graphic 1 0)
                           :internal-border-color (face-foreground 'shadow nil t)
                           :background-color (face-background (if graphic 'default 'highlight)
                                                              nil t)
                           :foreground-color (face-foreground 'default nil t)))
          (add-hook 'pre-command-hook #'agent-shell-usage-limits--hide-posframe))
      (message "%s" text))))

;;;; Listing every account

(defvar agent-shell-usage-limits-list-buffer "*Claude Code usage limits*"
  "Name of the buffer of `agent-shell-usage-limits-list'.")

(defun agent-shell-usage-limits--list-shells (account)
  "Return the names of the watched shells of ACCOUNT, sorted."
  (sort (delq nil (mapcar (lambda (buffer)
                            (and (equal (buffer-local-value 'agent-shell-usage-limits--account buffer)
                                        account)
                                 (buffer-name buffer)))
                          (buffer-list)))
        #'string<))

(defun agent-shell-usage-limits--list-windows ()
  "Return the window keys received by any account, in display order.
The order of `agent-shell-usage-limits-window-labels' first, then the
windows not listed there."
  (let ((known (mapcar #'car agent-shell-usage-limits-window-labels))
        (received (seq-uniq
                   (apply #'append
                          (mapcar (lambda (entry)
                                    (mapcar #'car (plist-get (cdr entry) :windows)))
                                  agent-shell-usage-limits-accounts)))))
    (append (seq-filter (lambda (key) (memq key received)) known)
            (seq-remove (lambda (key) (memq key known)) received))))

(defun agent-shell-usage-limits--list-cell (window)
  "Return the list cell of WINDOW for the account in the value variables.
For example \"53% in 1:05\", in the `warning' or `error' face from the
thresholds, or \"-\" when the window has not been received."
  (let ((util (agent-shell-usage-limits-utilization window))
        (left (agent-shell-usage-limits-seconds-until-reset window)))
    (if (null util)
        "-"
      (propertize
       (format "%d%% %s" util
               (if (and left (zerop left))
                   "reset passed"
                 (format "in %s" (agent-shell-usage-limits--format-duration left))))
       'face (cond ((>= util agent-shell-usage-limits-error-threshold) 'error)
                   ((>= util agent-shell-usage-limits-warning-threshold) 'warning))))))

(defun agent-shell-usage-limits--list-refresh ()
  "Set the columns and rows of the list buffer, one row per account."
  (let* ((windows (agent-shell-usage-limits--list-windows))
         (accounts (agent-shell-usage-limits-account-list))
         (label-width (apply #'max 7
                             (mapcar (lambda (account)
                                       (string-width
                                        (agent-shell-usage-limits-account-label account)))
                                     accounts))))
    (setq tabulated-list-format
          (vconcat (list (list "Account" (min label-width 40) t))
                   (mapcar (lambda (window)
                             (list (agent-shell-usage-limits-window-label window) 18 nil))
                           windows)
                   (list (list "Received" 9 t)
                         (list "Shells" 0 t))))
    (setq tabulated-list-entries
          (mapcar (lambda (account)
                    (agent-shell-usage-limits--with-account account
                      (list account
                            (vconcat
                             (list (agent-shell-usage-limits-account-label account))
                             (mapcar #'agent-shell-usage-limits--list-cell windows)
                             (list (if agent-shell-usage-limits-updated-at
                                       (format-time-string "%H:%M:%S"
                                                           agent-shell-usage-limits-updated-at)
                                     "-")
                                   (string-join (agent-shell-usage-limits--list-shells account)
                                                ", "))))))
                  accounts))
    (tabulated-list-init-header)))

(defun agent-shell-usage-limits-list-visit ()
  "Switch to a shell of the account on the current line."
  (interactive)
  (let* ((account (tabulated-list-get-id))
         (shell (and account (car (agent-shell-usage-limits--list-shells account)))))
    (if shell
        (pop-to-buffer shell)
      (user-error "No shell of this account is open"))))

(defvar agent-shell-usage-limits-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-usage-limits-list-visit)
    map)
  "Keymap of `agent-shell-usage-limits-list-mode'.")

(define-derived-mode agent-shell-usage-limits-list-mode tabulated-list-mode "Usage limits"
  "Major mode of the list of Claude Code usage limits, one row per account.
\\{agent-shell-usage-limits-list-mode-map}"
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'agent-shell-usage-limits--list-refresh nil t))

(defun agent-shell-usage-limits--list-update (_rate-limit)
  "Refresh the list buffer after an update, when it is open."
  (when-let* ((buffer (get-buffer agent-shell-usage-limits-list-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-usage-limits-list-mode)
        (revert-buffer)))))

;;;###autoload
(defun agent-shell-usage-limits-list ()
  "List the usage limits of every account, one row per account.
The columns are the windows received so far, when the account's last
update arrived and its open shells.  The list refreshes as updates
arrive, `g' refreshes it by hand, and RET switches to a shell of the
account on the line."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-usage-limits-list-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-usage-limits-list-mode)
        (agent-shell-usage-limits-list-mode))
      (revert-buffer))
    (add-hook 'agent-shell-usage-limits-update-functions
              #'agent-shell-usage-limits--list-update)
    (pop-to-buffer buffer)))

;;;; Mode line

(defvar agent-shell-usage-limits-mode-line-map
  (let ((map (make-sparse-keymap)))
    ;; Show the details while the button is held.  The release (mouse-1 or
    ;; drag-mouse-1) runs `ignore', and `pre-command-hook' closes the
    ;; popover before that command.
    (define-key map [mode-line down-mouse-1] #'agent-shell-usage-limits-describe)
    (define-key map [mode-line mouse-1] #'ignore)
    (define-key map [mode-line drag-mouse-1] #'ignore)
    map)
  "Keymap of the usage limits text in the mode line.")

(defun agent-shell-usage-limits-string ()
  "Return the usage limits text for the mode line.
Before the first update the numbers are asterisks in the `shadow' face.
In a watched shell the values are those of the shell's account (see
`agent-shell-usage-limits-account'), elsewhere of the account last
updated."
  (let (received text face)
    (agent-shell-usage-limits--for-account nil
      (setq received (and agent-shell-usage-limits-windows t)
            text (agent-shell-usage-limits--render (not received))
            face (if received (agent-shell-usage-limits--face) 'shadow)))
    ;; A percent sign is special in the mode line.
    (propertize (string-replace "%" "%%" text)
                'face face
                ;; Where the popover works, the tooltip only explains the
                ;; mouse, to avoid showing the details twice.
                'help-echo (lambda (&rest _)
                             (if (agent-shell-usage-limits--popover-p)
                                 "mouse-1 (hold): show usage limit details"
                               (agent-shell-usage-limits-details)))
                'local-map agent-shell-usage-limits-mode-line-map
                'mouse-face 'mode-line-highlight)))

(defconst agent-shell-usage-limits--element '(:eval (agent-shell-usage-limits-string))
  "The mode line construct that `agent-shell-usage-limits-mode' places.")

(defun agent-shell-usage-limits--setup-misc-info ()
  "Add the display to the buffer-local `mode-line-misc-info' of a watched shell."
  (when (and (eq agent-shell-usage-limits-placement 'misc-info)
             (agent-shell-usage-limits--watched-buffer-p)
             (not (member agent-shell-usage-limits--element mode-line-misc-info)))
    (setq-local mode-line-misc-info
                (append mode-line-misc-info (list agent-shell-usage-limits--element)))))

(defun agent-shell-usage-limits--enable ()
  "Subscribe in watched shells and place the display."
  ;; Shells opened from now on, and the ones already open.
  (add-hook 'agent-shell-mode-hook #'agent-shell-usage-limits--subscribe)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (agent-shell-usage-limits--subscribe)))
  (pcase agent-shell-usage-limits-placement
    ('none nil)
    ('misc-info
     (add-hook 'agent-shell-mode-hook #'agent-shell-usage-limits--setup-misc-info)
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (agent-shell-usage-limits--setup-misc-info))))
    ('front
     (unless (member agent-shell-usage-limits--element (default-value 'mode-line-format))
       (setq-default mode-line-format
                     (cons agent-shell-usage-limits--element
                           (default-value 'mode-line-format)))))
    (_
     (unless (member agent-shell-usage-limits--element global-mode-string)
       (setq global-mode-string
             (cons agent-shell-usage-limits--element
                   (if (listp global-mode-string)
                       global-mode-string
                     (list global-mode-string)))))))
  (force-mode-line-update t))

(defun agent-shell-usage-limits--disable ()
  "Unsubscribe everywhere and remove the display from every placement."
  (remove-hook 'agent-shell-mode-hook #'agent-shell-usage-limits--subscribe)
  (remove-hook 'agent-shell-mode-hook #'agent-shell-usage-limits--setup-misc-info)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (agent-shell-usage-limits--unsubscribe)
      (when (and (local-variable-p 'mode-line-misc-info)
                 (member agent-shell-usage-limits--element mode-line-misc-info))
        (setq-local mode-line-misc-info
                    (delete agent-shell-usage-limits--element mode-line-misc-info)))))
  (setq-default mode-line-format
                (delete agent-shell-usage-limits--element (default-value 'mode-line-format)))
  (setq global-mode-string
        (delete agent-shell-usage-limits--element global-mode-string))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode agent-shell-usage-limits-mode
  "Show Claude Code usage limits in agent-shell mode lines.
Subscribes to the `usage-update' event in the shells of the agents in
`agent-shell-usage-limits-agent-identifiers' and places the display as
`agent-shell-usage-limits-placement' says."
  :global t
  :group 'agent-shell-usage-limits
  (if agent-shell-usage-limits-mode
      (agent-shell-usage-limits--enable)
    (agent-shell-usage-limits--disable)))

(provide 'agent-shell-usage-limits)
;;; agent-shell-usage-limits.el ends here
