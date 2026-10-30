;;; agent-shell-usage-limits-test.el --- Tests for agent-shell-usage-limits  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tomohiro Miyasaka

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

;; Run with: make test

;;; Code:

(require 'ert)
(require 'agent-shell-usage-limits)

(defmacro agent-shell-usage-limits-test--with-defaults (&rest body)
  "Run BODY with empty state and the default customization."
  (declare (indent 0))
  `(let ((agent-shell-usage-limits-rate-limit nil)
         (agent-shell-usage-limits-updated-at nil)
         (agent-shell-usage-limits-windows nil)
         (agent-shell-usage-limits-update-functions nil)
         (agent-shell-usage-limits-window-labels
          '((five_hour . "5h") (seven_day . "7d") (seven_day_overage_included . "Fable")))
         (agent-shell-usage-limits-mode-line-windows '(five_hour seven_day))
         (agent-shell-usage-limits-format "%l:%u%%[%r]")
         (agent-shell-usage-limits-separator " ")
         (agent-shell-usage-limits-mode-line-format " %w ")
         (agent-shell-usage-limits-time-format-short "%h:%02m")
         (agent-shell-usage-limits-time-format-long "%dd/%Hh")
         (agent-shell-usage-limits-reset-time-format '("%H:%M" . "%m/%d %H:%M"))
         (agent-shell-usage-limits-warning-threshold 80)
         (agent-shell-usage-limits-error-threshold 95))
     ,@body))

(defun agent-shell-usage-limits-test--event (windows)
  "Return a `usage-update' event whose `_claude/rateLimit' has WINDOWS.
WINDOWS is an alist of (KEY . ((utilization . U) (resetsAt . T)))."
  `((:event . usage-update)
    (:data . ((:usage . ((:context-used . 100) (:context-size . 1000)))
              (:meta . ((_claude/rateLimit . ((status . "allowed")
                                              (unifiedWindows . ,windows)))))))))

(defun agent-shell-usage-limits-test--window (utilization seconds-from-now)
  "Return a window alist with UTILIZATION resetting SECONDS-FROM-NOW."
  `((utilization . ,utilization)
    (resetsAt . ,(floor (+ (float-time) seconds-from-now)))))

(ert-deftest agent-shell-usage-limits-test-remember-windows-merges-per-window ()
  "A later update without the Fable window keeps the Fable window."
  (agent-shell-usage-limits-test--with-defaults
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.07 100))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.4 1000))
        (seven_day_overage_included . ,(agent-shell-usage-limits-test--window 0.61 1000)))))
    (let ((fable-received (agent-shell-usage-limits-window-received 'seven_day_overage_included)))
      (agent-shell-usage-limits--on-usage-update
       (agent-shell-usage-limits-test--event
        `((five_hour . ,(agent-shell-usage-limits-test--window 0.1 100))
          (seven_day . ,(agent-shell-usage-limits-test--window 0.45 1000)))))
      (should (= 10 (agent-shell-usage-limits-utilization 'five_hour)))
      (should (= 45 (agent-shell-usage-limits-utilization 'seven_day)))
      (should (= 61 (agent-shell-usage-limits-utilization 'seven_day_overage_included)))
      (should (equal fable-received
                     (agent-shell-usage-limits-window-received 'seven_day_overage_included)))
      (should (>= (agent-shell-usage-limits-window-received 'five_hour) fable-received)))))

(ert-deftest agent-shell-usage-limits-test-update-without-rate-limit-is-ignored ()
  "An update of another agent, without `_claude/rateLimit', changes nothing."
  (agent-shell-usage-limits-test--with-defaults
    (let ((calls 0))
      (add-hook 'agent-shell-usage-limits-update-functions (lambda (_) (cl-incf calls)))
      (agent-shell-usage-limits--on-usage-update
       '((:event . usage-update)
         (:data . ((:usage . ((:context-used . 1))) (:meta . ((quota . ((token_count . 1)))))))))
      (should (null agent-shell-usage-limits-windows))
      (should (null agent-shell-usage-limits-updated-at))
      (should (= 0 calls)))))

(ert-deftest agent-shell-usage-limits-test-update-functions-run-with-new-values ()
  "The hook runs after the values are stored, with the rate limit alist."
  (agent-shell-usage-limits-test--with-defaults
    (let (seen)
      (add-hook 'agent-shell-usage-limits-update-functions
                (lambda (rate-limit)
                  (setq seen (list (map-elt rate-limit 'status)
                                   (agent-shell-usage-limits-utilization 'five_hour)))))
      (agent-shell-usage-limits--on-usage-update
       (agent-shell-usage-limits-test--event
        `((five_hour . ,(agent-shell-usage-limits-test--window 0.5 100)))))
      (should (equal '("allowed" 50) seen)))))

(ert-deftest agent-shell-usage-limits-test-stored-values-are-copies ()
  "Changing the event after the update does not change the stored values."
  (agent-shell-usage-limits-test--with-defaults
    (let* ((window (agent-shell-usage-limits-test--window 0.2 100))
           (event (agent-shell-usage-limits-test--event `((five_hour . ,window)))))
      (agent-shell-usage-limits--on-usage-update event)
      (setf (alist-get 'utilization window) 0.9)
      (should (= 20 (agent-shell-usage-limits-utilization 'five_hour))))))

(ert-deftest agent-shell-usage-limits-test-window-keys-follow-labels-then-unknown ()
  "Known windows come in label order, unknown ones after, oldest first."
  (agent-shell-usage-limits-test--with-defaults
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((seven_day_opus . ,(agent-shell-usage-limits-test--window 0.1 100))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.1 100))
        (five_hour . ,(agent-shell-usage-limits-test--window 0.1 100))
        (seven_day_sonnet . ,(agent-shell-usage-limits-test--window 0.1 100)))))
    (should (equal '(five_hour seven_day seven_day_opus seven_day_sonnet)
                   (agent-shell-usage-limits-window-keys)))
    (should (equal "seven_day_opus"
                   (agent-shell-usage-limits-window-label 'seven_day_opus)))
    (should (equal "5h" (agent-shell-usage-limits-window-label 'five-hour)))))

(ert-deftest agent-shell-usage-limits-test-accessors ()
  "Utilization is a whole percent; reset times count down and stop at zero."
  (agent-shell-usage-limits-test--with-defaults
    (should (null (agent-shell-usage-limits-utilization 'five_hour)))
    (should (null (agent-shell-usage-limits-resets-at 'five_hour)))
    (should (null (agent-shell-usage-limits-seconds-until-reset 'five_hour)))
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.07 100))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.4 -100)))))
    (should (= 7 (agent-shell-usage-limits-utilization 'five_hour)))
    (should (= 40 (agent-shell-usage-limits-utilization 'seven-day)))
    (should (floatp (agent-shell-usage-limits-resets-at 'five_hour)))
    (should (< 90 (agent-shell-usage-limits-seconds-until-reset 'five_hour) 101))
    (should (= 0 (agent-shell-usage-limits-seconds-until-reset 'seven_day)))))

(ert-deftest agent-shell-usage-limits-test-format-duration ()
  "Time left is hours:minutes under a day and days/hours from a day on."
  (agent-shell-usage-limits-test--with-defaults
    (should (equal "-" (agent-shell-usage-limits--format-duration nil)))
    (should (equal "0:00" (agent-shell-usage-limits--format-duration 0)))
    (should (equal "4:26" (agent-shell-usage-limits--format-duration 15960)))
    (should (equal "23:59" (agent-shell-usage-limits--format-duration 86399)))
    (should (equal "1d/0h" (agent-shell-usage-limits--format-duration 86400)))
    (should (equal "4d/13h" (agent-shell-usage-limits--format-duration 392400)))))

(ert-deftest agent-shell-usage-limits-test-render-placeholder-before-first-update ()
  "Before any update the numbers are asterisks and the face is `shadow'."
  (agent-shell-usage-limits-test--with-defaults
    (should (equal " 5h:**%[*:**] 7d:**%[*:**] "
                   (agent-shell-usage-limits--render t)))
    (let ((text (agent-shell-usage-limits-string)))
      (should (equal " 5h:**%%[*:**] 7d:**%%[*:**] " (substring-no-properties text)))
      (should (eq 'shadow (get-text-property 0 'face text))))))

(ert-deftest agent-shell-usage-limits-test-render-windows-with-values ()
  "Each shown window is rendered with the format and joined with the separator."
  (agent-shell-usage-limits-test--with-defaults
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.07 (+ 15960 30)))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.4 (+ 392400 30))))))
    (should (equal " 5h:7%[4:26] 7d:40%[4d/13h] " (agent-shell-usage-limits--render nil)))
    (let ((agent-shell-usage-limits-mode-line-windows '(seven_day five_hour))
          (agent-shell-usage-limits-format "%k=%3u")
          (agent-shell-usage-limits-separator ", ")
          (agent-shell-usage-limits-mode-line-format "[%w]"))
      (should (equal "[seven_day= 40, five_hour=  7]" (agent-shell-usage-limits--render nil))))))

(ert-deftest agent-shell-usage-limits-test-render-window-not-received ()
  "A shown window that has not arrived yet renders as dashes."
  (agent-shell-usage-limits-test--with-defaults
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.07 (+ 15960 30))))))
    (let ((agent-shell-usage-limits-mode-line-windows
           '(five_hour seven_day_overage_included))
          (agent-shell-usage-limits-format "%l:%u%%[%r] %t"))
      (should (equal " 5h:7%[4:26] " (substring (agent-shell-usage-limits--render nil) 0 13)))
      (should (string-suffix-p " Fable:-%[-] - " (agent-shell-usage-limits--render nil))))))

(ert-deftest agent-shell-usage-limits-test-face-from-shown-windows ()
  "The face follows the highest utilization among the windows shown."
  (agent-shell-usage-limits-test--with-defaults
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.5 100))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.85 100))
        (seven_day_overage_included . ,(agent-shell-usage-limits-test--window 0.99 100)))))
    (should (eq 'warning (agent-shell-usage-limits--face)))
    (let ((agent-shell-usage-limits-mode-line-windows '(five_hour)))
      (should (null (agent-shell-usage-limits--face))))
    (let ((agent-shell-usage-limits-mode-line-windows
           '(five_hour seven_day seven_day_overage_included)))
      (should (eq 'error (agent-shell-usage-limits--face))))))

(ert-deftest agent-shell-usage-limits-test-details ()
  "Details list every window, mark stale ones and passed resets."
  (agent-shell-usage-limits-test--with-defaults
    (should (string-prefix-p "Claude Code usage limits: waiting"
                             (agent-shell-usage-limits-details)))
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((seven_day_overage_included . ,(agent-shell-usage-limits-test--window 0.61 1000)))))
    ;; Pretend the Fable window arrived two minutes before the next update.
    (cl-decf (plist-get (alist-get 'seven_day_overage_included agent-shell-usage-limits-windows)
                        :received)
             120)
    (agent-shell-usage-limits--on-usage-update
     (agent-shell-usage-limits-test--event
      `((five_hour . ,(agent-shell-usage-limits-test--window 0.07 100))
        (seven_day . ,(agent-shell-usage-limits-test--window 0.4 -10)))))
    (let ((lines (split-string (agent-shell-usage-limits-details) "\n")))
      (should (= 4 (length lines)))
      (should (string-prefix-p "Claude Code usage limits (received " (nth 0 lines)))
      ;; Labels are padded to the widest one, "Fable".
      (should (string-prefix-p "5h       7%  resets " (nth 1 lines)))
      (should (string-match-p "(reset passed)" (nth 2 lines)))
      (should (string-prefix-p "Fable   61%  resets " (nth 3 lines)))
      (should (string-match-p "  as of [0-9/]+ [0-9:]+$" (nth 3 lines)))
      (should-not (string-match-p "as of" (nth 1 lines))))))

(provide 'agent-shell-usage-limits-test)
;;; agent-shell-usage-limits-test.el ends here
