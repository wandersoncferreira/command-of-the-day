;;; command-of-the-day.el --- Simple tracker of key bindings pressing

;; Author: Wanderson Ferreira <wanderson.ferreira@protonmail.com>
;; URL: https://github.com/wandersoncferreira/command-of-the-day
;; Keywords: keys, bindings
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sharp your Emacs-fu

;;; Code:

(defgroup command-of-the-day nil
  "Customization group for Command of the day."
  :package-version '(command-of-the-day . "1.0")
  :group 'local
  :prefix "command-of-the-day")

(defcustom command-of-the-day-apprentice-level 3
  "How many hits you need to leave Apprentice stage."
  :group 'command-of-the-day
  :type 'integer)

(defcustom command-of-the-day-journeyman-level 10
  "How many hits you need to leave Journeyman stage."
  :group 'command-of-the-day
  :type 'integer)

(defcustom command-of-the-day-user-feedback-time 3
  "Amount of seconds that we will keep the feedback message in the modeline."
  :group 'command-of-the-day
  :type 'integer)

(defvar command-of-the-day-practice-schedule (list)
  "Hold the training schedule to practice your key bindings.
This is a plist with the name of the day and the bindings to be followed.
e.g. (list :monday (list 'C-c C-k') :any (list 'C-v'))

:any keyword is used to match no particular day, preference will
be given to explicit day named.")

(defvar command-of-the-day-table (make-hash-table :test 'equal)
  "Hold the counters for each track-able command.")

(defun command-of-the-day-where-is? (command)
  "Find which keybinding is associated with a command."
  (let ((sy (mapcar 'key-description (where-is-internal command))))
    (if (equalp 1 (length sy))
	(car sy)
      (cadr sy))))

(defun command-of-the-day-tracked-command? (command-binding)
  (let* ((todays-name (intern (concat ":" (downcase (format-time-string "%A" (current-time))))))
	 (base-command-list (plist-get command-of-the-day-practice-schedule :base))
	 (command-list (plist-get command-of-the-day-practice-schedule todays-name))
	 (command-list (if command-list command-list (plist-get command-of-the-day-practice-schedule :any))))
    (member command-binding (append command-list base-command-list))))
 
(defun command-of-the-day-user-feedback (text &optional buffer delay)
  "Display TEXT in BUFFER's mode line.
The text is shown for DELAY seconds (default 2), or until a user event.
So call this last in a sequence of user-visible actions."
  (message nil) ; Remove any current msg
  (with-current-buffer (or buffer  (current-buffer))
    (make-local-variable 'mode-line-format) ; Needed for Emacs 21+.
    (let ((mode-line-format  (append mode-line-format (list text))))
      (force-mode-line-update) (sit-for (or delay  2)))
    (force-mode-line-update)))

(defun command-of-the-day-user-message (command-binding counter)
  (command-of-the-day-user-feedback
   (cond
    ((< counter command-of-the-day-apprentice-level)
     (format "Apprentice: %s: %s hits" command-binding counter))
    
    ((< counter command-of-the-day-journeyman-level)
     (format "Journeyman: %s: %s hits" command-binding counter))
    
    (t (format "Master: %s: %s hits" command-binding counter)))))

(defun command-of-the-day-pre-command-hook ()
  (let* ((command real-last-command) count
	 (command-binding (command-of-the-day-where-is? command)))
    (when (and command (symbolp command) (command-of-the-day-tracked-command? command-binding))
      (setq count (gethash command-binding command-of-the-day-table))
      (setq count (if count (1+ count) 1))
      (command-of-the-day-user-message command-binding count)
      (puthash command-binding count command-of-the-day-table))))

;;;###autoload
(defun command-of-the-day-report ()
  "Report your current hits."
  (interactive)
  (let ((buf-name "*CommandOfTheDay"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (get-buffer-create buf-name)
    (with-current-buffer buf-name
      (insert (json-encode command-of-the-day-table))
      (json-pretty-print-buffer))
    (switch-to-buffer-other-window buf-name)
    (special-mode)))

;;;###autoload
(define-minor-mode command-of-the-day-mode
  "Keep track of your commands today."
  :global t
  :lighter " COTD"
  :keymap nil
  :group 'command-of-the-day
  (if command-of-the-day-mode
      (add-hook 'pre-command-hook 'command-of-the-day-pre-command-hook)
    (remove-hook 'pre-command-hook 'command-of-the-day-pre-command-hook)))

(provide 'command-of-the-day)
;;; command-of-the-day.el ends here
