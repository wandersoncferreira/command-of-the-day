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
  (cadr (mapcar 'key-description
		(where-is-internal command))))

(defun command-of-the-day-tracked-command? (command-binding)
  (let* ((todays-name (intern (downcase (format-time-string "%A" (current-time)))))
	 (command-list (plist-get command-of-the-day-practice-schedule todays-name))
	 (command-list (if command-list command-list (plist-get command-of-the-day-practice-schedule :any))))
    (member command-binding command-list)))

(defun command-of-the-day-user-message (command-binding counter)
  (cond
   ((< counter command-of-the-day-apprentice-level)
    (message "Apprentice: You hit %s %s times today" command-binding counter))
   
   ((< counter command-of-the-day-journeyman-level)
    (message "Journeyman: You hit %s %s times today" command-binding counter))
   
   ((t (message "Master: You hit %s %s times today" command-binding counter)))))

(defun command-of-the-day-post-command-hook ()
  (let* ((command real-last-command) count
	 (command-binding (command-of-the-day-where-is? command)))
    (when (and command (symbolp command) (command-of-the-day-tracked-command? command-binding))
      (setq count (gethash command-binding command-of-the-day-table))
      (setq count (if count (1+ count) 1))
      (command-of-the-day-user-message command-binding count)
      (puthash command-binding count command-of-the-day-table))))

;;;###autoload
(define-minor-mode command-of-the-day-mode
  "Keep track of your commands today."
  :global t
  :lighter " COTD"
  :keymap nil
  :group 'command-of-the-day
  (if command-of-the-day-mode
      (add-hook 'post-command-hook 'command-of-the-day-post-command-hook)
    (remove-hook 'post-command-hook 'command-of-the-day-post-command-hook)))

(provide 'command-of-the-day)

