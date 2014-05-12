(defvar grepper-buffer "*grepper result*")

(defun grepper--read-pattern ()
  (let* ((default (thing-at-point 'symbol))
         (pattern (read-string (format "Pattern (default %s): " default))))
    (if (eq (length pattern) 0)
        default
      pattern)))

(defun grepper (pattern)
  (interactive (list (grepper--read-pattern)))
  (let ((buffer (get-buffer-create grepper-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process "git" nil buffer nil "grep" "-n" pattern))
    (pop-to-buffer buffer)))
