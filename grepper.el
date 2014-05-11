(defvar grepper-buffer "*grepper result*")

(defun grepper (pattern)
  (interactive "sPattern: ")
  (let ((buffer (get-buffer-create grepper-buffer)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (call-process "git" nil buffer nil "grep" "-n" pattern)
    (pop-to-buffer buffer)))
