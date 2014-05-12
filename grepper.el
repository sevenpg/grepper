(defvar grepper-buffer-name "*grepper result*")
(defvar grepper-cat-path "/bin/cat")

(defun grepper--read-pattern ()
  (let* ((default (thing-at-point 'symbol))
         (pattern (read-string (format "Pattern (default %s): " default))))
    (if (eq (length pattern) 0)
        default
      pattern)))

(defun grepper--filter (proc output)
  (with-current-buffer (process-buffer proc)
    (let ((lines (split-string output "[\r\n]+" t))
          parts)
      (dolist (line lines)
        (setq parts (split-string line "[ \t]*:[ \t]*"))
        (insert (concat (mapconcat 'identity parts "\t") "\n"))))))

(defun grepper--sentinel (proc status)
  (if (equal status "finished\n")
      (pop-to-buffer (process-buffer proc))))

(defmacro grepper--with-cat-pager (&rest body)
  `(let ((pager (make-symbol "pager")))
     (setq pager (getenv "PAGER"))
     (setenv "PAGER" grepper-cat-path)
     (unwind-protect
        ,@body
       (setenv "PAGER" pager))))

(defun grepper (pattern)
  (interactive (list (grepper--read-pattern)))
  (let ((buffer (get-buffer-create grepper-buffer-name))
        proc)
    (with-current-buffer buffer
      (erase-buffer))
    (grepper--with-cat-pager
      (setq proc (start-process "grepper" buffer "git" "grep" "-n" (shell-quote-argument pattern))))
    (set-process-sentinel proc 'grepper--sentinel)
    (set-process-filter proc 'grepper--filter)))
