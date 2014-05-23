(defvar grepper-buffer-name "*grepper result*")
(defvar grepper-tmp-buffer-name " *grepper tmp*")

(defvar grepper-git-grep-options "-n --full-name")
(defvar grepper-gtags-options "-x --path-style through")

(defvar grepper-program-list '("git-grep" "gtags"))
(defvar grepper-last-program "git-grep")

(defun grepper (program pattern)
  (interactive (list (completing-read "program: " grepper-program-list nil t grepper-last-program)
                     (grepper-read-pattern)))
  (setq grepper-last-program program)
  (let* ((cmd (grepper-command pattern))
         grep-result
         parse-results)
    (setq grep-result (grepper-get-grep-result cmd))
    (setq parse-results (grepper-parse grep-result))
    (grepper-show-parse-results parse-results)))


(defun grepper-read-pattern ()
  (let* ((default (thing-at-point 'symbol))
         (pattern (read-string (format "Pattern (default %s): " default))))
    (if (eq (length pattern) 0)
        default
      pattern)))

(defun grepper-command (pattern)
  (funcall (intern (concat "grepper-" grepper-last-program "-command")) pattern))

(defun grepper-parse (grep-result)
  (funcall (intern (concat "grepper-" grepper-last-program "-parse")) grep-result))

(defun grepper-root-dir ()
  (funcall (intern (concat "grepper-" grepper-last-program "-root-dir"))))


(defun grepper-get-grep-result (cmd)
  (let (result)
    (with-current-buffer (get-buffer-create grepper-tmp-buffer-name)
      (erase-buffer)
      (call-process-shell-command cmd nil (current-buffer))
      (setq result (buffer-substring (point-min) (point-max)))
      (kill-buffer))
    result))

(defun grepper-show-parse-results (results)
  (if (eq (length results) 1)
      (let ((result (nth 0 results)))
        (cd (grepper-root-dir))
        (grepper-go-to-fileline (plist-get result :file)
                                 (plist-get result :line)
                                 nil))
    (with-current-buffer (get-buffer-create grepper-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((max-fileline 0)
            fileline)
        (dolist (result results)
          (setq fileline (format "%s:%s" (plist-get result :file) (plist-get result :line)))
          (plist-put result :fileline fileline)
          (setq max-fileline (max max-fileline (length fileline))))
        (dolist (result results)
          (insert (plist-get result :fileline))
          (move-to-column max-fileline t)
          (insert (format "  %s\n" (plist-get result :desc)))))
      (goto-char (point-min))
      (cd (grepper-root-dir))
      (grepper-result-mode)
      (pop-to-buffer (current-buffer)))))


;;
;; result-mode
;;

(defvar grepper-result-mode-map nil)
(unless grepper-result-mode-map
  (setq grepper-result-mode-map (make-sparse-keymap))
  (define-key grepper-result-mode-map "n" 'next-line)
  (define-key grepper-result-mode-map "p" 'previous-line)
  (define-key grepper-result-mode-map "q" 'kill-buffer)
  (define-key grepper-result-mode-map (kbd "RET") 'grepper-go-to-file))

(defun grepper-result-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'grepper-result-mode
        mode-name "grepper result"
        buffer-read-only t)
  (set-buffer-modified-p nil)
  (use-local-map grepper-result-mode-map)
  (run-hooks 'grepper-result-mode-hook))

(defun grepper-go-to-file ()
  (interactive)
  (let (file
        line
        beg-pos)
    (save-excursion
      (beginning-of-line)
      (setq beg-pos (point))
      (skip-chars-forward "^:")
      (setq file (buffer-substring beg-pos (point)))
      (forward-char)
      (setq beg-pos (point))
      (skip-chars-forward "[0-9]")
      (setq line (buffer-substring beg-pos (point))))
    (grepper-go-to-fileline file line t)))

(defun grepper-go-to-fileline (file line other-window)
  (if other-window
      (find-file-other-window file)
    (find-file file))
  (goto-char (point-min))
  (forward-line (1- (string-to-number line))))


;;
;; git-grep
;;

(defun grepper-git-grep-command (pattern)
  (format "git grep %s \"%s\" %s"
          grepper-git-grep-options
          (shell-quote-argument pattern)
          "`git rev-parse --show-toplevel`"))

(defun grepper-git-grep-parse (result)
  (let ((lines (split-string result "[\r\n]+" t))
        parts)
    (mapcar (lambda (line)
              (setq parts (split-string line "[ \t]*:[ \t]*"))
              (list :file (nth 0 parts)
                    :line (nth 1 parts)
                    :desc (nth 2 parts)))
            lines)))

(defun grepper-git-grep-root-dir ()
  (replace-regexp-in-string "[\r\n]+$" "" (shell-command-to-string "git rev-parse -show-toplevel")))


;;
;; gtags
;;

(defun grepper-gtags-command (pattern)
  (format "global %s \"%s\""
          grepper-gtags-options
          (shell-quote-argument pattern)))

(defun grepper-gtags-parse (result)
  (let ((lines (split-string result "[\r\n+]" t)))
    (mapcar (lambda (line)
              (unless (string-match "[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)$" line)
                (error "Invalid line: %s" line))
              (list :file (match-string 2 line)
                    :line (match-string 1 line)
                    :desc (match-string 3 line)))
            lines)))

(defun grepper-gtags-root-dir ()
  (replace-regexp-in-string "[\r\n]+$" "" (shell-command-to-string "git rev-parse --show-toplevel")))
