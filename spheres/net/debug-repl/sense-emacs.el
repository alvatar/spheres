;; Remote Gambit debugging

(require 'cmuscheme)

(defun get-sense-process ()
  (get-process "sense"))

(defun sense-receive-command (proc string)
  (message string)
  (sense-display-msg string)
  (if (string-match "EMACS-EVAL:" string)
      (let ((processed-string (substring string (match-end 0))))
        (message (concat "Process form:\n" processed-string))
        (sense-run-command processed-string))))

(defun sense-display-msg (str)
  (with-current-buffer (process-buffer (get-sense-process))
    (let ((move (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert str)
        (set-marker (process-mark proc) (point)))
      (if move (goto-char (process-mark proc))))))

(defun sense-run-command (cmd)
  ;; (message (concat "EVAL: " cmd))
  ;; Todo, don't do this
  (eval (read cmd)))

(defun sense-pump-buffer-name (id)
  (concat "sense-pump-" (number-to-string id)))

;; Run when SchemeSpheres' debug-server sends the request
(defun sense-open-client (id port)
  (let* ((name (sense-pump-buffer-name id))
         (buffer-name (concat "*" name "*"))
         (master (= id 2)))
    (if master (sense-cleanup-processes))
    (save-current-buffer
      (message "Running pump...")
      (set-buffer (make-comint name "/usr/local/Gambit/bin/sense-pump"
                               nil (number-to-string port)))
      (inferior-scheme-mode)
      (if master (sense-make-scheme-buffer))
      (switch-to-buffer (current-buffer)))))

(defun sense-make-scheme-buffer ()
  (interactive)
  (setq scheme-buffer (current-buffer)))

(defun sense-any-buffersp ()
  (setq res nil)
  (dolist (buf (buffer-list) res)
    (setq res
          (or res
              (string-match "sense-pump" (buffer-name buf))))))

(defun sense-cleanup-processes ()
  (interactive)
  (dolist (buf (buffer-list))
    (if (string-match "sense-pump" (buffer-name buf))
        (progn
          (if (get-buffer-process buf)
              (kill-process (get-buffer-process buf)))))))

(defun sense-cleanup ()
  (interactive)
  (dolist (buf (buffer-list))
    (if (string-match "sense-pump" (buffer-name buf))
        (progn
          (if (get-buffer-process buf)
              (kill-process (get-buffer-process buf)))
          (kill-buffer buf)
          (message "killing buffer...")))))

(defun sense-kill ()
  (interactive)
  (sense-cleanup)
  (let ((proc (get-process "sense")))
    (if proc (kill-process proc)))
  (let ((buf (get-buffer "*sense-messages*")))
    (if buf (kill-buffer buf))))

(defun sense ()
  (interactive)
  (sense-kill)
  (let ((proc (start-process "sense"
                             "*sense-messages*"
                             "/usr/local/Gambit/bin/sense-emacs"
                             "20000")))
    (set-process-filter proc 'sense-receive-command)
    (message "Sense server started...")))
