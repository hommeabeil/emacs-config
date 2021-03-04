(defun clang-format-bindings ()
  (define-key c++-mode-map (kbd "M-q") 'clang-format-buffer))

(defun add-mock-path (path)
  (concat path "../../test/mock"))

(defun add-file-name (path)
  (concat path "/" "Mock" (file-name-nondirectory (buffer-name))))

(defun expected-mock-file ()
  (let ((relatif-path (thread-first (buffer-name)
                        (add-mock-path)
                        (add-file-name))))
    (expand-file-name relatif-path)))

(defun mock-it (activate)
  (let* ((dest (helm-read-file-name "MockFile"
                                   :initial-input (expected-mock-file)))
         (dest-file (file-name-nondirectory dest))
         (file-to-mock (buffer-file-name))
         (file-was-existing (file-exists-p dest)))
    (if file-was-existing
        (if activate
            (find-file-other-window dest)
          (find-file dest)))
    (end-of-buffer)
    (call-process "gmock_gen.py" nil dest-file nil file-to-mock)
    (if (not file-was-existing)
        (if activate
            (switch-to-buffer-other-window dest-file)
          (switch-to-buffer dest-file)))
    (write-file dest)))

(defun mock-this-file ()
  (interactive)
  (mock-it nil))

(defun mock-this-file-s ()
  (interactive)
  (mock-it t))

(defun switch-between-test ()
    (interactive)
    (projectile-find-other-file t))

(defun copy-file-name ()
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new (file-name-nondirectory file-name)))
      (error "Buffer not visiting a file"))))

(defun break-points-gdb ()
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name)))
        (line (number-to-string (line-number-at-pos nil))))
    (message (kill-new (concat "b " file-name ":" line)))))

(defun my/dired-jump (close-current &optional other-window file-name)
  "Jump to Dired buffer corresponding to current buffer.
If in a file, Dired the current directory and move to file's line.
If in Dired already, pop up a level and goto old directory's line.
In case the proper Dired file line cannot be found, refresh the dired
buffer and try again.
When OTHER-WINDOW is non-nil, jump to Dired buffer in other window.
Interactively with prefix argument, read FILE-NAME and
move to its line in dired."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Jump to Dired file: "))))
  (if (bound-and-true-p tar-subfile-mode)
      (switch-to-buffer tar-superior-buffer)
    (let* ((file (or file-name buffer-file-name))
           (dir (if file (file-name-directory file) default-directory))
           (orig (current-buffer)))
      (if (and (eq major-mode 'dired-mode) (null file-name))
          (progn
            (setq dir (dired-current-directory))
            (dired-up-directory other-window)
            (unless (dired-goto-file dir)
              ;; refresh and try again
              (dired-insert-subdir (file-name-directory dir))
              (dired-goto-file dir)))
        (if other-window
            (dired-other-window dir)
          (dired dir))
        (if close-current
            (kill-buffer orig))
        (if file
            (or (dired-goto-file file)
                ;; refresh and try again
                (progn
                  (dired-insert-subdir (file-name-directory file))
                  (dired-goto-file file))
                ;; Toggle omitting, if it is on, and try again.
                (when dired-omit-mode
                  (dired-omit-mode)
                  (dired-goto-file file))))))))

(defun my/dired-jump-close ()
  (interactive)
  (my/dired-jump t))

(defun sourcer-indent-buffer ()
  (interactive)
  (let* ((file (buffer-file-name))
         (orig-windows (get-buffer-window-list (current-buffer)))
         (orig-window-starts (mapcar #'window-start orig-windows))
         (orig-point (point)))
    (unwind-protect
        (call-process-region (point-min) (point-max)
                             "/home/jbouchard/bin/erlang_ls" t t nil "-i" file "--stdout")
      (dotimes (index (length orig-windows))
        (set-window-start (nth index orig-windows)
                          (nth index orig-window-starts)))
      (goto-char orig-point))))

(defun copy-module ()
    (interactive)
  (let* ((file (buffer-file-name))
         (module (file-name-base file)))
    (kill-new module)))

(defun add-my-dirs ()
  (let* ((current-file (symbol-file 'add-my-dirs))
         (current-dir (file-name-directory current-file))
         (snippet-dir (concat current-dir ".yas.d/")))
    (setq yas-snippet-dirs (append `(,snippet-dir) yas-snippet-dirs))))

(defun add-my-keys
    ()
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-/") 'yas-expand)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "mf" 'mock-this-file)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "mF" 'mock-this-file-s)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "gt" 'switch-between-test)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "bb" 'break-points-gdb)
  (spacemacs/set-leader-keys-for-major-mode 'erlang-mode "M-q" 'sourcer-indent-buffer)
  (spacemacs/set-leader-keys "fY" 'copy-file-name)
  (spacemacs/set-leader-keys "bB" 'ibuffer)
  (spacemacs/set-leader-keys "fS" 'save-some-buffers)
  (spacemacs/set-leader-keys "aoC" 'org-cycle-agenda-files)
  (spacemacs/set-leader-keys "gp" 'magit-pull)
  (spacemacs/set-leader-keys "gCc" 'magit-checkout)
  (spacemacs/set-leader-keys "gCC" 'magit-branch-and-checkout)
  (define-key evil-normal-state-map (kbd "-") 'my/dired-jump-close))

(defun add-my-hooks
    ()
  (add-hook 'c++-mode-hook 'clang-format-bindings)
  (with-eval-after-load "persp-mode-autoloads"
    (setq wg-morph-on nil) ;; switch off animation
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  )

(defun configure-erlang-dev
    ()
  (setq-default flycheck-disabled-checkers '(erlang-rebar3))
  (add-to-list 'auto-mode-alist '("\\.conf\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\rebar.config\\'" . erlang-mode))
  (add-hook 'erlang-mode-hook (lambda () (setq fill-column 120)))
  (add-hook 'erlang-mode-hook 'fci-mode)
  ;; (add-hook 'erlang-mode-hook (lambda () (define-key erlang-mode-map (kbd "M-q") 'sourcer-indent-buffer)))
  )
