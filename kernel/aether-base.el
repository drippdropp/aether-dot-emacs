;;; base.el --- Aether Base
;;
;; Author     : Daniel Marvin <daniel@nullmap.org>
;; Created    : Wed Oct 28, 2020
;; Version    : 0.1
;; Keywords   : init

;; Copyright (C) 2020 by Daniel Marvin
;;

;; This file is not part of GNU Emacs and is licensed differently.

;;; Commentary:

;;

;;; Code:

(defun aether-reload-emacs-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(defun aether-edit-emacs-init ()
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el")
  )

(defun aether-open-kernel-dired ()
  (interactive)
  (when (boundp 'aether-kernel-dir) t
        (counsel-dired-jump aether-kernel-dir)
        )
  )

(defun aether-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
   If there is no region given, the current line will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg))))
  )

(defun aether-move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start)))
  )

(defun aether-move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (aether-move-region start end (if (null n) -1 (- n)))
  )

(defun aether-move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (aether-move-region start end (if (null n) 1 n))
  )

;; When a key sequence is initiated, a menu will display addition
;;   possible key sequences.
(use-package which-key
  :ensure t
  :defer t
  :custom (echo-keystrokes 0.00000001)
  :hook (after-init . which-key-mode))

;; For searching.
(use-package counsel
  :ensure t
  :defer t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (string= system-type "darwin")
    (exec-path-from-shell-initialize))
  (let ((path (shell-command-to-string ". ~/.zshrc; . ~/.zshenv; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(use-package system-packages
  :ensure t
  :config
  (when (string= system-type "darwin")
    (setq system-packages-use-sudo nil)
    (setq system-packages-package-manager 'brew)))

(use-package paren
  :defer t
  :init
  (show-paren-mode 1)
  :custom-face (show-paren-match ((t (:weight extra-bold :underline t  ))))
  :custom ((show-paren-style 'parentheses)
	   (show-paren-delay 0.000001)))

(use-package rainbow-mode
  :if window-system
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons :ensure t)

(use-package all-the-icons-ivy
  :after 'all-the-icons
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf))
  )

(add-hook 'after-init-hook #'all-the-icons-ivy-setup)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package gnutls
  :defer t
  :ensure-system-package (gnutls-cli . "brew install gnutls")
  :config
  (setq tls-program '("gnutls-cli -p %p %h")
        imap-sdsl-program '("gnutls-cli -p %p %s")
        smtpmail-stream-type 'starttls))

;; settings
(setq use-dialog-box nil
      use-file-dialog nil)

(setq ivy-use-virtual-buffers t)

(setq enable-recursive-minibuffers t)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t)

;; additional hooks

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(provide 'aether-base)
;;; end of base.el
