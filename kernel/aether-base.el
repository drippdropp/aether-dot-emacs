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

(use-package which-key
  :ensure t
  :defer t
  :custom (echo-keystrokes 0.00000001)
  :hook (after-init . which-key-mode))

(use-package counsel
  :ensure t
  :defer t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package ibuffer
  :defer t
  :init
  (defun aether/use-default-filter-group ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :custom ((ibuffer-saved-filter-groups
            (quote (("default"
                     ("magit" (name . "^magit.*:"))
                     ("dired" (or (mode . dired-mode)
                                  (mode . wdired-mode)))
		     ("development"
		      (or (mode . emacs-lisp-mode)
			  (mode . haskell-mode)
			  (mode . julia-mode)
			  (mode . python-mode)
			  (mode . ruby-mode)))
                     ("org"   (mode . org-mode))
                     ("term" (mode . term-mode))
                     ("emacs" (or (name . "^\\*package.*results\\*$")
                                  (name . "^\\*Shell.*Output\\*$")
                                  (name . "^\\*Compile-Log\\*$")
                                  (name . "^\\*Completions\\*$")
                                  (name . "^\\*Backtrace\\*$")
                                  (name . "^\\*dashboard\\*$")
                                  (name . "^\\*Messages\\*$")
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*info\\*$")
                                  (name . "^\\*Help\\*$")))))))
           (uniquify-buffer-name-style 'forward)
           (uniquify-after-kill-buffer-p t)
           (initial-scratch-message ""))
  :hook (ibuffer-mode . aether/use-default-filter-group)
  :bind (("C-x b" . ibuffer)
         ("C-x C-b" . nil)
         ("C-x k" . kill-this-buffer)))

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

;; (use-package avy
;;   :ensure t
;;   :defer t
;;   :bind ("M-s" . avy-goto-char))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (string= system-type "darwin")
    (exec-path-from-shell-initialize))
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
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
  :custom-face (show-paren-match
		((t (:weight extra-bold
			     :underline t
			     ))))
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

(use-package flyspell
  :if (executable-find "aspell")
  :defer t
  :config
  ;; Spell checking configuration
  (setq ispell-program-name "aspell")
  ;; Enable flyspell for text files and enable superword mode
  (dolist (mode '(text-mode-hook))
    (add-hook mode (lambda ()
                     (flyspell-mode 1)
                     (diminish 'flyspell-mode)
                     ;; Enable superword mode, useful for “snake_case”.
                     (superword-mode 1)
                     (diminish 'superword-mode)
                     )))
  (dolist (mode '(prog-mode-hook conf-mode-hook))
    (add-hook mode (lambda ()
                     (flyspell-prog-mode)
                     (diminish 'flyspell-mode)
                     ;; Enable superword mode, useful for “snake_case”.
                     (superword-mode 1)
                     (diminish 'superword-mode)
                     )))
  (global-set-key (kbd "<f8>") 'ispell-word)
  (global-set-key (kbd "M-<f8>") 'flyspell-goto-next-error)
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3]
         #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3]
         #'undefined))))

;; linter
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; put the linting message in a floating window
(use-package flycheck-posframe
  :if window-system
  :after flycheck
  :ensure t
  :defer t
  :custom ((posframe-mouse-banish nil)
           (flycheck-posframe-position 'window-bottom-left-corner))
  :hook ((flycheck-mode . flycheck-posframe-mode)
         (flycheck-posframe-mode . flycheck-posframe-configure-pretty-defaults)))

(use-package all-the-icons :ensure t)

(use-package alld-the-icons-ivy
  :after 'all-the-icodns
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

;; key bindings

(global-set-key (kbd "s-<left>") 'move-to-left-margin)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "M-<up>") #'aether-get-selected-text)


(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'aether-duplicate-line)

(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-c d") 'counsel-descbinds)

(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'other-window)
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-b"))
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-b") 'backward-word)
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<left>"))
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<f12>") 'aether-edit-emacs-init)
;;(global-set-key)

;; additional hooks

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(provide 'aether-base)
;;; end of base.el
