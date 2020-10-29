;;; user.el --- Initialization
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

(delete-selection-mode 1)

(setq scroll-margin 0
      auto-window-vscroll nil
      scroll-preserve-screen-position 1
      scroll-conservatively most-positive-fixnum
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(use-package flyspell
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
  ;; Enable flyspell for code and enable superword mode
  (dolist (mode '(prog-mode-hook))
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

;; An Emacs minor-mode for Flycheck which colors the mode-line
;; according to the Flycheck state of the current buffer.
(use-package flycheck-color-mode-line
  :ensure t
  :defer t)

;; Flycheck errors display in tooltip
(use-package flycheck-pos-tip
  :ensure t
  :defer t)
  
(use-package markdown-mode
  :ensure t
  :defer t
  :ensure-system-package (markdown . "brew install markdown")
  :hook
  (markdown-mode . markdown-config-opts)
  :config
  (set-face-attribute 'markdown-code-face nil :background "#282C34")
  (set-face-attribute 'markdown-code-face nil :foreground "#ABB2BF"))

(use-package markdown-toc
  :ensure t
  :defer t)

(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))


(defun is-markdown-mode ()
  (interactive)
  (if (string-equal major-mode "gfm-mode")
      (message "Currently in Markdown mode"))
  )
(defun set-font-for-gfm-mode ()
  (set-face-attribute 'markdown nil
                      :family "Roboto Mono for Powerline"
                      :height 160
                      :weight "normal"))
(defun markdown-config-opts ()
  (setq show-trailing-whitespace t)
  (setq markdown-enable-math t)
  (flyspell-prog-mode)
  (superword-mode 1))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
  
(use-package multiple-cursors
  :ensure t)

(use-package display-line-numbers
  :defer t
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

(provide 'aether-editor)
