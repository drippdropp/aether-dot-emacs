;;; ui.el --- User Interface & Visual Elements
;;
;; Author     : Daniel Marvin <daniel@nullmap.org>
;; Created    : Wed Oct 28, 2020
;; Updated    : Thu Jan 07, 2020
;; Version    : 1.0
;; Keywords   : initialization, configuration, ui, user-interface

;; Copyright (C) 2020 by Daniel Marvin
;;

;; This file is not part of GNU Emacs and is licensed differently.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

;;(use-package sanityinc-solarized-light-theme
(use-package abyss-theme
  :if window-system
  :ensure t
  :init
;;  (load-theme 'sanityinc-solarized-light t)
  (load-theme 'arc-dark t)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (when (member "Fira Mono for Powerline" (font-family-list))
    (set-face-attribute 'default nil
			:family "Fira Mono for Powerline"
			:height 130))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol
        	      (font-spec :family "Noto Color Emoji")
        	      nil 'prepend))
  (setq inhibit-compacting-font-caches t)
  (set-face-background 'fringe (face-background 'default))
  (fringe-mode '(6 . 1))
  (set-face-background 'line-number (face-background 'default))
  (setq window-divider-default-bottom-width 8
	window-divider-default-places t
	window-divider-default-right-width 1)
  (let ((color (face-background 'mode-line)))
    (dolist
	(face '(window-divider-first-pixel
		window-divider-last-pixel
		window-divider))
      (set-face-foreground face color)))
  (window-divider-mode 1)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha 90))
  (add-to-list 'default-frame-alist '(alpha . 90))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 140))
  :config
  (set-face-attribute 'mode-line nil
                    :family "Iosevka Fixed"
                    :foundry "nil"
                    :width 'condensed
                    :height 140
                    :weight 'Regular
                    :slant 'normal
                    :box '(:line-width 1 :color "#798ea1")
                    :overline nil
                    :underline nil
                    :inverse-video nil
                    :foreground "#cccccc"
                    :background "#222222")
  (set-face-attribute 'mode-line-inactive nil
                    :family "Iosevka Fixed"
                    :foundry "nil"
                    :width 'condensed
                    :height 140
                    :weight 'Regular
                    :slant 'normal
                    :box '(:line-width 1 :color "#565063")
                    :inverse-video nil
                    :foreground "#565063"
                    :background "black")
  )

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package display-line-numbers
  :defer t
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (progn
              (setq prettify-symbols-unprettify-at-point 'right-edge)
              (prettify-symbols-mode))))

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background "black" :foreground "white"))))
   `(company-scrollbar-bg ((t (:background "black" :foreground "#33CCFF"))))
   `(company-scrollbar-fg ((t (:background "#455a64"))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; additional settings and configuration

(size-indication-mode t)
(tooltip-mode -1)
(show-paren-mode 1)

(global-hl-line-mode 1)
(lighten-hl-background 10)

(darken-hl-foreground 10)
(set-face-foreground 'highlight nil)
(set-face-foreground 'hl-line nil)
(set-face-attribute 'region nil
		    :background "#1D3642"
		    )
(global-visual-line-mode +1)
(diminish 'visual-line-mode)

(setq ns-use-srgb-colorspace t)
(setq mac-allow-anti-aliasing t)
(setq ns-use-proxy-icon nil)

(setq frame-inhibit-implied-resize t)

(setq indicate-empty-lines t)
(setq indent-tabs-mode t)

(setq-default fill-column 120)
(setq-default cursor-type 'bar)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t)

(blink-cursor-mode nil)
(setq-default cursor-type 'box)


(unless (bound-and-true-p linum-mode)
  (set-window-margins nil 1)
  (setq left-fringe-width 1)
  (setq visual-line-fringe-indicators
	'(left-curly-arrow right-curly-arrow))
  )

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; additional hooks

(dolist (mode '(emacs-lisp-mode-hook
                tuareg-mode-hook
                markdown-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                inferior-ruby-mode-hook
                haskell-mode-hook
                julia-mode-hook))
  (add-hook mode
            (lambda ()
              (setq show-trailing-whitespace t))))

;;(add-hook 'prog-mode-hook 'aether-set-fw-face-in-buffer)
(add-hook 'conf-mode-hook 'aether-set-fw-face-in-buffer)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

(provide 'aether-ui)
;;; end of ui.el
