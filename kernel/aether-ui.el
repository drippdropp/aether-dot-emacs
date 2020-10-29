;;; ui.el --- User Interface & Visual Elements
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

(require 'cl-lib)

(use-package boron-theme
  :if window-system
  :ensure t
  :init
  (load-theme 'boron t)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  (when (member "Noto Mono for Powerline" (font-family-list))
    (set-face-attribute 'default nil
			:font "Noto Mono for Powerline"
			:height 120))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol
		      (font-spec :family "Noto Color Emoji")
		      nil 'prepend))
  (setq inhibit-compacting-font-caches t)
  (set-face-background 'fringe (face-background 'default))
  (fringe-mode 10)
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
  )

(use-package mood-line
  :ensure t
  :defer t
  :init
  (mood-line-mode 1)
  (line-number-mode t)
  (column-number-mode t)
  (display-time-mode t)
  (display-battery-mode 1)
  :custom ((display-time-format "%a %m/%d %H:%M")
           (display-time-day-and-date t)
           (display-time-24hr-format t)))

(use-package smart-mode-line
  :ensure t
  :requires all-the-icons
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

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

;; additional settings and initalizations

(line-number-mode +1)
(column-number-mode +1)
(size-indication-mode t)
(display-time-mode +1)
(setq display-time-day-and-date t
      display-time-24hr-format t)
(tooltip-mode -1)
(show-paren-mode 1)

(global-hl-line-mode +1)
(lighten-hl-background 5)
(set-face-foreground 'highlight nil)
(set-face-foreground 'hl-line nil)

(global-visual-line-mode t)
(diminish 'visual-line-mode)

(set-face-attribute 'mode-line nil
                    :font "Roboto Mono for Powerline"
                    :height 120)

(add-to-list 'default-frame-alist '(font . "Noto Mono for Powerline-14"))


(setq ns-use-srgb-colorspace t)
(setq mac-allow-anti-aliasing t)
(setq ns-use-proxy-icon nil)

(setq frame-inhibit-implied-resize t)
(setq-default left-fringe-width nil
	      indicate-empty-lines t
	      indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default cursor-type 'bar)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t)

(blink-cursor-mode t)
(setq-default cursor-type 'box)

(setq visual-line-fringe-indicators 
	'(left-curly-arrow right-curly-arrow))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)



;; additional hooks

(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                inferior-ruby-mode-hook
                haskell-mode-hook
                julia-mode-hook))
  (add-hook mode
            (lambda ()
              (setq show-trailing-whitespace t))))

(add-hook 'prog-mode-hook 'aether-set-fw-face-in-buffer)
(add-hook 'conf-mode-hook 'aether-set-fw-face-in-buffer)
(add-hook 'text-mode-hook #'turn-on-auto-fill)


(provide 'aether-ui)
;;; end of ui.el
