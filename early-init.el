;;; early-init.el --- Early Initialization
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

;; Remove menubar, toolbar, and scrollbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set text and background color while initializing.
(set-face-foreground 'default "#aeaeae")
(set-face-background 'default "#111111")

(setq-default apropos-do-all t)

;;
(defvar aether-file-name-handler-alist file-name-handler-alist
  "Temporary storage for `file-name-handler-alist' during startup.")
(defun aether-revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist' to its default value after startup."
  (setq file-name-handler-alist aether-file-name-handler-alist))

(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'aether-revert-file-name-handler-alist)

(load-file
	(expand-file-name "kernel/aether-helpers.el" 
	(file-name-directory load-file-name)))

(aether-gc-defer)
(add-hook 'emacs-startup-hook #'aether-gc-restore)

;;; end of early-init.el
