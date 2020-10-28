;;; init.el --- Initialization
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

;; aether repository source information
(defvar aether-git-repo "https://github.com/drippdropp/aether-dot-emacs.git")
(defvar aether-web-address "https://aether.solelliptika.com")

;; directory structure for aether emacs
(defvar aether-dir (file-name-directory load-file-name))
;;
(defvar aether-kernel-dir
  (expand-file-name "kernel" aether-dir)
  "Aether main codebase directory")
(unless (memq aether-kernel-dir load-path) t
	(add-to-list 'load-path aether-kernel-dir))
;;
(defvar aether-modules-dir
  (expand-file-name "modules" aether-dir)
  "Aether modules directory")
(unless (memq aether-modules-dir load-path) t
	(add-to-list 'load-path aether-modules-dir))
;; 
(defvar aether-personal-dir
  (expand-file-name "personal" aether-dir)
  "Aether personal directory")
;; create `aether-personal-dir' if it doesn't exist.
(unless (file-exists-p aether-personal-dir) t 
	(make-directory aether-personal-dir))
;; add `aether-personal-dir' to the load-path if not already present.
(unless (memq aether-personal-dir load-path) t 
	(add-to-list 'load-path aether-personal-dir))

;; define the custom file as `aether-custom.el'
(setq custom-file (expand-file-name "aether-custom.el" aether-personal-dir))
;; create the custom file if it doesn't currently exist.
(unless (file-exists-p custom-file) t (write-region "" nil custom-file))

;; set rules for backups and auto-saving
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

(load-file (expand-file-name "user.el" aether-modules-dir))

;; bootstrap package loads and initializers as well as helper functions
(load-file (expand-file-name "packages.el" aether-kernel-dir))
(load-file (expand-file-name "helpers.el" aether-kernel-dir))

(load-file (expand-file-name "base.el" aether-kernel-dir))
(load-file (expand-file-name "ui.el" aether-kernel-dir))
(load-file (expand-file-name "editor.el" aether-kernel-dir))

(setq confirm-kill-emacs #'yes-or-no-p)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

; (add-hook 'minibuffer-setup-hook #'aether-gc-defer)
; (add-hook 'minibuffer-exit-hook #'aether-gc-restore)

;; binding to create an empty buffer
(global-set-key (kbd "<f7>") 'aether-emacs-new-empty-buffer)

(load-file (expand-file-name "company.el" aether-modules-dir))
(load-file (expand-file-name "git.el" aether-modules-dir))
(load-file (expand-file-name "ruby.el" aether-modules-dir))
(load-file (expand-file-name "clojure.el" aether-modules-dir))
(load-file (expand-file-name "lisp.el" aether-modules-dir))
(load-file (expand-file-name "markdown.el" aether-modules-dir))
(load-file (expand-file-name "org.el" aether-modules-dir))
(load-file (expand-file-name "haskell.el" aether-modules-dir))
(load-file (expand-file-name "julia.el" aether-modules-dir))
(load-file (expand-file-name "c-cpp.el" aether-modules-dir))


;; load user custom configuration overrides
(when (file-exists-p custom-file) (load custom-file 'noerror))

;;; end of init.el