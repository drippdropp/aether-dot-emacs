;;; init.el --- -*- lexical-binding: t -*-
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

(load-file (expand-file-name "aether-user.el" aether-modules-dir))

;; bootstrap package loads and initializers as well as helper functions
(load-file (expand-file-name "aether-packages.el" aether-kernel-dir))
(load-file (expand-file-name "aether-helpers.el" aether-kernel-dir))

(load-file (expand-file-name "aether-base.el" aether-kernel-dir))
(load-file (expand-file-name "aether-editor.el" aether-kernel-dir))
(load-file (expand-file-name "aether-ui.el" aether-kernel-dir))
(load-file (expand-file-name "aether-dashboard.el" aether-kernel-dir))

(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

(setq confirm-kill-emacs #'yes-or-no-p)
(setq ring-bell-function 'ignore)

;; load modules
(load-file (expand-file-name "aether-avy.el" aether-modules-dir))
(load-file (expand-file-name "aether-c-cpp.el" aether-modules-dir))
;;(load-file (expand-file-name "aether-clojure.el" aether-modules-dir))
(load-file (expand-file-name "aether-company.el" aether-modules-dir))
(load-file (expand-file-name "aether-dired.el" aether-modules-dir))
(load-file (expand-file-name "aether-doomline.el" aether-modules-dir))
(load-file (expand-file-name "aether-git.el" aether-modules-dir))
(load-file (expand-file-name "aether-haskell.el" aether-modules-dir))
(load-file (expand-file-name "aether-http.el" aether-modules-dir))
(load-file (expand-file-name "aether-ivy.el" aether-modules-dir))
(load-file (expand-file-name "aether-julia.el" aether-modules-dir))
(load-file (expand-file-name "aether-latex.el" aether-modules-dir))
(load-file (expand-file-name "aether-lisp.el" aether-modules-dir))
(load-file (expand-file-name "aether-markdown.el" aether-modules-dir))
(load-file (expand-file-name "aether-nameframe.el" aether-modules-dir))
(load-file (expand-file-name "aether-neotree.el" aether-modules-dir))
(load-file (expand-file-name "aether-ocaml.el" aether-modules-dir))
(load-file (expand-file-name "aether-org.el" aether-modules-dir))
(load-file (expand-file-name "aether-osx.el" aether-modules-dir))
(load-file (expand-file-name "aether-projectile.el" aether-modules-dir))
(load-file (expand-file-name "aether-ruby.el" aether-modules-dir))
(load-file (expand-file-name "aether-typo.el" aether-modules-dir))
(load-file (expand-file-name "aether-yasnippet.el" aether-modules-dir))

(load-file (expand-file-name "aether-bindings.el" aether-kernel-dir))

;; load user custom configuration overrides
(when (file-exists-p custom-file) (load custom-file 'noerror))

(provide 'init)
;;; end of init.el

(put 'dired-find-alternate-file 'disabled nil)

(put 'upcase-region 'disabled nil)
