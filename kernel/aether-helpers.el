;;; helpers.el --- Helper Functions
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

(defun aether-gc-defer ()
  "Raise garbage collection threshold to maximum to speed up load."
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6))

(defun aether-gc-restore ()
  "Restore garbage collection defaults."
  (setq gc-cons-threshold (* 128 1024 1024)
	gc-cons-percentage 0.1))

(defun aether-duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (progn 
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(defun aether-get-selected-text (start end)
  (interactive "r")
  (let ((region-text (buffer-substring start end)))
    (message region-text)))

(defun aether-emacs-new-empty-buffer ()
  (interactive)
  (let ((new-buf (generate-new-buffer "untitled")))
    (switch-to-buffer new-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

;;; retrieve list of files in directory
(defun aether-files-in-directory (directory)
  "Generate list of all files in a given DIRECTORY."
  (interactive)
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (if (equal "." (substring (car (car current-directory-list)) -1)) ()
          (setq el-files-list (append (files-in-below-directory
                                       (car (car current-directory-list)))
                                      el-files-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))

(defun aether-create-dir-unless-exists (target-dir)
  "Create `target-dir' if it does not currently exist."
  (unless (file-exists-p target-dir)
    (make-directory target-dir))
  )

(defun aether-create-file-unless-exists (target-file)
  "Create `target-file' if it does not currently exist."
  (unless (file-exists-p target-file)
    (write-region "" nil target-file))
  )

(defun aether-include-load-path (target-dir)
  "Add directory to load path if not previously addede."
  (unless (memq target-dir load-path) t
	  (add-to-list 'load-path target-dir))
  )

;;; package management helper functions

(defun package--save-selected-packages (&rest opt)
  "Corrects undesired behavior, OPT is unused."
  nil)

;;; font/ui related macros and functions

(defun lighten-hl-background (amt)
  (set-face-background 'hl-line (color-lighten-name (face-background 'default) amt)))

(defun darken-hl-foreground (amt)
  (set-face-foreground 'hl-line (color-darken-name (face-foreground 'default) amt)))

(defmacro aether-emacs-with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun aether-emacs-disable-number-and-visual-line ()
  (visual-line-mode 0)
  (if (version< emacs-version "26.1")
      (linum-mode 0)
    (display-line-numbers-mode 0)))

(defmacro aether-set-md-fw-font-attr (face)
  "Set FACE to use Noto Mono for Powerline-14"
  `(when (member "Noto Mono for Powerline" (font-family-list))
     (set-face-attribute ,face nil
                         :font "Noto Mono for Powerline"
                         :height 140)))

(defmacro aether-set-lg-fw-font-attr (face)
  "Set FACE to use Noto Mono for Powerline-18"
  `(when (member "Noto Mono for Powerline" (font-family-list))
     (set-face-attribute ,face nil
                         :font "Noto Mono for Powerline"
                         :height 180)))

(defmacro aether-set-md-serif-font-attr (face)
  "Set FACE to use Georgia Regular-14"
  `(when (member "Georgia" (font-family-list))
     (set-face-attribute ,face nil
                         :font "Georgia"
                         :height 240)))

(defun aether-set-fw-face-in-buffer ()
   "Set fixed width font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Noto Mono for Powerline" :height 140 :width semi-condensed))
   (buffer-face-mode))

(defun aether-set-serif-face-in-buffer ()
   "Set fixed width font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Georgia" :height 140))
   (buffer-face-mode))

(provide 'aether-helpers)
;;; end of ui.el
