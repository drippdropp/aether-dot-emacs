;;; .aether-bindings.el -- Collection of all key bindings.
;;;;

;;; Code:;;
;; Author     : Daniel Marvin <daniel@nullmap.org>
;; Created    : Wed Oct 28, 2020
;; Version    : 0.1
;; Keywords   : init

;; Copyright (C) 2020, 2021 by Daniel Marvin
;;

;; This file is not part of GNU Emacs and is licensed differently.

;;; Commentary:

;;

;;; Code:
;;
;; Since I am using `use-package' extensively throughout Aether Emacs,
;; it is common to include key bindings directly within the include blocks
;; using the `:bind' keyword, however I personally find it easier to have
;; a central location where I can consolidate key bindings and easily access
;; all of them. As helpful as it is to modularize different packages into
;; files which reflect their specific types, it is a challenge to track down
;; where specific key bindings live in some cases.

(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'aether-get-selected-text)



;; (kbd "C-w") is `kill-region'

;; unset default reverse search
(global-unset-key (kbd "C-r"))

;; we re-define this to duplicate line/region
(global-unset-key (kbd "C-x C-d"))
;; duplicate the current line or region
(global-set-key (kbd "C-x C-d") 'aether-duplicate-current-line-or-region)

;; We re-define this with "M-o"
(global-unset-key (kbd "C-x o"))

;; unset some 2-column commands that are kind of annoying
(global-unset-key (kbd "<f2> <f2>"))
(global-unset-key (kbd "<f2> s"))
(global-unset-key (kbd "<f2> 2"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-n"))

;; Associate CMD+f and OPT+RIGHT to step forward by word
(global-unset-key (kbd "s-b"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-k"))
;; use swiper-search globally
(global-set-key (kbd "C-s") 'swiper-isearch)

;; use `counsel' for expression search instead of the default.
(global-set-key (kbd "M-x") 'counsel-M-x)

;; use `counsel-find-file' instead of the default.
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; `counsel-yank-pop' displays the kill-ring/paste-bin
(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; `counsel-describe-function' loads the describe function list.
(global-set-key (kbd "<f1> f") 'counsel-describe-function)

;; `counsel-describe-variable' loads the describe variable list.
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

;; `counsel-describe-library' loads the describe library list.
(global-set-key (kbd "<f1> l") 'counsel-find-library)

;; `counsel-describe-symbol' loads the symbol lookup list.
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)

;; `counsel-unicode-char' loads the unicode list.
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; `counsel-set-variable' is an interactive way to auto-generate
;; (setq VARIABLE VALUE)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)

;; Use `ivy-switch-buffer' instead of the default switch buffer
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; `kill-this-buffer' will kill the current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; `counsel-locate' is a way to search the file system for filenames
(global-set-key (kbd "C-x l") 'counsel-locate)

;; `counsel-descbinds' opens a search interface to quickly see
;; what functions are bound to key sequences.
(global-set-key (kbd "C-c d") 'counsel-descbinds)

;; With `ivy-switch-view' we can switch between layouts.
(global-set-key (kbd "C-x C-b") 'ivy-switch-view)

;; An ivy view is a layout of windows. We can store these.
(global-set-key (kbd "C-c v") 'ivy-push-view)

;; This removes a previously stored ivy view.
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Ivy Read Buffers
(global-set-key (kbd "C-b") #'aether-read-ivy-buffers)

;; I'm not really familiar with this command. Need to do more research.
(global-set-key (kbd "C-c J") 'counsel-file-jump)

;; Delete File when in Ivy Find File
(global-set-key (kbd "C-c d") #'aether-ivy-delete-file)

;; Change to other window with OPT+o
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'other-window)

;; Switch to another Emacs window (like a "Mac OSX window")
(global-set-key (kbd "M-P") 'nameframe-switch-frame)
;; A new frame is a new Emacs window (like a "Mac OSX window")
(global-set-key (kbd "s-n") 'nameframe-create-frame)

;; Convert lines into comma-separated list
(global-set-key (kbd "C-c o") #'aether-lines-to-cslist)

;; CMD+<left> is analogous to "C-a" (go to the beginning of the line.)
;; CMD+<right> is analogous to "C-e" (go to the end of the line.)

;; Associate CMD+LEFT to move to beginning of line.
(global-set-key (kbd "s-<left>") 'move-to-left-margin)
;; Associate CMD+RIGHT to move to end of line.
(global-set-key (kbd "s-<right>") 'move-end-of-line)

;; Use OPT+<left> and OPT+<right> to move forward and back by one word.
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)

;; Select a line or region and move it
(global-set-key (kbd "C-M-<up>") 'aether-move-region-up)
(global-set-key (kbd "C-M-<down>") 'aether-move-region-down)

;;; Open up the `init.el file`
;; OPT+F12 to edit emacs init file.
(global-set-key (kbd "M-<f11>") 'aether-edit-emacs-init)

;; OPT+F11 to reload emacs init file.
(global-set-key (kbd "M-<f12>") 'aether-reload-emacs-init)

;; binding to create an empty buffer (new Mac OSX window)
(global-set-key (kbd "<f6>") #'aether-emacs-new-empty-markdown-buffer)
(global-set-key (kbd "<f7>") #'aether-emacs-new-empty-buffer)

;; <f8> is bound to aspell, to identify possible spelling corrections

(global-set-key (kbd "<f9>") #'neotree-toggle)
(global-set-key (kbd "<f10>") #'aether-neotree-project-dir)

;; Specific bindings for `tuareg'
(eval-after-load 'tuareg '(define-key tuareg-mode-map (kbd "s-<return>") 'utop-eval-phrase))
(eval-after-load 'tuareg '(define-key tuareg-mode-map (kbd "M-<return>") 'utop-eval-buffer))


(provide 'aether-bindings)
;; aether-bindings.el ends here`
