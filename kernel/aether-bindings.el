;;; aether-bindings.el -- Collection of all key bindings.
;;
;;
;; Since I am using `use-package' extensively throughout Aether Emacs,
;; it is common to include key bindings directly within the include blocks
;; using the `:bind' keyword, however I personally find it easier to have
;; a central location where I can consolidate key bindings and easily access
;; all of them. As helpful as it is to modularize different packages into
;; files which reflect their specific types, it is a challenge to track down
;; where specific key bindings live in some cases.

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
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-x C-b") 'ivy-switch-view)

(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c J") 'counsel-file-jump)

;; Change to other window with OPT+o
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'other-window)

;; Associate CMD+f and OPT+RIGHT to step forward by word
(global-unset-key (kbd "s-f"))
(global-set-key (kbd "s-f") 'forward-word)
(global-unset-key (kbd "M-<right>"))
(global-set-key (kbd "M-<right>") 'forward-word)

;; Associate CMD+b and Opt+LEFT to step back by word
(global-unset-key (kbd "s-b"))
(global-set-key (kbd "s-b") 'backward-word)
(global-unset-key (kbd "M-<left>"))
(global-set-key (kbd "M-<left>") 'backward-word)

;; Associate CMD+LEFT to move to beginning of line.
(global-set-key (kbd "s-<left>") 'move-to-left-margin)
;; Associate CMD+RIGHT to move to end of line.
(global-set-key (kbd "s-<right>") 'move-end-of-line)

;; OPT+F12 to edit emacs init file.
(global-set-key (kbd "M-<f12>") 'aether-edit-emacs-init)
;; OPT+F11 to reload emacs init file.
(global-set-key (kbd "M-<f11>") 'aether-reload-emacs-init)

;; binding to create an empty buffer
(global-set-key (kbd "<f7>") 'aether-emacs-new-empty-buffer)

(provide 'aether-bindings)
;; aether-bindings.el ends here.
