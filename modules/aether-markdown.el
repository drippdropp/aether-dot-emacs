(defun markdown-config-opts ()
  (setq show-trailing-whitespace t)
  (setq markdown-enable-math t)
  (flyspell-prog-mode)
  (superword-mode 1))

(defun is-markdown-mode ()
  (interactive)
  (if (string-equal major-mode "gfm-mode")
      (message "Currently in Markdown mode"))
  )

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

;;(add-hook 'markdown-mode-hook 'set-font-for-gfm-mode)


(defun set-markdown-faces ()
  (dolist (face  '(markdown-header-face
                   markdown-header-face-1
                   markdown-header-face-2
                   markdown-header-face-3
                   markdown-header-face-4
                   markdown-header-face-5
                   markdown-header-face-6
                   markdown-header-delimiter-face
                   markdown-bold-face
                   markdown-markup-face
                   markdown-reference-face
                   markdown-comment-face
                   markdown-blockquote-face
                   ))
    (set-face-attribute face nil
                        :family "Georgia")))
