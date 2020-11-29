;;; aether-markdown.el

(defun markdown-config-opts ()
  (setq markdown-enable-math t))
  ;;(flyspell-prog-mode)
  ;;(superword-mode 1)

;; (defun set-markdown-faces ()
;;   (dolist (face  '(markdown-header-face
;;                    markdown-header-face-1
;;                    markdown-header-face-2
;;                    markdown-header-face-3
;;                    markdown-header-face-4
;;                    markdown-header-face-5
;;                    markdown-header-face-6
;;                    markdown-gfm-checkbox-face
;;                    markdown-footnote-text-face
;;                    markdown-highlight-face
;;                    markdown-header-delimiter-face
;;                    markdown-pre-face
;;                    markdown-bold-face
;;                    markdown-comment-face
;;                    markdown-markup-face
;;                    markdown-reference-face
;;                    markdown-comment-face
;;                    markdown-blockquote-face
;;                    markdown-link-title-face
;;                    markdown-link-face
;;                    markdown-missing-link-face
;;                    markdown-plain-url-face
;;                    ))
;;     (set-face-attribute face nil
;;                         :family "Palatino"
;;                         :height 160)))

(use-package markdown-mode
  :ensure t
  :defer t
  :ensure-system-package (markdown . "brew install markdown")
  :hook
  (markdown-mode . markdown-config-opts)
  (markdown-mode . buffer-face-mode)
  :config
  (set-face-attribute 'markdown-code-face nil :background "#282C34")
  (set-face-attribute 'markdown-code-face nil :foreground "#ABB2BF")
  ;;(setq buffer-face-mode-face '(:family "Georgia" :weight "Light" :height 160))
  ;;(set-markdown-faces)
  (set-face-attribute 'markdown-header-face-1 nil :height 240)
  (set-face-attribute 'markdown-header-face-2 nil :height 220)
  (set-face-attribute 'markdown-header-face-3 nil :height 200)
  )

(use-package markdown-toc
  :ensure t
  :defer t)

(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))
