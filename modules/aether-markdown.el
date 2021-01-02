;;; aether-markdown.el

(defun aether-markdown-config ()
  (local-set-key (kbd "M-q") 'ignore))

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
  (markdown-mode . visual-line-mode)
  (markdown-mode . aether-markdown-config)
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

(defun as/markdown-region-to-latex (start end)
  (interactive "r")
  (goto-char start)
  (save-restriction
    (let (in-list skip-to)
      (narrow-to-region start end)
      (while (re-search-forward "\\*\\|\n\\|\\`" nil t)
	(goto-char (match-beginning 0))
	(if (= (point) (match-end 0))
	    (setq skip-to (1+ (point)))
	  (setq skip-to (match-end 0)))
	(cond ((looking-at "\\*\\*\\b\\([^*]*?\\)\\b\\*\\*")
	       (replace-match "\\\\textbf{\\1}"))
	      ((looking-at "\\*\\b\\([^*]*?\\)\\b\\*")
	       (replace-match "\\\\textit{\\1}"))
	      ((looking-at "^# \\(.*\\)")
	       (replace-match "\\\\section{\\1}"))
	      ((looking-at "^## \\(.*\\)")
	       (replace-match "\\\\subsection{\\1}"))
	      ((looking-at "^### \\(.*\\)")
	       (replace-match "\\\\subsubsection{\\1}"))
	      ((looking-at "^\\* ")
	       (replace-match (if in-list "\\\\item " "\\\\begin{itemize}\n\\\\item "))
	       (setq in-list "itemize"))
	      ((looking-at "^[0-9]+\\. ")
	       (replace-match (if in-list "\\\\item " "\\\\begin{enumerate}\n\\\\item "))
	       (setq in-list "enumerate"))
	      ((and in-list (looking-at "^"))
	       (replace-match (format "\\\\end{%s}\n" in-list))
	       (setq in-list nil))
	      (t (goto-char skip-to)))))))
