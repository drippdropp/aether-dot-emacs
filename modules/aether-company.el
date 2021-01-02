;; company
(use-package company
  :ensure t
  :diminish company-mode
  :custom
  (company-tooltip-align-annotations t)
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.01)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  :hook
  ((prog-mode utop-mode) . company-mode)
  ((prog-mode tuareg-mode) . company-mode)
  )

(use-package company-c-headers
  :ensure t
  :defer t)

;; Company completion backend for lsp-mode
(use-package company-lsp
  :ensure t
  :defer t)

;; Emacs client/library for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :defer t)

(use-package lsp-ui
  :ensure t
  :defer t)
