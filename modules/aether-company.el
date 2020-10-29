;; company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

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

