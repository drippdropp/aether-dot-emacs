;;; aether-typo

(use-package typo
  :config
  (typo-global-mode +1)
  :hook
  (text-mode-hook . 'typo-mode)
  )


