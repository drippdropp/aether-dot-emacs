(use-package cc-mode :ensure t)
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))
(use-package company-c-headers)

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'dired-mode)
                (ggtags-mode 1))))
  )

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-navigation-mode-map (kbd "C-c s") 'ggtags-navigation-isearch-forward)))

