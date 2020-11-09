(use-package ruby-electric
  :ensure t
  )

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-installation-dir "~/.rbenv")
  )

(use-package seeing-is-believing
  :ensure
  :config
  (add-hook 'ruby-mode-hook (lambda ()
                              (ruby-electric-mode t)))
  (add-hook 'ruby-mode-hook 'seeing-is-believing)
  )
