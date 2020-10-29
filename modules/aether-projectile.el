;; projectile
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

