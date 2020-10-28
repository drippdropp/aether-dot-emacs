(use-package julia-mode
  :ensure t
  :defer t)

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))


