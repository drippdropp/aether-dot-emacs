(use-package julia-mode
  :ensure t
  :defer t)

;; can set `julia-snail-executable' as string referencing executable path for different versions.
;; can point this var to bin file hook in jlenv tool.
(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode)
  )


