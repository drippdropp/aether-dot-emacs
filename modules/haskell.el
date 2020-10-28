(use-package haskell-mode
  :ensure t
  :defer t
  :custom (haskell-stylish-on-save t)
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

