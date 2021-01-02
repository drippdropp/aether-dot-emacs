;; ocaml support


(defun setup-opam-variables ()
  (dolist
      (var (car (read-from-string
                 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator))
  (push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp") load-path)
  (push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../bin") load-path)
  )

(and (require 'cl-lib)
     (use-package tuareg
       :defer t
       :ensure t
       :config
       (setup-opam-variables)
       (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
       (setq auto-mode-alist
             (append '(("\\.ml[ily]?$" . tuareg-mode)
                       ("\\.topml$" . tuareg-mode))
                     auto-mode-alist)))
     (use-package merlin
       :ensure t
       :config
       (add-hook 'tuareg-mode-hook 'merlin-mode)
       (add-hook 'merln-mode-hook #'company-mode)
       (autoload 'merlin-mode "merlin" "Merlin mode" t)
       (setq merlin-command "ocamlmerlin")
       (setq merlin-error-after-save nil))

     (use-package utop
       :ensure t
       :config
       (autoload 'utop "utop" "Toplevel for OCaml" t)
       (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
       (add-hook 'tuareg-mode-hook 'utop-minor-mode)
       (setq utop-command "opam config exec -- utop -emacs")
       )
     )

