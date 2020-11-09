;; (use-package cider
;;   :ensure t
;;   :config
;;   (add-hook 'cider-repl-mode-hook #'company-mode)
;;   (add-hook 'cider-mode-hook #'company-mode)
;;   (add-hook 'cider-mode-hook #'eldoc-mode)
;;   (setq cider-repl-use-pretty-printing t)
;;   (setq cider-repl-display-help-banner nil)

;;     :bind (("M-r" . cider-namespace-refresh)
;;            ("C-c r" . cider-repl-reset)
;;            ("C-c ." . cider-reset-test-run-tests))
;;     )

; (use-package clj-refactor
;   :ensure t
;   :config (add-hook 'clojure-mode-hook (lambda ()
;                               (clj-refactor-mode 1)
;                                ;; insert keybinding setup here
;                                ))
; (cljr-add-keybindings-with-prefix "C-c C-m")
; (setq cljr-warn-on-eval nil)
; :bind ("C-c '" . hydra-cljr-help-menu/body)
; )
