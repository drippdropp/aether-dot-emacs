(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :defer t
  :init
  (setq yas-snippet-dir (expand-file-name "snippets" aether-dir))
  :config
  (yas-reload-all)
  (require 'warnings)
  :hook
  (term-mode . (lambda () (yas-minor-mode -1)))
  (python-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  (prog-mode . yas-minor-mode)
  )
(use-package yasnippet-snippets :defer t)
(use-package ivy-snippets :defer t)


(defvar aether-company-point nil)
(advice-add 'company-complete-common
            :before (lambda ()
                      (setq aether-company-point (point))))
(advice-add 'company-complete-common
            :after (lambda ()
                     (when
                         (equal aether-company-point (point))
                       (yas-expand))))

(provide 'aether-yasnippet)

