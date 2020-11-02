(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-reload-all)
  (require 'warnings)
  :hook
  (term-mode . (lambda () (yas-minor-mode -1)))
  (python-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed))))
(use-package yasnippet-snippets)

(defvar aether-company-point nil)
(advice-add 'company-complete-common
            :before (lambda ()
                      (setq aether-company-point (point))))
(advice-add 'company-complete-common
            :after (lambda ()
                     (when
                         (equal aether-company-point (point))
                       (yas-expand))))

