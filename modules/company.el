(use-package company
  :ensure t
  :defer t
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-show-numbers nil)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("SPC" . company-abort)))

(use-package company-emoji
  :after company
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends #'company-emoji))

(use-package company-box
  :defer
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package company-c-headers
  :after (c-mode company)
  :config (add-to-list 'company-backends 'company-c-headers))
; (use-package company-inf-ruby
;   :after (ruby-mode company)
;   :config (add-to-list 'company-backends 'company-irb-ruby))
; (use-package company-math
;   :after (text-mode company)
;   :config (add-to-list 'company-backends 'company-math))