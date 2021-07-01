(use-package org
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map (kbd "C-c C-l") 'org-store-link)
  (setq org-list-indent-offset 10)
  (setq org-adapt-indentation nil)
  (setq org-agenda-files (list "/Users/dmarvin/local/orgs/todo.org"
			       "/users/dmarvin/local/orgs/2021-04-work.org"
			       ))
;; set maximum indentation for description lists
  (setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                             (?B . (:foreground "LightSteelBlue"))
                             (?C . (:foreground "OliveDrab"))))
  (setq org-agenda-window-setup (quote current-window))
  (define-key global-map (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "/Users/dmarvin/local/orgs/todo.org" "Tasks")
           "* TODO [#A] #?")))
  (setq org-log-done t)
  (setq org-todo-keyword-faces '(("DONE"      . (:foreground "#afd8af"     :weight bold))
                                 ("WAITING"   . (:foreground "#aabbc8" :weight bold))
                                 ("CANCELLED" . (:foreground "#887788"    :weight bold))
                                 ("HOWTO"     . (:foreground "SkyBlue3"    :weight bold))
                                 ("INFO"      . (:foreground "khaki1"      :weight bold))
                                 ("COLLECT"   . (:foreground "MediumSeaGreen" :weight bold))
                                 ("SOLVE"     . (:foreground "orange red"    :weight bold))
                                 ))
  )
