
(use-package dashboard
  :ensure t
  :preface
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :config
  (setq dashboard-banner-length 300)
  (setq dashboard-set-footer +1)
  (setq dashboard-set-init-info +1)
  (setq dashboard-set-heading-icons +1)
  (setq dashboard-banners-directory (expand-file-name "banners/aether" aether-dir))
  (setq dashboard-startup-banner 1)
  (setq dashboard-footer-messages '("Through the Aether..." "And, so it begins..."))
;;  (setq dashboard-footer "Through the Aether...")
  (setq dashboard-set-heading-icons +1)
  (setq dashboard-set-file-icons +1)
  (setq dashboard-center-content +1)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  (setq dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "Homepage"
            "Visit project Homepage"
            (lambda (&rest _) (browse-url "https://github.com/drippdropp/aether-dot-emacs"))))))
  (dashboard-setup-startup-hook)
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))

(provide 'aether-dashboard)

