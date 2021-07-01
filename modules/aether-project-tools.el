;;; aether-project.el --  projectile initialization and customization
;;
;; https://docs.projectile.mx/projectile/configuration.html
;;
;; projectile (using `projectile-mode') is used to manage projects.
;;
;;; Notes:
;;
;; For any git repository, projectile will immediately pick up on the collection
;; of files as a project. I can also create a projectile file which defines the
;; directory structure as a project. With this established, I'm able to:
;; - grep across the entire project to find text within the files,
;; - quickly access files within the project,
;; - easily integrate commands such compiling and executing.
;;
;; Helpful functions:
;; - `projectile-find-file' (search within current project)
;; - `projectile-find-file-in-known-projects' (search across all projects)
;; - `projectile-find-file-dwim' (search within current dir path first)
;;
;;; Code:
;;
(defun aether-create-projectile-project-root-file ()
  "Create projectile file in project-root."
  (let ((projectile-file (expand-file-name "/.projectile" projectile-project-root)))
    (unless (file-exists-p projectile-file)
            (write-region "\n" nil projectile-file))))
;;
;; (defun aether-add-ignore-path-to-project-file (ignore-path)
;;   "Get path via interactive input and add to the projectile project file"
;;   (interactive "sEnter ignore path relative to project root: ")
;;
;;   ;; if projectile root file doesn't exist, then create it
;;   (aether-create-projectile-project-root-file)
;;   (when
;;       (and (file-exists-p (expand-file-name ignore-path projectile-project-root))
;;            (file-exists-p (expand-
;;            )
;;     (write-region (expand-file-name ignore-path projectile-project-root)
;;                   nil
;;                   (expand-file-name ".projectile
;;   )

(use-package counsel-dash
  :defer t
  :ensure t
  :requires (counsel ivy)
  )
(use-package counsel-gtags
  :defer t
  :ensure t
  :requires (counsel ivy ggtags)
  )
(use-package ivy-todo
  :defer t
  :requires (ivy)
  )
(use-package flyspell-correct-ivy
  :defer t
  :ensure t
  )

(use-package projectile
  :ensure t
  :requires (counsel ivy)
  :diminish projectile-mode
  :delight " [⚙] "
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/local/" "~/.emacs.d/" "~/env-local/" "~/local-devel/env-development/"))

  ;; Use ivy as the completion system. The other common CS is helm, but helm is
  ;; a much heavier. There is one key feature of helm though which has to do with
  ;; utilizing the mini-buffer, so we'll see if I end up switching back.
  (setq projectile-completion-system 'ivy)

  ;; options for indexing are: `native', `hybrid', and `alien'.
  (setq projectile-indexing-method 'alien)

  ;; options for sort-order are: `default', `recentf', `recently-active',
  ;; `modification-time', and `access-time'.
  (setq projectile-sort-order 'recently-active)

  ;; (C-u s-p f) will invalidate the cache prior to prompting a file jump
  ;; (s-p z) will add the current file to the cache, but this usually happens
  ;;     upon opening a  (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "C-c p SPC") 'counsel-projectile)
  (projectile-mode)
  (projectile-register-project-type 'julia '("Project.toml")
                                    :project-file "Project.toml"
                                    :compile ""
                                    )
  )
;;; https://github.com/ericdanan/counsel-projectile
;;
;; `counsel-projectile' handles integrating ivy, counsel, and projectile.

(use-package counsel-projectile
  :ensure t
  :delight " [⚙] "
  :defer t
  :after projectile
  :init
  (define-key projectile-mode-map (kbd "C-c p SPC") 'counsel-projectile)
  (define-key projectile-mode-map (kbd "C-c p p") 'counsel-projectile-switch-project)
;;  (define-key projectile-mode-map (kbd "
  :config
  (counsel-projectile-mode))

(use-package nameframe
  :ensure t
  )

(use-package nameframe-projectile
  :defer t
  :config
  (nameframe-projectile-mode t))

(provide 'aether-project-tools)
;; end of aether-project-tools.el

