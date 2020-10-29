;;; aether-project.el --  projectile initialization and customization
;;
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
(use-package projectile
  :ensure t
  :requires (counsel ivy)
  :diminish projectile-mode
  :delight "prjm"
  :bind-keymap ("C-c p" . projectile-command-map)
  :init

  (setq projectile-project-search-path '("~/projects" "~/local" "~/opt"))

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
  ;;     upon opening a file.
  (setq projectile-enable-caching t)

  ;; this makes projectile usable in every directory, w or w/o a project file
  (setq projectile-require-project-root nil)

  ;; include the current project in the list of projects when switching
  (setq projectile-current-project-on-switch t)

  ;; We don't need to define this since we'll be using counsel-projectile.
  ;;(setq projectile-switch-project-action 'counsel-find-file)

  ;; We want to show the project name and type on the mode line.
  (setq projectile-dynamic-mode-line t)

  ;; Show a unicode gear, `⚙' on the mode line instead of the text "Projectile".
  (setq projectile-mode-line-prefix "⚙")

  :config (projectile-mode))

;;; https://github.com/ericdanan/counsel-projectile
;;
;; `counsel-projectile' handles integrating ivy, counsel, and projectile.
(use-package counsel-projectile
  :ensure t
  :delight "cnsprj"
  :defer t
  :after projectile
  :config
  (counsel-projectile-mode))

(provide 'aether-projectile)
;; end of aether-projectile.el

