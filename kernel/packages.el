;;; packages.el --- Aether Base
;;
;; Author     : Daniel Marvin <daniel@nullmap.org>
;; Created    : Wed Oct 28, 2020
;; Version    : 0.1
;; Keywords   : init

;; Copyright (C) 2020 by Daniel Marvin
;;

;; This file is not part of GNU Emacs and is licensed differently.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t
  )

(setq package-selected-packages '(ag
				  all-the-icons
				  all-the-icons-dired
				  all-the-icons-ivy
				  all-the-icons-ivy-rich
				  async
				  auto-package-update
				  avy
				  avy-flycheck
				  bind-key
				  browse-kill-ring
				  buffer-move
				  cider
				  company
				  company-box
				  company-math
				  company-emoji
				  company-jedi
				  counsel
				  dashboard
				  diminish
				  dotenv-mode
				  epl
				  epresent
				  exec-path-from-shell
				  flycheck
				  flycheck-package
				  flycheck-posframe
				  graphviz-dot-mode
				  haskell-mode
				  hungry-delete
				  julia-mode
				  julia-snail
				  imenu-anywhere
				  ivy
				  lisp-mode
				  magit
				  minibuffer-line
				  mood-line
				  neotree
				  nlinum
				  nov
				  org
				  org-bullets
				  org-tempo
				  page-break-lines
				  popup-kill-ring
				  rainbow-delimiters
				  rainbow-mode
				  solarized-theme
				  sorcery-theme
				  sudo-edit
				  swiper
				  system-packages
				  toc-org
				  try
				  vterm
				  wdired
				  which-key))
(package-initialize)

(setq package-enable-at-startup nil)

(setq use-package-compute-statistics t)

(use-package diminish :ensure t)

(use-package async
  :ensure t 
  :defer t
  :init (dired-async-mode 1)
  		(async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all))
  )

(use-package try
  :ensure t
  :defer t
  )

(use-package auto-package-update
  :ensure t
  :defer t
  :custom ((auto-package-update-interval 2)
           (auto-package-update-hide-results t)
           (auto-package-update-delete-old-versions t))
  :hook (after-init . auto-package-update-maybe)
  )

(provide 'packages)
;;; end of packages.el