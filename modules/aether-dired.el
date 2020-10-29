(require 'dired)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

(define-key dired-mode-map
  (kbd "^") (lambda ()
              (interactive)
              (find-alternate-file "..")))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

