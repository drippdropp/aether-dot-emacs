(use-package ag
  :defer t)

(use-package ivy
  :demand
  :defer t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  )

(defun aether-read-ivy-buffers ()
  (interactive)
  (ivy-read "My buffers: "
	    (mapcar #'buffer-name (buffer-list))
	    :action '(1
		      ("s" (lambda (x)
			     (switch-to-buffer x)) "switch")))
  )

(defun aether-ivy-delete-file ()
  (interactive)
  (ivy-set-actions
      'counsel-find-file
      '(("d" delete-file "delete")))
  )
