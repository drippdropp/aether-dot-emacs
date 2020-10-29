;;; user.el --- Initialization
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

(defvar user-full-name "Daniel Marvin")
(defvar user-email-address "daniel@nullmap.org")
(defvar user-git-user-name "drippdropp")
(defmacro generate--user-git ()
	`(defvar user-git-webaddress 
		(#'cl-concat "https://www.github.com/" ,user-git-user-name)
		)
	)

(provide 'user)