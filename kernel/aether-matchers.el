(setq range-expression
      `(or "0"
	   (sequence "1" (optional digit (optional digit)))
	   (sequence "2" (optional
			  (or
			   (sequence (any "0-4") (optional digit))
			   (sequence "5" (optional (any "0-5")))
			   (any "6-9"))))
	   (sequence (any "3-9") (optional digit))))

(setq range-pattern (rx bol (eval range-expression) eol))

(require 'cl)
(cl-every (lambda (number)
	    (s-matches-p range-pattern (number-to-string number)))
	  (number-sequence 0 255))

"0\\|1\\(?:[[:digit:]][[:digit:]]?\\)?\\|2\\(?:[0-4][[:digit:]]?\\|5[0-5]?\\|[6-9][[:digit:]]?\\)?\\|[3-9][[:digit:]]?"

(setq ip4-pattern (rx bol
		      (repeat 3 (sequence (eval range-expression) "."))
		      (eval range-expression)
		      eol))

(s-matches-p range-pattern "30")
(s-matches-p range-pattern "300")
(s-matches-p range-pattern "61.12.234.30")

"\\(?:\\(?:0\\|1\\(?:[[:digit:]][[:digit:]]?\\)?\\|2\\(?:[0-4][[:digit:]]?\\|5[0-5]?\\|[6-9][[:digit:]]?\\)?\\|[3-9][[:digit:]]?\\)\\.\\)\\{3\\}\\(?:0\\|1\\(?:[[:digit:]][[:digit:]]?\\)?\\|2\\(?:[0-4][[:digit:]]?\\|5[0-5]?\\|[6-9][[:digit:]]?\\)?\\|[3-9][[:digit:]]?\\)"

