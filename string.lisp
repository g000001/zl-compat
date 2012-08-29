(in-package #:zl-internal)


(defun string-pluralize (string)
  "Return a plural form of STRING.
Attempts to preserve the case-pattern in STRING."
  (setq string (coerce string 'string))
  (if (equal string "")
      ""
    (let* (flush add
	   (last-char-raw (char string (1- (string-length string))))
	   (last-char (char-upcase last-char-raw))
	   (last-char-lc-flag (char/= last-char last-char-raw))
	   (penult-char (char-upcase (if (> (string-length string) 1)
					 (char string (- (string-length string) 2))
				         #\Nul)))
	   (last-3 (substring string (max 0 (- (string-length string) 3)))))
      (cond ((and (char-equal last-char #\Y)
;character lossage assumes font=0
		  (not (memq penult-char '(#\A #\E #\I #\O #\U))))
	     (setq flush 1 add "ies"))
	    ((or (string-equal string "ox") (string-equal string "vax"))
	     (setq add "en"))
	    ((or (and (eq last-char #\H)
		      (memq penult-char '(#\C #\S)))
		 (memq last-char '(#\S #\Z #\X)))
	     (setq add "es"))
	    ((string-equal last-3 "man")
	     (setq flush 2 add "en"))
	    ((string-equal last-3 "fan")
	     (setq flush 2 add "en"))
	    ((string-equal last-3 "ife")
	     (setq flush 2 add "ves"))
	    (t (setq add "s")))
      (and flush (setq string (substring string 0 (- (string-length string) flush))))
      (cond (add (concatenate 'string
                              string
                              (cond (last-char-lc-flag add)
                                    (t (string-upcase add)))))
	    (t string)))))


;;; eof
