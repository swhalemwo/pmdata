(load-file "/home/johannes/Dropbox/phd/pmdata/data_sources/artfacts/af-instns.el")

(defun artfacts-pmdb-match()
  "bring up consult menu for filtering collectors in Artnews to mark matches with PMDB"
  (interactive)

  (let* ((af-instn (consult--read af-instns
		     :annotate (lambda (cand)
				 (consult--annotate-align
				   cand
				   ;; "kappa"
				   ;; (assoc-default 'id cand)
				   (assoc-default 'info (assoc-default cand af-instns))
				   ))
		     ))
	  (AF-IID (assoc-default 'id (assoc-default af-instn af-instns))))
    (insert AF-IID)
    )
  )


(define-key inferior-ess-mode-map (kbd "C-c m") 'artfacts-pmdb-match)

