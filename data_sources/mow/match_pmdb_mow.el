(load-file "/home/johannes/Dropbox/phd/pmdata/data_sources/mow/mow-museums.el")

(defun mow-pmdb-match ()
  "bring up consult menu for filtering museums in MOW to mark matches with PMDB"
  (interactive)
  (let* ((mow-museum (consult--read mow-museums
		       :annotate (lambda (cand)
				   (consult--annotate-align
				     cand
				     ;; "kappa"
				     ;; (assoc-default 'id cand)
				     (assoc-default 'info (assoc-default cand mow-museums))
				     ))

		       ))
	  (mow-id (assoc-default 'id (assoc-default mow-museum mow-museums))))
    (insert mow-id)
    )
  )

(define-key inferior-ess-mode-map (kbd "C-c m") 'mow-pmdb-match)




  

  
