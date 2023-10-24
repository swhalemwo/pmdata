(load-file "/home/johannes/Dropbox/phd/pmdata/data_sources/mow/mow-museums.el")

(defun mow-dedupl ()
  "bring up consult menu for filtering museums in MOW to mark matches with PMDB"
  (interactive)
  (let* ((mow-museum (consult--read mow-museums))
	  (mow-id (assoc-default mow-museum mow-museums)))
    (insert mow-id)
    )
  )



  

  
