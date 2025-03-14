;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "smm main Alire"
  :gpr-file "build/smm_alire.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; create-alire-prj doesn't have :case-exception-files, so we do this here
(setf (wisi-prj-case-exception-files (project-current))
      (list (expand-file-name "build/smm.casing")))

(wisi--case-read-all-exceptions (project-current))
;; end of file
