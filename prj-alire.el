;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "smm main Alire"
  :gpr-file "build/smm_alire.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; end of file
