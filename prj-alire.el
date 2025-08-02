;; Set up building with Alire -*- no-byte-compile : t -*-

;; WORKAROUND: wisi parser keeps dying
(setq-default wisi-parser-verbosity "parse=1 debug=1")

(wisi-prj-select-cache
 "smm-alire.prj"
 (create-alire-prj
  :name "smm work_1 Alire"
  :compile-env
  '("SERVER_DATA=/d/Music/server_data" ;; msys2 syntax for make shell
    "SOCKET=openssl")                  ;; support https connections
;;  :gpr-file "build/smm_alire.gpr"
  :gpr-file "build/smm_test.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; end of file
