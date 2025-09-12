;; Set up building with Alire -*- no-byte-compile : t -*-

;; WORKAROUND: wisi parser keeps dying
;; (setq-default wisi-parser-verbosity "parse=1 debug=1")

;; WORKAROUND: when run from .make, something causes alire to use a
;; different version of utilada_curl, and it aborts with an error
;; about redefining UTILADA_CURL_ALIRE_PREFIX and
;; UTILADA_ALIRE_PREFIX, which we cache in wisi-prj-file-env. We don't
;; need that otherwise, so delete it here.
(require 'wisi-prj)
(let*
    ((project
      (create-alire-prj
       :name "smm work_1 Alire"
       :compile-env
       '("SERVER_DATA=/Projects/music_server_data")
       :gpr-file "build/smm_alire.gpr"
       ;;  :gpr-file "build/smm_test.gpr"
       :xref-label 'gpr_query)))

  (let ((process-environment (copy-sequence (wisi-prj-file-env project))))
      (setenv "UTILADA_CURL_ALIRE_PREFIX" nil)
      (setenv "UTILADA_ALIRE_PREFIX" nil)
      (setf (wisi-prj-file-env project) (copy-sequence process-environment)))

  (wisi-prj-select-cache "smm-alire.prj" project "Alire.make"))

;; end of file
