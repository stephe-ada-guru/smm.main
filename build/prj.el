;; Set up building with Alire -*- no-byte-compile : t -*-

;; to change ALIRE_BUILD_PROFILE: edit below, then (setq wisi-prj--cache nil), then load prj.el
(require 'wisi-prj)
(let*
    ((project
      (create-alire-prj
       :name "smm main"
       :gpr-file "build/smm.gpr"
       :xref-label 'gpr_query
       :compile-env
       (list
	"SERVER_DATA=/var/www/html/music_server_data"
	"SERVER_IP=127.0.0.1"
	"SERVER_PORT=16#9003#"
;;     "ALIRE_BUILD_PROFILE=release"
	"ALIRE_BUILD_PROFILE=development"
	 ))))

  ;; WORKAROUND: when run from .make, something causes alire to use a
  ;; different version of utilada_curl, and it aborts with an error
  ;; about redefining UTILADA_CURL_ALIRE_PREFIX and
  ;; UTILADA_ALIRE_PREFIX, which we cache in wisi-prj-file-env. We
  ;; don't need that otherwise, so delete it here. But (setenv
  ;; ... nil) does _not_ remove the item from process-environment; it
  ;; just removes the "=value" part of the string. Then gnat 15
  ;; complains about a malformed environment variable. So actually
  ;; delete it here.
  (let ((process-environment (copy-sequence (wisi-prj-file-env project))))
    (setenv "UTILADA_CURL_ALIRE_PREFIX" nil)
    (setenv "UTILADA_ALIRE_PREFIX" nil)
    (setq process-environment (delete "UTILADA_CURL_ALIRE_PREFIX" process-environment))
    (setq process-environment (delete "UTILADA_ALIRE_PREFIX" process-environment))
    (setf (wisi-prj-file-env project) (copy-sequence process-environment)))

  (wisi-prj-select-cache "smm.prj" project "Alire.make"))

;; end of file
