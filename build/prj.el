;; Project definitions

(wisi-prj-select-cache
 "smm.prj"
 (create-ada-prj
  :name "smm main"
  :compile-env
  '("SERVER_DATA=/d/Music/server_data") ;; msys2 syntax for make shell
  ))

;; end of file
