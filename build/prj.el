;; Project definitions

(require 'ada-project)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "smm.prj"))
       (prj-name "smm main")
       (prj (make-ada-project
	     :env-vars '(("SERVER_DATA" . "/d/Music/server_data")) ;; msys2 syntax
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; end of file
