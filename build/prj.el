;; Project definitions

(require 'ada-project)
(require 'xref-ada)

(ada-parse-prj-file "smm.prj")
(ada-select-prj-file "smm.prj")

(add-to-list 'project-find-functions 'project-menu-prj)

;; Ada mode adds another layer of project selection
(project-menu-select "Ada mode")

;; end of file
