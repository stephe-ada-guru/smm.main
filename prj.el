;; elisp project file
(require 'sal-android)

(wisi-prj-select-cache
 "prj.el" ;; project-file; placeholder
 (make-android-prj
  :name "Stephe's Music Android main"
  :global-project-root default-directory
  :path-non-rec
  (list
   (concat default-directory "app")
   (concat default-directory "app/src/main"))
  :path-rec ;; include java, because global does not index variables
  (list ;;
   (concat default-directory "app/src/main/")
   ))
  "Makefile")

;; end of file
