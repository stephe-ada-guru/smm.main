;; elisp project file
(require 'sal-android)

(wisi-prj-set-dominating
 "Makefile"
 (or load-file-name (buffer-file-name)) ;; project-file
 (make-android-prj
  :name "Stephe's Music Android main"
  :global-project-root default-directory
  :global-android-root "c:/Users/stephe/AppData/Local/Android/sdk/sources/android-24/"
  :path-non-rec
  (list
   (concat default-directory "app")
   (concat default-directory "app/src/main"))
  :path-rec ;; include java, because global does not index variables
  (list ;;
   (concat default-directory "app/src/main/")
   )))

;; end of file
