;; elisp project file
(require 'sal-android)

(sal-android-project
 :id "Stephe's Music Android main"
 :project-root default-directory
 :android-root "c:/Users/stephe/AppData/Local/Android/sdk/sources/android-24/"
 :non-recursive ;;
 (list
  (concat default-directory "app")
  (concat default-directory "app/src/main"))
 :recursive ;; include java, because global does not index variables
 (list ;;
  (concat default-directory "app/src/main/")
  ))

;; end of file
