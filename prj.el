;; elisp project file
(require 'sal-android)

(sal-android-project
 :id "Stephe's Music Android main"
 :project-root default-directory
 :android-root "c:/Users/stephe/AppData/Local/Android/sdk/sources/android-24/"
 :non-recursive ;; not in global
 (list
  (concat default-directory "app")
  (concat default-directory "app/src/main"))
 :recursive ;; not in global
 (list ;;
  (concat default-directory "app/src/main/res")
  ))
;; end of file
