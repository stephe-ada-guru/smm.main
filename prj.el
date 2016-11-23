;; elisp project file
(require 'sal-android)

(sal-android-project
 "Stephe's Music Android main"
 (list ;; non-recursive
  default-directory
  (concat default-directory "app")
  (concat default-directory "app/src/main"))
 ;; "*java/*" accessed via Gnu global
 (list ;; recursive
  (concat default-directory "app/src/main/res")
  ;; Including the android sources confuses completion, partly because of the long path.
  ;; "c:/Users/stephe/AppData/Local/Android/sdk/sources/android-23/" ;; can't combine two Gnu Global spaces
  )
 )
;; end of file
