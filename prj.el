;; random elisp project file

(require 'cedet-global)
(require 'ede-files-patches) ;; find-file-project ede, ede-find-file
(require 'ede/generic) ;; generic-autoloader
(require 'project-patches)   ;; project-find-file
(require 'project-menu)
;; (require 'project-multi-lang)
;; (require 'project-ml-global)

(add-to-list 'ede-locate-setup-options 'ede-locate-global)

(sal-check-ede "Generic Monotone")

(unless (project-try-ede ".")
  (error "EDE not enabled properly"))

;; Set the EDE locator object directly (not via
;; ede-enable-locate-on-project), to select ede-locate-global-path and
;; set the path.

(let ((root (file-name-directory (or #$ default-directory)))
      (prj (ede-toplevel)))

  (oset prj locate-obj
	(ede-locate-global-path
	 :root (expand-file-name ".")
	 :path-iter
	 (make-path-iterator
	 :user-path-non-recursive
	 (list
	  root ;; prj.el, design.text, build.gradle
	  (concat root "build") ;; main Makefile
	  (concat root "app/src/main") ;; AndroidManifest
	  )
	  ;; "app/src/main/java" accessed via Gnu global
	 :user-path-recursive
	 (list (concat root "app/src/main/res"))
	 )))

  (add-to-list 'project-find-functions 'project-menu-prj)
  (add-to-list 'project-find-functions 'project-try-ede)

  (add-to-list 'project-menu-list (cons "Stephe's Music main" prj))

  (project-menu-select "Stephe's Music main")
)

;; end of file
