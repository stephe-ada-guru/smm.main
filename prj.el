;; random elisp project file

(require 'cc-mode);; java-mode-hook
(require 'cedet-global)
(require 'ede-files-patches) ;; find-file-project ede, ede-find-file
(require 'ede/generic) ;; generic-autoloader
(require 'project-patches) ;; project-find-file
(require 'project-menu)
(require 'symref-patches) ;; semantic-symref-xref-mode

(add-hook 'java-mode-hook 'semantic-symref-xref-mode)

(add-to-list 'ede-locate-setup-options 'ede-locate-global)

(sal-ensure-ede "Generic Monotone" ".")

;; Set the EDE locator object directly (not via
;; ede-enable-locate-on-project), to select ede-locate-global-path and
;; set the path.

;; When loaded from build/Makefile, #$ is "../prj.el", and `default-directory' is "build"
;; When run interactively, `default-directory' is "."
(let* ((root (file-name-directory (or #$ default-directory)))
       ;; If Generic Monotone was enabled by `sal-ensure-ede', then
       ;; this buffer was not properly initialized by
       ;; `ede-initialize-state-current-buffer'; only
       ;; `ede-load-project-file' will load the Monotone project.
       (prj (ede-load-project-file root)))

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
