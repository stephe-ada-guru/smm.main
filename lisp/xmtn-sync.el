;;; xmtn-sync.el --- database sync handling for DVC backend for monotone
;;
;; Copyright (C) 2010 Stephen Leake
;;
;; Author: Stephen Leake
;; Keywords: tools
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

(eval-when-compile
  ;; these have macros we use
  )

(eval-and-compile
  ;; these have functions (and possibly macros) we use
  (require 'xmtn-automate)
  (require 'xmtn-basic-io)
  )

;;; User variables
(defvar xmtn-sync-branch-file "~/.dvc/branches"
  "File associating branch name with workspace root")

(defvar xmtn-sync-executable
  (cond
    ((equal system-type 'windows-nt)
     ;; Native MinGW does not support file: or ssh: - assume Cygwin is
     ;; installed, but not first in path
     "c:/bin/mtn")
    (t
     ;; Unix or Cygwin; assume mtn is in path
     "mtn"))
  "Executable for running sync command on local db; overrides xmtn-executable.")

(defvar xmtn-sync-config "xmtn-sync-config"
  "File to store `xmtn-sync-branch-alist' and `xmtn-sync-remote-exec-alist'; relative to `dvc-config-directory'.")

;;; Internal variables
(defconst xmtn-sync-required-command-version '(0 46)
  "Minimum mtn version for automate sync; overrides xmtn--minimum-required-command-version.")

(defconst xmtn-sync-remote-exec-default "mtn"
  "Default executable command to run on remote host for file: or ssh:; see `xmtn-sync-remote-exec-alist'.")

;; loaded from xmtn-sync-config
(defvar xmtn-sync-branch-alist nil
  "Alist associating branch name with workspace root")

(defvar xmtn-sync-remote-exec-alist
  (list
   (list "file://" xmtn-sync-executable))
  "Alist of host and remote command. Overrides `xmtn-sync-remote-exec-default'.")

;; buffer-local
(defvar xmtn-sync-local-db nil
  "Absolute path to local database.")
(make-variable-buffer-local 'xmtn-sync-local-db)

(defvar xmtn-sync-remote-db nil
  "Absolute path to remote database.")
(make-variable-buffer-local 'xmtn-sync-remote-db)

(defvar xmtn-sync-ewoc nil
  "Buffer-local ewoc for displaying sync.
All xmtn-sync functions operate on this ewoc.
The elements must all be of type xmtn-sync-sync.")
(make-variable-buffer-local 'xmtn-sync-ewoc)

(defstruct (xmtn-sync-branch
            (:copier nil))
  ;; ewoc element; data for a branch that was received
  name ;; monotone branch name
  revlist ;; list of '(backend revid, date, author, changelog)
  print-mode ;; 'summary | 'brief | 'full | 'done
  )

(defun xmtn-sync-print-rev (rev print-mode)
  "Print a REV (element of branch revlist) according to PRINT-MODE ('brief or 'full)."
  (let ((date (nth 1 rev))
	(author (nth 2 rev))
	(changelog (nth 3 rev)))
    (insert "%s %s\n" date author)
    (ecase print-mode
      ('brief
       (insert (substring changelog 0 (string-match "\n" changelog))))
      ('full
       (insert changelog)))))

(defun xmtn-sync-printer (branch)
  "Print an ewoc element; BRANCH must be of type xmtn-sync-branch."
  (insert (xmtn-sync-branch-name branch))
  (ecase (xmtn-sync-branch-print-mode branch)
    ('summary
     (insert (format " %d\n" (length (xmtn-sync-branch-revlist branch)))))

    (('brief 'full)
     (loop for rev in (xmtn-sync-branch-revlist branch) do
	(xmtn-sync-print-rev rev (xmtn-sync-branch-print-mode branch))))

    ('done
     (insert "\n")))
  )

(defun xmtn-sync-status ()
  "Start xmtn-status-one for current ewoc element."
  (let* ((data (ewoc-data (ewoc-locate xmtn-sync-ewoc)))
         (branch (xmtn-sync-branch-name data))
         (work (assoc branch xmtn-sync-branch-alist)))
    (if (not work)
        (progn
          (setq work (read-directory-name (format "workspace root for %s: " branch)))
          (push (list branch work) xmtn-sync-branch-alist)))
    (xmtn-status-one work)))

(defvar xmtn-sync-ewoc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?0]  '(menu-item "0) status"
                                      'xmtn-sync-status))
    map)
  "Keyboard menu keymap for xmtn-sync-ewoc.")

(defvar xmtn-sync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map "\M-d" xmtn-sync-ewoc-map)
    map)
  "Keymap used in `xmtn-sync-mode'.")

(easy-menu-define xmtn-sync-mode-menu xmtn-sync-mode-map
  "`xmtn-sync' menu"
  `("Xmtn-sync"
    ["Do the right thing"  xmtn-sync-ewoc-map t]
    ["Quit"                dvc-buffer-quit t]
    ))

(define-derived-mode xmtn-sync-mode fundamental-mode "xmtn-sync"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq xmtn-sync-ewoc (ewoc-create 'xmtn-sync-printer))
  (setq dvc-buffer-refresh-function nil)
  (dvc-install-buffer-menu)
  (buffer-disable-undo))

(defun xmtn-sync-parse-revisions (direction)
  "Parse revisions with associated certs."
  (let (revid cert-label branch date author changelog nodes)
    (xmtn-basic-io-skip-blank-lines)
    (while (xmtn-basic-io-optional-line "revision" (setq revid (cadar value)))
      (xmtn-basic-io-check-empty)
      (while (xmtn-basic-io-optional-line "cert" (setq cert-label (cadar value)))
	(cond
	 ((string= cert-label "branch")
	  (xmtn-basic-io-check-line "value" (setq branch (cadar value)))
	  (xmtn-basic-io-check-line "key" nil))

	 ((string= cert-label "changelog")
	  (xmtn-basic-io-check-line "value" (setq changelog (cadar value)))
	  (xmtn-basic-io-check-line "key" nil))

	 ((string= cert-label "date")
	  (xmtn-basic-io-check-line "value" (setq date (cadar value)))
	  (xmtn-basic-io-check-line "key" nil))

	 ((string= cert-label "author")
	  (xmtn-basic-io-check-line "value" (setq author (cadar value)))
	  (xmtn-basic-io-check-line "key" nil))

	 (t
	  ;; ignore other certs
	  (xmtn-basic-io-skip-stanza))
	 )
	(xmtn-basic-io-skip-blank-lines) ;; might be at end of parsing region
	) ;; end while cert

      (setq nodes
	    (ewoc-collect
	     xmtn-sync-ewoc
	     (lambda (data branch)
	       (string= branch (xmtn-sync-branch-name data)))
	     branch))

      (if nodes
	  ;; already some data for branch; nodes is a list of data,
	  ;; not a list of nodes
	  (let* ((data (car nodes))
		 (revlist (xmtn-sync-branch-revlist data)))
	    (setf (xmtn-sync-branch-revlist data)
		  (add-to-list 'revlist
			       (list revid date author changelog)
			       t)))
	;; new branch
	(ewoc-enter-last xmtn-sync-ewoc
			 (make-xmtn-sync-branch
			  :name branch
			  :revlist (list revid date author changelog)
			  :print-mode 'summary)))
      )))

(defun xmtn-sync-parse (begin end)
  "Parse region BEGIN END in current buffer, fill in `xmtn-sync-ewoc', erase BEGIN END."
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)

  ;; receive revision
  ;;
  ;; revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;
  ;;     cert "branch"
  ;;    value "foo2"
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;;     cert "changelog"
  ;;    value "more
  ;; "
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;;     cert "date"
  ;;    value "2010-09-21T08:29:11"
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;;     cert "author"
  ;;    value "tester@test.net"
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;;     ... more certs
  ;;
  ;; revision [f05b1451d082698243328e31829e9b2a39fb7c69]
  ;;
  ;;     ... certs
  ;;
  ;; ... more revisions with certs
  ;;
  ;; receive cert
  ;;
  ;;     cert "branch"
  ;; revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;    value "foo2"
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;;     cert "changelog"
  ;; revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;    value "more
  ;; "
  ;;      key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;
  ;; ... more unattached certs
  ;;
  ;; receive key
  ;;
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; ... more keys
  ;;
  ;; send revision
  ;;
  ;; ... sent revisions, certs
  ;;
  ;; send cert
  ;;
  ;; send key

  (xmtn-basic-io-optional-line-2 '("receive" (symbol "revision"))
    (xmtn-sync-parse-revisions 'receive))

  (xmtn-basic-io-skip-blank-lines)
  (xmtn-basic-io-optional-line-2 '("receive" (symbol "cert"))
    (xmtn-sync-parse-certs 'receive nil))

  (xmtn-basic-io-skip-blank-lines)
  (xmtn-basic-io-optional-line-2 '("receive" (symbol "key"))
    (xmtn-sync-parse-keys 'receive))

  (xmtn-basic-io-skip-blank-lines)
  (xmtn-basic-io-optional-line-2 '("send" (symbol "revision"))
    (xmtn-sync-parse-revisions 'send))

  (xmtn-basic-io-skip-blank-lines)
  (xmtn-basic-io-optional-line-2 '("send" (symbol "cert"))
    (xmtn-sync-parse-certs 'send nil))

  (xmtn-basic-io-skip-blank-lines)
  (xmtn-basic-io-optional-line-2 '("send" (symbol "key"))
    (xmtn-sync-parse-keys 'send))

  (delete-region begin end)
  )

;;;###autoload
(defun xmtn-sync-sync (local-db remote-host remote-db)
  "Sync LOCAL-DB with REMOTE-HOST REMOTE-DB, display sent and received branches.
Remote-db should include branch pattern in URI syntax."
  (interactive "flocal db: \nMremote-host: \nMremote-db: ")
  (pop-to-buffer (get-buffer-create "*xmtn-sync*"))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))

  ;; `xmtn-sync-parse' creates ewoc entries, which are inserted into
  ;; the xmtn-sync buffer. Since it is parsing the same buffer, we
  ;; need them to be inserted _after_ the text that is being
  ;; parsed. `xmtn-sync-mode' creates the ewoc at point.

  ;; FIXME: need the ticker to show sync progress

  (let ((xmtn-executable xmtn-sync-executable)
        (xmtn--minimum-required-command-version xmtn-sync-required-command-version)
	parse-end
	(header (concat
		 (format "   local db: %s\n" local-db)
		 (format "remote host: %s\n" remote-host)
		 (format "  remote db: %s\n" remote-db)))
	(footer "")
	(msg "Running mtn sync ..."))

    (message msg)

    ;; pass remote command to mtn via Lua hook get_mtn_command; see
    ;; xmtn-hooks.lua
    (setenv "XMTN_SYNC_MTN"
            (or (cadr (assoc remote-host xmtn-sync-remote-exec-alist))
                xmtn-sync-remote-exec-default))

    (xmtn-automate-command-output-buffer
     default-directory ; root
     (current-buffer) ; output-buffer
     (list
      (list "db" local-db) ;; options
      "sync" (concat remote-host remote-db)) ;; command, args
     )

    (message (concat msg " done"))

    (goto-char (point-max))
    (setq parse-end (point-max))
    (xmtn-sync-mode)
    (setq buffer-read-only nil)
    (ewoc-set-hf xmtn-sync-ewoc header footer)

    (xmtn-sync-parse (point-min) parse-end)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(provide 'xmtn-sync)

;; end of file
