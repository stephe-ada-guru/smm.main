;;; dvc-diff.el --- A generic diff mode for DVC

;; Copyright (C) 2005-2007 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;;; Code:

(require 'diff-mode)
(require 'dvc-ui)
(require 'dvc-unified)
(require 'dvc-defs)
(require 'dvc-core)
(require 'dvc-fileinfo)
(eval-when-compile (require 'cl))

(defvar dvc-diff-base nil
  "BASE revision-id for the changes currently displayed.")
(make-variable-buffer-local 'dvc-diff-base)

(defvar dvc-diff-modified nil
  "MODIFIED revision-id for the changes currently displayed.")
(make-variable-buffer-local 'dvc-diff-modified)

(defun dvc-dvc-search-file-in-diff (file)
  "Default for back-end-specific regexp maching diff beginning."
  (re-search-forward (concat "^\\+\\+\\+ \\(b\\|mod\\)/" file "\\(.+[0-9][0-9][0-9][0-9]\\)?$")))

(defun dvc-diff-prepare-buffer (dvc root base modified)
  "Create and return a buffer to run command showing diffs.

Sets `dvc-diff-base' and `dvc-diff-modified' to BASE and
MODIFIED.

ROOT must be root of workspace for DVC."
  (with-current-buffer
      (dvc-get-buffer-create dvc (if (eq (dvc-revision-get-type modified) 'local-tree) 'diff 'revision-diff) root)
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((dvc-temp-current-active-dvc dvc))
      (funcall (dvc-function dvc "diff-mode")))
    (setq dvc-diff-base     base)
    (setq dvc-diff-modified modified)
    (current-buffer)))

;; ----------------------------------------------------------------------------
;; dvc-diff-mode
;; ----------------------------------------------------------------------------

(defvar dvc-diff-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map "\C-m"   'dvc-diff-jump-to-change)
    (define-key map [return] 'dvc-diff-jump-to-change)
    (define-key map [(control x) (control j)] 'dvc-dired-jump)
    (define-key map "\M-="          'dvc-diff-scroll-up-or-diff)
    (define-key map [(meta return)] 'dvc-diff-scroll-down-or-diff)
    (define-key map "\M-\C-m"       'dvc-diff-scroll-down-or-diff)
    (define-key map [?=] 'dvc-diff-diff)
    (define-key map dvc-keyvec-add  'dvc-add-files)
    (define-key map [?h] 'dvc-buffer-pop-to-partner-buffer)
    (define-key map dvc-keyvec-logs    'dvc-log)
    (define-key map "l"                'dvc-diff-log)
    (define-key map dvc-keyvec-ediff   'dvc-diff-ediff)
    (define-key map dvc-keyvec-refresh 'dvc-generic-refresh)
    (define-key map dvc-keyvec-commit  'dvc-log-edit)
    ;; TODO move this somewhere else.
    (define-key map [?I] 'tla-inventory)
    (define-key map dvc-keyvec-inventory 'dvc-pop-to-inventory)

    (define-key map dvc-keyvec-next      'dvc-diff-next)
    (define-key map dvc-keyvec-previous  'dvc-diff-prev)
    (define-key map dvc-keyvec-revert    'dvc-revert-files)
    (define-key map dvc-keyvec-quit      'dvc-buffer-quit)
    (define-key map dvc-keyvec-remove    'dvc-remove-files)
    (define-key map [?d]                 'dvc-remove-files); as in dired
    (define-key map dvc-keyvec-mark      'dvc-fileinfo-mark-file)
    (define-key map dvc-keyvec-unmark    'dvc-fileinfo-unmark-file)
    (define-key map [backspace]          'dvc-fileinfo-unmark-file-up)
    (define-key map [?v]                 'dvc-diff-view-source)
    (define-key map dvc-keyvec-parent    'dvc-diff-master-buffer)
    (define-key map [?j]                 'dvc-diff-diff-or-list)
    (define-key map (dvc-prefix-kill-ring ?d) 'dvc-diff-save-current-defun-as-kill)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    ;; Ignore file handling
    (define-key map (dvc-prefix-tagging-method ?i) 'dvc-ignore-files)
    (define-key map (dvc-prefix-tagging-method ?I) 'dvc-ignore-file-extensions)
    ;; C-i is not a good choice since it's TAB in emacs -nw
    (define-key map (dvc-prefix-tagging-method ?e) 'dvc-edit-ignore-files)

    ;; working copy bindings
    (define-key map (vector dvc-key-working-copy) nil) ;; unbind ?W, before it can be used
    (define-key map (dvc-prefix-working-copy ?s) 'dvc-save-diff)

    ;; the merge group
    (define-key map (dvc-prefix-merge ?u) 'dvc-update)
    (define-key map (dvc-prefix-merge ?f) 'dvc-pull) ;; hint: fetch, p is reserved for push
    (define-key map (dvc-prefix-merge ?m) 'dvc-missing)
    (define-key map (dvc-prefix-merge ?M) 'dvc-merge)
    map)
  "Keymap used in `dvc-diff-mode'.")

;;
;; Menu
;;
(defconst dvc-diff-file-menu-list
  '("File Changes"
    ["Jump to File"                   dvc-diff-jump-to-change t]
    ["Jump to Diffs"                  dvc-diff-diff-or-list   t]
    ["View Diff in Separate Buffer"   dvc-diff-diff           t]
    ["View Diff with Ediff"           dvc-diff-ediff          t]
    ["Log (full tree)"                dvc-log                 t]
    ["Log (single file)"              dvc-diff-log            t]
    "--"
    ["Delete File"                    dvc-remove-files        t]
    ["Revert File"                    dvc-revert-files        t]
    )
  "Used both in the global and the context menu of `dvc-diff-mode'.")

(easy-menu-define dvc-diff-file-menu nil
  "Menu used on a `dvc-diff' file"
  dvc-diff-file-menu-list)

(defconst dvc-diff-mode-menu-list
  `(["Refresh Buffer" dvc-generic-refresh t]
    ["Edit log before commit" dvc-log-edit t]
    ["View other revisions" tla-tree-revisions t]
    ("Merge"
     ["Update" dvc-update t]
     ["Pull" dvc-pull t]
     ["Show missing" dvc-missing t]
     ["Merge" dvc-merge t]
     )
    ("Ignore"
     ["Ignore Files" dvc-ignore-files t]
     ["Ignore File Extensions" dvc-ignore-file-extensions t]
     ["Edit Ignore File" dvc-edit-ignore-files t]
     )
    ,dvc-diff-file-menu-list
    ))

(easy-menu-define dvc-diff-mode-menu dvc-diff-mode-map
  "`dvc-changes' menu"
  `("DVC-Diff"
    ,@dvc-diff-mode-menu-list))

(defvar dvc-diff-file-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'dvc-diff-jump-to-change-by-mouse)
    map)
  "Keymap used on files in `dvc-diff-mode' buffers.")

;; "<back-end>-diff-mode", if defined, will be used instead of this
;; one. If so, it should be derived from dvc-diff-mode (via
;; `define-derived-mode'), and rely on it for as many features as
;; possible (one can, for example, extend the menu and keymap). See
;; `xgit-diff-mode' in xgit.el for a good example.
(define-derived-mode dvc-diff-mode fundamental-mode "dvc-diff"
  "Major mode to display changesets. Derives from `diff-mode'.

Use '\\<dvc-diff-mode-map>\\[dvc-diff-mark-file]' to mark files, and '\\[dvc-diff-unmark-file]' to unmark.
If you commit from this buffer (with '\\<dvc-diff-mode-map>\\[dvc-log-edit]'), then,
the list of selected files (in this buffer) will be commited (with the text you
entered as a comment) at the time you actually commit with \\<dvc-log-edit-mode-map>\\[dvc-log-edit-done].

Commands:
\\{dvc-diff-mode-map}
"
  (let ((diff-mode-shared-map (copy-keymap dvc-diff-mode-map))
        major-mode mode-name)
    (diff-mode))

  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (set (make-local-variable 'font-lock-defaults)
       (list 'tla-changes-font-lock-keywords t nil nil))
  (set (make-local-variable 'dvc-get-file-info-at-point-function)
       'dvc-diff-get-file-at-point)
  (setq dvc-buffer-refresh-function 'dvc-diff-generic-refresh)
  (setq dvc-fileinfo-ewoc (ewoc-create 'dvc-fileinfo-printer))
  (setq dvc-buffer-marked-file-list nil)
  (dvc-install-buffer-menu)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun dvc-diff-generic-refresh ()
  "Refresh the diff buffer."
  (interactive)
  (if (eq (dvc-revision-get-type dvc-diff-modified) 'local-tree)
      (dvc-diff dvc-diff-base)
    (error "Don't know how to refresh buffer")))

(defun dvc-diff-in-ewoc-p ()
  "Return non-nil if in ewoc section of diff buffer."
  (let ((elem (ewoc-locate dvc-fileinfo-ewoc)))
    (>= (ewoc-location elem) (line-beginning-position))))

(defun dvc-diff-jump-to-change (&optional other-file)
  "Jump to the corresponding file and location of the change.
OTHER-FILE (default prefix) if non-nil means visit the original
file; otherwise visit the modified file."
  (interactive "P")
  (if (dvc-diff-in-ewoc-p)
      (find-file (dvc-fileinfo-current-file))
    (dvc-diff-diff-goto-source other-file)))

(defun dvc-diff-scroll-or-diff (up-or-down)
  "If file-diff buffer is visible, scroll. Otherwise, show it."
  (let ((file (dvc-get-file-info-at-point)))
    (unless file
      (error "No file at point."))
    ;; TODO
    (let ((buffer (dvc-get-buffer dvc-buffer-current-active-dvc 'file-diff file)))
      (unless (tla--scroll-maybe buffer up-or-down)
        (tla-file-diff file nil t)))))

(defun dvc-diff-scroll-up-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-up))

(defun dvc-diff-scroll-down-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-down))

(defun dvc-diff-diff-goto-source (other-file)
  "Almost the same as `diff-goto-source'.
But the target file is transformed by `tla--changes-what-changed-original-file'
to handle files in what-changed directory.
OTHER-FILE if non-nil means visit the original
file; otherwise visit the modified file."
  (let ((dvc-original-file-exists-p (symbol-function
                                     'file-exists-p))
        (dvc-original-find-file-noselect (symbol-function
                                          'find-file-noselect)))
    (flet ((file-exists-p (file)
                          (unless (string= "/dev/null" file)
                            (funcall
                             dvc-original-file-exists-p
                             ;; TODO make this generic.
                             ;; <DVC>-diff-get-filename
                             (tla--changes-what-changed-original-file file))))
           (find-file-noselect (file &optional nowarn rawfile wildcards)
             (if (featurep 'xemacs)
                 (funcall dvc-original-find-file-noselect
                          (tla--changes-what-changed-original-file file)
                          nowarn rawfile)
               (funcall dvc-original-find-file-noselect
                        (tla--changes-what-changed-original-file file)
                        nowarn rawfile wildcards))))
      (diff-goto-source other-file))))

(defun dvc-diff-diff-or-list ()
  "Jump between ewoc entries and diffs.
When in the ewoc, jump to the corresponding
diff. When on a diff, jump to the corresponding entry in the ewoc."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (progn
        (dvc-call "dvc-search-file-in-diff" (dvc-fileinfo-current-file))
        (diff-hunk-next))
    (goto-char (ewoc-location (dvc-fileinfo-find-file (dvc-diff-get-file-at-point))))))

(defun dvc-diff-diff ()
  "Show diff for file at point."
  (interactive)
  (let ((on-modified-file (dvc-get-file-info-at-point)))
    (if on-modified-file
        (let ((buf (current-buffer)))
          (dvc-file-diff on-modified-file dvc-diff-base
                         dvc-diff-modified t)
          (pop-to-buffer buf))
      (error "Not on a modified file"))))

(defun dvc-diff-next ()
  "Move to the next ewoc line or diff hunk."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-next)
    (diff-hunk-next)))

(defun dvc-diff-prev ()
  "Move to the previous ewoc line or diff hunk."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-prev)
    (diff-hunk-prev)))

(defun dvc-diff-ediff ()
  "Run ediff on the current changes."
  (interactive)
  (unless (and (car dvc-diff-base)
               (car dvc-diff-modified))
    (error "No revision information to base ediff on"))
  (let ((on-modified-file (dvc-get-file-info-at-point))
        (loc (point)))

    (if (and on-modified-file
             (dvc-diff-in-ewoc-p))
        ;; on ewoc item; just ediff
        (dvc-file-ediff-revisions on-modified-file
                                  dvc-diff-base
                                  dvc-diff-modified)
      ;; in diff section; find hunk index, so we can jump to it in the ediff.
      (end-of-line)
      (dvc-trace "loc=%S" loc)
      (let ((hunk 1))
        (re-search-backward "^--- ")
        (re-search-forward "^--- ")
        (diff-hunk-next)
        (while (<= (re-search-forward "\\(^[\\+-].*\n\\)+" nil t) loc)
          (dvc-trace "hunk=%S" hunk)
          (setq hunk (1+ hunk)))
        (goto-char loc)
        (with-current-buffer
            (dvc-file-ediff-revisions on-modified-file
                                      dvc-diff-base
                                      dvc-diff-modified)
          (ediff-jump-to-difference hunk))))))

(defun dvc-diff-log (&optional last-n)
  "Show log for current file, LAST-N entries (default
`dvc-log-last-n'; all if nil). LAST-N may be specified
interactively."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) dvc-log-last-n)))
  (dvc-log (dvc-get-file-info-at-point) last-n))

(defun dvc-diff-find-file-name ()
  "Same as `diff-find-file-name', but works in more cases."
  (cond ((re-search-backward "^\\+\\+\\+ \\(mod/\\|b/\\)?\\([^\n]*?\\)\\([ \t].*\\)?$" nil t)
         (match-string-no-properties 2))
        ((not (ewoc-locate dvc-fileinfo-ewoc (point))) ;; the buffer contains no diff
         "")
        (t
         (diff-find-file-name))))

(defun dvc-diff-get-file-at-point ()
  "Return filename for file at point.
Throw an error when not on a file."
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-current-file)
    (save-excursion
      (expand-file-name (concat (file-name-as-directory
                                 default-directory)
                                (dvc-diff-find-file-name))))))

(defvar dvc-header nil
  "Free variable used to pass info from the parser to
`dvc-diff-show-buffer' (defined with a (let ...) in
dvc-diff-show-buffer, and altered by called functions).

This is just a lint trap.")

(defun dvc-diff-show-buffer (process-buffer parser &optional
                                 diff-buffer no-switch
                                 header-end-regexp cmd)
  "Show the diff buffer built from the process buffer PROCESS-BUFFER.

PARSER is a function that parses the diff and fills in the ewoc list.

DIFF-BUFFER must have been created by dvc-diff-prepare-buffer.

If NO-SWITCH is nil, don't switch to the created buffer.

If non-nil, HEADER-END-REGEXP is a regexp matching the first line
which is not part of the diff header.

CMD, if non-nil, is appended to dvc-header."
  (let* ((root (with-current-buffer diff-buffer default-directory))
         (dvc-header ""))
    (if (or no-switch dvc-switch-to-buffer-first)
        (set-buffer diff-buffer)
      (dvc-switch-to-buffer diff-buffer))
    (let (buffer-read-only)
      (dvc-fileinfo-delete-messages)
      (with-current-buffer process-buffer
        (goto-char (point-min))
        (when cmd
          (setq dvc-header
                (concat dvc-header
                        (dvc-face-add cmd 'dvc-header) "\n"
                        (dvc-face-add (make-string  72 ?\ ) 'dvc-separator))))
        (when header-end-regexp
          (setq dvc-header
                (concat dvc-header
                        (buffer-substring-no-properties
                         (goto-char (point-min))
                         (progn (re-search-forward header-end-regexp nil t) ;; "^[^*\\.]"
                                (beginning-of-line)
                                (point))))))
        (beginning-of-line)
        (funcall parser diff-buffer)
        (let ((footer (concat
                       (dvc-face-add (make-string  72 ?\ ) 'dvc-separator)
                       "\n\n"
                       (buffer-substring-no-properties
                        (point) (point-max)))))
          (with-current-buffer diff-buffer
            (ewoc-set-hf dvc-fileinfo-ewoc dvc-header footer))))))
  (toggle-read-only 1)
  (if (progn (goto-char (point-min))
             (re-search-forward "^---" nil t))
      (when (or global-font-lock-mode font-lock-mode)
        (let ((font-lock-verbose nil))
          (font-lock-fontify-buffer)))
    ;; Disabling font-lock mode (it's useless and it removes other
    ;; faces with Emacs 21)
    (setq font-lock-keywords nil)
    (font-lock-mode -1)
    (ewoc-refresh dvc-fileinfo-ewoc))
  (if (ewoc-nth dvc-fileinfo-ewoc 0)
      (goto-char (ewoc-location (ewoc-nth dvc-fileinfo-ewoc 0)))))

(defun dvc-diff-no-changes (diff-buffer msg dir)
  "Function to call from diff parser when there are no changes in a tree.

Inserts a message in the changes buffer, and in the minibuffer.

DIFF-BUFFER is the buffer prepared by dvc-diff-prepare-buffer.
MSG is the format string for the message to the user.
DIR is a string, passed to `format' with MSG."
;; FIXME: should standardize message, using back-end user display of
;; dvc-diff-base and dvc-diff-modified.
  (with-current-buffer diff-buffer
    (let ((inhibit-read-only t))
      (dvc-fileinfo-delete-messages)
      (ewoc-enter-last dvc-fileinfo-ewoc
                       (make-dvc-fileinfo-message
                        :text (concat "* " (format msg dir) ".\n\n")))
      (ewoc-refresh dvc-fileinfo-ewoc)
      (recenter '(4))))
  (message msg dir))

(defun dvc-diff-error-in-process (diff-buffer msg output error)
  "Enter a message in DIFF-BUFFER (created by
dvc-diff-prepare-buffer), consisting of MSG and the contents of
OUTPUT and ERROR. Should be called by the error handler in the
diff parser."
  (with-current-buffer diff-buffer
    (let ((inhibit-read-only t))
      (dvc-fileinfo-delete-messages)
      (ewoc-enter-last dvc-fileinfo-ewoc
                       (make-dvc-fileinfo-message
                        :text (concat "* " msg ":\n"
                                (dvc-buffer-content output)
                                (dvc-buffer-content error))))
      (ewoc-refresh dvc-fileinfo-ewoc)
      (recenter)))
    (message msg))

(defun dvc-diff-clear-buffers (dvc root msg)
  "Clears all DVC diff buffers with root ROOT, insert message MSG.

Usefull to clear diff buffers after a commit."
  (dvc-trace "dvc-diff-clear-buffers (%S %S)"
              root msg)
  (dolist (buffer (list (dvc-get-buffer dvc 'diff root)
                        (dvc-get-buffer dvc 'revision-diff root)
                        (dvc-get-buffer dvc 'status root)))
    (when buffer
      (dvc-trace "buffer=%S" buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (ewoc-filter dvc-fileinfo-ewoc (lambda (x) nil))
          (ewoc-set-hf dvc-fileinfo-ewoc "" "")
          (ewoc-enter-first dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text msg))
          (ewoc-refresh dvc-fileinfo-ewoc))))))

;;;###autoload
(defun dvc-file-ediff (file)
  "Run ediff of FILE (default current buffer file) against last revision."
  (interactive (list (buffer-file-name)))
  (let ((file-buffer (find-file-noselect file))
        (pristine-buffer
         (dvc-revision-get-file-in-buffer
          file `(,(dvc-current-active-dvc)
                 (last-revision
                  ,(dvc-tree-root file t)
                  1)))))
    (with-current-buffer pristine-buffer
      (set-buffer-modified-p nil)
      (toggle-read-only 1)
      (let ((buffer-file-name file))
        (set-auto-mode t)))
    (dvc-ediff-buffers pristine-buffer file-buffer)))

(defun dvc-file-ediff-revisions (file base modified)
  "View changes in FILE between BASE and MODIFIED using ediff."
  (dvc-ediff-buffers
   (dvc-revision-get-file-in-buffer file base)
   (dvc-revision-get-file-in-buffer file modified)))

;;;###autoload
(defun dvc-dvc-file-diff (file &optional base modified dont-switch)
  "Default for back-end-specific file diff. View changes in FILE
between BASE (default last-revision) and MODIFIED (default
workspace version)."
  (let* ((dvc (or (car base) (dvc-current-active-dvc)))
         (base (or base `(,dvc (last-revision ,file 1))))
         (modified (or modified `(,dvc (local-tree ,file))))
         (diff-buffer (dvc-get-buffer-create
                       dvc
                       'file-diff
                       (dvc-uniquify-file-name file)))
         (base-buffer
          (dvc-revision-get-file-in-buffer file base))
         (modified-buffer
          (dvc-revision-get-file-in-buffer file modified))
         (base-file (make-temp-file "DVC-file-diff-base"))
         (modified-file (make-temp-file "DVC-file-diff-mod")))
    (with-temp-file base-file
      (insert (with-current-buffer base-buffer (buffer-string)))
      (setq buffer-file-coding-system (with-current-buffer base-buffer
                                        buffer-file-coding-system)))
    (with-temp-file modified-file
      (insert (with-current-buffer modified-buffer (buffer-string)))
      (setq buffer-file-coding-system (with-current-buffer modified-buffer
                                        buffer-file-coding-system)))
    (dvc-switch-to-buffer diff-buffer)
    (let ((inhibit-read-only t)
          (slash (unless (file-name-absolute-p file) "/")))
      (erase-buffer)
      (call-process dvc-diff-executable nil diff-buffer nil
                    "-u"
                    ;; FIXME: If the file has been renamed between
                    ;; BASE and MODIFIED, the file names as
                    ;; displayed here may be incorrect.  The
                    ;; protocol needs to be extended to allow the
                    ;; backend to supply the correct file names.
                    (concat "-La" slash file)
                    (concat "-Lb" slash file)
                    base-file modified-file))
    (delete-file base-file)
    (delete-file modified-file)
    (message "")
    (toggle-read-only 1)
    (goto-char (point-min))
    (diff-mode)))

(defun dvc-ediff-startup-hook ()
  "Passed as a startup hook for ediff.

Programs ediff to return to the current window configuration after
quitting."
  ;; ediff-after-quit-hook-internal is local to an ediff session.
  (add-hook 'ediff-after-quit-hook-internal
            (dvc-capturing-lambda ()
               (set-window-configuration
                (capture dvc-window-config)))
            nil 'local))

(defvar dvc-window-config nil
  "Used for inter-function communication. Actual value is let-bound.")

(defun dvc-ediff-buffers (bufferA bufferB)
  "Wrapper around `ediff-buffers'.

Calls `ediff-buffers' on BUFFERA and BUFFERB."
  (let ((dvc-window-config (current-window-configuration)))
    (ediff-buffers bufferA bufferB
                   '(dvc-ediff-startup-hook) 'dvc-ediff)))

(provide 'dvc-diff)
;;; dvc-diff.el ends here
