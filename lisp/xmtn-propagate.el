;;; xmtn-propagate.el --- manage multiple propagations for DVC backend for monotone

;; Copyright (C) 2009 Stephen Leake

;; Author: Stephen Leake
;; Keywords: tools

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

(defvar xmtn-propagate-from-root ""
  "Buffer-local variable holding `from' root directory.")
(make-variable-buffer-local 'xmtn-propagate-from-root)

(defvar xmtn-propagate-to-root ""
  "Buffer-local variable holding `to' root directory.")
(make-variable-buffer-local 'xmtn-propagate-to-root)

(defstruct (xmtn-propagate-data (:copier nil))
  workspace          ; directory name (relative to xmtn-propagate-from-root or xmtn-propagate-to-root)
  from-rev           ; mtn rev string
  to-rev             ; mtn rev string
  propagate-needed   ; nil | t
  from-at-head       ; nil | t | count of heads
  to-at-head         ; 
  from-local-changes ; nil | t
  to-local-changes   ; nil | t
  conflicts          ; nil | t
;  need-clean         ; nil | t
  )

(defun xmtn-propagate-printer (data)
  "Print an ewoc element."
  (insert (format "workspace:          %s\n" (xmtn-propagate-data-workspace data)))
  (insert (format "propagate-needed:   %s\n" (xmtn-propagate-data-propagate-needed data)))
  (insert (format "from-at-head:       %s\n" (xmtn-propagate-data-from-at-head data)))
  (insert (format "to-at-head:         %s\n" (xmtn-propagate-data-to-at-head data)))
  (insert (format "from-local-changes: %s\n" (xmtn-propagate-data-from-local-changes data)))
  (insert (format "to-local-changes:   %s\n" (xmtn-propagate-data-to-local-changes data)))
  (insert (format "conflicts:          %s\n" (xmtn-propagate-data-conflicts data)))
  )

(defvar xmtn-propagate-ewoc nil
  "Buffer-local ewoc for displaying propagations.
All xmtn-propagate functions operate on this ewoc.
The elements must all be of class xmtn-propagate-data.")
(make-variable-buffer-local 'xmtn-propagate-ewoc)

(define-derived-mode xmtn-propagate-mode nil "xmtn-propagate"
  "Major mode to propagate multiple workspaces."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))
;;   (use-local-map xmtn-propagate-mode-map)
;;   (easy-menu-add xmtn-propagate-mode-menu)
  (setq dvc-buffer-refresh-function nil)

  ;; don't do normal clean up stuff
  (set (make-local-variable 'before-save-hook) nil)
  (set (make-local-variable 'write-file-functions) nil)

  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(defun xmtn-propagate-local-changes (work)
  "t if WORK has local changes."
  (let ((default-directory work))
    (message "checking %s for local changes" work)

    (dvc-run-dvc-sync
     'xmtn
     (list "status")
     :finished (lambda (output error status arguments)
                 ;; we don't get an error status for not up-to-date,
                 ;; so parse the output.
                 ;; FIXME: add option to automate inventory to just return status; can return on first change
                 ;; FIXME: 'patch' may be internationalized.
                 (set-buffer output)
                 (goto-char (point-min))
                 (not (not (search-forward "patch" (point-max) t))))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))))

  )

(defun xmtn-propagate-needed (from-rev to-rev)
  "t if branch in workspace FROM-WORK needs to be propagated to TO-WORK."
  (let ((result nil))

    (if (string= from-rev to-rev)
        nil
      ;; check for to descendant of from
      (let ((descendents (xmtn-automate-simple-command-output-lines from-work (list "descendents" from-rev)))
            (done nil))
        (while (and descendents (not done))
          (if (string= to-rev (car descendents))
              (progn
                (setq result t)
                (setq done t)))
          (setq descendents (cdr descendents)))))
    result
  ))

(defun xmtn-propagate-conflicts (from-work from-rev to-work to-rev)
  "t if there are unresolved conflicts between FROM-WORK and TO-WORK."
  (let ((default-directory to-work)
        (conflicts-buffer (dvc-get-buffer 'xmtn 'conflicts default-directory)))
    
    (if (not conflicts-buffer)
        (progn
          (if (not (file-exists-p "_MTN/conflicts"))
              (progn
                ;; create conflicts file
                (xmtn-conflicts-save-opts from-work to-work)
                (dvc-run-dvc-sync
                 'xmtn
                 (list "conflicts" "store" from-rev to-rev)
                 :finished (lambda (output error status arguments)
                             (xmtn-dvc-log-clean)
                             
                             :error (lambda (output error status arguments)
                                      (xmtn-dvc-log-clean)
                                      (pop-to-buffer error))))))
          ;; create conflicts buffer
          (save-excursion
            (let ((dvc-switch-to-buffer-first nil))
              (xmtn-conflicts-review default-directory)
              (setq conflicts-buffer (current-buffer))))))

    (save-excursion
      (set-buffer conflicts-buffer)
      (xmtn-conflicts-update-counts)
      (> xmtn-conflicts-total-count xmtn-conflicts-resolved-count))))

(defun xmtn-propagate-refresh ()
  "Refresh status of each ewoc element."
  (interactive)
  (ewoc-map
   (lambda (data)
     (let ((from-work (concat xmtn-propagate-from-root (xmtn-propagate-data-workspace data)))
           (to-work (concat xmtn-propagate-to-root (xmtn-propagate-data-workspace data))))
       
       (setf (xmtn-propagate-data-from-rev data) (xmtn--get-base-revision-hash-id-or-null from-work))
       (setf (xmtn-propagate-data-to-rev data) (xmtn--get-base-revision-hash-id-or-null to-work))
       
       (setf (xmtn-propagate-data-propagate-needed data)
             (xmtn-propagate-needed
              (xmtn-propagate-data-from-rev data)
              (xmtn-propagate-data-to-rev data)))
       
       (let ((heads (xmtn--heads from-work nil)))
         (if (> 1 (length heads))
             (setf (xmtn-propagate-data-from-at-head data) (length heads))
           (setf (xmtn-propagate-data-from-at-head data)
                 (string= (xmtn-propagate-data-from-rev data) (nth 0 heads)))))
       
       (let ((heads (xmtn--heads to-work nil)))
         (if (> 1 (length heads))
             (setf (xmtn-propagate-data-to-at-head data) (length heads))
           (setf (xmtn-propagate-data-to-at-head data)
                 (string= (xmtn-propagate-data-to-rev data) (nth 0 heads)))))
       
       (if (xmtn-propagate-data-propagate-needed data)
           ;; these checks are slow, so don't do them if they probably are not needed.
           (progn
             (setf (xmtn-propagate-data-from-local-changes data) (xmtn-propagate-local-changes from-work))
             (setf (xmtn-propagate-data-to-local-changes data) (xmtn-propagate-local-changes to-work))
             
             (setf (xmtn-propagate-data-conflicts data)
                   (xmtn-propagate-conflicts from-work (xmtn-propagate-data-from-rev data)
                                             to-work (xmtn-propagate-data-to-rev data))))))

     ;; return non-nil to refresh display as we go along
     t)
   xmtn-propagate-ewoc))

(defun xmtn--filter-non-dir (dir)
  "Return list of all directories in DIR, excluding '.', '..'."
  (let ((default-directory dir)
        (subdirs (directory-files dir)))
    (setq subdirs
          (mapcar (lambda (filename)
                    (if (and (file-directory-p filename)
                             (not (string= "." filename))
                             (not (string= ".." filename)))
                        filename))
                  subdirs))
    (delq nil subdirs)))

;;;###autoload
(defun xmtn-propagate-multiple (from-dir to-dir)
  "Show all actions needed to propagate all projects under FROM-DIR to TO-DIR."
  (interactive "DPropagate all from (root directory): \nDto (workspace directory): ")
  (let ((from-workspaces (xmtn--filter-non-dir from-dir))
        (to-workspaces (xmtn--filter-non-dir to-dir)))

    (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
    (xmtn-propagate-mode)
    (setq xmtn-propagate-from-root (file-name-as-directory from-dir))
    (setq xmtn-propagate-to-root (file-name-as-directory to-dir))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (dolist (workspace from-workspaces)
      (if (member workspace to-workspaces)
          (ewoc-enter-last xmtn-propagate-ewoc (make-xmtn-propagate-data :workspace workspace))))

    (xmtn-propagate-refresh)))
  
(provide 'xmtn-propagate)

;; end of file
