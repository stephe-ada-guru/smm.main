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

(eval-and-compile
  ;; these have macros we use
  (require 'xmtn-ids))

(eval-when-compile
  ;; these have functions we use
  (require 'xmtn-conflicts))

(defvar xmtn-propagate-from-root ""
  "Buffer-local variable holding `from' root directory.")
(make-variable-buffer-local 'xmtn-propagate-from-root)

(defvar xmtn-propagate-to-root ""
  "Buffer-local variable holding `to' root directory.")
(make-variable-buffer-local 'xmtn-propagate-to-root)

(defstruct (xmtn-propagate-data (:copier nil))
  from-work          ; directory name relative to xmtn-propagate-from-root
  to-work            ; directory name relative to xmtn-propagate-to-root
  need-refresh       ; nil | t; if an async process was started that invalidates state data
  from-rev           ; mtn rev string
  to-rev             ; mtn rev string
  conflicts-buffer   ; *xmtn-conflicts* buffer for this propagation
  propagate-needed   ; nil | t
  from-at-head       ; nil | t | count of heads
  to-at-head         ;
  from-local-changes ; nil | t
  to-local-changes   ; nil | t
  conflicts          ; nil | count of unresolved conflicts
  )

(defun xmtn-propagate-printer (data)
  "Print an ewoc element."
  (if (string= (xmtn-propagate-data-from-work data)
               (xmtn-propagate-data-to-work data))
      (insert (dvc-face-add (format "%s\n" (xmtn-propagate-data-from-work data)) 'dvc-keyword))
    (insert (dvc-face-add (format "%s -> %s\n"
                                  (xmtn-propagate-data-from-work data)
                                  (xmtn-propagate-data-to-work data))
                          'dvc-keyword)))

  (if (xmtn-propagate-data-need-refresh data)
      (insert (dvc-face-add "  need refresh\n" 'dvc-conflict))

    (if (xmtn-propagate-data-propagate-needed data)
        (progn
          (if (xmtn-propagate-data-from-local-changes data)
              (insert (dvc-face-add "  need dvc-status from\n" 'dvc-header)))
          (if (xmtn-propagate-data-to-local-changes data)
              (insert (dvc-face-add "  need dvc-status to\n" 'dvc-header)))

          (if (not (xmtn-propagate-data-from-at-head data))
              (insert (dvc-face-add "  need dvc-missing from\n" 'dvc-conflict)))
          (if (not (xmtn-propagate-data-to-at-head data))
              (insert (dvc-face-add "  need dvc-missing to\n" 'dvc-conflict)))

          (if (and (xmtn-propagate-data-from-at-head data)
                   (xmtn-propagate-data-to-at-head data)
                   (xmtn-propagate-data-conflicts data))
              (insert (dvc-face-add "  need conflicts resolved\n" 'dvc-conflict)))
          )

      ;; propagate not needed
      (if (not (xmtn-propagate-data-to-at-head data))
          (insert (dvc-face-add "  need dvc-update to\n" 'dvc-conflict)))
      )))

(defvar xmtn-propagate-ewoc nil
  "Buffer-local ewoc for displaying propagations.
All xmtn-propagate functions operate on this ewoc.
The elements must all be of class xmtn-propagate-data.")
(make-variable-buffer-local 'xmtn-propagate-ewoc)

(defun xmtn-propagate-clean ()
  "Clean current workspace, delete from ewoc"
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (xmtn-conflicts-clean))
    (ewoc-delete xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-cleanp ()
  "Non-nil if clean is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (not (xmtn-propagate-data-propagate-needed data))
         (not (xmtn-propagate-data-to-at-head data))
         (file-exists-p (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data) "/_MTN/conflicts")))))

(defun xmtn-propagate-refreshp ()
  "Non-nil if refresh is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (xmtn-propagate-data-need-refresh data)))

(defun xmtn-propagate-update ()
  "Update current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (xmtn-dvc-update))
    (xmtn-propagate-refresh-one data)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-updatep ()
  "Non-nil if update is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (not (xmtn-propagate-data-propagate-needed data))
         (not (xmtn-propagate-data-to-at-head data)))))

(defun xmtn-propagate-propagate ()
  "Propagate current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (xmtn-conflicts-do-propagate))
    (xmtn-propagate-refresh-one data)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-propagatep ()
  "Non-nil if propagate is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (xmtn-propagate-data-from-at-head data)
         (xmtn-propagate-data-to-at-head data)
         (= 0 (xmtn-propagate-data-conflicts data)))))

(defun xmtn-propagate-status-to ()
  "Run xmtn-status on current to workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-need-refresh data) t)
    (ewoc-invalidate xmtn-propagate-ewoc elem)
    (xmtn-status (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data)))))

(defun xmtn-propagate-status-top ()
  "Non-nil if xmtn-status is appropriate for current to workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (xmtn-propagate-data-to-local-changes data))))

(defun xmtn-propagate-status-from ()
  "Run xmtn-status on current from workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-need-refresh data) t)
    (ewoc-invalidate xmtn-propagate-ewoc elem)
    (xmtn-status (concat xmtn-propagate-from-root (xmtn-propagate-data-from-work data)))))

(defun xmtn-propagate-status-fromp ()
  "Non-nil if xmtn-status is appropriate for current from workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (xmtn-propagate-data-from-local-changes data))))

(defun xmtn-propagate-missing-to ()
  "Run xmtn-missing on current to workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-need-refresh data) t)
    (ewoc-invalidate xmtn-propagate-ewoc elem)
    (xmtn-missing nil (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data)))))

(defun xmtn-propagate-missing-top ()
  "Non-nil if xmtn-missing is appropriate for current to workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (not (xmtn-propagate-data-to-at-head data)))))

(defun xmtn-propagate-missing-from ()
  "Run xmtn-missing on current from workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-need-refresh data) t)
    (ewoc-invalidate xmtn-propagate-ewoc elem)
    (xmtn-missing nil (concat xmtn-propagate-from-root (xmtn-propagate-data-from-work data)))))

(defun xmtn-propagate-missing-fromp ()
  "Non-nil if xmtn-missing is appropriate for current from workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (not (xmtn-propagate-data-from-at-head data)))))

(defvar xmtn-propagate-actions-map
  (let ((map (make-sparse-keymap "actions")))
    (define-key map [?c]  '(menu-item "c) clean/quit"
                                      xmtn-propagate-clean
                                      :visible (xmtn-propagate-cleanp)))
    (define-key map [?g]  '(menu-item "g) refresh"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-propagate-refresh-one
                                         (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
                                      :visible (xmtn-propagate-refreshp)))
    (define-key map [?5]  '(menu-item "5) update"
                                      xmtn-propagate-update
                                      :visible (xmtn-propagate-updatep)))
    (define-key map [?4]  '(menu-item "4) propagate"
                                      xmtn-propagate-propagate
                                      :visible (xmtn-propagate-propagatep)))
    (define-key map [?3]  '(menu-item "3) status from"
                                      xmtn-propagate-status-from
                                      :visible (xmtn-propagate-status-fromp)))
    (define-key map [?2]  '(menu-item "2) status to"
                                      xmtn-propagate-status-to
                                      :visible (xmtn-propagate-status-top)))
    (define-key map [?1]  '(menu-item "1) dvc-missing from"
                                      xmtn-propagate-missing-from
                                      :visible (xmtn-propagate-missing-fromp)))
    (define-key map [?0]  '(menu-item "0) dvc-missing to"
                                      xmtn-propagate-missing-to
                                      :visible (xmtn-propagate-missing-top)))
    map)
  "Keyboard menu keymap used to manage propagates.")

(dvc-make-ewoc-next xmtn-propagate-next xmtn-propagate-ewoc)
(dvc-make-ewoc-prev xmtn-propagate-prev xmtn-propagate-ewoc)

(defvar xmtn-propagate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" xmtn-propagate-actions-map)
    (define-key map [?g]  'xmtn-propagate-refresh)
    (define-key map [?n]  'xmtn-propagate-next)
    (define-key map [?p]  'xmtn-propagate-prev)
    (define-key map [?q]  (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Keymap used in `xmtn-propagate-mode'.")

(define-derived-mode xmtn-propagate-mode nil "xmtn-propagate"
  "Major mode to propagate multiple workspaces."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))

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

(defun xmtn-propagate-needed (from-work from-rev to-rev)
  "t if branch in workspace FROM-WORK needs to be propagated to TO-WORK."
  (let ((result nil))

    (if (string= from-rev to-rev)
        nil
      ;; check for to descendant of from
      (let ((descendents (xmtn-automate-simple-command-output-lines from-work (list "descendents" from-rev)))
            (done nil))
        (if (not descendents)
            (setq result t)
          (while (and descendents (not done))
            (if (string= to-rev (car descendents))
                (progn
                  (setq result t)
                  (setq done t)))
            (setq descendents (cdr descendents))))))
    result
  ))

(defun xmtn-propagate-conflicts-buffer (from-work from-rev to-work to-rev)
  "return a conflicts buffer for FROM-WORK, TO-WORK."
  (let ((default-directory to-work)
        (conflicts-buffer (dvc-get-buffer 'xmtn 'conflicts to-work)))

    (or conflicts-buffer
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
              (current-buffer)))))))

(defun xmtn-propagate-conflicts (conflicts-buffer)
  "Count of unresolved conflicts in CONFLICTS-BUFFER."
    (with-current-buffer conflicts-buffer
      (xmtn-conflicts-update-counts)
      (- xmtn-conflicts-total-count xmtn-conflicts-resolved-count)))

(defun xmtn-propagate-refresh-one (data)
  "Refresh DATA."
  (let ((from-work (concat xmtn-propagate-from-root (xmtn-propagate-data-from-work data)))
        (to-work (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data))))

    (setf (xmtn-propagate-data-from-rev data) (xmtn--get-base-revision-hash-id-or-null from-work))
    (setf (xmtn-propagate-data-to-rev data) (xmtn--get-base-revision-hash-id-or-null to-work))

    (setf (xmtn-propagate-data-propagate-needed data)
          (xmtn-propagate-needed
           from-work
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

          (setf (xmtn-propagate-data-conflicts-buffer data)
                (xmtn-propagate-conflicts-buffer from-work (xmtn-propagate-data-from-rev data)
                                                 to-work (xmtn-propagate-data-to-rev data)))

          (setf (xmtn-propagate-data-conflicts data)
                (xmtn-propagate-conflicts (xmtn-propagate-data-conflicts-buffer data))))

      ;; show data not valid
      (setf (xmtn-propagate-data-conflicts data) nil))

    (setf (xmtn-propagate-data-need-refresh data) nil))

  ;; return non-nil to refresh display as we go along
  t)

(defun xmtn-propagate-refresh ()
  "Refresh status of each ewoc element."
  (interactive)
  (ewoc-map 'xmtn-propagate-refresh-one xmtn-propagate-ewoc)
  (message "done"))

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
  (interactive "DPropagate all from (root directory): \nDto (root directory): ")
  (let ((from-workspaces (xmtn--filter-non-dir from-dir))
        (to-workspaces (xmtn--filter-non-dir to-dir)))

    (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
    (xmtn-propagate-mode)
    (setq xmtn-propagate-from-root (file-name-as-directory from-dir))
    (setq xmtn-propagate-to-root (file-name-as-directory to-dir))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (ewoc-set-hf
     xmtn-propagate-ewoc
     (concat
      (format "From root : %s\n" xmtn-propagate-from-root)
      (format "  To root : %s\n" xmtn-propagate-to-root)
      )
     "")
    (dolist (workspace from-workspaces)
      (if (member workspace to-workspaces)
          (ewoc-enter-last xmtn-propagate-ewoc
                           (make-xmtn-propagate-data
                            :to-work workspace
                            :from-work workspace
                            :need-refresh t))))

    (xmtn-propagate-refresh)
    (xmtn-propagate-next)))

(defun xmtn-propagate-one (from-work to-work)
  "Show all actions needed to propagate FROM-WORK to TO-WORK."
  (interactive "DPropagate all from (workspace): \nDto (workspace): ")
  (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
  (xmtn-propagate-mode)
  (setq xmtn-propagate-from-root (expand-file-name (concat (file-name-as-directory from-work) "../")))
  (setq xmtn-propagate-to-root (expand-file-name (concat (file-name-as-directory to-work) "../")))

  (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
  (ewoc-set-hf
   xmtn-propagate-ewoc
   (concat
    (format "From root : %s\n" xmtn-propagate-from-root)
    (format "  To root : %s\n" xmtn-propagate-to-root)
    )
   "")
  (ewoc-enter-last xmtn-propagate-ewoc
                   (make-xmtn-propagate-data
                    :from-work (file-name-nondirectory from-work)
                    :to-work (file-name-nondirectory to-work)
                    :need-refresh t))

  (xmtn-propagate-refresh)
  (xmtn-propagate-next))

(provide 'xmtn-propagate)

;; end of file
