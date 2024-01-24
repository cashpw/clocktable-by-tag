;;; clocktable-by-tag.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: November 14, 2023
;; Modified: November 14, 2023
;; Version: 0.0.1
;; Keywords: calendar
;; Homepage: https://github.com/cashpw/clocktable-by-tag
;; Package-Requires: ((emacs "29.1") (s "1.13.1") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Group clocktable entries by their first tag.
;;
;;; Code:

(require 'org-clock)
(require 's)
(require 'dash)

(defgroup clocktable-by-tag nil
  "Options related to the clocktable-by-tag dblock."
  :tag "Org Clock by tag")

(defcustom clocktable-by-tag--dblock-name "clocktable-by-tag"
  "Name of the dblock this package generates."
  :type 'string
  :group 'clocktable-by-tag)

(defcustom clocktable-by-tag--default-properties '(:maxlevel 2 :files org-agenda-files)
  "Default properties for new clocktable-by-tag.

These are inserted into the BEGIN line when we generate a new report."
  :type 'plist
  :group 'clocktable-by-tag)

(defun in-clocktable-by-tag-p ()
  "Check if the cursor is in a clocktable-by-tag."
  (let ((pos (point))
        (begin-target (s-lex-format "^[ \t]*#\\+BEGIN:[ \t]+${clocktable-by-tag--dblock-name}"))
        (end-target "^[ \t]*#\\+END:.*")
        start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward begin-target nil t)
           (setq start (match-beginning 0))
           (re-search-forward end-target nil t)
           (>= (match-end 0) pos)
           start))))

(defun clocktable-by-tag-report (&optional arg)
  "Update or create a table containing a report about clocked time by tag.

If point is inside an existing clocktable block, update it.
Otherwise, insert a new one.

The new table inherits its properties from the variable
`clocktable-by-tag--default-properties'.

The scope of the clocktable, when not specified in the previous
variable, is `subtree' of the current heading when the function is
called from inside heading, and `file' elsewhere (before the first
heading).

When called with a prefix argument, move to the first clock table
in the buffer and update it.

Based on `org-clock-report'."
  (interactive "P")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock clocktable-by-tag--dblock-name)
    (org-fold-show-entry))
  (pcase (in-clocktable-by-tag-p)
    (`nil
     (org-create-dblock
      (org-combine-plists
       (list :scope (if (org-before-first-heading-p) 'file 'subtree))
       clocktable-by-tag--default-properties
       `(:name ,clocktable-by-tag--dblock-name))))
    (start (goto-char start)))
  (org-update-dblock))

(defun clocktable-by-tag--shift-cell (n)
  "Return a N-wide table shift."
  (s-repeat n "| "))

(defun clocktable-by-tag--get-clock-data (files params)
  "Return a list of all clock table data entries from FILES.

- PARAMS is a plist; see `org-clock-get-table-data'"
  (seq-reduce (lambda (all-clock-data file)
                (let* ((clock-data (with-current-buffer (find-file-noselect file)
                                     (org-clock-get-table-data (buffer-name)
                                                               params))))
                  (cl-destructuring-bind (_ total-minutes entries) clock-data
                    (if (> total-minutes 0)
                        (append all-clock-data
                                entries)
                      all-clock-data))))
              files
              '()))

(defun clocktable-by-tag--insert-row (tag entries merge-duplicate-headlines)
  "Insert a row of ENTRIES for TAG.

- ENTRIES: List of entries with TAG; see `org-clock-get-table-data'"
  (insert "|--\n")
  (insert (s-lex-format "| ${tag} | *Tag time* |\n"))
  (let ((total 0))
    (if merge-duplicate-headlines
        (let ((entries-by-headline (-group-by (lambda (entry)
                                                (cl-destructuring-bind (_ headline _ _ _ _) entry
                                                  headline))
                                              entries)))
          (cl-dolist (entry-alist entries-by-headline)
            (let* ((headline (car entry-alist))
                   (entries (cdr entry-alist))
                   (level (cl-destructuring-bind (level _ _ _ _ _) (nth 0 entries)
                            level))
                   (minutes (--reduce-from (+ acc
                                              (cl-destructuring-bind (_ _ _ _ minutes _) it
                                                minutes))
                                           0
                                           entries)))
              (setq total (+ total minutes))
              (let ((indent (org-clocktable-indent-string level))
                    (shift-cell (clocktable-by-tag--shift-cell level))
                    (duration (org-duration-from-minutes minutes)))
                (insert (s-lex-format "| |${indent}${headline} | ${shift-cell} ${duration} |\n"))))))
      (cl-dolist (entry entries)
        (cl-destructuring-bind (level headline _ _ minutes _) entry
          (setq total (+ total minutes))
          (let ((indent (org-clocktable-indent-string level))
                (shift-cell (clocktable-by-tag--shift-cell level))
                (duration (org-duration-from-minutes minutes)))
            (insert (s-lex-format "| |${indent}${headline} | ${shift-cell} ${duration} |\n"))))))
    (save-excursion
      (let ((duration (org-duration-from-minutes total)))
        (re-search-backward "*Tag time*")
        (org-table-next-field)
        (org-table-blank-field)
        (insert (s-lex-format "*${duration}*")))))
  (org-table-align))

(defun clocktable-by-tag--get-tags (clock-data-entries)
  "Return unique list of tags within CLOCK-DATA-ENTRIES.

This function expects output in the form of a list of
entries from `org-clock-get-table-data'."
  (seq-uniq
   (seq-reduce
    (lambda (tags-with-duplicates entry)
      (append tags-with-duplicates
              (nth 2 entry)))
    clock-data-entries
    '())))

(defun clocktable-by-tag--get-entries-by-tag-hash (entries)
  "Build a hash table of ENTRIES indexed by their first tag."
  (let ((entry-hash (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((tags (nth 2 entry))
             (tag (nth 0 tags))
             (entries (gethash tag
                               entry-hash)))
        (puthash tag
                 (append entries
                         `(,entry))
                 entry-hash)))
    entry-hash))

(defun clocktable-by-tag--sum-durations (clock-data)
  "Return the total minutes logged for all entries in CLOCK-DATA."
  (seq-reduce (lambda (total-minutes entry)
                (cl-destructuring-bind (_ _ _ _ minutes _) entry
                  (setq total-minutes (+ total-minutes minutes))))
              clock-data
              0))

(defun clocktable-by-tag--get-files (params)
  "Return list of files from which to construct clocktable.

See `org-dblock-write:clocktable' for information on PARAMS.

Users can provide files in two ways:

1. ':files': A list file paths or variable containing such a list
2. ':files-fn': A function which is called without arguments
   and should return a list of file paths

If both are provided, ':files' is used."
  (let* ((files-fn (plist-get params :files-fn))
         (files (plist-get params :files))
         (files (if (symbolp files)
                    (symbol-value files)
                  files)))
    (when (and (not files-fn)
               (not files))
      (error "ERROR [clocktable-by-tag] You must provide either :files-fn or :files as parameters."))
    (or files
        (funcall files-fn))))

(defun clocktable-by-tag--insert-table-headings ()
  "Insert the initial table headings."
  (insert "| | | <r> |\n")
  (insert "| Tag | Headline | Time |\n")
  (insert "|--\n")
  (insert "| | All *Total time* | \n"))

(defun clocktable-by-tag--insert-caption (params)
  "Insert caption for when table was last updated.

- PARAMS: See `org-dblock-write:clocktable'

See `org-clocktable-write-default'."
  (let* ((block (plist-get params :block))
         (summary-at (format-time-string (org-time-stamp-format t t)))
         (for-block (if block
                        (let ((range-text (nth 2 (org-clock-special-range
                                                  block nil t
                                                  (plist-get params :wstart)
                                                  (plist-get params :mstart)))))
                          (format ", for %s." range-text))
                      "")))
    (insert-before-markers
     (s-lex-format "#+CAPTION: Clock summary at ${summary-at}${for-block}\n"))))

(defun org-dblock-write:clocktable-by-tag (params)
  "Create a clocktable grouped by tags. Only look at first tag on each headline.

- PARAMS: See `org-dblock-write:clocktable'"
  (clocktable-by-tag--insert-caption params)
  (clocktable-by-tag--insert-table-headings)
  ;; We can't sort by tags unless we collect the tags
  (plist-put params :tags t)
  (let* ((files (clocktable-by-tag--get-files params))
         (clock-data (clocktable-by-tag--get-clock-data files
                                                        params))
         (entries-hash (clocktable-by-tag--get-entries-by-tag-hash clock-data))
         (merge-duplicate-headlines (plist-get params :merge-duplicate-headlines))
         (tags (hash-table-keys entries-hash)))
    (dolist (tag tags)
      (clocktable-by-tag--insert-row tag
                                     (gethash tag entries-hash)
                                     merge-duplicate-headlines))
    (save-excursion
      (let ((duration (org-duration-from-minutes (clocktable-by-tag--sum-durations clock-data))))
        (re-search-backward "*Total time*")
        (org-table-next-field)
        (org-table-blank-field)
        (insert (s-lex-format "*${duration}*"))
        (org-table-align)))))

(provide 'clocktable-by-tag)
;;; clocktable-by-tag.el ends here
