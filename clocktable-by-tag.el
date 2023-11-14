;;; clocktable-by-tag.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: November 14, 2023
;; Modified: November 14, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cashpw/clocktable-by-tag
;; Package-Requires: ((emacs "27.1") (s "1.13.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Group clocktable entries by their first tag.
;;
;;; Code:

(defun clocktable-by-tag--shift-cell (n)
  (let ((str ""))
    (dotimes (i n)
      (setq str (concat str "| ")))
    str))

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
                      all-clock-data)
                    )))
              files
              '()))

(defun clocktable-by-tag--insert-row (tag entries)
  "Insert a row of ENTRIES for TAG.

- ENTRIES: List of entries with TAG; see `org-clock-get-table-data'"
  (insert "|--\n")
  (insert (s-lex-format "| ${tag} | *Tag time* |\n"))
  (let ((total 0))
    (dolist (entry entries)
      (cl-destructuring-bind (level headline _ _ minutes _) entry
        (setq total (+ total minutes))
        (let ((indent (org-clocktable-indent-string level))
              (shift-cell (clocktable-by-tag--shift-cell level))
              (duration (org-duration-from-minutes minutes)))
          (insert (s-lex-format "| |${indent}${headline} | ${shift-cell} ${duration} |\n")))))
    (save-excursion
      (let ((duration (org-duration-from-minutes total)))
        (re-search-backward "*Tag time*")
        (org-table-next-field)
        (org-table-blank-field)
        (insert (s-lex-format "*${duration}*")))))
  (org-table-align))

(defun clocktable-by-tag--get-tags (clock-data-entries)
  "Return unique list of tags within CLOCK-DATA-ENTRIES.

This function expects output in the form of a list of entries from `org-clock-get-table-data'."
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

(defun org-dblock-write:clocktable-by-tag (params)
  "Create a clocktable grouped by tags. Only look at first tag on each headline.

- PARAMS: See `org-dblock-write:clocktable'"
  (insert "| | | <r> |\n")
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|--\n")
  (insert "| | All *Total time* | \n")

  ;; We can't sort by tags unless we collect the tags
  (plist-put params :tags t)

  (let* ((files (plist-get :files params))
         (clock-data (clocktable-by-tag--get-clock-data files
                                                        params))
         (entries-hash (clocktable-by-tag--get-entries-by-tag-hash clock-data))
         (tags (hash-table-keys entries-hash)))
    (dolist (tag tags)
      (clocktable-by-tag--insert-row tag
                                     (gethash tag entries-hash)))
    (save-excursion
      (let ((duration (org-duration-from-minutes (clocktable-by-tag--sum-durations clock-data))))
        (re-search-backward "*Total time*")
        (org-table-next-field)
        (org-table-blank-field)
        (insert (s-lex-format "*${duration}*"))
        (org-table-align)))))

(provide 'clocktable-by-tag)
;;; clocktable-by-tag.el ends here
