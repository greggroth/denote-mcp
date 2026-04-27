;;; denote-mcp-tools.el --- Tool implementations for denote-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gregg Roth

;; Author: Gregg Roth
;; Keywords: convenience, files, matching, outlines, hypermedia
;; URL: https://github.com/greggroth/denote-mcp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pure tool functions for denote-mcp.  Each function accepts plain
;; Elisp arguments and returns a plain alist suitable for JSON
;; serialisation.  Registration with `mcp-server-lib' lives in
;; `denote-mcp.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'denote)

(defvar denote-mcp-list-default-limit)
(defvar denote-mcp-search-default-context-lines)
(defvar denote-mcp-include-journal-default)
(defvar denote-mcp-default-file-type)
(defvar denote-mcp-confirm-write-operations)

;;;; Errors

(define-error 'denote-mcp-error "denote-mcp error")
(define-error 'denote-mcp-not-found "denote-mcp: note not found"
  'denote-mcp-error)
(define-error 'denote-mcp-bad-input "denote-mcp: invalid input"
  'denote-mcp-error)
(define-error 'denote-mcp-write-disabled
  "denote-mcp: write operations are disabled" 'denote-mcp-error)

(defun denote-mcp-tools--error (type fmt &rest args)
  "Signal an error of TYPE with formatted FMT and ARGS."
  (signal type (list (apply #'format fmt args))))

;;;; Argument coercion

(defun denote-mcp-tools--coerce-string (value)
  "Coerce VALUE to a non-empty string, or nil."
  (cond
   ((null value) nil)
   ((and (stringp value) (string-empty-p value)) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun denote-mcp-tools--coerce-keywords (value)
  "Coerce VALUE to a list of keyword strings.
Accepts nil, a single string (split on commas), or a list/vector of
strings."
  (cond
   ((null value) nil)
   ((vectorp value)
    (denote-mcp-tools--coerce-keywords (append value nil)))
   ((listp value)
    (delq nil
          (mapcar (lambda (k)
                    (let ((s (denote-mcp-tools--coerce-string k)))
                      (and s (string-trim s))))
                  value)))
   ((stringp value)
    (denote-mcp-tools--coerce-keywords
     (split-string value "[,[:space:]]+" t)))
   (t (denote-mcp-tools--error
       'denote-mcp-bad-input "Cannot coerce keywords: %S" value))))

(defun denote-mcp-tools--coerce-bool (value)
  "Coerce VALUE to a strict boolean."
  (cond
   ((null value) nil)
   ((eq value :false) nil)
   ((eq value :true) t)
   ((eq value t) t)
   ((stringp value)
    (member (downcase value) '("t" "true" "yes" "1")))
   (t (and value t))))

(defun denote-mcp-tools--coerce-positive-int (value default)
  "Coerce VALUE to a positive integer, falling back to DEFAULT."
  (let ((n (cond
            ((integerp value) value)
            ((stringp value)
             (and (string-match-p "\\`[0-9]+\\'" value)
                  (string-to-number value)))
            (t nil))))
    (if (and n (> n 0)) n default)))

(defun denote-mcp-tools--require-string (name value)
  "Validate that VALUE for parameter NAME is a non-empty string."
  (let ((s (denote-mcp-tools--coerce-string value)))
    (unless s
      (denote-mcp-tools--error
       'denote-mcp-bad-input "Missing required parameter: %s" name))
    s))

;;;; Date helpers

(defun denote-mcp-tools--parse-date (value)
  "Parse VALUE into an Emacs time value, or nil for today.
Accepts nil, \"today\", \"tomorrow\", \"yesterday\", or YYYY-MM-DD."
  (cond
   ((or (null value) (and (stringp value) (string-empty-p value))
        (equal value "today"))
    nil)
   ((equal value "tomorrow")
    (time-add (current-time) (days-to-time 1)))
   ((equal value "yesterday")
    (time-subtract (current-time) (days-to-time 1)))
   ((and (stringp value)
         (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                         value))
    (let ((parsed (parse-time-string (concat value " 00:00:00"))))
      (encode-time parsed)))
   (t (denote-mcp-tools--error
       'denote-mcp-bad-input
       "Invalid date %S (expected YYYY-MM-DD or today/tomorrow/yesterday)"
       value))))

;;;; File / metadata helpers

(defun denote-mcp-tools--file-by-id (id)
  "Return the absolute file path for ID or signal an error."
  (let ((id (denote-mcp-tools--require-string "id" id)))
    (or (denote-get-path-by-id id)
        (denote-mcp-tools--error
         'denote-mcp-not-found "No denote note with id %s" id))))

(defun denote-mcp-tools--file-type (file)
  "Return the file-type symbol for FILE, falling back to org."
  (or (denote-filetype-heuristics file) 'org))

(defun denote-mcp-tools--title (file file-type)
  "Return the front-matter title for FILE, falling back to the filename slug."
  (or (and (fboundp 'denote-retrieve-title-or-filename)
           (ignore-errors (denote-retrieve-title-or-filename file file-type)))
      (denote-retrieve-filename-title file)
      ""))

(defun denote-mcp-tools--metadata (file)
  "Return a metadata alist for FILE."
  (let* ((id (denote-retrieve-filename-identifier file))
         (file-type (denote-mcp-tools--file-type file))
         (title (denote-mcp-tools--title file file-type))
         (signature (denote-retrieve-filename-signature file))
         (keywords (denote-extract-keywords-from-path file)))
    `((id . ,id)
      (title . ,title)
      (keywords . ,(vconcat (or keywords nil)))
      ,@(when signature `((signature . ,signature)))
      (file_type . ,(symbol-name file-type))
      (file . ,file)
      (link . ,(denote-format-link file title file-type nil)))))

(defun denote-mcp-tools--summary (file)
  "Return a compact summary alist for FILE (used by list tools)."
  (let* ((id (denote-retrieve-filename-identifier file))
         (file-type (denote-mcp-tools--file-type file))
         (title (denote-mcp-tools--title file file-type))
         (signature (denote-retrieve-filename-signature file))
         (keywords (denote-extract-keywords-from-path file)))
    `((id . ,id)
      (title . ,title)
      (keywords . ,(vconcat (or keywords nil)))
      ,@(when signature `((signature . ,signature)))
      (file . ,file)
      (link . ,(denote-format-link file title file-type nil)))))

(defun denote-mcp-tools--journal-keyword-regexp ()
  "Return a regexp matching journal entries, or nil if denote-journal absent."
  (when (and (require 'denote-journal nil t)
             (fboundp 'denote-journal--keyword-regex))
    (denote-journal--keyword-regex)))

(defun denote-mcp-tools--filter-journal (files include-journal)
  "Return FILES, optionally filtering out journal entries.
INCLUDE-JOURNAL non-nil keeps everything."
  (if include-journal
      files
    (let ((regexp (denote-mcp-tools--journal-keyword-regexp)))
      (if regexp
          (cl-remove-if (lambda (f) (string-match-p regexp f)) files)
        files))))

(defun denote-mcp-tools--write-allowed-p ()
  "Return non-nil if write operations are permitted."
  (if denote-mcp-confirm-write-operations
      (getenv "DENOTE_MCP_ALLOW_WRITES")
    t))

(defun denote-mcp-tools--ensure-write-allowed ()
  "Signal an error unless write operations are permitted."
  (unless (denote-mcp-tools--write-allowed-p)
    (denote-mcp-tools--error
     'denote-mcp-write-disabled
     "Write operations are disabled (denote-mcp-confirm-write-operations is t and DENOTE_MCP_ALLOW_WRITES is unset)")))

(defun denote-mcp-tools--refresh-buffer (file)
  "Revert any visiting buffer for FILE."
  (let ((buf (find-buffer-visiting file)))
    (when (and buf (not (buffer-modified-p buf)))
      (with-current-buffer buf
        (revert-buffer t t t)))))

;;;; Read tools

(cl-defun denote-mcp-tools-list-notes
    (&key title-regex keywords signature date-from date-to
          include-journal limit)
  "Return a list of summary alists for matching notes.
TITLE-REGEX filters by title (regex on filename).  KEYWORDS is a
list of strings; all must be present.  SIGNATURE filters by exact
signature.  DATE-FROM and DATE-TO bound the identifier date
(YYYY-MM-DD).  INCLUDE-JOURNAL nil omits journal entries.  LIMIT
caps results."
  (let* ((include-journal
          (if (eq include-journal 'unset)
              denote-mcp-include-journal-default
            (denote-mcp-tools--coerce-bool include-journal)))
         (limit (denote-mcp-tools--coerce-positive-int
                 limit denote-mcp-list-default-limit))
         (kw-list (denote-mcp-tools--coerce-keywords keywords))
         (sig (denote-mcp-tools--coerce-string signature))
         (title-rx (denote-mcp-tools--coerce-string title-regex))
         (from (denote-mcp-tools--coerce-string date-from))
         (to   (denote-mcp-tools--coerce-string date-to))
         (files (denote-directory-files nil nil :text-only))
         (files (denote-mcp-tools--filter-journal files include-journal)))
    (when title-rx
      (setq files
            (cl-remove-if-not
             (lambda (f)
               (string-match-p title-rx
                               (or (denote-retrieve-filename-title f) "")))
             files)))
    (when sig
      (setq files
            (cl-remove-if-not
             (lambda (f) (equal sig (denote-retrieve-filename-signature f)))
             files)))
    (when kw-list
      (setq files
            (cl-remove-if-not
             (lambda (f)
               (let ((have (denote-extract-keywords-from-path f)))
                 (cl-every (lambda (k) (member k have)) kw-list)))
             files)))
    (when (or from to)
      (setq files
            (cl-remove-if-not
             (lambda (f)
               (let* ((id (denote-retrieve-filename-identifier f))
                      (date (and id (>= (length id) 8)
                                 (concat (substring id 0 4) "-"
                                         (substring id 4 6) "-"
                                         (substring id 6 8)))))
                 (and date
                      (or (null from) (not (string< date from)))
                      (or (null to) (not (string< to date))))))
             files)))
    (let ((results (mapcar #'denote-mcp-tools--summary files)))
      (vconcat
       (if (and limit (> (length results) limit))
           (cl-subseq results 0 limit)
         results)))))

(cl-defun denote-mcp-tools-search-notes-content
    (&key query regex limit context-lines)
  "Search note bodies for QUERY.
If REGEX is non-nil, treat QUERY as a regular expression; otherwise
escape it as a literal substring.  Returns up to LIMIT results, each
with up to CONTEXT-LINES of context surrounding lines (currently
matched lines only)."
  (let* ((q (denote-mcp-tools--require-string "query" query))
         (regex (denote-mcp-tools--coerce-bool regex))
         (limit (denote-mcp-tools--coerce-positive-int limit 50))
         (_context (denote-mcp-tools--coerce-positive-int
                    context-lines
                    denote-mcp-search-default-context-lines))
         (pattern (if regex q (regexp-quote q)))
         (files (denote-directory-files nil nil :text-only))
         (alist (when files
                  (let ((xref-file-name-display 'abs))
                    (xref--analyze
                     (xref-matches-in-files pattern files)))))
         (results '()))
    (catch 'done
      (dolist (entry alist)
        (let* ((file (car entry))
               (matches
                (mapcar
                 (lambda (m)
                   (let* ((loc (xref-match-item-location m))
                          (line (and (fboundp 'xref-location-line)
                                     (xref-location-line loc)))
                          (text (xref-match-item-summary m)))
                     `((line . ,(or line 0))
                       (text . ,(or text "")))))
                 (cdr entry)))
               (meta (denote-mcp-tools--summary file)))
          (push `(,@meta (matches . ,(vconcat matches))) results)
          (when (>= (length results) limit)
            (throw 'done nil)))))
    (vconcat (nreverse results))))

(defun denote-mcp-tools-get-note (id)
  "Return the metadata + body for the note with ID."
  (let* ((file (denote-mcp-tools--file-by-id id))
         (meta (denote-mcp-tools--metadata file))
         (body (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (append meta `((body . ,body)))))

(defun denote-mcp-tools-format-link (id &optional description)
  "Return a denote link string for ID with optional DESCRIPTION."
  (let* ((file (denote-mcp-tools--file-by-id id))
         (file-type (denote-mcp-tools--file-type file))
         (desc (or (denote-mcp-tools--coerce-string description)
                   (denote-mcp-tools--title file file-type)
                   "")))
    `((id . ,id)
      (title . ,desc)
      (link . ,(denote-format-link file desc file-type nil)))))

(cl-defun denote-mcp-tools-list-keywords (&key min-count prefix)
  "Return all keywords with usage counts.
MIN-COUNT filters out keywords below that threshold.  PREFIX restricts
results to keywords starting with the given string."
  (let* ((min-n (denote-mcp-tools--coerce-positive-int min-count 1))
         (prefix (denote-mcp-tools--coerce-string prefix))
         (counts (make-hash-table :test #'equal)))
    (dolist (file (denote-directory-files nil nil :text-only))
      (dolist (k (denote-extract-keywords-from-path file))
        (puthash k (1+ (gethash k counts 0)) counts)))
    (let (rows)
      (maphash
       (lambda (k n)
         (when (and (>= n min-n)
                    (or (null prefix) (string-prefix-p prefix k)))
           (push `((keyword . ,k) (count . ,n)) rows)))
       counts)
      (setq rows
            (sort rows
                  (lambda (a b)
                    (let ((ca (cdr (assq 'count a)))
                          (cb (cdr (assq 'count b))))
                      (if (= ca cb)
                          (string< (cdr (assq 'keyword a))
                                   (cdr (assq 'keyword b)))
                        (> ca cb))))))
      (vconcat rows))))

(cl-defun denote-mcp-tools-find-backlinks (&key id limit)
  "Return notes whose body contains a reference to ID.
Capped at LIMIT (defaults to `denote-mcp-list-default-limit')."
  (let* ((id (denote-mcp-tools--require-string "id" id))
         (limit (denote-mcp-tools--coerce-positive-int
                 limit denote-mcp-list-default-limit))
         (target-file (denote-get-path-by-id id))
         (all-files (denote-directory-files nil nil :text-only))
         (files (when all-files
                  (let ((xref-file-name-display 'abs))
                    (delete-dups
                     (mapcar (lambda (group) (car group))
                             (xref--analyze
                              (xref-matches-in-files
                               (regexp-quote id) all-files)))))))
         (files (cl-remove-if (lambda (f) (equal f target-file)) files))
         (results (mapcar #'denote-mcp-tools--summary files)))
    (vconcat
     (if (and limit (> (length results) limit))
         (cl-subseq results 0 limit)
       results))))

;;;; Write tools

(cl-defun denote-mcp-tools-create-note
    (&key title keywords signature file-type body subdirectory date)
  "Create a new denote note and optionally append BODY.
Returns metadata for the new note."
  (denote-mcp-tools--ensure-write-allowed)
  (let* ((title (denote-mcp-tools--require-string "title" title))
         (keywords (denote-mcp-tools--coerce-keywords keywords))
         (signature (denote-mcp-tools--coerce-string signature))
         (file-type-str (denote-mcp-tools--coerce-string file-type))
         (file-type-sym (cond
                         (file-type-str (intern file-type-str))
                         (denote-mcp-default-file-type
                          denote-mcp-default-file-type)
                         (t nil)))
         (subdir (denote-mcp-tools--coerce-string subdirectory))
         (directory (cond
                     ((null subdir) nil)
                     ((file-name-absolute-p subdir) subdir)
                     (t (expand-file-name
                         subdir
                         (if (fboundp 'denote-directories-get-common-root)
                             (denote-directories-get-common-root)
                           (with-no-warnings (denote-directory)))))))
         (date-time (denote-mcp-tools--parse-date date))
         (file (denote title keywords file-type-sym directory
                       date-time nil signature)))
    (let ((path (cond ((stringp file) file)
                      ((bufferp file) (buffer-file-name file))
                      (t (buffer-file-name)))))
      (when path
        (let ((buf (get-file-buffer path)))
          (when buf
            (with-current-buffer buf
              (let ((inhibit-message t))
                (save-buffer))))))
      (when (and (denote-mcp-tools--coerce-string body) path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert body)
          (unless (bolp) (insert "\n"))
          (write-region (point-min) (point-max) path nil 'silent))
        (denote-mcp-tools--refresh-buffer path))
      (denote-mcp-tools--metadata path))))

(cl-defun denote-mcp-tools-append-to-note (&key id content add-newline)
  "Append CONTENT to the body of the note with ID.
ADD-NEWLINE non-nil ensures the appended content begins on a new line."
  (denote-mcp-tools--ensure-write-allowed)
  (let* ((file (denote-mcp-tools--file-by-id id))
         (content (denote-mcp-tools--require-string "content" content))
         (newline (if (eq add-newline 'unset)
                      t
                    (denote-mcp-tools--coerce-bool add-newline)))
         (start-size (nth 7 (file-attributes file))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (when (and newline (not (bolp))) (insert "\n"))
      (insert content)
      (write-region (point-min) (point-max) file nil 'silent))
    (denote-mcp-tools--refresh-buffer file)
    (let ((end-size (nth 7 (file-attributes file))))
      `((id . ,(denote-retrieve-filename-identifier file))
        (file . ,file)
        (bytes_added . ,(- end-size start-size))))))

(cl-defun denote-mcp-tools-rename-note
    (&key id title keywords signature)
  "Rename the note with ID, preserving its identifier.
TITLE, KEYWORDS, and SIGNATURE override the existing values when
non-nil."
  (denote-mcp-tools--ensure-write-allowed)
  (let* ((file (denote-mcp-tools--file-by-id id))
         (old-title (denote-retrieve-filename-title file))
         (old-keywords (denote-extract-keywords-from-path file))
         (old-signature (denote-retrieve-filename-signature file))
         (new-title (or (denote-mcp-tools--coerce-string title) old-title))
         (kw-arg (or keywords 'unset))
         (new-keywords (if (eq kw-arg 'unset)
                           old-keywords
                         (denote-mcp-tools--coerce-keywords keywords)))
         (sig-arg (or signature 'unset))
         (new-signature (if (eq sig-arg 'unset)
                            (or old-signature "")
                          (or (denote-mcp-tools--coerce-string signature) "")))
         (new-file (let ((denote-rename-confirmations nil)
                         (denote-save-buffers t)
                         (denote-kill-buffers nil))
                     (denote-rename-file file new-title new-keywords
                                         new-signature nil id))))
    (denote-mcp-tools--refresh-buffer new-file)
    (let ((file-type (denote-mcp-tools--file-type new-file)))
      `((id . ,id)
        (old_file . ,file)
        (new_file . ,new-file)
        (link . ,(denote-format-link new-file new-title file-type nil))))))

(cl-defun denote-mcp-tools-create-journal-entry (&key date)
  "Create or open the journal entry for DATE (default today)."
  (denote-mcp-tools--ensure-write-allowed)
  (unless (require 'denote-journal nil t)
    (denote-mcp-tools--error
     'denote-mcp-error
     "denote-journal is not installed"))
  (let* ((time (denote-mcp-tools--parse-date date))
         (path-fn (cond
                   ((fboundp 'denote-journal-path-to-new-or-existing-entry)
                    (lambda () (denote-journal-path-to-new-or-existing-entry time)))
                   (t nil)))
         (existed (and path-fn
                       (let ((p (funcall path-fn)))
                         (and p (file-exists-p p)))))
         (path (if path-fn
                   (funcall path-fn)
                 (save-window-excursion
                   (denote-journal-new-or-existing-entry time)
                   (buffer-file-name)))))
    (unless (file-exists-p path)
      (save-window-excursion
        (denote-journal-new-or-existing-entry time)
        (when (buffer-file-name) (save-buffer)))
      (setq existed nil))
    (let ((meta (denote-mcp-tools--metadata path)))
      (append meta `((created . ,(if existed :false t)))))))

(provide 'denote-mcp-tools)
;;; denote-mcp-tools.el ends here
