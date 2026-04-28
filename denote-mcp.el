;;; denote-mcp.el --- MCP server for Denote -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gregg Roth

;; Author: Gregg Roth
;; Keywords: convenience, files, matching, outlines, hypermedia
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (mcp-server-lib "0.2.0"))
;; Version: 0.1.0
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

;; denote-mcp exposes Denote (https://protesilaos.com/emacs/denote)
;; through the Model Context Protocol so AI agents can list, search,
;; create, and link denote notes without re-discovering filename
;; conventions, identifiers, or front-matter formats on every turn.
;;
;; Wire it up:
;;
;;   (use-package denote-mcp
;;     :after denote
;;     :config (denote-mcp-enable))
;;
;; Then run M-x mcp-server-lib-start.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mcp-server-lib)
(require 'denote)
(require 'denote-mcp-tools)

;;;; Customization

(defgroup denote-mcp nil
  "MCP server for Denote."
  :group 'denote
  :prefix "denote-mcp-")

(defconst denote-mcp--server-id "denote-mcp"
  "Server ID used when registering tools and resources.")

(defcustom denote-mcp-enabled-tools
  '(list-notes
    search-notes-content
    get-note
    format-link
    list-keywords
    find-backlinks
    create-note
    append-to-note
    rename-note
    create-journal-entry)
  "Subset of denote-mcp tools to register.
Each element is a symbol naming one of the bundled tools."
  :type '(repeat symbol)
  :group 'denote-mcp)

(defcustom denote-mcp-include-journal-default t
  "Default value for the include_journal flag on listing tools."
  :type 'boolean
  :group 'denote-mcp)

(defcustom denote-mcp-default-file-type nil
  "Default file type symbol when creating notes.
When nil, defer to denote's own default."
  :type '(choice (const :tag "Use denote default" nil)
                 (const org)
                 (const markdown-yaml)
                 (const markdown-toml)
                 (const text))
  :group 'denote-mcp)

(defcustom denote-mcp-list-default-limit 50
  "Default cap on results returned by listing/backlink tools."
  :type 'integer
  :group 'denote-mcp)

(defcustom denote-mcp-search-default-context-lines 1
  "Default number of context lines around a search match."
  :type 'integer
  :group 'denote-mcp)

(defcustom denote-mcp-confirm-write-operations nil
  "If non-nil, refuse write operations unless an env flag is set.
Set the environment variable DENOTE_MCP_ALLOW_WRITES (to any value)
to permit writes when this is t.  Useful for sandboxed sessions."
  :type 'boolean
  :group 'denote-mcp)

;;;; JSON helpers

(defun denote-mcp--json-encode (value)
  "Encode VALUE as JSON for tool responses."
  (let ((json-encoding-pretty-print nil)
        (json-false :json-false)
        (json-null :json-null))
    (json-encode value)))

(defun denote-mcp--with-error-handling (body)
  "Run BODY (a thunk) and convert denote-mcp errors to MCP errors."
  (condition-case err
      (funcall body)
    (denote-mcp-not-found
     (mcp-server-lib-tool-throw (cadr err)))
    (denote-mcp-bad-input
     (mcp-server-lib-tool-throw (cadr err)))
    (denote-mcp-write-disabled
     (mcp-server-lib-tool-throw (cadr err)))
    (denote-mcp-error
     (mcp-server-lib-tool-throw (cadr err)))
    (error
     (mcp-server-lib-tool-throw (error-message-string err)))))

(defmacro denote-mcp--respond (&rest body)
  "Evaluate BODY and return its result as a JSON-encoded string."
  (declare (indent 0))
  `(denote-mcp--with-error-handling
    (lambda ()
      (denote-mcp--json-encode (progn ,@body)))))

;;;; Tool wrappers

(defun denote-mcp--tool-list-notes
    (title_regex keywords signature date_from date_to include_journal limit)
  "List denote notes with optional filters.

MCP Parameters:
  TITLE_REGEX - Regex matching the note title (optional)
  KEYWORDS - Comma-separated string or array of keywords (optional)
  SIGNATURE - Exact signature to match (optional)
  DATE_FROM - Lower-bound date YYYY-MM-DD (optional)
  DATE_TO - Upper-bound date YYYY-MM-DD (optional)
  INCLUDE_JOURNAL - \"true\"/\"false\" (optional)
  LIMIT - Maximum number of results (optional)"
  (denote-mcp--respond
    (denote-mcp-tools-list-notes
     :title-regex title_regex
     :keywords keywords
     :signature signature
     :date-from date_from
     :date-to date_to
     :include-journal (if (or (null include_journal)
                              (and (stringp include_journal)
                                   (string-empty-p include_journal)))
                          'unset
                        include_journal)
     :limit limit)))

(defun denote-mcp--tool-search-notes-content
    (query regex limit context_lines)
  "Search note bodies for QUERY.

MCP Parameters:
  QUERY - Text or regex to search for
  REGEX - \"true\" to treat QUERY as regex, \"false\" for literal (optional)
  LIMIT - Maximum number of file results (optional)
  CONTEXT_LINES - Lines of context around matches (optional)"
  (denote-mcp--respond
    (denote-mcp-tools-search-notes-content
     :query query
     :regex regex
     :limit limit
     :context-lines context_lines)))

(defun denote-mcp--tool-get-note (id)
  "Read the metadata + body of a denote note.

MCP Parameters:
  ID - Denote identifier (e.g. 20231005T142233)"
  (denote-mcp--respond
    (denote-mcp-tools-get-note id)))

(defun denote-mcp--tool-format-link (id description)
  "Return a denote link for ID.

MCP Parameters:
  ID - Denote identifier
  DESCRIPTION - Override link text (optional; defaults to title)"
  (denote-mcp--respond
    (denote-mcp-tools-format-link id description)))

(defun denote-mcp--tool-list-keywords (min_count prefix)
  "List all denote keywords with usage counts.

MCP Parameters:
  MIN_COUNT - Minimum count to include (optional)
  PREFIX - Restrict to keywords starting with this prefix (optional)"
  (denote-mcp--respond
    (denote-mcp-tools-list-keywords
     :min-count min_count
     :prefix prefix)))

(defun denote-mcp--tool-find-backlinks (id limit)
  "Return notes that reference ID.

MCP Parameters:
  id - Denote identifier of the target note
  LIMIT - Maximum number of results (optional)"
  (denote-mcp--respond
    (denote-mcp-tools-find-backlinks
     :id id
     :limit limit)))

(defun denote-mcp--tool-create-note
    (title keywords signature file_type body subdirectory date)
  "Create a new denote note.

MCP Parameters:
  TITLE - Note title
  KEYWORDS - Comma-separated string or array of keywords (optional)
  SIGNATURE - Optional signature
  FILE_TYPE - One of: org, markdown-yaml, markdown-toml, text (optional)
  BODY - Body text appended after the front matter (optional)
  SUBDIRECTORY - Directory under variable `denote-directory' (or
    absolute) (optional)
  DATE - Date for the identifier YYYY-MM-DD (optional, defaults now)"
  (denote-mcp--respond
    (denote-mcp-tools-create-note
     :title title
     :keywords keywords
     :signature signature
     :file-type file_type
     :body body
     :subdirectory subdirectory
     :date date)))

(defun denote-mcp--tool-append-to-note (id content add_newline)
  "Append CONTENT to the body of the note with ID.

MCP Parameters:
  id - Denote identifier
  content - Text to append
  add_newline - \"true\" (default) to ensure the append starts on a new line"
  (denote-mcp--respond
    (denote-mcp-tools-append-to-note
     :id id
     :content content
     :add-newline (if (or (null add_newline)
                          (and (stringp add_newline)
                               (string-empty-p add_newline)))
                      'unset
                    add_newline))))

(defun denote-mcp--tool-rename-note (id title keywords signature)
  "Rename a denote note while preserving its identifier.

MCP Parameters:
  ID - Denote identifier of the note to rename
  TITLE - New title (optional; keeps existing if blank)
  KEYWORDS - Comma-separated string or array of new keywords (optional)
  SIGNATURE - New signature (optional)"
  (denote-mcp--respond
    (denote-mcp-tools-rename-note
     :id id
     :title title
     :keywords keywords
     :signature signature)))

(defun denote-mcp--tool-create-journal-entry (date)
  "Create or open a denote-journal entry.

MCP Parameters:
  DATE - YYYY-MM-DD or today/tomorrow/yesterday (optional, defaults today)"
  (denote-mcp--respond
    (denote-mcp-tools-create-journal-entry
     :date date)))

;;;; Resource handlers

(defun denote-mcp--resource-text-mime (file)
  "Return an MCP MIME type appropriate for FILE."
  (let ((type (denote-mcp-tools--file-type file)))
    (cond
     ((memq type '(markdown-yaml markdown-toml)) "text/markdown")
     (t "text/plain"))))

(defun denote-mcp--resource-by-id (params)
  "Resource handler returning the body of the note in PARAMS."
  (denote-mcp--with-error-handling
   (lambda ()
     (let* ((id (cdr (assoc "id" params)))
            (file (denote-mcp-tools--file-by-id id)))
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(defun denote-mcp--resource-meta-by-id (params)
  "Resource handler returning JSON metadata for the note in PARAMS."
  (denote-mcp--with-error-handling
   (lambda ()
     (let* ((id (cdr (assoc "id" params)))
            (file (denote-mcp-tools--file-by-id id)))
       (denote-mcp--json-encode (denote-mcp-tools--metadata file))))))

(defun denote-mcp--resource-list (_params)
  "Resource handler returning a JSON listing of all notes."
  (denote-mcp--with-error-handling
   (lambda ()
     (denote-mcp--json-encode
      (denote-mcp-tools-list-notes
       :include-journal 'unset
       :limit denote-mcp-list-default-limit)))))

(defun denote-mcp--resource-journal (params)
  "Resource handler returning the journal entry body for PARAMS' date."
  (denote-mcp--with-error-handling
   (lambda ()
     (unless (require 'denote-journal nil t)
       (denote-mcp-tools--error
        'denote-mcp-error "denote-journal is not installed"))
     (let* ((date (cdr (assoc "date" params)))
            (time (denote-mcp-tools--parse-date date))
            (path (denote-journal-path-to-new-or-existing-entry time)))
       (unless (and path (file-exists-p path))
         (denote-mcp-tools--error
          'denote-mcp-not-found "No journal entry for %s" (or date "today")))
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))))

;;;; Registration

(defconst denote-mcp--tool-registry
  '((list-notes
     :id "denote-list-notes"
     :handler denote-mcp--tool-list-notes
     :read-only t
     :description "List denote notes with optional filters.\n\nFilters: title_regex, keywords (comma-separated or array), signature, date_from, date_to (YYYY-MM-DD), include_journal, limit.\n\nReturns a JSON array of {id, title, keywords[], signature?, file, link}.")
    (search-notes-content
     :id "denote-search-notes-content"
     :handler denote-mcp--tool-search-notes-content
     :read-only t
     :description "Search the bodies of denote notes for a substring (default) or regex (when regex=\"true\").\n\nReturns a JSON array of {id, title, keywords[], signature?, file, link, matches: [{line, text}]}.")
    (get-note
     :id "denote-get-note"
     :handler denote-mcp--tool-get-note
     :read-only t
     :description "Read a denote note by identifier.\n\nReturns JSON: {id, title, keywords[], signature?, file_type, file, link, body}.")
    (format-link
     :id "denote-format-link"
     :handler denote-mcp--tool-format-link
     :read-only t
     :description "Build a denote link string for a known identifier.\n\nReturns JSON: {id, title, link}.")
    (list-keywords
     :id "denote-list-keywords"
     :handler denote-mcp--tool-list-keywords
     :read-only t
     :description "List all keywords across the corpus, sorted by descending usage.\n\nReturns JSON: [{keyword, count}, ...].")
    (find-backlinks
     :id "denote-find-backlinks"
     :handler denote-mcp--tool-find-backlinks
     :read-only t
     :description "Return notes whose body references the given identifier.\n\nReturns the same shape as denote-list-notes.")
    (create-note
     :id "denote-create-note"
     :handler denote-mcp--tool-create-note
     :description "Create a new denote note.\n\nDelegates to denote.el so filename, identifier, and front matter follow your local denote configuration.\n\nReturns metadata for the new note.")
    (append-to-note
     :id "denote-append-to-note"
     :handler denote-mcp--tool-append-to-note
     :description "Append text to the body of an existing denote note.\n\nReturns JSON: {id, file, bytes_added}.")
    (rename-note
     :id "denote-rename-note"
     :handler denote-mcp--tool-rename-note
     :description "Rename a denote note while preserving its identifier.\n\nUses denote-rename-file to keep filename semantics intact. Omitted fields are kept.\n\nReturns JSON: {id, old_file, new_file, link}.")
    (create-journal-entry
     :id "denote-create-journal-entry"
     :handler denote-mcp--tool-create-journal-entry
     :description "Create (or open) the denote-journal entry for a date.\n\nReturns the note's metadata plus a `created` boolean."))
  "Static registry of tools that denote-mcp can register.")

(defun denote-mcp--register-tool (entry)
  "Register one tool described by ENTRY."
  (let ((handler (plist-get (cdr entry) :handler))
        (id (plist-get (cdr entry) :id))
        (description (plist-get (cdr entry) :description))
        (read-only (plist-get (cdr entry) :read-only)))
    (apply #'mcp-server-lib-register-tool handler
           (append
            (list :id id
                  :description description
                  :server-id denote-mcp--server-id)
            (when (plist-member (cdr entry) :read-only)
              (list :read-only read-only))))))

(defconst denote-mcp--resource-registry
  '(("denote://id/{id}"
     :handler denote-mcp--resource-by-id
     :name "Denote note"
     :description "Full content of the denote note with identifier {id}."
     :mime-type "text/plain")
    ("denote-meta://id/{id}"
     :handler denote-mcp--resource-meta-by-id
     :name "Denote note metadata"
     :description "JSON metadata for the denote note with identifier {id}."
     :mime-type "application/json")
    ("denote-list://all"
     :handler denote-mcp--resource-list
     :name "Denote note list"
     :description "JSON listing of denote notes (capped by denote-mcp-list-default-limit)."
     :mime-type "application/json")
    ("denote-journal://{date}"
     :handler denote-mcp--resource-journal
     :name "Denote journal entry"
     :description "Journal entry for {date} (YYYY-MM-DD or today/tomorrow/yesterday)."
     :mime-type "text/plain"))
  "Static registry of MCP resources to register.")

(defun denote-mcp--register-resource (entry)
  "Register one resource described by ENTRY."
  (let ((uri (car entry))
        (props (cdr entry)))
    (apply #'mcp-server-lib-register-resource
           uri (plist-get props :handler)
           (append
            (list :name (plist-get props :name)
                  :description (plist-get props :description)
                  :mime-type (plist-get props :mime-type)
                  :server-id denote-mcp--server-id)))))

;;;###autoload
(defun denote-mcp-enable ()
  "Register denote-mcp tools and resources with mcp-server-lib."
  (interactive)
  (dolist (entry denote-mcp--tool-registry)
    (when (memq (car entry) denote-mcp-enabled-tools)
      (denote-mcp--register-tool entry)))
  (dolist (entry denote-mcp--resource-registry)
    (denote-mcp--register-resource entry))
  (message "denote-mcp: registered %d tools and %d resources"
           (length (cl-remove-if-not
                    (lambda (e) (memq (car e) denote-mcp-enabled-tools))
                    denote-mcp--tool-registry))
           (length denote-mcp--resource-registry)))

;;;###autoload
(defun denote-mcp-disable ()
  "Unregister denote-mcp tools and resources."
  (interactive)
  (dolist (entry denote-mcp--tool-registry)
    (mcp-server-lib-unregister-tool
     (plist-get (cdr entry) :id) denote-mcp--server-id))
  (dolist (entry denote-mcp--resource-registry)
    (mcp-server-lib-unregister-resource (car entry) denote-mcp--server-id)))

(provide 'denote-mcp)
;;; denote-mcp.el ends here
