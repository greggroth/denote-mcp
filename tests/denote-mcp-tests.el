;;; denote-mcp-tests.el --- Tests for denote-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gregg Roth
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; ERT suite for denote-mcp.  Each test runs against a freshly-created
;; temporary `denote-directory' so tests are isolated and reproducible.
;;
;; Run with:
;;   emacs -Q --batch -L . -L tests -l tests/denote-mcp-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json)
(require 'denote)
(require 'denote-mcp)
(require 'denote-mcp-tools)

(defvar denote-mcp-tests--dir nil)

(defmacro denote-mcp-tests--with-dir (&rest body)
  "Run BODY inside a fresh temporary `denote-directory'."
  (declare (indent 0))
  `(let* ((denote-mcp-tests--dir (make-temp-file "denote-mcp-test" t))
          (denote-directory denote-mcp-tests--dir)
          (denote-mcp-confirm-write-operations nil)
          (denote-mcp-include-journal-default t))
     (unwind-protect
         (progn ,@body)
       (when (and denote-mcp-tests--dir
                  (file-exists-p denote-mcp-tests--dir))
         (delete-directory denote-mcp-tests--dir t)))))

(defun denote-mcp-tests--first-id ()
  "Return the id of the first listed note."
  (let ((notes (denote-mcp-tools-list-notes :include-journal t)))
    (cdr (assq 'id (aref notes 0)))))

(defun denote-mcp-tests--decode (json-string)
  "Decode JSON-STRING with sensible defaults for tool output."
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (json-key-type 'symbol))
    (json-read-from-string json-string)))

;;;; Read tools

(ert-deftest denote-mcp-tests-list-notes-empty ()
  (denote-mcp-tests--with-dir
    (let ((result (denote-mcp-tools-list-notes :include-journal t)))
      (should (equal result []))
      (should (equal (denote-mcp--tool-list-notes
                      nil nil nil nil nil nil nil)
                     "[]")))))

(ert-deftest denote-mcp-tests-create-and-list ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Hello world"
                                   :keywords "foo,bar"
                                   :body "Some body content.")
    (let* ((notes (denote-mcp-tools-list-notes :include-journal t))
           (note (aref notes 0)))
      (should (= (length notes) 1))
      (should (equal (cdr (assq 'title note)) "Hello world"))
      (should (equal (append (cdr (assq 'keywords note)) nil)
                     '("bar" "foo"))))))

(ert-deftest denote-mcp-tests-get-note ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Read me" :body "Body.")
    (let* ((id (denote-mcp-tests--first-id))
           (note (denote-mcp-tools-get-note id)))
      (should (equal (cdr (assq 'id note)) id))
      (should (equal (cdr (assq 'title note)) "Read me"))
      (should (string-match-p "Body\\." (cdr (assq 'body note)))))))

(ert-deftest denote-mcp-tests-get-note-missing-id ()
  (denote-mcp-tests--with-dir
    (should-error (denote-mcp-tools-get-note "20990101T000000")
                  :type 'denote-mcp-not-found)))

(ert-deftest denote-mcp-tests-format-link ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Linked")
    (let* ((id (denote-mcp-tests--first-id))
           (out (denote-mcp-tools-format-link id)))
      (should (equal (cdr (assq 'id out)) id))
      (should (equal (cdr (assq 'title out)) "Linked"))
      (should (string-match-p (regexp-quote (format "[[denote:%s]" id))
                              (cdr (assq 'link out)))))))

(ert-deftest denote-mcp-tests-list-keywords ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "A" :keywords "alpha,beta")
    (denote-mcp-tools-create-note :title "B" :keywords "beta")
    (let* ((kws (denote-mcp-tools-list-keywords))
           (alist (mapcar (lambda (k) (cons (cdr (assq 'keyword k))
                                            (cdr (assq 'count k))))
                          (append kws nil))))
      (should (equal (cdr (assoc "beta" alist)) 2))
      (should (equal (cdr (assoc "alpha" alist)) 1)))))

(ert-deftest denote-mcp-tests-list-keywords-empty ()
  (denote-mcp-tests--with-dir
    (should (equal (denote-mcp-tools-list-keywords) []))))

(ert-deftest denote-mcp-tests-find-backlinks ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Target")
    (let* ((target-id (denote-mcp-tests--first-id)))
      (denote-mcp-tools-create-note
       :title "Source"
       :body (format "See [[denote:%s][Target]]." target-id))
      (let ((bls (denote-mcp-tools-find-backlinks :id target-id)))
        (should (= (length bls) 1))
        (should (equal (cdr (assq 'title (aref bls 0))) "Source"))))))

(ert-deftest denote-mcp-tests-search-content ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Pancakes" :body "I love pancakes.")
    (denote-mcp-tools-create-note :title "Waffles" :body "Waffles are fine.")
    (let ((hits (denote-mcp-tools-search-notes-content :query "pancakes")))
      (should (= (length hits) 1))
      (should (equal (cdr (assq 'title (aref hits 0))) "Pancakes")))))

(ert-deftest denote-mcp-tests-search-empty-corpus ()
  (denote-mcp-tests--with-dir
    (should (equal (denote-mcp-tools-search-notes-content :query "anything")
                   []))))

;;;; Write tools

(ert-deftest denote-mcp-tests-append-to-note ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Grow me" :body "Line 1.")
    (let* ((id (denote-mcp-tests--first-id))
           (out (denote-mcp-tools-append-to-note
                 :id id :content "Line 2." :add-newline t))
           (note (denote-mcp-tools-get-note id)))
      (should (> (cdr (assq 'bytes_added out)) 0))
      (should (string-match-p "Line 1\\." (cdr (assq 'body note))))
      (should (string-match-p "Line 2\\." (cdr (assq 'body note)))))))

(ert-deftest denote-mcp-tests-append-missing-id ()
  (denote-mcp-tests--with-dir
    (should-error
     (denote-mcp-tools-append-to-note
      :id "20990101T000000" :content "x" :add-newline t)
     :type 'denote-mcp-not-found)))

(ert-deftest denote-mcp-tests-rename-note ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Old" :keywords "oldtag")
    (let* ((id (denote-mcp-tests--first-id))
           (out (denote-mcp-tools-rename-note
                 :id id :title "New title" :keywords "newtag"))
           (note (denote-mcp-tools-get-note id)))
      (should (equal (cdr (assq 'id out)) id))
      (should (equal (cdr (assq 'title note)) "New title"))
      (should (member "newtag"
                      (append (cdr (assq 'keywords note)) nil))))))

(ert-deftest denote-mcp-tests-write-disabled ()
  (denote-mcp-tests--with-dir
    (let ((denote-mcp-confirm-write-operations t))
      (unless (getenv "DENOTE_MCP_ALLOW_WRITES")
        (should-error
         (denote-mcp-tools-create-note :title "Nope")
         :type 'denote-mcp-write-disabled)))))

;;;; Tool wrapper layer (JSON-encoded responses)

(ert-deftest denote-mcp-tests-wrapper-list-after-create ()
  (denote-mcp-tests--with-dir
    (denote-mcp--tool-create-note "Wrapped" "k1,k2" nil nil "Body." nil nil)
    (let* ((json (denote-mcp--tool-list-notes nil nil nil nil nil nil nil))
           (data (denote-mcp-tests--decode json)))
      (should (= (length data) 1))
      (should (equal (cdr (assq 'title (car data))) "Wrapped")))))

(ert-deftest denote-mcp-tests-wrapper-error-becomes-mcp-error ()
  (denote-mcp-tests--with-dir
    (should-error
     (denote-mcp--tool-get-note "20990101T000000")
     :type 'mcp-server-lib-tool-error)))

;;;; Date parsing

(ert-deftest denote-mcp-tests-parse-date-keywords ()
  (should-not (denote-mcp-tools--parse-date "today"))
  (should-not (denote-mcp-tools--parse-date nil))
  (should-not (denote-mcp-tools--parse-date ""))
  (should (denote-mcp-tools--parse-date "yesterday"))
  (should (denote-mcp-tools--parse-date "tomorrow"))
  (should (denote-mcp-tools--parse-date "2024-01-15"))
  (should-error (denote-mcp-tools--parse-date "garbage")
                :type 'denote-mcp-bad-input))

;;;; Argument coercion

(ert-deftest denote-mcp-tests-coerce-keywords ()
  (should (equal (denote-mcp-tools--coerce-keywords "a,b,c") '("a" "b" "c")))
  (should (equal (denote-mcp-tools--coerce-keywords '("a" "b")) '("a" "b")))
  (should (equal (denote-mcp-tools--coerce-keywords ["a" "b"]) '("a" "b")))
  (should (equal (denote-mcp-tools--coerce-keywords "") nil))
  (should (equal (denote-mcp-tools--coerce-keywords nil) nil)))

(ert-deftest denote-mcp-tests-coerce-bool ()
  (should (denote-mcp-tools--coerce-bool t))
  (should (denote-mcp-tools--coerce-bool "true"))
  (should-not (denote-mcp-tools--coerce-bool "false"))
  (should-not (denote-mcp-tools--coerce-bool nil))
  (should-not (denote-mcp-tools--coerce-bool "")))

;;;; Resource handlers

(ert-deftest denote-mcp-tests-resource-by-id ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Resource" :body "B.")
    (let* ((id (denote-mcp-tests--first-id))
           (text (denote-mcp--resource-by-id `(("id" . ,id)))))
      (should (string-match-p "Resource" text))
      (should (string-match-p "B\\." text)))))

(ert-deftest denote-mcp-tests-resource-meta-by-id ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "Meta")
    (let* ((id (denote-mcp-tests--first-id))
           (json (denote-mcp--resource-meta-by-id `(("id" . ,id))))
           (alist (denote-mcp-tests--decode json)))
      (should (equal (cdr (assq 'id alist)) id))
      (should (equal (cdr (assq 'title alist)) "Meta")))))

(ert-deftest denote-mcp-tests-resource-list ()
  (denote-mcp-tests--with-dir
    (denote-mcp-tools-create-note :title "One")
    (denote-mcp-tools-create-note :title "Two")
    (let* ((json (denote-mcp--resource-list nil))
           (data (denote-mcp-tests--decode json)))
      (should (= (length data) 2)))))

;;;; Enable/disable lifecycle

(ert-deftest denote-mcp-tests-enable-disable ()
  (denote-mcp-tests--with-dir
    (denote-mcp-enable)
    (unwind-protect
        ;; If registration succeeded, re-enabling should also work
        ;; (mcp-server-lib will overwrite).
        (denote-mcp-enable)
      (denote-mcp-disable))))

(provide 'denote-mcp-tests)
;;; denote-mcp-tests.el ends here
