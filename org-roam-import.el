;;; org-roam-import.el --- Sync Readwise highlights with Org-mode -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-readwise
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (request "0.3.2") (org "9.1"))
;; Keywords: tools, convenience, outlines, hypermedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Original:
;; This package integrates (Readwise) highlight syncing with Org-mode.
;; It provides commands to fetch highlights from Readwise and insert
;; them into an Org buffer or a specified file.
;;
;; This fork separates the insertion logic from the importing, allowing for multiple backends.
;; Further, we use org-roam for the annotations, specifying the file by using roam-refs.

;;; Code:
(require 'org)
(require 'org-roam)

(defcustom org-readwise-last-sync-time-file "~/.emacs.d/org-readwise-last-sync"
  "File to store the last sync time for org-readwise."
  :group 'org-readwise
  :type 'string)

(defcustom org-readwise-output-directory 
  (expand-file-name "readwise" org-directory)
  "Directory to store individual Readwise document files."
  :group 'org-readwise
  :type 'directory)

(defcustom org-readwise-output-location 'buffer
  "Specify where to output the Readwise highlights: 'buffer or a file path."
  :group 'org-readwise
  :type '(choice (const :tag "Buffer" buffer)
                 (file :tag "File")))

(defvar org-readwise-last-sync-time nil
  "The timestamp of the last successful sync.")

(defun org-readwise--sanitize-filename (title)
  "Convert TITLE to a safe filename."
  (let ((sanitized (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" title)))
    (replace-regexp-in-string "\\s+" "-" (string-trim sanitized))))

(defun org-readwise--get-or-create-document-buffer (doc-id title)
  "Get or create buffer for document DOC-ID with TITLE.
If file exists, move point to end. If new, create with heading."
  (let* ((filename (format "%s.org" (org-readwise--sanitize-filename title)))
         (filepath (expand-file-name filename org-readwise-output-directory))
         (exists (file-exists-p filepath))
         (buffer (find-file-noselect filepath)))
    (with-current-buffer buffer
      (if exists
          (goto-char (point-max))
        (erase-buffer)
        (org-mode))
      buffer)))

(defun org-readwise--insert-org-heading (level title id &optional author url body tags buffer use-readwise-id)
  "Insert an org-mode heading.
LEVEL is the heading level.
TITLE is the heading title.
ID is the ID property.
AUTHOR is the optional author property.
URL is the optional URL property.
BODY is the optional body content.
TAGS are optional tags for the heading.
BUFFER is the buffer or file to insert the heading into.
USE-READWISE-ID when non-nil uses :READWISE-ID: instead of :ID:."
  (with-current-buffer (or buffer (current-buffer))
    (insert (format "%s %s" (make-string level ?*) title))
    (when (and tags (not (string-empty-p tags)))
      (insert (format " :%s:" tags)))
    (insert "\n  :PROPERTIES:\n")
    (if use-readwise-id
        (insert (format "  :READWISE-ID: %s\n" id))
      (insert (format "  :ID: %s\n" id)))
    (when author
      (setq author (replace-regexp-in-string "\n" " " author))
      (insert (format "  :AUTHOR: %s\n" author)))
    (when url
      (insert (format "  :URL: %s\n" url)))
    (insert "  :END:\n")
    (when body
      (insert (format "  %s\n\n" body)))))


(defun org-readwise--process-highlight (highlight buffer)
  "Process a single HIGHLIGHT and insert it into BUFFER."
  (let ((highlight-id (number-to-string (assoc-default 'id highlight)))
        (text (assoc-default 'text highlight))
        (note (assoc-default 'note highlight)))
    (org-readwise--insert-org-heading 2 "Highlight" highlight-id nil nil text nil buffer t)
    (when (and note (not (string-empty-p note)))
      (org-readwise--insert-org-heading 3 "Note" (concat highlight-id "-note") nil nil note nil buffer t))))


(defun org-readwise--process-book (book buffer)
  "Process a single BOOK and insert its details.
BUFFER parameter is ignored - creates/updates per-document file."
  (let* ((book-id (number-to-string (assoc-default 'user_book_id book)))
         (title (or (assoc-default 'title book) "No Title"))
         (author (assoc-default 'author book))
         (tags (mapconcat (lambda (tag) (assoc-default 'name tag))
                          (assoc-default 'book_tags book) ":"))
         (summary (assoc-default 'summary book))
         (source-url (assoc-default 'source_url book))
         (doc-buffer (org-readwise--get-or-create-document-buffer book-id title)))
    
    (with-current-buffer doc-buffer
      ;; Only insert document heading if file is new (point at beginning)
      (when (= (point-min) (point))
        (org-readwise--insert-org-heading 1 title book-id author source-url summary tags doc-buffer nil))
      
      ;; Process highlights
      (let ((highlights (assoc-default 'highlights book)))
        (when (vectorp highlights)
          (setq highlights (append highlights nil)))
        (dolist (highlight highlights)
          (org-readwise--process-highlight highlight doc-buffer)))
      
      ;; Save the document file
      (save-buffer))))


(defun org-readwise--process-highlights (results)
  "Process highlights data and print it in a structured way."
  (org-readwise-debug 1 "Processing %d books" (length results))
  (org-readwise-debug 2 "Results type: %s" (type-of results))
  (when (or (listp results) (vectorp results))
    (setq results (append results nil))  ; Convert vector to list if necessary
    (let* ((output-buffer (when (eq org-readwise-output-location 'buffer)
                            (get-buffer-create "*Readwise Highlights*")))
           (output-file (when (and (stringp org-readwise-output-location)
                                   (not (eq org-readwise-output-location 'buffer)))
                          org-readwise-output-location)))
      (org-readwise-debug 2 "Output buffer: %s" output-buffer)
      (org-readwise-debug 2 "Output file: %s" output-file)
      (with-current-buffer (or output-buffer (find-file-noselect output-file))
        (goto-char (point-max))
        (dolist (book results)
          (org-readwise-debug 1 "Processing book: %s (ID: %s)" (or (assoc-default 'title book) "No Title") (assoc-default 'user_book_id book))
          (org-readwise--process-book book (current-buffer))
          (org-readwise-debug 2 "Finished processing book: %s (ID: %s)" (or (assoc-default 'title book) "No Title") (assoc-default 'user_book_id book))))
      (when output-buffer
        (with-current-buffer output-buffer
          (org-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun org-readwise--process-document (document buffer &optional parent-id)
  "Process a single DOCUMENT.
BUFFER parameter is ignored - creates/updates per-document file.
PARENT-ID is currently ignored."
  (let* ((doc-id (assoc-default 'id document))
         (title (or (assoc-default 'title document) "No Title"))
         (author (assoc-default 'author document))
         (summary (assoc-default 'summary document))
         (notes (assoc-default 'notes document))
         (source-url (assoc-default 'source_url document))
         (tags (assoc-default 'tags document))
         (doc-buffer (org-readwise--get-or-create-document-buffer doc-id title)))
    
    (org-readwise-debug 2 "Inserting document with ID: %s, Title: %s" doc-id title)
    
    ;; Convert tags to string
    (when (and tags (listp tags))
      (org-readwise-debug 1 "Processing tags for document ID: %s" doc-id)
      (setq tags (mapconcat (lambda (tag)
                              (if (stringp tag)
                                  tag
                                (assoc-default 'name tag)))
                            tags ":")))
    
    (with-current-buffer doc-buffer
      ;; Only insert document heading if file is new
      (when (= (point-min) (point))
        (org-readwise--insert-org-heading 1 title doc-id author source-url summary tags doc-buffer nil))
      
      ;; Insert notes if present
      (when (and notes (not (string-empty-p notes)))
        (org-readwise--insert-org-heading 2 "Notes" (concat doc-id "-notes") nil nil notes nil doc-buffer t))
      
      ;; Save the document file
      (save-buffer))))

(defun org-readwise--process-documents (documents buffer)
  "Process documents data and insert them into Org mode, handling parent-child relationships."
  ;; Convert documents to a list if it's a vector
  (when (vectorp documents)
    (org-readwise-debug 2 "Converting documents vector to list")
    (setq documents (append documents nil)))  ;; Convert vector to list

  ;; Ensure it's a list before proceeding
  (when (listp documents)
    (org-readwise-debug 1 "Processing %d documents" (length documents))
    (let ((document-hash (make-hash-table :test 'equal))
          (processed-ids (make-hash-table :test 'equal)))  ;; Keep track of processed documents
      ;; First, populate the hash table with all documents by their IDs
      (dolist (document documents)
        (let ((doc-id (assoc-default 'id document)))
          (org-readwise-debug 2 "Adding document to hash with ID: %s" doc-id)
          (puthash doc-id document document-hash)))
      ;; Now process each document
      (dolist (document documents)
        (let ((doc-id (assoc-default 'id document))
              (parent-id (assoc-default 'parent_id document)))
          (unless (gethash doc-id processed-ids)  ;; Skip if document is already processed
            (org-readwise-debug 1 "Processing document with ID: %s" doc-id)
            (if parent-id
                (let ((parent-document (gethash parent-id document-hash)))
                  (if (and parent-document (not (gethash parent-id processed-ids)))
                      (progn
                        (org-readwise-debug 2 "Found parent document with ID: %s for document: %s" parent-id doc-id)
                        (org-readwise--process-document document buffer parent-id))
                    (org-readwise-debug 1 "Parent document with ID: %s not found for document: %s" parent-id doc-id)))
              (org-readwise--process-document document buffer))
            ;; Mark as processed and log the action
            (org-readwise-debug 1 "Document with ID: %s has been processed" doc-id)
            (puthash doc-id t processed-ids)))))))

(provide 'org-roam-import)
