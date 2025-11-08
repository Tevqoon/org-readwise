;;; org-readwise.el --- Sync Readwise highlights with Org-mode -*- lexical-binding: t; -*-
;; Author: CountGreven
;; URL: https://github.com/CountGreven/org-readwise
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

;; This package integrates Readwise highlight syncing with Org-mode.
;; It provides commands to fetch highlights from Readwise and insert
;; them into an Org buffer or a specified file.

;;; Code:
(require 'org)
(require 'auth-source)
(require 'url)
(require 'request)
(require 'json)
(require 'readwise-lib)
(require 'org-roam-import)

(defvar org-readwise--last-cursor nil
  "Tracks the last cursor received from the Readwise API during pagination.")

(defgroup org-readwise ()
  "Integrate the Readwise.io highlight syncing service with `org-mode`."
  :group 'files
  :prefix "org-readwise-"
  :link '(info-link "(org-readwise) Top"))

(defcustom org-readwise-sync-highlights t
  "Toggle whether to sync Readwise highlights."
  :group 'org-readwise
  :type 'boolean)

(defcustom org-readwise-sync-reader t
  "Toggle whether to sync Readwise reader documents."
  :group 'org-readwise
  :type 'boolean)

(defcustom org-readwise-debug-level 0
  "Debug level for the org-readwise package.
0 - No debug output.
1 - Basic debug output.
2 - Detailed debug output."
  :group 'org-readwise
  :type 'integer)

(defun org-readwise--load-last-sync-time ()
  "Load the last sync time from `org-readwise-last-sync-time-file`."
  (when (file-exists-p org-readwise-last-sync-time-file)
    (with-temp-buffer
      (insert-file-contents org-readwise-last-sync-time-file)
      (setq org-readwise-last-sync-time (buffer-string)))))

(defun org-readwise--save-last-sync-time (timestamp)
  "Save the last sync time TIMESTAMP to `org-readwise-last-sync-time-file`."
  (with-temp-file org-readwise-last-sync-time-file
    (insert timestamp))
  (setq org-readwise-last-sync-time timestamp))

(defun org-readwise--get-access-token ()
  "Get the access token for Readwise.
This function expects the token to be present in one of the files defined in
`auth-sources`. If it is not present, it will prompt the user for their access
token. It returns a list containing the token and a save function."
  (let ((found (nth 0 (auth-source-search :host "readwise.io"))))
    (if found
        (list (let ((secret (plist-get found :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret))
              (plist-get found :save-function))
      nil)))

(defun org-readwise--get-highlights (&optional cursor updated-after)
  "Get highlights from the Readwise API, handling pagination with CURSOR.
Include the UPDATED-AFTER parameter only in the initial request."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (concat readwise-api-base-url "v2/export"
                      (when cursor (concat "?pageCursor=" (format "%s" cursor)))
                      (when updated-after (concat (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))))
    (org-readwise-debug 1 "Making request to: %s" url)
    (request url
      :headers token-header
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-readwise-debug 2 "Response Data: %S" data)
                  (let ((results (assoc-default 'results data))
                        (next-cursor (let ((nc (assoc-default 'nextPageCursor data)))
                                       (if (numberp nc) (number-to-string nc) nc))))
                    (when results
                      (org-readwise--process-highlights results))
                    (if (and next-cursor (not (string-empty-p next-cursor))
                             (not (string= next-cursor org-readwise--last-cursor)))
                        (progn
                          (setq org-readwise--last-cursor next-cursor)
                          (org-readwise--get-highlights next-cursor updated-after))
                      (org-readwise-debug 1 "No more pages to fetch or cursor repeated, stopping pagination.")))))
      :error (cl-function
              (lambda (&key response data &allow-other-keys)
                (let* ((status-code (request-response-status-code response))
                       (retry-after (assoc-default 'Retry-After (request-response-headers response)))
                       (detail (assoc-default 'detail data))
                       (retry-seconds (if retry-after
                                          (string-to-number retry-after)
                                        (if (and detail (string-match "in \\([0-9]+\\) seconds" detail))
                                            (string-to-number (match-string 1 detail))
                                          30))))  ;; Default to 30 seconds if no information is available
                  (if (eq status-code 429)
                      (progn
                        (org-readwise-debug 1 "Rate limit hit, retrying after %s seconds" retry-seconds)
                        (run-at-time retry-seconds nil #'org-readwise--get-highlights cursor updated-after))
                    (org-readwise-debug 1 "Error Response: %S" response)
                    (message "Error fetching highlights: %S" status-code))))))))




(defun org-readwise--get-documents (&optional cursor updated-after)
  "Get documents from the Readwise API v3, handling pagination with CURSOR.
Include the UPDATED-AFTER parameter only in the initial request."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (concat "https://readwise.io/api/v3/list/"
                      (when cursor (concat "?pageCursor=" cursor))
                      (when updated-after (concat (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))))
    (org-readwise-debug 1 "Making request to: %s" url)
    (request url
      :headers token-header
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-readwise-debug 2 "Response Data: %S" data)
                  (let ((results (assoc-default 'results data))
                        (next-cursor (assoc-default 'nextPageCursor data)))
                    (when results
                      (org-readwise--process-documents results (get-buffer-create "*Readwise Highlights*")))
                    (if (and next-cursor (not (string-empty-p next-cursor))
                             (not (string= next-cursor org-readwise--last-cursor)))
                        (progn
                          (setq org-readwise--last-cursor next-cursor)
                          (org-readwise--get-documents next-cursor updated-after))
                      (org-readwise-debug 1 "No more pages to fetch or cursor repeated, stopping pagination.")))))
      :error (cl-function
              (lambda (&key response data &allow-other-keys)
                (let* ((status-code (request-response-status-code response))
                       (retry-after (assoc-default 'Retry-After (request-response-headers response)))
                       (detail (assoc-default 'detail data))
                       (retry-seconds (if retry-after
                                          (string-to-number retry-after)
                                        (if (string-match "in \\([0-9]+\\) seconds" detail)
                                            (string-to-number (match-string 1 detail))
                                          30))))  ;; Default to 30 seconds if no information is available
                  (if (eq status-code 429)
                      (progn
                        (org-readwise-debug 1 "Rate limit hit, retrying after %s seconds" retry-seconds)
                        (run-at-time retry-seconds nil #'org-readwise--get-documents cursor updated-after))
                    (org-readwise-debug 1 "Error Response: %S" response)
                    (message "Error fetching documents: %S" status-code))))))))

(defun string-empty-p (str)
  "Check whether STR, coerced to a string, is empty."
  (string= (format "%s" str) ""))

(defun org-readwise--get-next-page-cursors (&optional cursor updated-after)
  "Get nextPageCursor from the Readwise API v3, handling pagination with CURSOR."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (concat "https://readwise.io/api/v3/list/"
                      (when cursor (concat "?pageCursor=" cursor))
                      (when updated-after (concat (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))))
    (request url
      :headers token-header
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((next-cursor (assoc-default 'nextPageCursor data)))
           ;; Print nextPageCursor at debug level 1
           (when next-cursor
             (org-readwise-debug 1 "Next Page Cursor: %s" next-cursor))
           ;; The rest of the processing at debug level 2
           (org-readwise-debug 2 "Response Data: %S" data)
           (when (and next-cursor (not (string= next-cursor cursor))) ;; Avoid looping on the same cursor
             (org-readwise--get-next-page-cursors next-cursor updated-after)))))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (message "Error fetching documents: %S" (request-response-status-code response)))))
    :status-code '((401 . (lambda (&rest _) (message "Unauthorized"))))))

(defun org-readwise-sync-cursors (&optional all)
  "Print only nextPageCursor from Readwise during pagination."
  (interactive "P")
  (setq org-readwise--last-cursor nil)  ;; Reset the cursor before sync
  (org-readwise--load-last-sync-time)
  (let ((updated-after (unless all org-readwise-last-sync-time)))
    (org-readwise--get-next-page-cursors nil updated-after)))

(defun org-readwise-sync (&optional all)
  "Synchronize highlights and documents from Readwise and insert them into an Org buffer or file.
If ALL is non-nil (when called with a universal argument), pull all highlights and documents."
  (interactive "P")
  ;; Explicitly initialize output-buffer and output-file to nil
  (let* ((output-buffer nil)
         (output-file nil))

    ;; Log initialization
    (org-readwise-debug 1 "Initializing output-buffer and output-file as nil.")

    ;; Set output-buffer or output-file based on user preferences
    (setq output-buffer (if (eq org-readwise-output-location 'buffer)
                            (get-buffer-create "*Readwise Highlights*")
                          nil))

    (setq output-file (if (and (stringp org-readwise-output-location)
                               (not (eq org-readwise-output-location 'buffer)))
                          org-readwise-output-location
                        nil))

    ;; Log post-setup values
    (org-readwise-debug 1 "After initialization: output-buffer: %s, output-file: %s"
                        (if output-buffer "set" "nil")
                        (if output-file "set" "nil"))

    ;; Check if neither output-buffer nor output-file is set
    (unless (or output-buffer output-file)
      (org-readwise-debug 1 "Neither output-buffer nor output-file is set. Exiting sync.")
      (message "No output buffer or file specified. Exiting sync.")
      (return-from org-readwise-sync))

    ;; Continue with sync logic
    (when output-buffer
      (with-current-buffer output-buffer
        (erase-buffer)
        (org-readwise-debug 1 "Erased output-buffer.")))

    (setq org-readwise--last-cursor nil)  ;; Reset the cursor before sync
    (org-readwise--load-last-sync-time)
    (let ((updated-after (unless all org-readwise-last-sync-time)))
      ;; Sync highlights from v2 if enabled
      (when org-readwise-sync-highlights
        (org-readwise-debug 1 "Syncing highlights...")
        (org-readwise--get-highlights nil updated-after))
      ;; Sync documents from v3 if enabled
      (when org-readwise-sync-reader
        (org-readwise-debug 1 "Syncing documents...")
        (org-readwise--get-documents nil updated-after))
      ;; Save the last sync time
      (org-readwise--save-last-sync-time (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))

  ;; Write to the file if output-file is provided
  (when output-file
    (org-readwise-debug 1 "Writing output to file: %s" output-file)
    (with-temp-file output-file
      (when output-buffer  ;; Ensure output-buffer exists
        (insert-buffer-substring output-buffer))))

  ;; Check for output-buffer being displayed
  (when output-buffer
    (with-current-buffer output-buffer
      (org-mode)
      (goto-char (point-min))
      (org-readwise-debug 1 "Displaying output-buffer.")
      (display-buffer (current-buffer)))))

(defun org-readwise-sync (&optional all)
  "Synchronize highlights and documents from Readwise and insert them into an Org buffer or file.
If ALL is non-nil (when called with a universal argument), pull all highlights and documents."
  (interactive "P")
  (let* ((output-buffer (when (eq org-readwise-output-location 'buffer)
                          (get-buffer-create "*Readwise Highlights*")))
         (output-file (when (and (stringp org-readwise-output-location)
                                 (not (eq org-readwise-output-location 'buffer)))
                        org-readwise-output-location)))
    (when output-buffer
      (with-current-buffer output-buffer
        (erase-buffer)))
    (setq org-readwise--last-cursor nil)  ;; Reset the cursor before sync
    (org-readwise--load-last-sync-time)
    (let ((updated-after (unless all org-readwise-last-sync-time)))
      ;; Sync highlights from v2 if enabled
      (when org-readwise-sync-highlights
        (org-readwise--get-highlights nil updated-after))
      ;; Sync documents from v3 if enabled
      (when org-readwise-sync-reader
        (org-readwise--get-documents nil updated-after))
      ;; Save the last sync time
      (org-readwise--save-last-sync-time (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))

  ;; Writing to file if output-file is non-nil
  (when (and (boundp 'output-file) output-file)
    (org-readwise-debug 1 "Writing output to file: %s" output-file)
    (with-temp-file output-file
      (when output-buffer
        (insert-buffer-substring output-buffer))))

  (when (and (boundp 'output-buffer) output-buffer)
    (with-current-buffer output-buffer
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'org-readwise)
