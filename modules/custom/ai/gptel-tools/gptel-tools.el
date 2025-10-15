;;; gptel-tools.el --- collection of gptel-tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 11, 2025
;; Modified: October 11, 2025
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/agzam/.doom.d
;; Package-Requires: ((emacs "30.2"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Mostly inspired by Karthik's experiments:
;; https://github.com/karthink/.emacs.d/blob/master/lisp/llm-tools.el#L1
;;
;;; Code:

(require 'gptel)
(require 'eww)
(require 'url-http)
(require 'auth-source)

(defvar url-http-end-of-headers) ; used by url-http

(defun my/gptel-brave-search (callback query &optional count)
  "Return a JSON array of COUNT web search results for QUERY.

CALLBACK is called on the result."
  (let* ((brave-url "https://api.search.brave.com/res/v1/web/search")
         (brave-api-key
          (lambda () (auth-source-pick-first-password :host "api.search.brave.com")))
         (brave-url-string
          (lambda (q) (concat brave-url "?"
                              (url-build-query-string
                               `(("q" ,(url-hexify-string q))
                                 ("count" ,(format "%s" (or count 5)))
                                 ("page" ,(format "%s" 0)))))))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
            ("Accept" . "application/json")
            ("Accept-Encoding" . "gzip")
            ("X-Subscription-Token" . ,(let ((key brave-api-key))
                                         (if (functionp key) (funcall key) key))))))
    (url-retrieve (funcall brave-url-string query)
                  (lambda (_)
                    (goto-char url-http-end-of-headers)
                    (when-let* ((attrs (ignore-errors (json-parse-buffer :object-type 'plist)))
                                (raw-results (map-nested-elt attrs '(:web :results)))
                                (annotated-results
                                 (vconcat
                                  (mapcar
                                   (lambda (item)
                                     (let* ((title (map-elt item :title))
                                            (url (map-elt item :url))
                                            (desc (map-elt item :description)))
                                       (list :url url :title title :description desc)))
                                   raw-results))))
                      (funcall callback annotated-results)))
                  nil 'silent)))

(gptel-make-tool
 :name "search_web"
 :function 'my/gptel-brave-search
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a plist of objects.  Each object has the keys `:url`, `:title` and `:description` for the corresponding search result.

If required, consider using the url as the input to the `read_url` tool to get the contents of the url.  Note that this might not work as the `read_url` tool does not handle javascript-enabled pages."
 :args `((:name "query" :type string :description "The natural language search query, can be multiple words."))
 :async t
 :category "web")

(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min)) (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (eww-score-readability dom)
                   (shr-insert-document (eww-highest-readability dom))
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type "string"
               :description "The URL to read"))
 :category "web")

(defun my/gptel-youtube-metadata (callback url)
  (let* ((video-id
          (and (string-match
                (concat
                 "^\\(?:http\\(?:s?://\\)\\)?\\(?:www\\.\\)?\\(?:youtu\\(?:\\(?:\\.be\\|be\\.com\\)/\\)\\)"
                 "\\(?:watch\\?v=\\)?" "\\([^?&]+\\)")
                url)
               (match-string 1 url)))
         (dir (file-name-concat temporary-file-directory "yt-dlp" video-id)))
    (if (file-directory-p dir) (delete-directory dir t))
    (make-directory dir t)
    (let ((default-directory dir) (idx 0)
          (data (list :description nil :transcript nil)))
      (make-process :name "yt-dlp"
                    :command `("yt-dlp" "--write-description" "--skip-download" "--output" "video" ,url)
                    :sentinel (lambda (proc status)
                                (cl-incf idx)
                                (let ((default-directory dir))
                                  (when (file-readable-p "video.description")
                                    (plist-put data :description
                                               (with-temp-buffer
                                                 (let ((coding-system-for-read 'utf-8-unix))
                                                   (insert-file-contents "video.description")
                                                   (buffer-string))))))
                                (when (= idx 2)
                                  (funcall callback data)
                                  (delete-directory dir t))))
      (make-process :name "yt-dlp"
                    :command `("yt-dlp" "--skip-download" "--write-auto-subs" "--sub-langs"
                               "en,-live_chat" "--convert-subs" "srt" "--output" "video" ,url)
                    :sentinel (lambda (proc status)
                                (cl-incf idx)
                                (let ((default-directory dir))
                                  (when (file-readable-p "video.en.srt")
                                    (plist-put data :transcript
                                               (with-temp-buffer
                                                 (let ((coding-system-for-read 'utf-8-unix))
                                                   (insert-file-contents "video.en.srt")
                                                   (buffer-string))))))
                                (when (= idx 2)
                                  (funcall callback data)
                                  (delete-directory dir t)))))))

(gptel-make-tool
 :name "get_youtube_meta"
 :function #'my/gptel-youtube-metadata
 :description "Find the description and video transcript for a youtube video.  Return a JSON object containing two fields:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
          :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
          :type "string"))
 :category "web"
 :async t
 :include t)

(provide 'gptel-tools)
;;; gptel-tools.el ends here
