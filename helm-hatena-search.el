;;; helm-hatena-search.el --- Helm interface for Hatena Bookmark full text search  -*- lexical-binding: t; -*-

;;; Code:

;; TODO logging
;; TODO test

;; requires
(require 'dash)
(require 'helm)
(require 'helm-net)

(defgroup helm-hatena-search nil
  "Helm interface for Hatena full text search"
  :group 'helm
  :prefix "helm-hatena-search-")

(defcustom helm-hatena-search-username nil
  "Hatena's user nameto use search API."
  :type 'string
  :group 'helm-hatena-search)

(defcustom helm-hatena-search-apikey nil
  "API key for hatena API."
  :type 'string
  :group 'helm-hatena-search)

(defcustom helm-hatena-search-max-url-length 100
  "Maximum length of displayed bookmark's url."
  :type 'integer
  :group 'helm-hatena-search)

;; Faces

(defgroup helm-hatena-search-faces nil
  "Customize the appearance of helm-hatena-search."
  :prefix "helm-"
  :group 'helm-hatena-search
  :group 'helm-faces)

(defface helm-hatena-search-title
  '((t :inherit hl-line
       :weight bold))
  "Face used for the title of Hatena search result."
  :group 'helm-hatena-search-faces)

(defface helm-hatena-search-timestamp
  '((t :inherit font-lock-variable-name-face))
  "Face used for the timestamp of Hatena search result."
  :group 'helm-hatena-search-faces)

(defface helm-hatena-search-url
  '((t :inherit font-lock-type-face))
  "Face used for the url of Hatena search result."
  :group 'helm-hatena-search-faces)

(defface helm-hatena-search-snippet
  '((t :inherit font-lock-comment-face))
  "Face used for the timestamp of Hatena search result."
  :group 'helm-hatena-search-faces)

(defvar helm-hatena-search-url
  "http://b.hatena.ne.jp/%s/search/json"
  "Url for Hatena full text search.")

(defvar helm-hatena-search-sources
  (helm-build-sync-source "Hatena Bookmark Search Results"
    :candidates #'helm-hatena-search-search-keyword
    :action '(("Browse" . helm-hatena-search-browse-action))
    :multiline t
    :requires-pattern 3
    :volatile t))

(defun helm-hatena-search-browse-action (candidate)
  (-let* (((&alist 'entry entry) candidate)
          ((&alist 'url url) entry))
    (helm-browse-url url)))

(defun helm-hatena-search-search-keyword ()
  (message (format "Querying %s..." helm-pattern))
  (with-current-buffer (generate-new-buffer "helm-hatena-search")
    (call-process
     "curl" nil t nil "-s"
     "-H" "Accept: application/json"
     "-H" (format
           "X-WSSE: %s"
           (helm-hatena-search-wsse-credential
            helm-hatena-search-username
            helm-hatena-search-apikey))
     (format (concat helm-hatena-search-url "?q=%s")
             helm-hatena-search-username
             (url-encode-url helm-pattern)))
    (helm-hatena-search-parse-buffer)))

(defun helm-hatena-search-wsse-credential (username apikey)
  (let* ((nonce (secure-hash
                 'sha1
                 (format-time-string "%Y-%m-%dT%T%z" (current-time))))
         (now (prog2 (set-time-zone-rule t)
                  (format-time-string "%Y-%m-%dT%TZ" (current-time))
                (set-time-zone-rule nil)))
         (digest (base64-encode-string
                  (secure-hash 'sha1 (concat nonce now apikey)
                               nil nil t))))
    (format
     (concat "UsernameToken Username=\"%s\""
             ", PasswordDigest=\"%s\""
             ", Nonce=\"%s\", Created=\"%s\"")
     username digest (base64-encode-string nonce) now)))

(defun helm-hatena-search-parse-buffer ()
  (goto-char (point-min))
  (when (re-search-forward "^{.*}$" nil t)
    (-let* ((response (json-read-from-string (match-string 0)))
            ((&alist 'bookmarks bookmarks) response))
      (cl-loop for bookmark across bookmarks
               collect (helm-hatena-search-format-candidate bookmark)))))

(defun helm-hatena-search-format-candidate (bookmark)
  (-let* (((&alist 'timestamp timestamp) bookmark)
          ((&alist 'entry entry) bookmark)
          ((&alist 'title title) entry)
          ((&alist 'url url) entry)
          ((&alist 'snippet snippet) entry)
          (stripped-snippet
           (replace-regexp-in-string "\n" " " snippet))
          (trancated-url
           (if (> (length url) 100)
               (concat (substring url 0 100) "…")
             url)))
    (cons
     (concat
      (propertize title 'face 'helm-hatena-search-title)
      (propertize
       (concat
        " "
        (format-time-string "%Y-%m-%d %H:%m" timestamp))
       'face 'helm-hatena-search-timestamp)
      (propertize
       (concat "\n" trancated-url) 'face 'helm-hatena-search-url)
      "\n"
      (propertize stripped-snippet 'face 'helm-hatena-search-snippet))
     bookmark)))

;;;###autoload
(defun helm-hatena-search ()
  "Show and browse the result of Hatena full text search."
  (interactive)
  (helm :sources helm-hatena-search-sources
        :buffer "*helm hatena bookmark search*"
        :prompt "> "
        :input-idle-delay 0.3
        :full-frame t))

(provide 'helm-hatena-search)

;;; helm-hatena-search.el ends here
