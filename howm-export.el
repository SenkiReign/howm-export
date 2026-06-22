;;; howm-export.el --- Pretty HTML export for howm notes

(defcustom howm-export-image-max-width 800
  "Maximum width for exported images in pixels."
  :type 'integer
  :group 'howm)

(defcustom howm-export-image-max-height 600
  "Maximum height for exported images in pixels."
  :type 'integer
  :group 'howm)

(defcustom howm-export-open-after-export t
  "If non-nil, open the exported HTML file in the default browser."
  :type 'boolean
  :group 'howm)

(defcustom howm-export-css
  "body {
    font-family: 'Courier New', Courier, monospace;
    max-width: 800px;
    margin: 0 auto;
    padding: 40px 32px;
    line-height: 1.6;
    color: #222;
    background: #faf9f5;
    font-size: 14px;
  }
  h1 {
    font-size: 1em;
    font-weight: bold;
    letter-spacing: 0.15em;
    text-transform: uppercase;
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin: 2rem 0 0.25rem;
    border: none;
    padding: 0;
  }
  .header-title { flex: 1; }
  .header-divider {
    border: none;
    border-top: 1px solid #ccc;
    margin: 0.3rem 0 1rem;
  }
  h1 .date {
    font-size: 0.8em;
    font-weight: normal;
    letter-spacing: 0.05em;
    color: #888;
    padding: 0;
    margin-left: 16px;
    border: none;
  }
  a {
    color: #444;
    text-decoration: none;
  }
  a:hover { text-decoration: underline; }
  .link-block {
    font-size: 0.85em;
    color: #666;
    margin: 0.4rem 0 0.4rem 1.5em;
    padding: 0;
    background: none;
    border: none;
  }
  .link-block::before { content: '\2192  '; }
  .date {
    display: inline-block;
    font-size: 0.75em;
    color: #888;
    letter-spacing: 0.05em;
    border: 1px solid #ccc;
    padding: 1px 6px;
    margin: 0.5rem 0;
    background: none;
  }
  .image-container {
    margin: 1.5rem 0;
    text-align: center;
  }
  .image-container img {
    max-width: 100%;
    height: auto;
    border: 1px solid #ccc;
  }
  p { margin: 0.4rem 0; }
  @media print {
    body { background: white; padding: 0; }
    .link-block { color: #333; }
    .date { border-color: #999; }
  }"
  "CSS styles for exported HTML."
  :type 'string
  :group 'howm)

(defun howm-export-get-image-dimensions (file)
  "Get dimensions of image FILE."
  (when (and (file-exists-p file)
             (image-type-available-p (image-type-from-file-name file)))
    (let* ((img (create-image file))
           (size (image-size img t)))
      (cons (car size) (cdr size)))))

(defun howm-export-calculate-scaled-dimensions (orig-width orig-height max-width max-height)
  "Calculate scaled dimensions maintaining aspect ratio."
  (let* ((width-scale (if (> orig-width max-width)
                          (/ (float max-width) orig-width)
                        1.0))
         (height-scale (if (> orig-height max-height)
                           (/ (float max-height) orig-height)
                         1.0))
         (scale (min width-scale height-scale)))
    (cons (round (* orig-width scale))
          (round (* orig-height scale)))))

(defun howm-export--note-bounds-at-point ()
  "Return (start . end) of the howm note surrounding point.
A note begins at a line starting with '= ' and ends just before
the next such line, or at the end of the buffer."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^=\\s-")
      (re-search-backward "^=\\s-" nil t))
    (when (looking-at "^=\\s-")
      (let ((start (point)))
        (forward-line 1)
        (let ((end (if (re-search-forward "^=\\s-" nil t)
                       (match-beginning 0)
                     (point-max))))
          (goto-char end)
          (skip-chars-backward "\n\t ")
          (cons start (point)))))))

(defun howm-export--lines-to-html (lines base-dir)
  "Convert a list of LINES to an HTML string, resolving images relative to BASE-DIR."
  (let ((html "")
        (i 0)
        (len (length lines)))
    (while (< i len)
      (let ((line (nth i lines)))
        (cond
         ;; Headers: = Header
         ((string-match "^=\\s-*\\(.+\\)" line)
          (let ((header-text (match-string 1 line))
                (next-line (when (< (+ i 1) len) (nth (+ i 1) lines))))
            (if (and next-line
                     (string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\)\\]" next-line))
                (progn
                  (let* ((date-str (match-string 1 next-line))
                         (rest-of-line (substring next-line (match-end 0)))
                         (desc (string-trim rest-of-line)))
                    (setq html (concat html
                                       (format "<h1><span class=\"header-title\">%s</span><span class=\"date\">%s</span></h1>\n<hr class=\"header-divider\"/>\n"
                                               header-text date-str)))
                    (when (and desc (> (length desc) 0))
                      (setq html (concat html (format "<p>%s</p>\n" desc)))))
                  (setq i (+ i 1)))
              (setq html (concat html (format "<h1><span class=\"header-title\">%s</span></h1>\n<hr class=\"header-divider\"/>\n" header-text))))))

         ;; Links: >>>url
         ((string-match "^>>>\\s-*\\(.+\\)" line)
          (let ((link (string-trim (match-string 1 line))))
            (setq html (concat html (format "<div class=\"link-block\"><a href=\"%s\">%s</a></div>\n" link link)))))

         ;; File links: file://path
         ((string-match "^file://\\(.+\\)" line)
          (let* ((file-path (string-trim (match-string 1 line)))
                 (full-path (expand-file-name file-path base-dir))
                 (is-image (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\)\\'" file-path)))
            (if is-image
                (let* ((dims (howm-export-get-image-dimensions full-path))
                       (scaled (when dims
                                 (howm-export-calculate-scaled-dimensions
                                  (car dims) (cdr dims)
                                  howm-export-image-max-width
                                  howm-export-image-max-height))))
                  (setq html (concat html
                                     (format "<div class=\"image-container\"><img src=\"%s\"%s /></div>\n"
                                             file-path
                                             (if scaled
                                                 (format " width=\"%d\" height=\"%d\""
                                                         (car scaled) (cdr scaled))
                                               "")))))
              (setq html (concat html (format "<div class=\"link-block\"><a href=\"%s\">%s</a></div>\n"
                                             file-path file-path))))))

         ;; Dates: [2024-01-01] text (standalone)
         ((string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\)\\]\\s-*\\(.+\\)" line)
          (setq html (concat html
                             (format "<div class=\"date\">%s</div>\n<p>%s</p>\n"
                                     (match-string 1 line)
                                     (match-string 3 line)))))

         ;; Empty lines
         ((string-match "^\\s-*$" line)
          (setq html (concat html "<br/>\n")))

         ;; Regular text
         (t
          (setq html (concat html (format "<p>%s</p>\n" (string-trim line)))))))

      (setq i (+ i 1)))
    html))

(defun howm-export--wrap-html (title body-html)
  "Wrap BODY-HTML in a full HTML document with TITLE."
  (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>%s</title>
  <style>%s</style>
</head>
<body>
  <div class=\"content\">
%s
  </div>
</body>
</html>"
          title howm-export-css body-html))

(defun howm-export-note-at-point ()
  "Export the howm note at point to HTML and open it."
  (interactive)
  (let ((bounds (howm-export--note-bounds-at-point)))
    (unless bounds
      (user-error "No howm note found at point"))
    (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (lines (split-string text "\n"))
           (title (if (string-match "^=\\s-*\\(.+\\)" (car lines))
                      (string-trim (match-string 1 (car lines)))
                    "howm note"))
           (base-dir default-directory)
           (slug (replace-regexp-in-string "[^a-zA-Z0-9]" "-" (downcase title)))
           (output-file (expand-file-name
                         (concat slug ".html")
                         (if buffer-file-name
                             (file-name-directory buffer-file-name)
                           default-directory)))
           (html (howm-export--lines-to-html lines base-dir))
           (full-html (howm-export--wrap-html title html)))
      (with-temp-file output-file
        (insert full-html))
      (message "Exported note \"%s\" to: %s" title output-file)
      (when howm-export-open-after-export
        (browse-url (concat "file://" (expand-file-name output-file))))
      output-file)))

(defun howm-export-html ()
  "Export the entire current buffer to HTML."
  (interactive)
  (let* ((output-file (if buffer-file-name
                          (concat (file-name-sans-extension buffer-file-name) ".html")
                        (read-file-name "Save HTML as: " nil nil nil "export.html")))
         (title (if buffer-file-name
                    (file-name-base buffer-file-name)
                  "Howm Export"))
         (base-dir default-directory)
         (lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
         (html (howm-export--lines-to-html lines base-dir))
         (full-html (howm-export--wrap-html title html)))
    (with-temp-file output-file
      (insert full-html))
    (message "Exported to: %s" output-file)
    (when howm-export-open-after-export
      (browse-url (concat "file://" (expand-file-name output-file))))
    output-file))

(provide 'howm-export)
;;; howm-export.el ends here
