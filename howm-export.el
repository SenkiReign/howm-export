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
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
    max-width: 900px;
    margin: 0 auto;
    padding: 20px;
    line-height: 1.4;
    color: #333;
    background: white;
  }
  .content {
    background: white;
    padding: 0;
  }
  h1 {
    color: #000;
    border-bottom: 3px solid #000;
    padding-bottom: 10px;
    margin-top: 10px;
    margin-bottom: 15px;
    font-size: 1.1em;
    font-weight: 600;
  }
  a {
    color: #2c3e50;
    text-decoration: none;
    border-bottom: 1px solid transparent;
    transition: border-color 0.2s;
  }
  a:hover {
    border-bottom-color: #000;
  }
  .link-block {
    background: #ecf0f1;
    padding: 12px 16px;
    margin: 10px 0;
    border-radius: 4px;
    border-left: 4px solid #000;
  }
  .date {
    display: inline-block;
    color: #000;
    font-size: 0.85em;
    font-weight: 600;
    padding: 4px 12px;
    border: 2px solid #000;
    border-radius: 4px;
    margin: 10px 0;
  }
  .image-container {
    text-align: center;
    margin: 15px 0;
  }
  .image-container img {
    max-width: 100%;
    height: auto;
    border-radius: 4px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  p {
    margin: 8px 0;
  }
  pre {
    background: #2c3e50;
    color: #ecf0f1;
    padding: 15px;
    border-radius: 4px;
    overflow-x: auto;
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

(defun howm-export-html ()
  "Export visible portion of current buffer to HTML."
  (interactive)
  (let* ((output-file (if buffer-file-name
                          (concat (file-name-sans-extension buffer-file-name) ".html")
                        (read-file-name "Save HTML as: " nil nil nil "export.html")))
         (title (if buffer-file-name
                    (file-name-base buffer-file-name)
                  "Howm Export"))
         (html "")
         (base-dir default-directory))
    
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (min (line-end-position) (point-max)))))
          
          (cond
           ;; Headers: = Header
           ((string-match "^=\\s-*\\(.+\\)" line)
            (setq html (concat html (format "<h1>%s</h1>\n" (match-string 1 line)))))
           
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
           
           ;; Dates: [2024-01-01] or [2024-01-01 12:30]
           ((string-match "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\)\\]" line)
            (setq html (concat html (format "<div class=\"date\">%s</div>\n" (match-string 1 line)))))
           
           ;; Empty lines
           ((string-match "^\\s-*$" line)
            (setq html (concat html "<br/>\n")))
           
           ;; Regular text
           (t
            (setq html (concat html (format "<p>%s</p>\n" (string-trim line)))))))
        
        (forward-line 1)))
    
    (let ((full-html (format "<!DOCTYPE html>
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
                            title
                            howm-export-css
                            html)))
      
      (with-temp-file output-file
        (insert full-html))
      
      (message "Exported to: %s" output-file)
      (when howm-export-open-after-export
        (browse-url (concat "file://" (expand-file-name output-file))))
      output-file)))

(provide 'howm-export)
;;; howm-export.el ends here
