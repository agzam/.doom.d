;;; custom/web-browsing/autoload/eww.el -*- lexical-binding: t; -*-

;; a bunch of these things were borrowed from: Protesilaos Stavrou and remodeled for my
;; own needs https://protesilaos.com/codelog/2021-03-25-emacs-eww

;;;###autoload
(defun eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(defun eww--capture-url-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings.  Strings are in the form LABEL @ URL.
When optional argument POSITION is non-nil, include position info
in the strings too, so strings take the form
LABEL @ URL ~ POSITION."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      ;; NOTE 2021-07-25: The first clause in the `or' is meant to
      ;; address a bug where if a URL is in `point-min' it does not get
      ;; captured.
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string "\n" " " ; NOTE 2021-07-25: newlines break completion
                                                (buffer-substring-no-properties
                                                 start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (if position
                (push (format "%-15s ~ %s  @ %s"
                              coordinates label url)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

(defmacro eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
If region is active run BODY within active region instead.
Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

;;;###autoload
(defun +eww-jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

When called without ARG (\\[universal-argument]) get URLs only
from the visible portion of the buffer.  But when ARG is provided
consider whole buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links
            (if arg
                (eww--capture-url-on-page t)
              (eww-act-visible-window
               (eww--capture-url-on-page t))))
           (prompt-scope (if arg
                             (propertize "URL on the page" 'face 'warning)
                           "visible URL"))
           (prompt (format "Jump to %s: " prompt-scope))
           (selection (completing-read prompt links nil t))
           (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
           (point (string-to-number position)))
      (goto-char point)
      (recenter))))

;;;###autoload
(defun +eww-open-in-other-window (url &rest args)
  "Use `eww-open-in-new-buffer' in another window."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; (interactive (list (car (eww-suggested-uris))))
  (other-window-prefix)  ; For emacs28 -- it's a hack, but why not?
  (eww url :new-buffer))

;;;###autoload
(defun +eww-copy-current-url ()
  (interactive)
  (let ((url (eww-current-url)))
    (kill-new url)
    (message url)))

;;;###autoload
(defun +eww-increase-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (+ cur 0.1)))
    (ignore-errors
     (text-scale-increase 0.5))))

;;;###autoload
(defun +eww-decrease-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (- cur 0.1)))
    (text-scale-decrease 0.5)))

(require 'transient)
;;;###autoload
(transient-define-prefix eww-zoom-transient ()
  "EWW"
  ["Fonts"
   [("j" "decrease" +eww-decrease-font-size :transient t)
    ("k" "increase" +eww-increase-font-size :transient t)]])

;;;###autoload
(defun eww-make-readable-a (fn charset url document point buffer)
  "Run npm script to extract readable html content."
  (let* ((tmp-in (make-temp-file "src" nil ".html"))
         (tmp-out (make-temp-file "dest" nil ".html"))
         (script-dir (expand-file-name (concat
                                        doom-user-dir
                                        "modules/custom/web-browsing/readable")))
         (script (concat script-dir "/extract.cljs")))
    (unless (file-directory-p (concat script-dir "/node_modules"))
      (shell-command "%1$s install -g nbb && %1$s install" (executable-find "npm")))
    (unwind-protect
        (progn
          (write-region (point) (point-max) tmp-in)
          (shell-command
           (format
            "%s %s %s %s"
            (executable-find "nbb")
            script tmp-in tmp-out))
          (with-current-buffer buffer
            (read-only-mode -1)
            (erase-buffer)
            (insert-file-contents tmp-out)
            (funcall fn charset url document point buffer)))
      (delete-file tmp-in)
      (delete-file tmp-out))))

;; discovered somewhere on GitHub, not entirely confident if it's the
;; best way to solve the problem. Don't follow my example - don't just
;; blindly copy it!
;;;###autoload
(defun shr-put-image* (spec alt &optional flags)
  "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.
Hack to use `insert-sliced-image' to avoid jerky image scrolling."
  (if (display-graphic-p)
      (let* ((size (cdr (assq 'size flags)))
             (data (if (consp spec)
                       (car spec)
                     spec))
             (content-type (and (consp spec)
                                (cadr spec)))
             (start (point))
             (image (cond
                     ((eq size 'original)
                      (create-image data nil t :ascent 100
                                    :format content-type))
                     ((eq content-type 'image/svg+xml)
                      (create-image data 'svg t :ascent 100))
                     ((eq size 'full)
                      (ignore-errors
                        (shr-rescale-image data content-type
                                           (plist-get flags :width)
                                           (plist-get flags :height))))
                     (t
                      (ignore-errors
                        (shr-rescale-image data content-type
                                           (plist-get flags :width)
                                           (plist-get flags :height)))))))
        (when image
          (let* ((image-pixel-cons (image-size image t))
                 (image-pixel-width (car image-pixel-cons))
                 (image-pixel-height (cdr image-pixel-cons))
                 (image-scroll-rows (round (/ image-pixel-height (default-font-height)))))
            ;; When inserting big-ish pictures, put them at the
            ;; beginning of the line.
            (when (and (< 0 (current-column))
                       (< 400 (car (image-size image t))))
              (insert "\n"))

            (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
            ;; (if (eq size 'original)
            ;;     (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
            ;;   (insert-image image (or alt "*")))

            (put-text-property start (point) 'image-size size)
            (when (and shr-image-animate
                       (cond ((fboundp 'image-multi-frame-p)
                              ;; Only animate multi-frame things that specify a
                              ;; delay; eg animated gifs as opposed to
                              ;; multi-page tiffs.  FIXME?
                              (cdr (image-multi-frame-p image)))
                             ((fboundp 'image-animated-p)
                              (image-animated-p image))))
              (image-animate image nil 60))))
        image)
    (insert (or alt ""))))
