;;; custom/writing/autoload/misc.el -*- lexical-binding: t; -*-

(defun find-latest-png (&optional folder)
  "Find latest .png file in specified FOLDER.
Default FOLDER is ~/Desktop. It scans the folder for .png files that
created within the last minute and grabs the latest."
  (let ((current-t (float-time (current-time))))
    (thread-last
      (directory-files (or folder "~/Desktop") :full "\\.png\\'")
      (seq-filter (lambda (f)
                    (let ((f-t (thread-last
                                 f file-attributes
                                 file-attribute-modification-time
                                 float-time)))
                      (when (and (>= (- current-t f-t) 0)
                                 (<= (- current-t f-t) 120))
                        f))))
      last car)))


;;;###autoload
(defun ocr-image-to-buffer (&optional image-path)
  "Using tesseract ocr image file at IMAGE-PATH."
  (interactive)
  (let* ((f (or image-path
                (find-latest-png)
                (let ((completion-regexp-list '("\\.png\\'")))
                  (read-file-name "Image to OCR: "
                                  "~/Desktop/"))))
         (cmd (format
               "%s '%s' /tmp/ocr"
               (executable-find "tesseract")
               (expand-file-name f))))
    (when (zerop (call-process-shell-command
                  cmd))
      (with-current-buffer (get-buffer-create
                            (format "* OCR %s *" f))
        (erase-buffer)
        (insert-file-contents "/tmp/ocr.txt")
        (delete-file "/tmp/ocr.txt")
        (display-buffer (current-buffer))))))
