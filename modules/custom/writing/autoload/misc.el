;;; custom/writing/autoload/misc.el -*- lexical-binding: t; -*-

(defun screenshot->file (filepath)
  "Writes clipboard content to a FILEPATH.png.
Returns the full path on success, nil if clipboard contains no image."
  (if-let* ((jxa-script
             (format
              "
ObjC.import('AppKit');

const pasteboard = $.NSPasteboard.generalPasteboard;
const pasteboardItems = pasteboard.pasteboardItems;
if (pasteboardItems.count > 0) {
  const pasteboardItem = pasteboardItems.objectAtIndex(0);
  const imageTypes = pasteboardItem.types;
  const fpath = '%s.png'
  for (let i = 0; i < imageTypes.count; i++) {
    const imageType = imageTypes.objectAtIndex(i);
    const imageTypeString = imageType.UTF8String;

    if (imageTypeString.includes('public.tiff') ||
        imageTypeString.includes('com.trolltech.anymime.application--x-qt-image'))
    {
        const imageData = pasteboardItem.dataForType(imageType);
        const image = $.NSImage.alloc.initWithData(imageData);
        const imageRep = image.representations.objectAtIndex(0);
        const pngData = imageRep.representationUsingTypeProperties($.NSPNGFileType,$())
	pngData.writeToFileAtomically(fpath,true)
        fpath
        break;
    }
  }
}
"
              filepath))
            (created (car-safe (run-jxa jxa-script))))
      created))

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
(defun ocr-clipboard-content ()
  "ocr clipboard content (if image)."
  (interactive)
  (when-let* ((shot-path (make-temp-name
                          (expand-file-name
                           "screenshot" temporary-file-directory)))
              (fpath (screenshot->file shot-path)))
    (ocr-image-to-buffer fpath)
    (delete-file fpath)))

;;;###autoload
(defun ocr-image-to-buffer (&optional image-path)
  "Using tesseract ocr image file at IMAGE-PATH."
  (interactive)
  (let* ((f (or image-path
                (find-latest-png)
                (let ((completion-regexp-list '("\\.png\\'")))
                  (read-file-name "Image to OCR: "
                                  "~/Desktop/"))))
         (ocr-file (make-temp-name
                    (expand-file-name "ocr" temporary-file-directory)))
         (cmd (format
               "%s --psm 6 '%s' %s"
               (executable-find "tesseract")
               (expand-file-name f)
               ocr-file))
         (ocr-file (concat ocr-file ".txt")))
    (when (zerop (call-process-shell-command
                  cmd))
      (with-current-buffer (get-buffer-create
                            (format "* OCR %s *" f))
        (erase-buffer)
        (insert-file-contents ocr-file)
        (delete-file ocr-file)
        (display-buffer (current-buffer))))))
