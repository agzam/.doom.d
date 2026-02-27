;;; custom/writing/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun spacehammer-edit-with-emacs-h (buffer-name pid title)
  (with-current-buffer (get-buffer buffer-name)
    ;; need to set a filename, otherwise lsp in that buffer won't work
    (set-visited-file-name (format "/tmp/%s_%s_%s" buffer-name pid title))
    (set-buffer-modified-p nil)
    (markdown-mode)
    ;; major-mode change wipes buffer-locals; permanent-local property
    ;; should preserve ours, but re-set pid just in case
    (setq-local spacehammer--caller-pid pid)
    (evil-insert-state)))

;;;###autoload
(defun spacehammer-before-finish-edit-with-emacs-h (bufname pid)
  (with-current-buffer bufname
    (set-buffer-modified-p nil)))

;;;###autoload
(defun spacehammer--hs-eval-fennel (fennel-form)
  "Evaluates given Fennel form in Hammerspoon IPC.
See https://www.hammerspoon.org/docs/hs.ipc.html for more."
  (unless (eq system-type 'darwin)
    (user-error "This function works only in OSX!"))
  (unless (executable-find "hs")
    (user-error "Hammerspoon IPC executable - 'hs' cmd util not found."))
  (let ((fennel-form* (replace-regexp-in-string "\"" "\\\\\"" fennel-form)))
    (process-lines
     (executable-find "hs")
     "-c"
     (format
      "local fennel = require(\"fennel\"); print(fennel.eval(\" %s \"));"
      fennel-form*))))

;;;###autoload
(defun menu-bar-item-set-clock-or-pomodoro ()
  "Show org-clock-current-task or org-pomodoro state in OSX menubar item."
  (when (eq system-type 'darwin)
    (let* ((color (cond ((seq-contains '(:break :long-break :short-break)
                                       org-pomodoro-state) 'green)
                        ((or (eq :pomodoro org-pomodoro-state)
                             org-clock-current-task) 'red)))
           (icon (if (eq color 'green) "🟢" "🔴"))
           (text (cond ((< 40 (length org-clock-current-task))
                        (concat (seq-take org-clock-current-task 40) "..."))
                       (org-clock-current-task org-clock-current-task)
                       ((not (eq org-pomodoro-state :none))
                        (substring (symbol-name org-pomodoro-state) 1))))
           (full-txt (when text
                       (substring-no-properties
                        (concat
                         icon " "
                         (replace-regexp-in-string "\"" "'" text)))))
           (fnl (cond
                 ((null text)
                  "(: (. _G :org-clock-menubar-item) :setTitle \"\")")
                 ((eq color 'green)
                  (format
                   "(: (. _G :org-clock-menubar-item) :setTitle (hs.styledtext.new %S '{:color {:green 0.7 :blue 0.1}}))"
                   full-txt))
                 ((eq color 'red)
                  (format "(: (. _G :org-clock-menubar-item) :setTitle (hs.styledtext.new %S '{:color {:red 1}}))"
                          full-txt)))))
      ;; create menubar item object if none
      (unless (ignore-errors
                (spacehammer--hs-eval-fennel  ; make sure gl_org_clock_menubar_item is menubar object
                 "(~= nil  (. (. _G :org-clock-menubar-item) :title))"))
        (spacehammer--hs-eval-fennel "(tset _G :org-clock-menubar-item (hs.menubar.new))"))
      (spacehammer--hs-eval-fennel fnl))))
