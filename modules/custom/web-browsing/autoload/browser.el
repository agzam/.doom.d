;;; custom/web-browsing/autoload/browser.el -*- lexical-binding: t; -*-

(defun run-jxa (jxa-script)
  "Run given JXA-SCRIPT using osascript."
  (if (not (eq system-type 'darwin))
      (user-error "This function only works on Mac.")
    (process-lines
     (executable-find "osascript")
     "-l" "JavaScript"
     "-e" jxa-script)))

(defun browser-find-default ()
  ;; in both JXA and AppleScript, when you refer to an application, you're usually pointing directly to its location on
  ;; disk. For example, `Application('Safari')` works because there's an application named Safari.app in the
  ;; /Applications folder. So we really need to do some bullshit like this to find out the default browser app on Mac
  (unless (eq system-type 'darwin) (user-error "This function only works on Mac."))
  (unless (executable-find "jq") (user-error "jq is not found!"))
  (string-trim
   (shell-command-to-string
    (concat
     "osascript -e \"tell application \\\"Finder\\\" to get the name of application file id"
     "\\\"$(plutil -convert json -o - ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist"
     "| jq -r '.LSHandlers[] | select(.LSHandlerURLScheme==\"https\") | .LSHandlerRoleAll')\\\"\""))))

(defun browser-get-tabs ()
  "Using JXA reads browser tabs."
  (let* ((tabs-script
          (format
           "const browser = Application(\"%s\");
            let tabsInfo = [];
            const activeTabId = browser.windows()[0].activeTab.id();
            browser.windows().forEach((window, windowIndex) => {
            window.tabs().forEach((tab, tabIndex) => {
              let tabInfo = {
                windowIndex: windowIndex + 1,
                tabIndex: tabIndex + 1,
                url: tab.url(),
                title: tab.name(),
                active: activeTabId === tab.id() ? true : false
              };
              tabsInfo.push(tabInfo);
            });
          });
          JSON.stringify(tabsInfo);"
           (browser-find-default)))
         (res (run-jxa tabs-script))
         (json-object-type 'plist))
    (when res (json-read-from-string (car res)))))

(defun browser-activate-tab (window tab)
  "Activate Browser TAB for WINDOW."
  (let ((script
         (format
          "const Brave = Application('%s');
           const winIndex = %s;
           const tabIndex = %s;
           const win = Brave.windows()[winIndex - 1];
           if(win) {
           const tab = win.tabs()[tabIndex - 1];
           if(tab) {
           win.activeTabIndex = tabIndex;
           win.index = 1;
           Brave.activate();
           } else {
           'Tab index out of range';
           }
           } else {
           'Window index out of range';
           }"
          (browser-find-default)
          window tab)))
    (run-jxa script)))

(defun browser--goto-tab-completing-fn (coll)
  (lambda (s _ flag)
    (pcase flag
      ('metadata
       `(metadata
         (annotation-function
          ,@(lambda (x)
              (let* ((row (alist-get x coll nil nil #'string=))
                     (tabn (plist-get row :tabIndex))
                     (url (plist-get row :url)))

                (concat
                 "\n\t"
                 (propertize
                  (format "%s %s" tabn url)
                  'face 'completions-annotations)))))
         (display-sort-function ; keep rows sorted
          ,@(lambda (xs) xs))
         (category . url)))
      ('t
       (all-completions s coll)))))

;;;###autoload
(defun browser-goto-tab ()
  "Lets you select from a list of current tabs in the browser and
jump to selected tab, activating it in the browser."
  (interactive)
  (let* ((coll (seq-map
                (lambda (x)
                  (let* ((title (plist-get x :title))
                         (url (plist-get x :url))
                         (win (number-to-string (plist-get x :windowIndex)))
                         (tab (number-to-string (plist-get x :tabIndex)))
                         (row (concat
                               url "\t"
                               (propertize title 'invisible t)
                               "\t"
                               (propertize
                                (format "#window:%s #tab:%s" win tab)
                                'invisible t))))
                    (cons row x)))
                (browser-get-tabs)))
         (selected (thread-last
                     (browser--goto-tab-completing-fn coll)
                     (completing-read "Browser tab: ")))
         (_ (string-match "#window:\\([0-9]+\\).+#tab:\\([0-9]+\\)" selected))
         (winnum (match-string 1 selected))
         (tabnum (match-string 2 selected)))
    (browser-activate-tab winnum tabnum)))

(defun browser--get-active-tab ()
  (thread-last
    (browser-get-tabs)
    (seq-filter (lambda (tab) (eq t (plist-get tab :active))))
    (car-safe)))

;;;###autoload
(defun browser-insert-link-from-active-tab ()
  "Insert link to currently active tab in the browser."
  (interactive)
  (cond
   ((featurep :system 'linux)
    (browser-copy-tab-link)
    (print (car kill-ring))
    (cond
     ((eq major-mode 'org-mode)
      (org-cliplink))
     (t (insert (car kill-ring)))))
   ((featurep :system 'macos)
    (when-let* ((tab (browser--get-active-tab))
                (url (plist-get tab :url))
                (title (replace-regexp-in-string
                        "\\(^\\(([0-9]+)\\)\\s-*\\)" ""
                        (plist-get tab :title))))
      (cond
       ((eq major-mode 'markdown-mode)
        (insert (format "[%s](%s)" title url)))
       ((eq major-mode 'org-mode)
        (insert (format "[[%s][%s]]" url title)))
       (t (insert url)))))))

;;;###autoload
(defun add-roam-ref-for-active-tab ()
  "Adds a roam_ref property for current heading in org mode, using url from active browser tab."
  (interactive)
  (when-let* ((tab (browser--get-active-tab))
              (url (plist-get tab :url)))
    (org-id-get-create)
    (org-roam-ref-add url)))

;;;###autoload
(defun browser-create-roam-node-for-active-tab ()
  "Captures new roam node based on info from active tab."
  (interactive)
  (require 'org)
  (require 'org-roam)
  (when-let* ((tab (browser--get-active-tab))
              (title (plist-get tab :title))
              (url (plist-get tab :url))
              (base (replace-regexp-in-string
                     "^http[s]?://" "" url)))
    (if-let ((refs (seq-filter
                    (lambda (x)
                      (string-match-p base (car x)))
                    (org-roam-ref-read--completions))))
        (org-roam-ref-find base)
      (org-roam-capture-
       :node (org-roam-node-create
              :title title)
       :info (list :ref url)
       :goto t))))

(defun browser-get-default ()
  "Determine default browser class."
  (cond ((featurep :system 'linux)
         (if-let* ((xdg-setttins (executable-find "xdg-settings"))
                   (browser-class (thread-last
                                    (shell-command-to-string
                                     (concat xdg-setttins " get default-web-browser"))
                                    (replace-regexp-in-string "\\(\n\\|\\.desktop\\)" ""))))
             browser-class
           (user-error "Err. xdg-settings failed to determine the browser")))

        (t (user-error "This function is implemented only for Linux."))))

;;;###autoload
(defun browser-copy-tab-link ()
  "Yanks the url of the active browser tab into kill ring"
  (interactive)
  (cond
   ((featurep :system 'linux)
    (when-let ((browser-class (browser-get-default))
               (xdotool (executable-find "xdotool")))
      (let* ((script (format
                      (concat
                       "cur_win_id=$(%1$s getactivewindow);"
                       "%1$s search --onlyvisible --class %2$s search --role browser windowactivate "
                       "--sync key --clearmodifiers ctrl+l keyup ctrl+l; "
                       "%1$s key --clearmodifiers ctrl+c sleep 0.2 keyup ctrl+c keyup shift; "
                       "%1$s windowactivate $cur_win_id")
                      xdotool browser-class)))
        (with-temp-buffer
          (call-process "/bin/sh" nil (current-buffer) nil "-c" script)
          (when-let ((content (shell-command-to-string "xclip -o")))
            (kill-new content)
            content)))))
   ((featurep :system 'macos)
    (when-let* ((tab (browser--get-active-tab))
                (url (plist-get tab :url)))
      (message url)
      (kill-new url)))))
