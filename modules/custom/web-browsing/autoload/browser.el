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
  (if (not (eq system-type 'darwin))
      (user-error "This function only works on Mac.")
    (string-trim
     (shell-command-to-string
      (concat "osascript -e \"tell application \\\"Finder\\\" to get the name of application file id"
              "\\\"$(plutil -convert json -o - ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist"
              "| jq -r '.LSHandlers[] | select(.LSHandlerURLScheme==\"https\") | .LSHandlerRoleAll')\\\"\"")))))

(defun browser-get-tabs ()
  "Using JXA reads browser tabs."
  (let* ((tabs-script
          (format
           "const browser = Application(\"%s\");
            let tabsInfo = [];
            browser.windows().forEach((window, windowIndex) => {
            window.tabs().forEach((tab, tabIndex) => {
              let tabInfo = {
                windowIndex: windowIndex + 1,
                tabIndex: tabIndex + 1,
                url: tab.url(),
                title: tab.name()
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
