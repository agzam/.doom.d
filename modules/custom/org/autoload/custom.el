;;; custom/org/autoload/custom.el -*- lexical-binding: t; -*-

;;;###autoload
(defun org-roam--link-to (node-title-or-id)
  "For a given NODE-TITLE-OR-ID tries to find the node and returns
Org-link text to the node."
  (when-let* ((nodes (org-roam-db-query
                      "select n.id, n.title from nodes n
                       left join aliases a
                       on n.id = a.node_id
                       where n.id = $s1
                         or title = $s1 collate nocase
                         or a.alias = $s1 collate nocase"
                      node-title-or-id))
              (node (cl-first nodes)))
    (apply 'format "[[id:%s][%s]]" node)))

;;;###autoload
(defun org--insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    (format "#+begin_quote\n%s\n#+end_quote" selection)))

;;;###autoload
(defun org-roam-get-current-capture-buffer ()
  "Finds current capture buffer based on `org-capture-plist'"
  (let* ((fname (lambda (ptrn-str)
                  (replace-regexp-in-string
                   "%<.*>" (lambda (s)
                             (format-time-string
                              (replace-regexp-in-string "%<\\|>" "" s)))
                   ptrn-str)))
         (buf (or (org-capture-get :buffer)
                  (find-file-noselect
                   (funcall fname (cadr (plist-get (org-capture-get :org-roam) :if-new)))
                   :no-warn))))
    buf))

;;;###autoload
(defun org-roam-capture-dailies--set-node-props (node-link)
  "It's to be used with org-roam-capture and specifically for
dailies. When called from a capture template it inserts title and
id, then finds the datetree entry and inserts link to a
NODE-LINK  - which is title or id or a node."
  (when-let* ((buf (org-roam-get-current-capture-buffer)))
    (with-current-buffer buf
      ;; insert ID (for new filenodes)
      (save-excursion
        (goto-char (point-min))
        (unless (org-entry-get (point) "ID")
          (insert (format ":PROPERTIES:\n:ID:  %s\n:END:\n" (org-id-new)))))

      ;; insert the title and link to the `NODE-LINK'
      (save-excursion
        (goto-char (point-min))
        (unless (search-forward "#+title:" nil :no-error)
          (goto-char (point-min))
          (search-forward ":END:" nil :no-error)
          (forward-line)
          (insert (format "#+title: %s %s notes\n"
                          (format-time-string
                           "%B %Y"
                           (org-capture-get :default-time))
                          (org-capture-get :description)))
          (insert (org-roam--link-to node-link))))

      ;; always append stuff to the day in the daytree
      ;; without this, it would create new heading at the top
      ;; (goto-char (org-element-property :begin (org-element-property :parent (org-element-at-point))))
      ;; (org-end-of-subtree)

      ;; append properties drawer to the heading, not the other way around
      (search-backward ":PROPERTIES:")

      ;; it needs to return a string
      "")))

;;;###autoload
(cl-defun org-roam-node-insert+ (&optional lines-before lines-after &key templates info)
  "Improved org-roam-node-insert that additionally also removes conflicting and
duplicating links around the context.  If a node has `collides_with:' property,
inserting a link to that node would remove any links to nodes with IDs contained
in that prop."
  (interactive)
  (require 'a)
  (atomic-change-group
    (let* (region-text
           beg end
           (_ (when (region-active-p)
                (setq beg (set-marker (make-marker) (region-beginning)))
                (setq end (set-marker (make-marker) (region-end)))
                (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
           (node-to-insert (org-roam-node-read region-text))   ; first we choose a node to insert
           (description (or region-text (org-roam-node-title node-to-insert)))
           ;; then we find the IDs of nodes, links that need to be removed if that link to be inserted
           (id+collides (seq-remove
                         'string-empty-p
                         (-> node-to-insert
                             (org-roam-node-properties)
                             (a-get "COLLIDES_WITH")
                             (concat " " (org-roam-node-id node-to-insert))
                             (split-string " "))))
           (re (concat "\\(\\[\\[\\)\\(id:\\|roam:\\)"
                       "\\(" (mapconcat 'identity id+collides "\\|")
                       "\\)\\]\\[\\w*\\]\\]"))
           (before (or lines-before 0))
           (after (or lines-after 0)))
      (save-mark-and-excursion
        (unless region-text
          (setq prev-pos (point))
          (forward-line (- before))
          (beginning-of-line)
          (set-mark (point))
          (forward-line (+ before after))
          (end-of-line))
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (if (org-roam-node-id node-to-insert)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (unless region-text
                  (goto-char 0)
                  (while (re-search-forward re nil :no-error)
                    (replace-match ""))
                  (goto-char prev-pos))
                (makunbound 'prev-pos)
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node-to-insert))
                         description)))
            (org-roam-capture-
             :node node-to-insert
             :info info
             :templates templates
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link
                           :immediate-finish t
                           :jump-to-captured nil))))
          ;; make sure links always separated by a single space
          (goto-char 0)
          (while (re-search-forward "\\]\\]\\[\\[" nil :no-error)
            (replace-match "]] [[")))))))

;;;###autoload
(defun org-roam-toggle-ui-xwidget ()
  (interactive)
  (let* ((url (concat "http://localhost:" (number-to-string org-roam-ui-port)))
         (buf (or (xwidget-webkit-get-url-buffer
                   (concat "localhost:" (number-to-string org-roam-ui-port)))
                  (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
    (if-let ((win (get-buffer-window buf)))
        (delete-window win)
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun get-gh-item-title (uri)
  "Based on given GitHub URI for pull-request or issue,
  return the title of that pull-request or issue."
  (cond
   (;; either PR or issue
    (string-match "\\(github.com\\).*\\(issues\\|pull\\)" uri)
    (pcase-let*
        ((`(_ _ ,owner ,repo ,type ,number) (remove "" (split-string uri "/")))
         (gh-resource (format "/repos/%s/%s/%s/%s"
                              owner
                              repo
                              (if (string= type "pull") "pulls" type)
                              number))
         (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s#%s — %s" owner repo number .title)))))

   ;; just a link to a repo
   ((string-match "\\(github.com\\)/[[:graph:]]+/[[:graph:]]+$" uri)
    (pcase-let* ((uri* (->> (split-string uri "/\\|\\?")
                            (remove "")
                            (-non-nil)))
                 (`(_ _ ,owner ,repo) uri*)
                 (gh-resource (format "/repos/%s/%s" owner repo))
                 (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s — %s" owner repo .description)))))

   (;; link to a file in a branch
    (string-match "\\(github.com\\).*" uri)
    (pcase-let* ((uri* (->> (split-string uri "/\\|\\?")
                            (remove "")
                            (-non-nil)))
                 (`(_ _ ,owner ,repo ,type ,branch ,dir ,file) uri*)
                 (branch (if (or (string= type "commit") (string= type "tree"))
                             (substring branch 0 7)  ; trim to shorten sha
                           branch)))
      (mapconcat
       'identity (->> (list owner repo type branch dir file) (-non-nil))
       "/")))
   (t uri)))

;;;###autoload
(defun +org-link-make-description-function (link desc)
  (cond ((not (s-blank? desc)) desc)
        ((string-match "\\(github.com\\).*" link)
         (get-gh-item-title link))
        (t desc)))

(defun org-link-parse (link)
  ;; borrowed and adopted from:
  ;; github.com/xuchunyang/emacs.d/blob/5f4f873cf7a671a36f686f3d1346fd7c5a5462bc/lisp/chunyang-misc.el#L488-L526
  (if (or (string-match (rx "[[" (group (0+ anything)) "][" (group (0+ anything)) "]]") link)
          (string-match (rx "[[" (group (0+ anything)) "]]") link)
          (string-match (rx  (group (0+ anything))) link))
      (seq-remove #'null
                  (list (match-string 1 link)
                        (match-string 2 link)))
    (error "Cannot parse %s as Org link" link)))


;;;###autoload
(defun org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

;;;###autoload
(defun edit-indirect-guess-mode-fn+ (parent-buffer beg _end)
  "Guess the major mode for an edit-indirect buffer."
  (let* ((type (with-current-buffer parent-buffer
                 (cond
                  ;; set markdown-mode for quote & verse blocks
                  ((and (eq major-mode 'org-mode)
                        (when-let ((s (save-mark-and-excursion
                                        (goto-char (- beg 1))
                                        (thing-at-point 'symbol))))
                          (string-match-p
                           "+begin_quote\\|+begin_verse" s)))
                   :quote)
                  ((and (eq major-mode 'org-mode)
                        (when-let ((s (save-mark-and-excursion
                                        (goto-char (- beg 1))
                                        (backward-word)
                                        (thing-at-point 'word))))
                          (string-match-p "results" s)))
                   :results-drawer)

                  ((eq major-mode 'org-mode) :org-mode)))))
    (cl-case type
      (:quote (markdown-mode))
      (:results-drawer (json-mode))
      (:org-mode (org-mode))
      (t (normal-mode)))))

;;;###autoload
(defun +org-goto-bottommost-heading (&optional maxlevel)
  "Go to the last heading in the current subtree."
  (interactive "P")
  (if (listp maxlevel)
      (setq maxlevel 4)
    (unless maxlevel (setq maxlevel 3)))
  (setq currlevel 1)
  (while (<= currlevel maxlevel)
    (org-next-visible-heading 1)
    (if (not (org-at-heading-p))
        (progn
          (org-previous-visible-heading 1)
          (org-cycle)
          (setq currlevel (1+ currlevel))))))

;;;###autoload
(defun +org-goto-datetree-date (&optional date)
  "Jump to selected date heading in the datetree."
  (interactive)
  (save-restriction
    (let* ((datetree-date (org-read-date))
           (dt (org-date-to-gregorian datetree-date)))
      (org-datetree-find-date-create dt t)
      (org-show-hidden-entry)
      (show-subtree))))

;;;###autoload
(defun +person-w-name-based-id ()
  "Returns a person record with name-based id. To be used in capture template."
  (interactive)
  (let* ((name-parts (thread-last
                       (gui-get-selection 'CLIPBOARD)
                       (read-from-minibuffer "Name: ")
                       (split-string)
                       (seq-map 's-trim)
                       (seq-map 'capitalize)))
         (id (mapconcat 'downcase (reverse name-parts) "-"))
         (name (format "%s %s" (car name-parts)
                       (substring (cadr name-parts) 0 1)))
         (full-name (mapconcat 'identity name-parts " ")))
    (format "%s\n:PROPERTIES:\n:ID: %s\n:roam_aliases: \"%s\"\n:END:" name id full-name)))


;;;###autoload
(defun org-roam-capture-dont-create-id-a (org-roam-capture-fn time &optional goto keys)
  "Skip the automatic ID creation by hijacking org-roam-dailies--capture.

I like to keep dailies a file/per month with a datetree. Org-roam automatically
generates IDs per each day, and I don't need that. A day heading by itself doesn't
carry a meaningful context (to which I have to extend a relation) for me."
  (let ((lexical-binding t))
    (cl-letf* ((org-entry-put-orig (symbol-function 'org-entry-put))
               ((symbol-function 'org-entry-put)
                (if goto
                    (lambda (pom property value)
                      (run-with-timer
                       0.01 nil
                       (lambda (pom property value org-entry-put-orig)
                         (if (buffer-modified-p)
                             (progn
                               (funcall org-entry-put-orig pom property value)
                               (goto-char pom)
                               (re-search-forward org-property-drawer-re)
                               (org-fold-reveal :siblings)
                               (org-fold-show-subtree)
                               (org-fold-show-context)
                               (insert "\n")
                               (evil-insert 1))
                           (progn
                             (goto-char pom)
                             (forward-line)
                             (org-fold-reveal :siblings)
                             (org-fold-show-subtree)
                             (org-fold-show-context)
                             (recenter))))
                       pom property value org-entry-put-orig))
                  org-entry-put-orig)))
      (apply org-roam-capture-fn time goto keys))))
