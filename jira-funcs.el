(defun jira-create-ticket (project type summary description)
  (interactive 
   (let ((project-keys (jira-get-projects-for-completion))
         (types (jira-get-types-for-completion)))                    
     (list
      (completing-read "Project: " project-keys nil t)
      (completing-read "Type: " types nil t)
      (read-string "Summary: ")
      (read-string "Description: "))))
  (if (or (equal project "")
          (equal type "")
          (equal summary "")
          (equal description ""))
      (message "Must provide all information!")
    (progn
      (setq ticket-alist (list (cons "project" project) 
                               (cons "type" type) 
                               (cons "summary" summary) 
                               (cons "description" description)))
      (jira2-create-issue ticket-alist))))

(defun jira-refresh-ticket ()
  (interactive)
  (jira-show-issue jira-current-issue))

(defun jira-comment-ticket (comment)
  (interactive (list (read-string "Comment: ")))
  (if (equal comment "")
      (message "Must provide comment!")
    (progn
      (jira2-add-comment (car jira-current-issue) comment)
      (jira-refresh-ticket))))

(defun jira-assign-ticket (assignee)
  (interactive (list (read-string "Assignee: ")))
  (if (equal assignee "")
      (message "Must provide assignee!")
    (progn
      (setq ticket-alist (list (cons "assignee" (vector assignee))))
      (jira-update-issue jira-current-issue ticket-alist)
      (jira-refresh-ticket))))

(defun jira-update-ticket-summary (summary)
  (interactive (list (read-string "Summary: ")))
  (if (equal summary "")
      (message "Must provide summary!")
    (progn
      (setq ticket-alist (list (cons "summary" (vector summary))))
      (jira-update-issue jira-current-issue ticket-alist)
      (jira-refresh-ticket))))

(defun jira-start-ticket ()
  (interactive)
  (setq ticket-alist (list (cons "status" (vector "3"))))
  (jira-update-issue jira-current-issue ticket-alist))

(defun jira-get-project-name (key)
  (let ((projects (car jira-projects-list))
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc 'key project)) key)
          (setf name (cdr (assoc 'name project)))))
    name))

(defun jira-get-type-name (id)
  (let ((types (jira2-get-issue-types))
        (name nil))
    (dolist (type types)
      (if (and (not (eq name nil))
               (equal (car type) id))
          (setq name (cdr type))))
    name))

(defun jira-get-status-name (id)
  (let ((statuses (jira2-get-statuses))
        (name nil))
    (dolist (status statuses)
      (if (and (not (eq name nil))
               (equal (car status) id))
          (setq name (cdr status))))
    name))

(defun jira-get-priority-name (id)
  (let ((priorities (car jira-priorities))
        (name nil))
    (dolist (priority priorities)
      (if (equal (cdr (assoc 'id priority)) id)
          (setf name (cdr (assoc 'name priority)))))
    (message name)))

(defun jira-get-user-fullname (username)
  (if (assoc username jira-user-fullnames)
      (cdr (assoc username jira-user-fullnames))
    (let* ((user (car (jira-get-user username)))
           (fullname (cdr (assoc 'fullname user))))
      (setq jira-user-fullnames (append jira-user-fullnames (list (cons username fullname))))
      fullname)))

(defun jira-next-comment ()
  (interactive)
  (let ((p (point)))
    (if (search-forward "Comment #" nil t)
        (progn
          (if (equal p (- (point) 9))
              (search-forward "Comment #" nil t))
          (recenter 0)
          (beginning-of-line)))))

(defun jira-previous-comment ()
  (interactive)
  (if (search-backward "Comment #" nil t)
      (progn
        (recenter 0)
        (beginning-of-line))
    (goto-char 0)))

(defun jira-send-region-as-comment (start end issue-key)
  "Send the currently selected region as an issue comment"
  (interactive "r\nsIssue Key: ")
  (jira-add-comment issue-key (buffer-substring start end)))

(defun jira-get-filter (filter-id)
  "Returns a filter given its filter ID."
  (flet ((id-match (filter)
                   (equal filter-id (cdr (assoc 'id filter)))))
    (find-if 'id-match (car jira-filters))))

(defun jira-get-filter-alist ()
  "Returns an association list mapping filter names to IDs"
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (car jira-filters)))

(defun jira-get-longest-status-length ()
  (let ((len 0))
    (dolist (status (jira2-get-statuses))
      (if (> (length (cdr status)) len)
          (setq len (length (cdr status)))))
    len))

(defun jira-get-components-for-issue (issue)
  (if (cdr (assoc 'components issue)) 
      (cdr (assoc 'components issue)) 
    "None"))

(defun jira-get-fix-versions-for-issue (issue)
  (if (cdr (assoc 'fixVersions issue)) 
      (cdr (assoc 'name (car (cdr (assoc 'fixVersions issue)))))
    "None"))

(defun jira-get-projects-for-completion ()
  (mapcar (lambda (project)
            (cdr (assoc 'key project)))
          (car jira-projects-list)))

(defun jira-get-types-for-completion ()
  (mapcar (lambda (type)
            (cdr type))
          (jira2-get-issue-types)))

(provide 'jira-funcs)