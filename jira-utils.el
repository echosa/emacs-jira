(defun jira-return ()
  (interactive)
  (cond 
   ((equal (face-at-point) 'jira-link-login-face)
    (call-interactively 'jira-login))
   ((equal (face-at-point) 'jira-link-logout-face)
    (call-interactively 'jira-logout))
   ((equal (face-at-point) 'jira-link-quit-face)
    (jira-quit))
   ((equal (face-at-point) 'jira-link-project-list-face)
    (jira-list-projects))
   ((equal (face-at-point) 'jira-link-filters-list-face)
    (jira-list-filters))
   ((equal (face-at-point) 'jira-link-search-issues-face)
    (call-interactively 'jira-search-issues))
   ((equal (face-at-point) 'jira-link-issue-face)
    (jira-show-issue (thing-at-point 'sexp)))
   ((equal (face-at-point) 'jira-link-project-face)
    (jira-search-project-issues (thing-at-point 'sexp) "" 20))
   ((equal (face-at-point) 'jira-link-filter-face)
    (jira-list-issues (thing-at-point 'sexp)))
   ((equal (face-at-point) 'jira-link-describe-mode-face)
    (describe-mode))))

(defun point-on-issue-p ()
  (save-excursion
    (search-backward " ")))

(defun delete-eob-whitespace ()
  (save-excursion
    (goto-char (point-max))
    (delete-horizontal-space)))


(defun jira-strip-cr (string)
  "Removes carriage returns from a string"
  (when string (replace-regexp-in-string "\r" "" string)))

(provide 'jira-utils)