(defgroup jira nil
  "Jira customization group."
  :group 'applications)

(defgroup jira-faces nil 
  "Faces for displaying Jira information."
  :group 'jira)

(defface jira-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira-faces)

(defface jira-issue-info-header-face
  '((t (:bold t :inherit 'jira-issue-info-face)))
  "Base face for issue headers."
  :group 'jira-faces)

(defface jira-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira-faces)

(defface jira-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira-faces)

(defface jira-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira-faces)

(defface jira-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira-faces)

(defface jira-link-project-face
  '((t (:underline t)))
  "Face for linked projects."
  :group 'jira-faces)

(defface jira-link-filter-face
  '((t (:underline t)))
  "Face for linked filters."
  :group 'jira-faces)

(defface jira-link-project-list-face
  '((t (:underline t)))
  "Face for project list link."
  :group 'jira-faces)

(defface jira-link-filters-list-face
  '((t (:underline t)))
  "Face for filters list link."
  :group 'jira-faces)

(defface jira-link-search-issues-face
  '((t (:underline t)))
  "Face for issue search link."
  :group 'jira-faces)

(defface jira-link-login-face
  '((t (:underline t)))
  "Face for login link."
  :group 'jira-faces)

(defface jira-link-logout-face
  '((t (:underline t)))
  "Face for logout link."
  :group 'jira-faces)

(defface jira-link-quit-face
  '((t (:underline t)))
  "Face for quit link."
  :group 'jira-faces)

(defface jira-link-describe-mode-face
  '((t (:underline t)))
  "Face for describe-mode link."
  :group 'jira-faces)

(provide 'jira-custom)