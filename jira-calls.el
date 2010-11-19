(defun jira-login ()
  "Logs the user into JIRA."
  (interactive)
  (call-interactively 'jira2-login))

(defun jira-logout ()
  "Logs the user out of JIRA"
  (interactive)
  (jira2-call "logout")
  (setq jira2-token nil)
  (setq jira2-wsdl nil))

(defun jira-add-comment (issue-key comment)
  "Adds a comment to an issue"
  (jira2-call "addComment" issue-key comment))

(defun jira-create-issue (r-issue-struct)
  "Creates an issue in JIRA from a Hashtable object."
  (jira2-call "createIssue" r-issue-struct))

(defun jira-get-comments (issue-key)
  "Returns all comments associated with the issue"
  (jira2-call "getComments" issue-key))

(defun jira-get-components (project-key)
  "Returns all components available in the specified project"
  (jira2-call "getComponents" project-key))

(defun jira-get-issue (issue-key)
  "Gets an issue from a given issue key."
  (jira2-call "getIssue" issue-key))

(defun jira-get-issues-from-filter (filter-id)
  "Executes a saved filter"
  (jira2-call "getIssuesFromFilter" filter-id))

(defun jira-get-issues-from-text-search (search-terms)
  "Find issues using a free text search"
  (jira2-call "getIssuesFromTextSearch" search-terms))

(defun jira-get-issues-from-text-search-with-project
  (project-keys search-terms max-num-results)
  "Find issues using a free text search, limited to certain projects"
  (jira2-call "getIssuesFromTextSearchWithProject" project-keys search-terms max-num-results))

(defun jira-get-priorities ()
  "Returns all priorities in the system"
  (jira2-call "getPriorities"))

(defun jira-get-projects ()
  "Returns a list of projects available to the user"
  (jira2-call "getProjectsNoSchemes"))

(defun jira-get-resolutions ()
  "Returns all resolutions in the system"
  (jira2-call "getResolutions"))

(defun jira-get-saved-filters ()
  "Gets all saved filters available for the currently logged in user"
  (jira2-call "getSavedFilters"))

(defun jira-get-server-info ()
  "Returns the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jira2-call "getServerInfo"))

(defun jira-get-sub-task-issue-types ()
  "Returns all visible subtask issue types in the system"
  (jira2-call "getSubTaskIssueTypes"))

(defun jira-get-user (username)
  "Returns a user's information given a username"
  (jira2-call "getUser" username))

(defun jira-get-versions (project-key)
  "Returns all versions available in the specified project"
  (jira2-call "getVersions" project-key))

(defun jira-update-issue (issue-key field-values)
  "Updates an issue in JIRA from a Hashtable object."
  (jira2-call "updateIssue" issue-key field-values))

(provide 'jira-calls)