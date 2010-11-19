;;; jira.el --- Connect to JIRA issue tracking software

;; Copyright (C) 2009 Brian Zwahr
;; original Copyright (C) 2007  Dave Benjamin

;; Authors: 
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>
;; Version: 0.3.3
;; Last modified: October 12, 2009

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; **********
;;; Commentary
;;; **********

;; This file provides jira-mode, an emacs major mode for connecting to and 
;; using a Jira server. (http://www.atlassian.com/software/jira/). This 
;; jira-mode is far from complete (more below) but is mostly usable as is 
;; for the features that are present.

;; Note that some functions/processes can be a bit slow. I believe this has 
;; something to do with XMLRPC.

;; Also, XMLRPC access to jira is incomplete. Certain Jira features cannot be 
;; used via XMLRPC such as (but not limited to):
;; - Changing ticket status
;; - Closing/resolving tickets
;; - Watching a ticket

;; All of the XML-RPC API is wrapped, though not all of the API is exposed
;; via interactive functions. For API details, see:

;; http://confluence.atlassian.com/pages/viewpage.action?pageId=1035
;; http://www.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/xmlrpc/XmlRpcService.html

;;; *************
;;; Configuration
;;; *************

;; 1.) Load the file jira.el, either manuall or place (require 'jira) in your .emacs with jira.el in the load path.
;; 2.) Customize the variable jira-url to point to the XML-RPC url of the Jira
;; installation to be accessed.
;; 3.) The faces can be customized for different look/feel/highlighting.

;;; *****
;;; Usage
;;; *****

;; M-x jira-mode will load the major mode into a new buffer named *Jira*.
;; You will be asked to login; use the username/password for the Jira server.
;; A few internal lists should be populated automatically containing a list
;; of projects, issue types, etc. 

;; The following commands/keyboard shorcuts can be used:

;; li - jira-list-issues
;; lp - jira-list-projects
;; lf - jira-list-filters
;; si - jira-search-issues
;; sp - jira-search-project-issues
;; i - jira-show-issue
;; c - jira-create-ticket
;; o - jira-comment-ticket
;; r - jira-refresh-ticket
;; a - jira-assign-ticket
;; n - jira-next-comment
;; p - jira-previous-comment
;; jl - jira-login
;; jL - jira-logout
;; Q - jira-mode-quit

;; When viewing an issues, pressing o, r, etc. acts upon that issue. 
;; For instance, while viewing an issue, pressing o will ask for a comment. 
;; That comment will be posted to the issue currently being viewed.

;; Some prompts have tab completions in the minibuffer/echo area. Try it out to
;; see which prompts do and which do not.

;;; Code:
(require 'cl)
(require 'jira2)
(require 'jira-custom)
(require 'jira-calls)
(require 'jira-funcs)
(require 'jira-utils)

(defvar jira-mode-hook nil
  "Primary variable for the mode. t if active, nil otherwise.")

(defvar jira-mode-map nil
  "Keymapping for jira-mode.")

(defvar jira-current-issue nil
  "This holds the currently selected issue.")

(defvar jira-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira-priorities nil
  "This holds a list of priorities.")

(defvar jira-filters nil
  "This holds a list of user filters.")

(defvar jira-user-fullnames nil
  "This holds a list of user fullnames.")

(if jira-mode-map
    nil
  (progn
    (setq jira-mode-map (make-sparse-keymap))
    (define-key jira-mode-map "h" 'jira-home-screen)
    (define-key jira-mode-map "li" 'jira-list-issues)
    (define-key jira-mode-map "lp" 'jira-list-projects)
    (define-key jira-mode-map "lf" 'jira-list-filters)
    (define-key jira-mode-map "si" 'jira-search-issues)
    (define-key jira-mode-map "sp" 'jira-search-project-issues)
    (define-key jira-mode-map "i" 'jira-show-issue)
    (define-key jira-mode-map "c" 'jira-create-ticket)
    (define-key jira-mode-map "o" 'jira-comment-ticket)
    (define-key jira-mode-map "r" 'jira-refresh-ticket)
    (define-key jira-mode-map "a" 'jira-assign-ticket)
    (define-key jira-mode-map "n" 'jira-next-comment)
    (define-key jira-mode-map "p" 'jira-previous-comment)
    (define-key jira-mode-map "jl" 'jira-login)
    (define-key jira-mode-map "jL" 'jira-logout)
    (define-key jira-mode-map "Q" 'jira-mode-quit)
    (define-key jira-mode-map [return] 'jira-return)
    (define-key jira-mode-map [mouse-1] 'jira-return)))

(defmacro jira-with-jira-buffer (&rest body)
  "Sends all output and buffer modifications to *Jira* buffer."
  `(with-current-buffer "*Jira*" 
     (setq buffer-read-only nil)
     (delete-region (point-min) (point-max))
     (setq truncate-lines t)
     ,@body
     (delete-eob-whitespace)
     (setq buffer-read-only t)))

(defun jira-mode ()
  "A mode for working with the Jira ticketing system.

\\{jira-mode-map}"
  (interactive)
  (if (or (equal jira2-host-url nil)
          (equal jira2-host-url ""))
      (message "jira-url not set! Please use 'M-x customize-variable RET jira-url RET'!")
    (progn
      (switch-to-buffer "*Jira*")
      (setq buffer-read-only t)
      (kill-all-local-variables)
      (setq major-mode 'jira-mode)
      (setq mode-name "Jira")
      (use-local-map jira-mode-map)
      (run-hooks 'jira-mode-hook)
      (call-interactively 'jira-login)
      (if (eq nil jira-projects-list)
          (jira-store-projects))
      (if (eq nil jira-priorities)
          (jira-store-priorities))
      (jira2-get-statuses)
      (jira2-get-issue-types)
      (if (eq nil jira-filters)
          (jira-store-filters))
      (jira-home-screen)
      (message "jira-mode loaded!"))))

(defun jira-mode-quit ()
  (interactive)
  (jira-logout)
  (kill-buffer "*Jira*"))

(defun jira-home-screen ()
  (interactive)
  (jira-with-jira-buffer
   (insert "Welcome to jira-mode!
Place the cursor on any underlined link to execute that action. 
The letters in parenthesis are the keyboard shortcuts for that action.
\(Remember: ")
   (jira-describe-mode-link)
   (insert " will show you all the keybindings for current mode.\)\n\n")

   (insert "Find Issues:\n\n")
   (jira-filters-list-link)
   (goto-char (point-max))
   (insert "\n")
   (jira-projects-list-link)
   (goto-char (point-max))
   (insert "\n")
   (jira-issue-search-link)
   (goto-char (point-max))
   (insert "\n\n")

   (insert "Jira Mode Functions:\n\n")
   (jira-login-link)
   (goto-char (point-max))
   (insert "\n")
   (jira-logout-link)
   (goto-char (point-max))
   (insert "\n")
   (jira-quit-link)
   (goto-char (point-min))
   (forward-line 4)))

(defun jira-list-projects ()
  "Displays a list of all available JIRA projects"
  (interactive)
  (let ((projects (car jira-projects-list)))
    (jira-with-jira-buffer
     (insert (number-to-string (length projects)) " JIRA projects found:\n\n")
     (dolist (project projects)
       (insert (format "%-12s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert 
           (cdr (assoc 'key project)))
          (point))
        '(face jira-link-project-face))
       (beginning-of-line)
       (forward-char 12)
       (insert (format "%s\n"
                       (cdr (assoc 'name project)))))
     (goto-char (point-min))
     (forward-line 1))))

(defun jira-list-filters ()
  "Displays a list of all saved JIRA filters"
  (interactive)
  (let ((filters (car jira-filters)))
    (jira-with-jira-buffer
     (insert (number-to-string (length filters)) " JIRA filters found:\n\n")
     (dolist (filter filters)
       (insert (format "%-8s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert (cdr (assoc 'id filter)))
          (point))
        '(face jira-link-filter-face))
       (beginning-of-line)
       (forward-char 8)
       (insert (format " %s\n"
                       (cdr (assoc 'name filter)))))
     (goto-char (point-min))
     (forward-line 1))))

(defun jira-list-issues (filter-id)
  "Displays a list of issues matching a filter"
  (interactive
   (list (let ((filter-alist (jira-get-filter-alist)))
           (cdr (assoc (completing-read "Filter: " filter-alist nil t)
                filter-alist)))))
    (when filter-id
      (let ((filter (jira-get-filter filter-id))
            (issues (car (jira-get-issues-from-filter filter-id))))
        (jira-with-jira-buffer
         (insert "Filter:\n" (cdr (assoc 'name filter))
                 " (" (cdr (assoc 'id filter)) ")\n\n")
         (when (cdr (assoc 'description filter))
           (insert "Description:\n")
           (let ((start (point)))
             (insert (cdr (assoc 'description filter)) "\n\n")
             (fill-region start (point))))
         (jira-display-issues issues)))))

(defun jira-search-issues (text)
  "Displays a list of issues maching a fulltext search"
  (interactive "sSearch: ")
  (let ((issues (car (jira-get-issues-from-text-search text))))
    (jira-with-jira-buffer
     (insert "Search: " text "\n\n")
     (jira-display-issues issues))))

(defun jira-search-project-issues (project text max-results)
  "Displays a list of issues within a project matching a fulltext search"
  (interactive
   (let ((project-keys
          (mapcar (lambda (project)
                    (cdr (assoc 'key project)))
                  (car jira-projects-list))))
     (list
      (completing-read "Project Key: " project-keys nil t)
      (read-string "Search: ")
      (read-number "Max Results: " 20))))
  (let ((issues (jira-get-issues-from-text-search-with-project
                 (apply 'vector (list project)) (if (equal text "") " " text) max-results)))
    (jira-with-jira-buffer
     (insert "Project Key: " project "\n"
             "Search: " text "\n"
             "Max Results: " (number-to-string max-results) "\n\n")
     (jira-display-issues issues))))

(defun jira-show-issue (issue-key)
  "Displays details about a particular issue."
  (interactive "sIssue Key: ")
  (let ((issue (jira2-get-issue issue-key))
        (comments (jira2-get-comments issue-key)))
    (setq jira-current-issue issue-key)
    (jira-with-jira-buffer
     (setq truncate-lines nil)
     (jira-display-issue-info issue)
     (insert "\n")
     (jira-display-issue-summary issue)
     (insert "\n\n")
     (jira-display-issue-description issue)
     (insert "\n\n")
     (jira-display-comments comments)))
  (goto-char (point-min)))

(defun jira-display-issue-info (issue)
  (let ((issue-display-info 
         (list 
          (cons "Project:   " (jira-get-project-name (cdr (assoc 'project issue))))
          (cons "Key:       " (cdr (assoc 'key issue)))
          (cons "Type:      " (jira-get-type-name (cdr (assoc 'type issue))))
          (cons "Status:    " (jira-get-status-name (cdr (assoc 'status issue))))
          (cons "Priority:  " (jira-get-priority-name (cdr (assoc 'priority issue))))
          (cons "Assignee:  " (jira-get-user-fullname (cdr (assoc 'assignee issue))))
          (cons "Reporter:  " (jira-get-user-fullname (cdr (assoc 'reporter issue))))
          (cons "Created:   " (cdr (assoc 'created issue)))
          (cons "Updated:   " (cdr (assoc 'updated issue)))
          (cons "Watchers:  " "N/A")
          (cons "Component(s): " (jira-get-components-for-issue issue))
          (cons "Fix Version(s): " (jira-get-fix-versions-for-issue issue)))))
    (dolist (issue-info issue-display-info)
      (let ((s (car issue-info)))
        (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
        (insert s))
      (condition-case var
          (let ((s (cdr issue-info)))
            (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
            (insert s))
        ('error (insert "ERROR")))
      (insert "\n"))))

(defun jira-display-issue-summary (issue)
  (condition-case var
      (let ((s (cdr (assoc 'summary issue))))
        (put-text-property 0 (length s) 'face 'jira-issue-summary-face s)
        (insert s))
    ('error (insert "ERROR"))))
  
(defun jira-display-issue-description (issue)
  (condition-case var
      (if (cdr (assoc 'description issue))
          (insert (cdr (assoc 'description issue)))
        (insert "No description."))
    ('error (insert "ERROR"))))

(defun jira-display-comments (comments)
  (condition-case var
      (if comments
          (let ((count 1))
            (dolist (comment comments)
              (insert "Comment #" (int-to-string count) "\n")
              (let ((s (concatenate 'string (jira-get-user-fullname (cdr (assoc 'author comment))) 
                                    " - " 
                                    (cdr (assoc 'created comment)))))
                (put-text-property 0 (length s) 'face 'jira-comment-header-face s)
                (insert s "\n"))
              (let ((c (jira-strip-cr (cdr (assoc 'body comment)))))
                
                (put-text-property 0 (length c) 'face 'jira-comment-face c)
                (insert c "\n\n"))
              (setf count (1+ count))))
        (insert "No comments."))
    ('error (insert "ERROR"))))

(defun jira-display-issues (issues)
  "Inserts a list of issues into the current buffer"
  (let ((format-string (concat "%-15s %-10s %-" (number-to-string (jira-get-longest-status-length)) "s %-9s %s\n")))
    (insert (number-to-string (length issues)) " JIRA issues found:\n\n")
    (insert (format format-string "Issue Key:" "Assignee:" "Status:" "Priority:" "Summary:"))
    (insert "\n")
    (dolist (issue issues)
      (let ((status (cdr (assoc 'status issue)))
            (priority (cdr (assoc 'priority issue))))
        (insert (format format-string
                        " "
                        (cdr (assoc 'assignee issue))
                        (cdr (assoc status (jira2-get-statuses)))
                        (if priority
                            (make-string (- 6 (string-to-number priority))
                                         ?*)
                          "")
                        (cdr (assoc 'summary issue))))
        (forward-line -1)
        (beginning-of-line)
        (delete-char (length (cdr (assoc 'key issue))))
        (funcall 'jira-issue-link (cdr (assoc 'key issue)))
        (end-of-line)
        (newline)))))

; Links
(defun jira-make-link (link-text link-face)
  (let ((my-plist '(face nil)))
    (setq my-plist (plist-put my-plist 'face link-face))
    (insert link-text)
    (add-text-properties
     (save-excursion
       (backward-char (length link-text))
       (point))
     (point)
     my-plist)))

(defun jira-issue-link (link-text)
  (jira-make-link link-text 'jira-link-issue-face))

(defun jira-projects-list-link ()
  (jira-make-link "Projects List (l-p)" 'jira-link-project-list-face))

(defun jira-filters-list-link ()
  (jira-make-link "My Filters (l-f)" 'jira-link-filters-list-face))

(defun jira-issue-search-link ()
  (jira-make-link "Search Issues (s-i)" 'jira-link-search-issues-face))

(defun jira-login-link ()
  (jira-make-link "Login to Jira (j-l)" 'jira-link-login-face))

(defun jira-logout-link ()
  (jira-make-link "Logout of Jira (j-L)" 'jira-link-logout-face))

(defun jira-quit-link ()
  (jira-make-link "Quit jira-mode (Q)" 'jira-link-quit-face))

(defun jira-describe-mode-link ()
  (jira-make-link "ctrl-h m" 'jira-link-describe-mode-face))

; Storage functions
(defun jira-store-projects ()
  (setf jira-projects-list (jira-get-projects)))

(defun jira-store-priorities ()
  (setf jira-priorities (jira-get-priorities)))

(defun jira-store-filters ()
  (setf jira-filters (jira-get-saved-filters)))

(provide 'jira)
;;; jira.el ends here
