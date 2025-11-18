;;; project-discover.el --- Discover projects using project.el  -*- lexical-binding: t; -*-

;; Author: Sam Precious <samwdp@gmail.com>
;; Keywords: projects, convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1

;;; Commentary:

;; Discover projects on disk using project.el and populate the known
;; project list so they show up in `project-switch-project`.
;;
;; Configure where to scan:
;;
;;   (setq project-discover-directory-alist
;;         '(("c:/Users/sam" . 3)
;;           ("d:/"          . 6)))
;;
;; Then run manually:
;;
;;   M-x project-discover-run
;;
;; or automatically on startup:
;;
;;   (add-hook 'emacs-startup-hook #'project-discover-run)
;;
;; You can also call the main function with any alist:
;;
;;   (project-discover-projects
;;    '(("c:/dev" . 4)
;;      ("d:/work" . 3)))

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup project-discover nil
  "Discover projects in directories using project.el."
  :group 'project)

(defcustom project-discover-directory-alist nil
  "Alist of directories and depths to search for projects.

Each element is (DIR . DEPTH) where:
- DIR   is a directory name (string).
- DEPTH is a non-negative integer; 0 means only DIR itself is checked,
  1 means DIR and its immediate subdirs, etc.

Example:
  '((\"c:/Users/sam\" . 3)
    (\"d:/\"          . 6))"
  :type '(alist :key-type (directory :tag "Directory")
                :value-type (integer :tag "Depth")))

(defcustom project-discover-ignored-directories
  '(".git" ".hg" ".svn"
    "node_modules" ".cache" "build" "dist"
    "$RECYCLE.BIN" "System Volume Information" "Config.Msi")
  "Directory names (not full paths) to ignore while scanning."
  :type '(repeat string))

(defvar project-discover--found-count 0
  "Number of project roots detected in the current run.")

(defvar project-discover--added-count 0
  "Number of projects added to project--list in the current run.")

(defun project-discover--ignored-dir-p (dir)
  "Return non-nil if DIR (a full path) should be ignored."
  (let ((name (file-name-nondirectory (directory-file-name dir))))
    (member name project-discover-ignored-directories)))

(defun project-discover--project-at-dir (dir)
  "Return a project.el project object for DIR if any, else nil."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (when (file-readable-p default-directory)
      (with-temp-buffer
        (setq default-directory default-directory)
        (condition-case nil
            (project-current nil)
          (error nil))))))

(defun project-discover--project-root-p (dir)
  "Return non-nil if DIR is a project root according to project.el.
Also logs a message when a root is detected."
  (let ((proj (project-discover--project-at-dir dir)))
    (when (and proj
               (file-equal-p (project-root proj)
                             (file-name-as-directory (expand-file-name dir))))
      (setq project-discover--found-count (1+ project-discover--found-count))
      (message "project-discover: found project root %s" dir)
      proj)))

(defun project-discover--add-project (root)
  "Add ROOT to the known project list for project.el and log it."
  (setq root (file-name-as-directory (expand-file-name root)))
  (let* ((plist (bound-and-true-p project--list))
         (existing (cl-find-if (lambda (entry)
                                 (file-equal-p (car entry) root))
                               plist)))
    (if existing
        (message "project-discover: project already known %s" root)
      (push (list root) project--list)
      (setq project-discover--added-count (1+ project-discover--added-count))
      (message "project-discover: added project %s" root)
      (when (boundp 'project-list-file)
        (project--write-project-list)))))

(defun project-discover--directory-files (dir)
  "Return immediate subdirectories of DIR, skipping ignored/unreadable ones."
  (let* ((dir (file-name-as-directory dir))
         (all (condition-case err
                  (directory-files dir t "\\`[^.]") ; no . or ..
                (file-error
                 (message "project-discover: cannot read %s (%s)" dir (cadr err))
                 nil))))
    (cl-remove-if-not
     (lambda (d)
       (and (file-directory-p d)
            (file-readable-p d)
            (not (project-discover--ignored-dir-p d))))
     all)))

(defun project-discover--walk (dir depth)
  "Walk DIR recursively up to DEPTH and discover projects."
  (let* ((dir (file-name-as-directory (expand-file-name dir))))
    (message "project-discover: walk %s (depth %d)" dir depth)
    (when (and (file-directory-p dir)
               (file-readable-p dir)
               (not (project-discover--ignored-dir-p dir)))
      (message "project-discover: scanning %s (depth %d)" dir depth)
      (when (project-discover--project-root-p dir)
        (project-discover--add-project dir))
      (when (> depth 0)
        (dolist (sub (project-discover--directory-files dir))
          (project-discover--walk sub (1- depth)))))))

;;;###autoload
(defun project-discover-projects (directory-alist)
  "Discover projects in DIRECTORY-ALIST using project.el.

DIRECTORY-ALIST should be an alist of (DIR . DEPTH) pairs.

Example:
  (project-discover-projects
   '((\"c:/Users/sam\" . 3)
     (\"d:/\"          . 6)))"
  (interactive
   (list
    (let ((dir (read-directory-name "Directory to scan: "))
          (depth (read-number "Depth: " 3)))
      (list (cons dir depth)))))

  ;; Reset counters for this run
  (setq project-discover--found-count 0
        project-discover--added-count 0)

  (message "project-discover: starting scan...")
  (dolist (entry directory-alist)
    (let ((dir (car entry))
          (depth (cdr entry)))
      (when (and (stringp dir)
                 (integerp depth)
                 (>= depth 0))
        (message "project-discover: scanning root %s (depth %d)" dir depth)
        (project-discover--walk dir depth))))
  (message "project-discover: finished. Found %d project roots, added %d new projects."
           project-discover--found-count project-discover--added-count))

;;;###autoload
(defun project-discover-run ()
  "Discover projects using `project-discover-directory-alist'."
  (interactive)
  (when project-discover-directory-alist
    (project-discover-projects project-discover-directory-alist)))

(provide 'project-discover)

;;; project-discover.el ends here
