;;; ng2-shared.el --- Major modes for editing Angular 2

;; Copyright 2016-2019 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; URL: http://github.com/AdamNiederer/ng2-mode
;; Version: 0.2.3
;; Keywords: typescript angular angular2
;; Package-Requires: ()

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions to be added
;;  - TODO list all components in current project
;;    - TODO find project root
;;    - TODO find component files (files named "$\(.*\).component.ts")

;; The main features of the modes are syntax highlighting (enabled with
;; `font-lock-mode' or `global-font-lock-mode'), and easy switching
;; between templates and components.
;;
;; Exported names start with "ng2-"; private names start with
;; "ng2--".

;;; Code:

(defun ng2--find-project-root (some-project-directory)
  (let* ((cwd some-project-directory)
         (old-cwd nil)
         (project-root nil)
         (guard 20))
    (while (and (eq project-root nil)
                (not (eq old-cwd cwd))
                (> guard 0))
      (when (file-exists-p "package.json")
        (setq project-root cwd))
      (setq guard (- guard 1)
            old-cwd cwd
            cwd (cd "..")))
    (progn
      (setq default-directory some-project-directory)
      project-root)))

(defvar project-components '()) ;; make this buffer-local in some hook

(defun ng2--find-project-components(dir)
  (when (file-directory-p dir)
    (append
     (directory-files dir t ".component.ts$")
     (apply 'append (mapcar 'ng2--calculate-project-components (directory-files dir t "[^.]+"))))))

  (let* ((project-root (ng--find-project-root default-directory))
         (app-directory (format "%s/src/app" project-root))
         (guard 1000)
         (components nil)
         (todo (list app-directory)))
    (while (and todo
                (> guard 0))
      (let ((next-dir (car todo)))
        (setq todo (cdr todo))




(defun ng2-open-component-files-in-frame ()
  "Open all files belonging to some component (css, ts, html) in a single frame."
  (interactive)
  (let* ((component-name (replace-regexp-in-string "\.[^.]*$" "" (buffer-name)))
         (ts-file (format "%s.ts" component-name))
         (css-file (format "%s.css" component-name))
         (html-file (format "%s.html" component-name)))
    (select-frame (new-frame))
    (find-file html-file)
    (split-window-below)
    (find-file css-file)
    (split-window-horizontally)
    (find-file ts-file)))

(defun ng2--re-opt (&rest strs)
  "Optimize, group, and place symbol-ends around a regex matching STRS."
  (concat "\\_<\\(?:" (regexp-opt strs) "\\)\\_>"))

(defun ng2--counterpart-name (file)
  "Return the file name of FILE's counterpart, or FILE if there is no counterpart."
  (when (not (ng2--is-component file)) file)
  (let ((ext (file-name-extension file))
        (base (file-name-sans-extension file)))
    (if (equal ext "ts")
        (concat base ".html")
      (concat base ".ts"))))

(defun ng2--is-component (file)
  "Return whether FILE is a component file."
  (equal (file-name-extension (file-name-sans-extension file)) "component"))

(defun ng2-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (ng2--counterpart-name (buffer-file-name))))

(defun ng2-cycle-counterparts ()
  "Cycles through the corresponding component files (html, ts, css)."
  (interactive)
  (if (string-match "^.*\.component\..*$" (buffer-name))
      (let* ((extension (file-name-extension (buffer-name)))
             (next-file (format "%s.%s"
                                (file-name-sans-extension (buffer-name))
                                (cond
                                 ((string= extension "html") "ts")
                                 ((string= extension "ts") "css")
                                 ((string= extension "css") "html")))))
        (find-file next-file))
    (message "The current buffer does not seem to belong to some component file.")))

(provide 'ng2-shared)
