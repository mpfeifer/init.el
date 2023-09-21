;;; host-settings.el --- List of host local settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Matthias Pfeifer

;; Author: Matthias Pfeifer <mpfeifer@ws-mpfeifer>
;; Keywords: configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar mpx-java-launcher nil "Location of java launcher.")
(defvar mpx-node-binary nil "Location of node binary.")
(defvar mpx-til-file nil "Location of _t_oday _i_ _l_earned file.")
(defvar mpx-org-agenda-file nil "Location of org agenda file.")
(defvar mpx-org-default-notes-file nil "Location of org default notes file.")
(defvar mpx-org-default-tasks-file nil "Location of org default tasks file.")
(defvar mpx-projectile-search-path nil "Projectile search path.")
(defvar mpx-org-plantuml-jar-path nil "Path to plantuml jar file.")

(provide 'host-settings)
;;; host-settings.el ends here
