;;; host-init.el --- Host specific initializations   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Matthias Pfeifer

;; Author: Matthias Pfeifer <mpfeifer@ws-mpfeifer>
;; Keywords: 

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

(setq
      tide-node-executable nil ;; TODO
      typescript-indent-level 4
      web-mode-markup-indent-offset 4
      org-agenda-files '("~/Dokumente/jira.org"))

(setq projectile-project-search-path '())

;;; host-init.el ends here
