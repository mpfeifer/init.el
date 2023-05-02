;;; x-js2.el --- Extensions to js2 mode              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Matthias Pfeifer

;; Author: Matthias
;; Keywords: node.js package.js

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

(defun x-js2-find-package ()
  (interactive)
  (message (thing-at-point 'symbol t)))

(provide 'x-js2)
;;; x-js2.el ends here
