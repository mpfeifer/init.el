;;; custom.el --- Template for custom settings       -*- lexical-binding: t; -*-

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-frame-buffer-list nil)
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e" "5f7bbe37beb2238ed447dab41f400471e26e9e6edd41c418a15a75675872e4e2" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" default))
 '(desktop-path '("~/.emacs.d/desktop/"))
 '(desktop-restore-frames nil)
 '(help-window-select t)
 '(linum-format "%4d")
 '(org-confirm-babel-evaluate nil)
 '(org-plantuml-jar-path nil) ;; TODO
 '(package-selected-packages
   '(gcmh org moccur-edit color-moccur vertico vergico graphviz-dot-mode projectile org-babel edit-list plantuml-mode markdown-live-preview markdown-preview-mode markdown-mode org-beautify-theme org-ai company-mode avy diminish fringe web-beautify web-mode prettier-js git-gutter-fringe git-gutter magit elisp-mode ahk-mode solarized-theme expand-region counsel swiper benchmark-init rainbow-delimiters smartparens yasnippet-snippets yasnippet company js2-mode json-mode all-the-icons all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy-rich ivy ivy-avy tree-sitter tree-sitter-langs treemacs typescript-mode immaterial-theme material-theme theme-changer tide use-package))
 '(plantuml-java-command nil) ;; TODO
 '(safe-local-variable-values '((checkdoc-package-keywords-flag)))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(success ((t (:foreground "deep sky blue")))))

;;; custom.el ends here
