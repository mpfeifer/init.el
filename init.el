;;; init.el --- Emacs initialization
;;; Commentary:
;;;
;;; Unordered pile of parenthesis
;;;
;;; Code:

;; (toggle-debug-on-error)

(defvar mpx-system-temp-dir "~/.emacs.d/temp" "Directory path for storing local system related files.")

(package-initialize)

(require 'use-package)

;; todo: move to ~/.emacs.d/custom/
(setq custom-file (format "~/.emacs.d/custom.el")
      package-enable-at-startup nil
      use-package-always-ensure t)

(load custom-file)

;; load system specific settings
;; todo: move to ~/.emacs.d/custom/
(load-file (format"~/.emacs.d/elisp/host-init-%s.el" (system-name)))

(defun package-list-packages-new-frame ()
  "Run package-list-packges in a newly created frame."
  (interactive)
  (select-frame (new-frame))
  (package-list-packages))

(use-package package
  :bind (("<f6>" . package-list-packages-new-frame))
  :ensure nil
  :config
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "https://elpa.gnu.org/packages/")
			   ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-archive-priorities '(("melpa" . 100)
				     ("gnu" . 50)
				     ("nongnu" . 25))))

(use-package benchmark-init
  :disabled
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package gcmh
  :custom
  (gc-cons-threshold (* 80 1024 1024))
  (gcmh-verbose t)
  :hook
  (emacs-startup . gcmh-mode))

(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-*" . er/contract-region)
         ("C-M-SPC" . er/expand-region))
  :defines (er/mark-html-tag-content er/mark-html-tag-content-with-tag er/mark-org-table-cell er--inside-org-table-p)
  :init
  (defun er--inside-org-table-p ()
    "Check via text properties. If 2nd element equals org-table assume
point is in org table."
    (eq
     (nth 1 (text-properties-at (point)))
     'org-table))

  (defun er/mark-html-tag-content ()
    "Mark the tag content"
    (interactive)
    (let ((point-pos (point)))
      (search-backward ">" (point-min) t)
      (forward-char 1)
      (set-mark (point))
      (goto-char point-pos)
      (search-forward "</" (point-max) t)
      (backward-char 2)
      (exchange-point-and-mark)))

  (defun er/mark-html-tag-content-with-tag ()
    "Mark the tag content and enclosing tag"
    (interactive)
    (let ((point-pos (point)))
      (search-backward "<" (point-min) t)
      (set-mark (point))
      (goto-char point-pos)
      (search-forward ">" (point-max) t)
      (exchange-point-and-mark)))

  (defun er/mark-org-table-cell ()
    "Mark the inside of a org table cell"
    (interactive)
    (let ((beginning-of-line-pos nil)
          (end-of-line-pos nil))
      (save-excursion
        (beginning-of-line)
        (setq beginning-of-line-pos (point))
        (end-of-line)
        (setq end-of-line-pos (point)))
      (when (er--inside-org-table-p)
        (search-backward "|" beginning-of-line-pos t)
        (forward-char 1)
        (set-mark (point))
        (search-forward "|" end-of-line-pos t)
        (backward-char 2)
        (exchange-point-and-mark)))))

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode +1))

(add-to-list 'load-path (format "%s/elisp" user-emacs-directory))

(use-package hl-line
  :disabled
  :ensure nil
  :config
  (global-hl-line-mode))

(defun indent-buffer ()
  "A helper function that is called after some templates are auto-inserted."
  (interactive)
  (indent-region (point-min) (point-max)) )

(use-package autoinsert
  :ensure nil
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/"
        auto-insert-query nil)
  (auto-insert-mode))

(use-package isearch
  :ensure nil
  :after (replace)
  :bind (:map isearch-mode-map
              ("C-o" . isearch-occur)
              ("C-." . mpx-isearch-yank-region-or-symbol-at-point)
              ("C-<" . isearch-beginning-of-buffer)
              ("C->" . isearch-end-of-buffer))

  :init
  (setq isearch-lazy-count t
        isearch-lazy-highlight t)

  (defun mpx-isearch-symbol-at-point ()
    "See function name."
    (interactive)
    (isearch-yank-string (symbol-to-string (symbol-at-point))))

  (defun isearch-occur ()
    "Invoke `occur' from within isearch."
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(use-package replace
  :ensure nil
  :hook ((occur-mode . hl-line-mode))
  :bind (("C-c o" . occur))
  :init
  (add-to-list 'display-buffer-alist
               '("*Occur*"
                 (display-buffer-at-bottom
                  display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.3)))
  (advice-add 'occur :after
              #'(lambda (origin &rest args)
                  (select-window (get-buffer-window "*Occur*"))
                  (goto-char (point-min))
                  )))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(use-package ffap
  :bind ("C-c f" . ffap)
  :ensure nil
  :after emacs)

(use-package emacs
  :defines(mp-find-config symbol-to-string string-to-symbol)
  :ensure nil
  :bind (("<f4>" . mp-find-config)
         ("M-Z" . zap-up-to-char))
  :hook ((before-save . delete-trailing-whitespace)
         (clone-indirect-buffer-hook . view-mode))
  :init
  (defun mp-find-config ()
    "Edit init.el"
    (interactive)
    (let ((pop-up-frames t))
      (select-frame (new-frame)))
    (find-file (format "%s/init.el" user-emacs-directory)))
  (global-unset-key (kbd "C-z"))
  (setq ring-bell-function 'ignore
        set-mark-command-repeat-pop t
        scroll-step 1
        scroll-conservatively 10000
        auto-window-vscroll nil
        delete-exited-processes t)
  (setq-default indent-tabs-mode nil)
  (put 'set-goal-column 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; handy alias to circumvent the not so intuitive emacs naming
  (defalias 'symbol-to-string 'symbol-name)
  (defalias 'string-to-symbol 'intern)
  (defalias 'yes-or-no-p 'y-or-n-p )
  (setq backup-directory-alist '(("." . "~/emacs-backups/"))
        backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backups
        delete-old-versions t  ; Automatically delete excess backups
        kept-new-versions 20   ; how many of the newest versions to keep
        kept-old-versions 5)   ; and how many of the old
  (setq-default indent-tabs-mode nil)
  (when window-system
    (progn
      (when (eq system-type 'windows-nt)
        (horizontal-scroll-bar-mode -1))
      (set-default 'frame-title-format '(:eval (format "%%b")))
      (tool-bar-mode -1)
      (menu-bar-mode 1)
      (tooltip-mode -1)
      (scroll-bar-mode -1)))
  ;; the mode-line
  (setq mode-line-modes nil)
  (display-time-mode)
  (line-number-mode)
  (column-number-mode))

(use-package ibuffer-git)

(use-package ibuffer-projectile)

(use-package ibuffer
  :after (all-the-icons-ibuffer ibuffer-git ibuffer-projectile)
  :bind (("C-x C-b" . ibuffer))
  :ensure nil
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :hook (ibuffer-mode . hl-line-mode)
  :init
  (defun mpx-ibuffer-collapse-all-filter-groups ()
    "Collapse all filter groups at once"
    (interactive)
    (setq ibuffer-hidden-filter-groups
          (mapcar #'car (ibuffer-current-filter-groups-with-position)))
    (ibuffer-update nil t))
  (advice-add #'ibuffer :after #'mpx-ibuffer-collapse-all-filter-groups)
  (add-to-list 'display-buffer-alist '("*Ibuffer*"  (display-buffer-same-window)))
  (defun mpx-setup-ibuffer ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (defun mpx-beginning-of-current-line()
    (move-beginning-of-line 1))
  (add-hook 'ibuffer-hook #'mpx-setup-ibuffer)
  (advice-add #'ibuffer-toggle-filter-group :before #'mpx-beginning-of-current-line)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (git-status-mini)
                " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                project-relative-file))))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file (format "%s/recentf" mpx-system-temp-dir))
  :init
  (recentf-mode))

(defun add-standard-display-buffer-entry (name)
  "Add an entry to display-buffer-alist for buffers called NAME."
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote name)
                 (display-buffer-at-bottom
                  display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.3))))

;; See https://github.com/jwiegley/use-package
;; Consider https://github.com/slotThe/vc-use-package
(require 'use-package)
(setq use-package-always-ensure t)

(use-package savehist
  :ensure nil
  :custom
  (savehist-file (format "%s/history" mpx-system-temp-dir))
  :init
  (savehist-mode))

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (defun company-mode-off ()
    (company-mode -1))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (local-set-key (kbd "C-c c") 'change-case))

(use-package typescript-mode
  :hook ((typescript-mode . setup-tide-mode))
  ;;         (before-save . tide-format-before-save))
  :custom
  (typescript-indent-level systems/typescript-indent-level)
  :config
  (setq-default typescript-indent-level 4))

;; this changes in emacs 29+
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after (dired)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :if (display-graphic-p))

(use-package all-the-icons-ivy-rich
  :if (display-graphic-p)
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-completion
  :if (display-graphic-p))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode))

(use-package avy
  :bind ("C-c j". avy-goto-word-1))

(use-package ivy
  :config
  (ivy-mode t))

(use-package server
  :ensure nil
  :config
  (setq server-use-tcp nil
        server-host "localhost"
        server-port 39246)
  (server-start)
  (add-hook 'server-visit-hook 'open-buffer-in-new-frame))

;; TODO formerly prog-mode need to be fiddled in into "prog modes"
;; electric-pair-mode
;; change-key
;; smartparens-mode
;; rainbox-delimiters
;; git-gutter-mode

(defun elisp-post-processor ()
  (interactive)
  (let ((match-found t))
    (progn
      (goto-char (point-min))
      (when (re-search-forward "_" nil t)
        (replace-match "") ) ) ) )

(defun re-seq (regexp string idx)
  "Get a list of all REGEXP matches in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string idx string) matches)
        (setq pos (match-end 0)))
      matches)))

(use-package sh-script
  :ensure nil
  :defines (shx-insert-variable shx-find-variables)
  :bind (:map sh-mode-map
              ("$" . 'shx-insert-variable))
  :hook (sh-mode . sh-mode-setup)
  :init

  (add-to-list 'auto-insert-alist
               '(".*\\.sh$" . [ "bash.sh" elisp-post-processor ] ) )

  (defun shx-find-variables ()
    "Extract list of variable names from shell script"
    (let ((undeclared (re-seq "^\\([0-9a-zA-Z_]+\\)=.*$" (buffer-substring-no-properties (point-min) (point-max)) 1))
          (declared (re-seq "^declare.*[[:space:]]\\([0-9a-zA-Z_]+\\)\\(=.*\\)?$" (buffer-substring-no-properties (point-min) (point-max)) 1))
          (forvars (re-seq "^[[:space:]]*for \\([0-9a-zA-Z_]+\\)[[:space:]]in[[:space:]].*$" (buffer-substring-no-properties (point-min) (point-max)) 1)))
      (delq nil (delete-dups (sort (append declared undeclared forvars) 'string<)))))

  (defconst shx-bash-builtin-variables (list "?" "BASH" "BASHOPTS" "BASHPID" "BASH_ALIASES" "BASH_ARGC" "BASH_ARGV" "BASH_CMDS" "BASH_COMMAND" "BASH_EXECUTION_STRING" "BASH_LINENO" "BASH_LOADABLES_PATH" "BASH_REMATCH" "BASH_SOURCE" "BASH_SUBSHELL" "BASH_VERSINFO" "BASH_VERSION" "COMP_CWORD" "COMP_KEY" "COMP_LINE" "COMP_POINT" "COMP_TYPE" "COMP_WORDBREAKS" "COMP_WORDS" "COPROC" "DIRSTACK" "EUID" "FUNCNAME" "GROUPS" "HISTCMD" "HOSTNAME" "HOSTTYPE" "LINENO" "MACHTYPE" "MAPFILE" "OLDPWD" "OPTARG" "OPTIND" "OSTYPE" "PIPESTATUS" "PPID" "PWD" "RANDOM" "READLINE_LINE" "READLINE_POINT" "REPLY" "SECONDS" "SHELLOPTS" "SHLVL" "UID" "BASH_COMPAT" "BASH_ENV" "BASH_XTRACEFD" "CDPATH" "CHILD_MAX" "COLUMNS" "COMPREPLY" "EMACS" "ENV" "EXECIGNORE" "FCEDIT" "FIGNORE" "FUNCNEST" "GLOBIGNORE" "HISTCONTROL" "HISTFILE" "HISTIGNORE" "HISTSIZE" "HISTTIMEFORMAT" "HOME" "HOSTFILE" "IFS" "IGNOREEOF" "INPUTRC" "LANG" "LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LC_NUMERIC" "LC_TIME" "LINES" "MAIL" "MAILCHECK" "MAILPATH" "OPTERR" "PATH" "POSIXLY_CORRECT" "PROMPT_COMMAND" "PROMPT_DIRTRIM" "PS0" "PS1" "PS2" "PS3" "PS4" "SHELL" "TIMEFORMAT" "TMOUT" "TMPDIR" "auto_resume" "histchars"))

  (defun shx-insert-variable ()
    "Ask user for variable (built-in variable + user defined ones). Does not respect
 variable scope."
    (interactive)
    (insert "${")
    (insert (completing-read "Variable? " (append
                                           (shx-find-variables)
                                           shx-bash-builtin-variables)))
    (insert "}"))

  (defun sh-mode-setup ()
    (interactive)
    (setq imenu-generic-expression
          (list
           '(nil "^\\(declare.*[[:space:]]\\)?\\([0-9a-zA-Z_]+\\)\\(=.*\\)?$" 2)
           '("functions" "^\\(function \\)?\\([a-z0-9A-Z_]+\\)() {$" 2)))
    (display-line-numbers-mode)
    (set-fill-column 120)))


(use-package ivy-avy)

(use-package vertico
  :init
  :disabled
  (vertico-mode))

(use-package counsel
  :disabled
  :bind (("M-x" . counsel-M-x)))

(use-package swiper
  :disabled
  :bind (("M-s" . counsel-grep-or-swiper)))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))

(use-package prettier-js
  ;; needs npm package
  :hook (js2-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "120"
                           )))


(use-package json-mode)

(use-package treemacs-all-the-icons)

(use-package treemacs
  :bind (("C-c t t" . treemacs)))

(use-package tree-sitter)

;; (use-package tree-sitter-langs)

(use-package lambda-line
  :ensure nil
  :after (lamda-themes))

(use-package solarized-theme)

(use-package calendar
  :ensure nil
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-date-style 'iso)
  (diary-file "~/Dokumente/diary"))

(use-package cal-dst
  :ensure nil
  :custom
  (calendar-daylight-time-zone-name "CEST")
  (calendar-standard-time-zone-name "CET"))

(use-package solar
  :ensure nil
  :custom
  (calendar-latitude 50.775555)
  (calendar-longitude 6.083611)
  (calendar-location-name "Aachen"))

(use-package theme-changer
  :after (solarized-theme solar)
  :defines (change-theme)
  :config
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  :config
  (change-theme 'solarized-light 'solarized-dark))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package smartparens)

(defun open-buffer-in-new-frame ()
  "Function to extend server-visit-hook.
If buffer was opened via emacsclient the buffer is opened in a new frame."
  (let ((buffer-from-emacsclient (current-buffer)))
    (when server-process
      (let ((pop-up-frames t))
        (select-frame (new-frame)))
      (set-window-buffer nil buffer-from-emacsclient))))

(defun imenu-add-to-menubar-0 ()
  "Add imenu to menubar with menu name \"Imenu\"."
  (imenu-add-to-menubar "Imenu"))

(defun mpx-abracadabra-el ()
  "Try to do what suitable for context of current point."
  (interactive)
  (let* ((thing (thing-at-point 'symbol t))
         (things-value (symbol-value (intern thing))))
    (when (listp things-value)
      (edit-list (intern thing)))))

(defun mpx-recompile-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (if (file-exists-p (byte-compile-dest-file buffer-file-name))
      (byte-compile-file buffer-file-name)))

(defun mpx-imenu-functions ()
  "Call imenu with `imenu-generic-expression` set for finding functions."
  (interactive)
  (let ((imenu-generic-expression
         '((nil "^.*([[:space:]]*defun[[:space:]]+\\([[:word:]-/]+\\)" 1))))
    (imenu-flush-cache)
    (call-interactively 'imenu)))

(defun mpx-imenu-use-package ()
  "Call imenu.
Have `imenu-generic-expression` set for finding use-pacakge declerations."
  (interactive)
  (let ((imenu-generic-expression
         '((nil "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1))))
    (imenu-flush-cache)
    (call-interactively 'imenu)))

(defun mpx-setup-emacs-lisp-mode ()
  "Setup Emacs Lisp mode from the."
  (company-mode +1)
  (flycheck-mode)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'mpx-recompile-elc-on-save))

(use-package imenu
  :ensure nil
  :custom (imenu-sort-function 'imenu--sort-by-name))

(use-package elisp-mode
  :after (imenu)
  :hook (emacs-lisp-mode . mpx-setup-emacs-lisp-mode)

  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . mpx-abracadabra-el))
  :config
  (local-set-key (kbd "C-c C-i f") 'mpx-imenu-functions)
  (local-set-key (kbd "C-c C-i u") 'mpx-imenu-use-package)
  :ensure nil)

(use-package projectile
  ;; C-c p p switch project
  ;; C-c p s g grep in project
  ;; C-c p ! run sync command in project root
  ;; C-c p T run tests
  ;; C-c p t toggle between file and test file
  ;; C-c p m projectile commander
  ;; C-c p c compile project
  ;; C-c p [45] f find file [in other window/frame]
  ;; C-c p [45] d find project dir [in other window/frame]
  :bind ("C-c p" . 'projectile-command-map)
  :custom
  (projectile-auto-discover nil)
  (projectile-completion-system 'ivy)
  (projectile-project-search-path systems/projectile-project-search-path)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (projectile-mode +1))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.02))

(use-package fringe
  :ensure nil
  :custom
  (fringe-mode (cons 16 0)))

(use-package git-gutter-fringe
  :custom
  (global-git-gutter-mode t))

(use-package ahk-mode)

(use-package web-mode
  :after expand-region
  :mode ("\\.html\\'")
  :custom
  (web-mode-markup-indent-offset systems/web-mode-markup-indent-offset)
  :config
  (add-hook 'web-mode-hook
            #'(lambda ()
                (add-to-list 'er/try-expand-list 'er/mark-html-tag-content-with-tag)
                (add-to-list 'er/try-expand-list 'er/mark-html-tag-content))))


(use-package web-beautify
  :bind (:map web-mode-map
              ("C-c b" . web-beautify-html)
              :map js2-mode-map
              ("C-c b" . web-beautify-js)))

(defun yas-expand-file-template (mode name)
  "Exapnd snippet template in MODE by its UUID"
  (yas-expand-snippet
   (yas-lookup-snippet name mode))
  (save-excursion
    (indent-region (point-min) (point-max))))

;; Yasnippet templates used in auto-insert mode
(use-package autoinsert
  :ensure nil
  :config
  (auto-insert-mode)
  (setq auto-insert-query nil)
  (define-auto-insert "\.java"
    '(lambda () (yas-expand-file-template 'java-mode "main_class"))))

(use-package linum
  :disabled
  :ensure nil
  :config
  (global-linum-mode))

(use-package org-ai
  :commands (org-ai-mode org-ai-global-mode)
  :init
  :disabled
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode))

(use-package org
  :hook (org-mode . company-mode-off)
  :defines (org-clock-goto org-clock-in)
  :bind (("<f12>" . 'org-agenda)
         ("<f10>" . 'org-clock-goto)
         ("C-<f10>" . 'org-clock-in)
         ("M-C-<f10>" . 'org-clock-out))
  :config

  (defun mpx-find-clocked-task-filename ()
    "Open file that has task with org clock running."
    (interactive)
    (org-clock-goto)
    (search-forward "** Bookmarks")
    (org-return))

  (setq org-default-notes-file systems/org-default-notes-file
        org-agenda-span 'day
        org-agenda-files `(,systems/org-agenda-file ,systems/org-projects-file ,systems/org-jira-file)
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)"))
        org-todo-keyword-faces '(("TODO" . "red")
                                 ("IN_PROGRESS" . "dark orange")
                                 ("WAITING" . "dark orange")
                                 ("DONE" . "sea green"))
        org-use-fast-todo-selection t
        org-tags-exclude-from-inheritance '("project")
        org-capture-templates
        `(("l" "TIL" entry (file+olp+datetree ,systems/org-til-file)
           "* %?\n%x%^g")
          ("T" "New task" entry ,systems/org-projects-file
           "* %?")
          ("t" "Task for clocked project" entry (clock)
           "** TODO %?")
          ("b" "Bookmark for clocked project (from x clipboard)" item (function mpx-find-clocked-task-filename) "%x%?")
          ("B" "Bookmark for clocked project (from region)" item (function mpx-find-clocked-task-filename) "%x%?")
          ("f" "Bookmark for clocked project (from visited file)" item (function mpx-find-clocked-task-filename) "%F%?"))
        org-clock-history-length 64
        org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution (quote when-no-clock-is-running)
        org-clock-report-include-clocking-task t
        org-startup-indented t
        org-log-done t
        org-confirm-babel-evaluate nil
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-image-actual-width '(700) ;; Set image width to 700
        org-src-tab-acts-natively t
        org-export-with-sub-superscripts nil
        org-startup-folded t)

  (defun mpx-org-capture-full-frame ()
    "Call org-capture using the full frame"
    (interactive "")
    (let ((display-buffer-alist '(("*Org Select*"
                                   (display-buffer-full-frame))
                                  )))
      (org-capture)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (plantuml . t)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package plantuml-mode
  :mode "\\.plantuml\\'")

(use-package markdown-mode
  :mode ("\\.md\\'"))

(use-package markdown-preview-mode)

(use-package edit-list
  :defines (edit-list)
  :after elisp-mode)

(use-package shell
  :bind (("<f5>" . shell))
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("*shell*"  (display-buffer-pop-up-window))))

(use-package graphviz-dot-mode)

(use-package windmove
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left))
  :config
  (setq windmove-wrap-around nil))

(use-package windows
  :ensure nil
  :bind
  (("<f2>" . new-frame)
   ("<f3>" . mpx-delete-frame)
   ("C-c w r" . mpx-rotate-windows)
   ("C-c w s" . mpx-swap-buffers)
   ("C-c w d" . mpx-detach-window))
  :init
  (defun new-frame ()
    (interactive)
    (let ((pop-up-frames t))
      (select-frame (make-frame)))))

(use-package vc
  :ensure nil
  :init
  (add-standard-display-buffer-entry "*vc-diff*"))

(use-package info
  :ensure nil
  :defines (mpx-new-infor-browser)
  :bind (("<f1>" . mpx-new-info-browser)
         ("C-h i" . mpx-new-info-browser))
  :init
  (defun mpx-new-info-browser ()
    "Open info browser in new frame."
    (interactive)
    (let ((pop-up-frames t))
      (select-frame (new-frame)))
    (info nil (generate-new-buffer-name "*info*"))))

(use-package scratchy
  :after (hydra)
  :ensure nil)

(use-package hydra)

(use-package help
  :ensure nil
  :init
  (add-standard-display-buffer-entry "*Help*"))

(use-package auto-mark
  :ensure nil
  :custom
  (auto-mark-command-class-alist
   '((goto-line . jump)
     (indent-for-tab-command . ignore)
     (undo . ignore)
     (tide-jump-to-definition . jump)))
  :config
  (global-auto-mark-mode 1))

(use-package eshell
  :ensure nil
  :defines (eshell-mode-map eshell-visual-options eshell-visual-subcommands)
  :custom
  (eshell-prompt-regexp "┗━━ \\$ ")
  (eshell-prompt-function (lambda nil
                            (concat "\n┏━ "
                                    (user-login-name) "@" (system-name) ":"
                                    (propertize (if (string= (eshell/pwd) (getenv "HOME"))
                                                    "~"
                                                  (replace-regexp-in-string
                                                   (concat "^" (getenv "HOME")) "~" (eshell/pwd)))
                                                'face `(:foreground "orange red"))
                                    "\n┗━━ "
                                    (if (= (user-uid) 0) "# " "$ "))))
  :init
  (declare-function eshell/pwd "pwd" ())

  (add-hook 'eshell-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook (lambda ()
			        (define-key eshell-mode-map (kbd "<tab>") 'company-complete)
			        (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
                                (add-to-list 'eshell-visual-subcommands '("vi"))
                                (add-to-list 'eshell-visual-subcommands '("less"))
			        (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))))

(use-package git-modes)

(use-package newcomment
  :bind (("C-/" . comment-region)
         ("C-M-/" . uncomment-region))
  :ensure nil)

(use-package change-case
  :ensure nil)

(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode))

(provide 'init)
;;; init.el ends here
