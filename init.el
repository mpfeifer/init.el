;;;;; Startup optimizations

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (expt 2 27))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;;;;; End of startup optimizations

(setq custom-file (format "~/.emacs.d/systems/%s/custom.el" (system-name)))

(load custom-file)

(package-initialize)

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode +1))

(add-to-list 'load-path (format "%s/elisp" user-emacs-directory))

(defun mp-find-config ()
  "Edit init.el"
  (interactive)
  (let ((pop-up-frames t))
    (select-frame (new-frame)))
  (find-file (format "%s/init.el" user-emacs-directory)))

(use-package hl-line
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


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(use-package emacs
  :ensure nil
  :bind (("<f4>" . mp-find-config)
         ("M-Z" . zap-up-to-char))
  :hook ((before-save-hook . delete-trailing-whitespace))
  :init
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
  (setq mode-line-modes nil)
  (add-to-list 'display-buffer-alist '("*Buffer List*"  (display-buffer-same-window))))

(use-package recentf
  :ensure nil
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

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities '(("melpa" . 100)
				   ("gnu" . 50)
				   ("nongnu" . 25)))

(defun new-frame ()
  (interactive)
  (let ((pop-up-frames t))
    (select-frame (make-frame))))

(global-set-key (kbd "<f2>") 'new-frame)
(global-set-key (kbd "<f3>") 'delete-frame)

(defun show-info-new-frame ()
  "Open info browser in new frame."
  (interactive)
  (let ((pop-up-frames t))
    (select-frame (new-frame)))
  (info nil (generate-new-buffer-name "*info*")))

(global-set-key (kbd "<f1>") 'show-info-new-frame)

;; See https://github.com/jwiegley/use-package
;; Consider https://github.com/slotThe/vc-use-package
(require 'use-package)

(setq use-package-always-ensure t)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  :config
  (setq company-idle-delay 0.3))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package typescript-mode
  :config
  (setq-default typescript-indent-level 4))

;; this changes in emacs 29+
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :if (display-graphic-p))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode))

(use-package avy
  :bind ("C-c j" . avy-goto-word-1))

(use-package expand-region
  :bind ("C-+" . er/expand-region))

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

(use-package prog-mode
  :hook ((prog-mode-hook .electric-pair-mode))
  :ensure nil)

(use-package fill-column-indicator
  :after (prog-mode)
  :hook ((prog-mode-hook . fci-mode))
  :init
  (setq fill-column 92))

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
  :bind (:map sh-mode-map
              ("$" . 'shx-insert-variable))
  :hook (sh-mode-hook . sh-mode-setup)
  :init

  (add-to-list 'auto-insert-alist
               '(".*\\.sh$" . [ "bash.sh" elisp-post-processor ] ) )

  (defun shx-find-variables ()
    "Extract list of variable names from shell script"
    (let ((undeclared (re-seq "^\\([0-9a-zA-Z_]+\\)=.*$" (buffer-substring-no-properties (point-min) (point-max)) 1))
          (declared (re-seq "^declare.*[[:space:]]\\([0-9a-zA-Z_]+\\)\\(=.*\\)?$" (buffer-substring-no-properties (point-min) (point-max)) 1)))
      (sort (append declared undeclared) 'string<)))

  (defconst shx-bash-builtin-variables (list "BASH" "BASHOPTS" "BASHPID" "BASH_ALIASES" "BASH_ARGC" "BASH_ARGV" "BASH_CMDS" "BASH_COMMAND" "BASH_EXECUTION_STRING" "BASH_LINENO" "BASH_LOADABLES_PATH" "BASH_REMATCH" "BASH_SOURCE" "BASH_SUBSHELL" "BASH_VERSINFO" "BASH_VERSION" "COMP_CWORD" "COMP_KEY" "COMP_LINE" "COMP_POINT" "COMP_TYPE" "COMP_WORDBREAKS" "COMP_WORDS" "COPROC" "DIRSTACK" "EUID" "FUNCNAME" "GROUPS" "HISTCMD" "HOSTNAME" "HOSTTYPE" "LINENO" "MACHTYPE" "MAPFILE" "OLDPWD" "OPTARG" "OPTIND" "OSTYPE" "PIPESTATUS" "PPID" "PWD" "RANDOM" "READLINE_LINE" "READLINE_POINT" "REPLY" "SECONDS" "SHELLOPTS" "SHLVL" "UID" "BASH_COMPAT" "BASH_ENV" "BASH_XTRACEFD" "CDPATH" "CHILD_MAX" "COLUMNS" "COMPREPLY" "EMACS" "ENV" "EXECIGNORE" "FCEDIT" "FIGNORE" "FUNCNEST" "GLOBIGNORE" "HISTCONTROL" "HISTFILE" "HISTIGNORE" "HISTSIZE" "HISTTIMEFORMAT" "HOME" "HOSTFILE" "IFS" "IGNOREEOF" "INPUTRC" "LANG" "LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LC_NUMERIC" "LC_TIME" "LINES" "MAIL" "MAILCHECK" "MAILPATH" "OPTERR" "PATH" "POSIXLY_CORRECT" "PROMPT_COMMAND" "PROMPT_DIRTRIM" "PS0" "PS1" "PS2" "PS3" "PS4" "SHELL" "TIMEFORMAT" "TMOUT" "TMPDIR" "auto_resume" "histchars"))

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
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "120"
                           ))
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package json-mode)

(use-package treemacs
  :bind (("C-c t t" . treemacs)))

(use-package tree-sitter)

;; (use-package tree-sitter-langs)

(use-package theme-changer
  :init
  (setq calendar-latitude 50.775555
        calendar-longitude 6.083611
        calendar-location-name "Aachen")
  (use-package material-theme)
  (use-package immaterial-theme)
  (use-package solarized-theme)
  :config
  (change-theme 'material-light 'material))

(use-package smartparens
  :after (prog-mode)
  :hook ((prog-mode-hook . smartparens-mode)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun open-buffer-in-new-frame ()
  "Function to extend server-visit-hook. If buffer was opened via
emacsclient the buffer is opened in a new frame."
  (let ((buffer-from-emacsclient (current-buffer)))
    (when server-process
      (let ((pop-up-frames t))
        (select-frame (new-frame)))
      (set-window-buffer nil buffer-from-emacsclient))))

(defun imenu-add-to-menubar-0 ()
  "Add imenu to menubar with menu name \"Imenu\""
  (imenu-add-to-menubar "Imenu"))

(defun mpx-abracadabra-el ()
  "Handle C-c C-c in emacs-lisp mode."
  (interactive)
  (let* ((thing (thing-at-point 'symbol t))
         (things-value (symbol-value (intern thing))))
    (when (listp things-value)
      (edit-list (intern thing)))))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . mpx-abracadabra-el))
  :ensure nil
  :init
  (defun recompile-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (byte-compile-dest-file buffer-file-name))
                    (byte-compile-file buffer-file-name)))))
  :config
  (company-mode)
  (add-hook 'emacs-lisp-mode-hook 'imenu-add-to-menubar-0)
  (add-hook 'emacs-lisp-mode-hook 'recompile-elc-on-save))

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  ;; C-c p p switch project
  ;; C-c p s g grep in project
  ;; C-c p ! run sync ommand in project root
  ;; C-c p T run tests
  ;; C-c p t toggle between file and test file
  ;; C-c p m projectile commander
  ;; C-c p c compile project
  ;; C-c p [45] f find file [in other window/frame]
  ;; C-c p [45] d find project dir [in other window/frame]
  :config
  (setq projectile-auto-discover nil)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (projectile-mode +1))


(use-package magit
  :bind ("C-c g" . magit-status))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package fringe
  :ensure nil
  :config
  (fringe-mode (cons 16 0)))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))

(use-package ahk-mode)

(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "focus/.*\\.html\\'")
          ("ctemplate" . "realtimecrm/.*\\.html\\'"))))

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
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (plantuml . t)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package markdown-mode
  :mode ("\\.md\\'"))

(use-package markdown-preview-mode)

(add-hook 'clone-indirect-buffer-hook 'view-mode)

(use-package edit-list
  :after elisp-mode)

(use-package shell
  :bind (("<f5>" . shell))
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("*shell*"  (display-buffer-pop-up-window))))

(use-package graphviz-dot-mode)

(use-package grep
  :ensure nil
  :config
  (defun setup-grep-mode ()
    (setq truncate-lines 't))
  (add-hook 'grep-mode-hook 'setup-grep-mode))

(use-package windmove
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left))
  :config
  (setq windmove-wrap-around nil))

(use-package windows ;; locally provided feature  required load-path
  :ensure nil
  :bind (("<f3>" . mpx-delete-frame)
         ("C-c w r" . mpx-rotate-windows)
         ("C-c w s" . mpx-swap-buffers)
         ("C-c w d" . mpx-detach-window)))

(use-package vc
  :ensure nil
  :init
  (add-standard-display-buffer-entry "*vc-diff*"))

(use-package info
  :ensure nil
  :bind (("C-h i" . mpx-open-info-browser))
  :init
  (defun mpx-open-info-browser()
    "Have info open new buffer on each invocation"
    (interactive)
    (select-frame (make-frame))
    (let* ((info-new-buffer-name (generate-new-buffer-name "*info*"))
           (info-buffer nil))
      (progn
        (setq info-buffer (get-buffer-create info-new-buffer-name))
        (info nil info-buffer)))))

(use-package help
  :ensure nil
  :init
  (add-standard-display-buffer-entry "*Help*"))

;; load system specific settings
(load-file (format"~/.emacs.d/systems/%s/host-init.el" (system-name)))

