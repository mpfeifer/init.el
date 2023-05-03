;; Replace scratch buffer by mode-specific scratch buffers

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(defvar scratchy-map (make-hash-table)
  "This one maps modes to buffers. One buffer for each mode.")

(defun scratchy-update-map (map)
  "Actually unused."
  (let ((updated-list (make-hash-table)))
    (progn
      (dolist (mode (map-keys map))
        (let ((buffer (gethash mode map)))
          (when (bufferp buffer)
            (puthash mode buffer map))))
      updated-list)))

(defun scratchy-goto-mode (mode)
  "Get the scratchy buffer for MODE and display it in a newly created window in a
 newly created frame."
  (interactive)
  (let ((scratchy-buffer (gethash mode scratchy-map)))
    (when (or
           (not (buffer-live-p scratchy-buffer))
           (null scratchy-buffer))
      (progn
        (setq scratchy-buffer (generate-new-buffer (format "*scratchy-%s*" (symbol-to-string mode))))
        (with-current-buffer scratchy-buffer
          (funcall mode))
        (puthash mode scratchy-buffer scratchy-map)))
    (select-frame (make-frame))
    (switch-to-buffer scratchy-buffer)))

(defun scratchy-goto-text-mode ()
  (interactive)
  (scratchy-goto-mode 'text-mode))

(defun scratchy-goto-emacs-lisp-mode ()
  (interactive)
  (scratchy-goto-mode 'emacs-lisp-mode))

(defun scratchy-goto-python-mode ()
  (interactive)
  (scratchy-goto-mode 'python-mode))

(defun scratchy-goto-java-mode ()
  (interactive)
  (scratchy-goto-mode 'java-mode))

(defun scratchy-goto-org-mode ()
  (interactive)
  (scratchy-goto-mode 'org-mode))

(defun scratchy-goto-sh-mode ()
  (interactive)
  (scratchy-goto-mode 'sh-mode))

(defun scratchy-goto-json-mode ()
  (interactive)
  (scratchy-goto-mode 'json-mode))

(defhydra hydra-global-scratchy (:color blue :hint nil)
  "
Mode^^
----------------------------------------------
_t_ext-mode    _p_ython-mode        _o_rg-mode
_J_ava-mode    emacs-_l_isp-mode    _s_h-mode
_j_son-mode
"
  ("t" scratchy-goto-text-mode)
  ("l" scratchy-goto-emacs-lisp-mode)
  ("p" scratchy-goto-python-mode)
  ("J" scratchy-goto-java-mode)
  ("o" scratchy-goto-org-mode)
  ("s" scratchy-goto-sh-mode)
  ("j" scratchy-goto-json-mode))

(global-set-key (kbd "C-c s") 'hydra-global-scratchy/body)

(provide 'scratchy)
