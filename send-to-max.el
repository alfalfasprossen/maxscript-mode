;;; send-to-max.el --- maxscript mode addition to allow sending of files/regions to max
;; -*- Mode:Emacs-Lisp -*-

;; Copyright (C) 2015 Johannes Becker

;; Author:  Johannes Becker <alfalfasprossen@gmail.com>
;; This file is NOT part of Emacs.

;; -----------------------------------------------------------------------------
;; This file will only take care of passing the region/buffer content to a python
;; script. See send_to_max.py
;; Inspired by etom.el and sublime3dsmax.
;;
;; How to use:
;;
;; (add-hook
;;  'maxscript-mode-hook
;;  (lambda ()
;;    (require 'send-to-max)
;;    (local-set-key [S-return] 'maxscript-send-line-or-region)
;;    (local-set-key (kbd "C-e") 'maxscript-send-file)
;;    (local-set-key (kbd "C-c C-c") 'maxscript-send-buffer)
;;    (local-set-key (kbd "C-c C-d") 'maxscript-clear-output)))
;;
;; (add-hook
;;  'python-mode-hook
;;  (lambda ()
;;    (require 'send-to-max)
;;    (local-set-key (kbd "C-c C-m C-r") 'maxscript-send-region-py)
;;    (local-set-key (kbd "C-c C-m C-e") 'maxscript-send-file)
;;    (local-set-key (kbd "C-c C-m C-c") 'maxscript-send-buffer-py)
;;    (local-set-key (kbd "C-c C-m C-d") 'maxscript-clear-listener)))
;;
;; -----------------------------------------------------------------------------
(require 'maxscript-mode)

(defconst maxscript/mode-base-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory this file resides in, used to call the python script with an absolute path.")

(defcustom maxscript-always-show-output t
  "Show the maxscript output buffer whenever sending a command."
  :type 'boolean
  :group 'maxscript)

(defcustom maxscript-always-clear-listener t
  "Clear the listener output in 3dsMax before sending a new command. This will make sure we only append the output that is new to our output buffer."
  :type 'boolean
  :group 'maxscript)

(defcustom maxscript-jump-lines t
  "Move point to the next line each time a single line has been sent."
  :type 'boolean
  :group 'maxscript)

(defun maxscript/send-command (cmd)
  "Send whatever pre-assembled command to the python script that will do the actual work."
  (let ((result ()))
    (setq result (shell-command-to-string
		  (concat "python " maxscript/mode-base-dir "/send_to_max.py "
			  cmd)))
    result))

(defun maxscript-send-line-or-region ()
  "Send the line at point if no region, send region if exists."
  (interactive)
  (if (use-region-p)
      (maxscript-send-region (region-beginning) (region-end))
    (progn
      (maxscript-send-region (point-at-bol) (point-at-eol))
      (if maxscript-jump-lines
	  (next-line)))))

(defun maxscript-send-region (start end &optional aspython) 
  "Send region to Max."
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end))
	(cmd ()))
    (if maxscript-always-clear-listener (maxscript-clear-listener))
    (setq cmd (concat (if aspython "-py " "-ms ") (shell-quote-argument content)))
    (maxscript/send-command cmd)
    (maxscript/fetch-output)
    (if maxscript-always-show-output (maxscript-show-output-buffer))))

(defun maxscript-send-region-py (start end)
  "Send region to Max as Python code."
  (interactive "r")
  (maxscript-send-region start end t))

(defun maxscript-send-buffer ()
  "Send whole buffer to Max."
  (interactive)
  (maxscript-send-region (point-min) (point-max)))

(defun maxscript-send-buffer-py ()
  "Send whole buffer to Max as Python code."
  (interactive)
  (maxscript-send-region (point-min) (point-max) t))

(defun maxscript-send-file ()
  "Send the current file to Max. The important difference to send-buffer is that the actual file of the buffer will be executed in max. This is important for relative imports etc, but also you should save the file before executing this, or modifications to the buffer will not be present in the file. "
  (interactive)
  (let ((cmd ()))
    (setq cmd (concat "-f " (buffer-file-name)))
    (maxscript/send-command cmd)
    (maxscript/fetch-output)
    (if maxscript-always-show-output (maxscript-show-output-buffer))))

;;; maxscript listener output related functions
(defvar maxscript/output-buffer "*mxs output*"
  "Name of buffer used to display Maxscript output.")

(defun maxscript/get-output-buffer ()
  "Return the output buffer, create it if necessary. Also, set maxscript-mode there as major-mode."
  (let ((buff (get-buffer-create maxscript/output-buffer)))
	(with-current-buffer buff
	  (maxscript-mode)
	  (maxscript/setup-compile)
	  (compilation-minor-mode))
    buff))

(defun maxscript-show-output-buffer ()
  (interactive)
  (display-buffer (maxscript/get-output-buffer)))

(defun maxscript-hide-output-buffer ()
  (interactive)
  (delete-windows-on (maxscript/get-output-buffer)))

(defun maxscript-clear-output ()
  "Clear the `maxscript/output-buffer' and the Maxscript-Listener in Max."
  (interactive)
  (with-current-buffer (get-buffer-create maxscript/output-buffer)
      (erase-buffer)
    )
  (maxscript-clear-listener))

(defun maxscript-clear-listener ()
  "Clear the Maxscript-Listener output in Max."
  (interactive)
  (let ((result (maxscript/send-command "-c")))
    (princ result)))

(defun maxscript/get-output ()
  "Get the response (listener output) from Max."
  (let ((result (maxscript/send-command "-g")))
    result))

(defun maxscript/fetch-output ()
  "Write the output from Max into the output buffer."
  (with-current-buffer (get-buffer-create maxscript/output-buffer)
    (insert-before-markers (maxscript/get-output))
    (goto-char (point-max))))

(require 'compile)
(defun maxscript/setup-compile ()
  (interactive)
  (set (make-local-variable 'compilation-error-regexp-alist)
       (list 'maxscript-compile-nogroup))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'maxscript-compile-nogroup
		    (list maxscript/error-pattern 1 2)))))

(provide 'send-to-max)

;;; send-to-max.el ends here
