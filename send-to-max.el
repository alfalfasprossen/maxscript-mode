;;; send-to-max.el --- maxscript mode addition to allow sending of files/regions to max
;; -*- Mode:Emacs-Lisp -*-

;; Copyright (C) 2015 Johannes Becker

;; Author:  Johannes Becker <alfalfasprossen@gmail.com>
;; This file is NOT part of Emacs.

;; -----------------------------------------------------------------------------
;; This file will only take care of passing the region/buffer content to a python
;; script. See send-to-max.py
;; Inspired by etom.el and sublime3dsmax.
;;
;; How to use:
;;
;; (add-hook
;;  'maxscript-mode-hook
;;  (lambda ()
;;    (require 'send-to-max)
;;    (local-set-key (kbd "C-c C-r") 'maxscript-send-region)
;;    (local-set-key (kbd "C-c C-e") 'maxscript-send-buffer)
;;    (local-set-key (kbd "C-c C-c") 'maxscript-send-buffer)
;;    (local-set-key (kbd "C-c C-d") 'maxscript-clear-listener)))
;;
;; (add-hook
;;  'python-mode-hook
;;  (lambda ()
;;    (require 'send-to-max)
;;    (local-set-key (kbd "C-c C-m C-r") 'maxscript-send-region-py)
;;    (local-set-key (kbd "C-c C-m C-e") 'maxscript-send-buffer-py)
;;    (local-set-key (kbd "C-c C-m C-c") 'maxscript-send-buffer-py)
;;    (local-set-key (kbd "C-c C-m C-d") 'maxscript-clear-listener)))
;;
;; -----------------------------------------------------------------------------

;; (defvar maxscript-buffer nil
;;   "Buffer used to display Maxscript output.")

(defconst mxs-mode-base-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun maxscript-send-region (start end &optional aspython) 
  "Send region to Max."
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end))
	(result ()))
	(setq result (shell-command-to-string
		 (concat "python " mxs-mode-base-dir "/send-to-max.py " (if aspython "-py " "-ms ")
			 (shell-quote-argument content))))
	(print result)))

(defun maxscript-send-region-py (start end)
  "Send region to Max as Python code."
  (interactive "r")
  (maxscript-send-region start end t))

;; (defun maxscript-send-region-or-buffer ()
;;   "Send region, if active region, send whole buffer otherwise"
;;   )

(defun maxscript-send-buffer ()
  "Send whole buffer to Max."
  (interactive)
  (maxscript-send-region (point-min) (point-max)))

(defun maxscript-send-buffer-py ()
  "Send whole buffer to Max as Python code."
  (interactive)
  (maxscript-send-region (point-min) (point-max) t))


(defun maxscript-clear-listener ()
  "Clear the Maxscript-Listener outpu."
  (interactive)
  (let ((result (shell-command-to-string
		 (concat "python " mxs-mode-base-dir "/send-to-max.py -c"))))
			 ;(shell-quote-argument "do stuff")))))
	(print result)))

(provide 'send-to-max)

;;; send-to-max.el ends here
