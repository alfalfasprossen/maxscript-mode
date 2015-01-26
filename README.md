maxscript-mode
==============

3DsMax script mode for Emacs.

Initial lisp file forked from 'akm'.

This mode provides syntax-highlighting and functionality to send a region (or the whole buffer) of maxscript or python code to a running 3DsMax to be executed directly.

To use it, put this in your init.el
```lisp
;; Maxscript mode for 3DsMax
(add-to-list 'load-path "~/.emacs.d/lisp/maxscript-mode/")
(autoload 'maxscript-mode "maxscript-mode" "maxscript-mode" t)
(setq auto-mode-alist (append '(("\.ms$" . maxscript-mode)) auto-mode-alist))

(add-hook
 'maxscript-mode-hook
 (lambda ()
   (require 'send-to-max)
   (local-set-key (kbd "C-m C-r") 'maxscript-send-region)
   (local-set-key (kbd "C-m C-e") 'maxscript-send-buffer)
   (local-set-key (kbd "C-m C-c") 'maxscript-send-buffer)
   (local-set-key (kbd "C-m C-d") 'maxscript-clear-listener)))

(add-hook
 'python-mode-hook
 (lambda ()
   (require 'send-to-max)
   (local-set-key (kbd "C-m C-r") 'maxscript-send-region-py)
   (local-set-key (kbd "C-m C-e") 'maxscript-send-buffer-py)
   (local-set-key (kbd "C-m C-c") 'maxscript-send-buffer-py)
   (local-set-key (kbd "C-m C-d") 'maxscript-clear-listener)))
```
