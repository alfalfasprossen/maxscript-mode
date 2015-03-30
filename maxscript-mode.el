;;; maxscript-mode.el --- 3dsmax script (maxscript) editing mode for GNU Emacs
;; -*- Mode:Emacs-Lisp -*-

;; Copyright (C) 2010 akm
;; Copyright (C) 2015 Johannes Becker

;; Authors:  akm <akm.gfx@gmail.com>,
;;           Johannes Becker <alfalfasprossen@gmail.com>
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; [Usage]
;; 
;;  (autoload 'maxscript-mode "maxscript-mode" "maxscript-mode" t)
;;  (setq auto-mode-alist
;;     (append '(("\\.ms$" . maxscript-mode)) auto-mode-alist))
;;

(eval-when-compile
  (require 'cc-mode))
(require 'regexp-opt)

(defgroup maxscript nil
  "Major mode for editing Maxscript script files."
  :group 'languages)

(defcustom maxscript-mode-hook nil
  "*List of hook functions run by `maxscript-mode' (see `run-hooks')"
  :type 'hook
  :group 'maxscript)

(defvar maxscript-mode-abbrev-table nil
  "")
(define-abbrev-table 'maxscript-mode-abbrev-table ())

(defvar maxscript-mode-map
  (let ((maxscript-mode-map (make-sparse-keymap)))
    (define-key maxscript-mode-map ")" 'maxscript-mode-electric-insert-close-brace)
    (define-key maxscript-mode-map (kbd "C-m") 'newline-and-indent)
    maxscript-mode-map)
  "Keymap used in Maxscript mode buffers.")

(defvar maxscript-indent-level 4 "The indentation level for Maxscript scripts.")

(eval-and-compile
  (defvar maxscript-keywords
    '(
       "about" "and" "animate" "as" "at"
       "by"
       "case" "catch" "collect" "continue" "coordsys"
       "do"
       "else" "exit"
       "fn" "for" "from" "function" "format"
       "global"
       "if" "in"
       "local"
       "macroscript" "mapped" "max"
       "not"
       "of" "off" "on" "or"
       "parameters" "persistent" "plugin" "print"
       "rcmenu" "return" "rollout" "set" "struct"
       "then" "throw" "to" "tool" "try"
       "undo" "utility"
       "when" "where" "while" "with"
       )
    "MAXScript keywords.")

  (defvar maxscript-constants
    '(
       "true" "false"
       "on" "off"
       "pi" "e"
       "red" "green" "blue" "white" "black" "orange" "yellow" "brown" "gray"
       "x_axis" "y_axis" "z_axis"
       "ok"
       "undefined"
       "unsupplied"
       "dontcollect"
       )
    "MAXScript constants.")

  (defvar maxscript-global-variables
    '(
       "activeGrid" "ambientColor" "ambientColorController" "animationRange" "animButtonEnabled" "animButtonState" "autoBackup.enabled" "autoBackup.time"
       "backgroundColor" "backgroundColorController" "backgroundImageFileName"
       "cui.commandPanelOpen" "currentMaterialLibrary"
       "displayGamma"
       "fileInGamma" "fileOutGamma"
       "displaySafeFrames"
       "environmentMap"
       "flyOffTime" "frameRate"
       "globalTracks"
       "hardwareLockID" "hotspotAngleSeparation"
       "keyboard.shiftPressed" "keyboard.controlPressed" "keyboard.altPressed" "keyboard.escPressed"
       "lightTintColor" "lightTintColorController" "lightLevel" "lightLevelController" "listener " "localTime" "logsystem.quietmode"
       "macroRecorder" "manipulateMode" "maxFileName" "maxFilePath" "meditMaterials"
       "numEffects" "numAtmospherics" "numSubObjectLevels"
       "playActiveOnly" "preferences.constantReferenceSystem" "preferences.dontRepeatRefMsg" "preferences.flyOffTime" "preferences.InvalidateTMOpt" "preferences.maximumGBufferLayers" "preferences.spinnerWrap" "preferences.spinnerPrecision " "preferences.spinnerSnap" "preferences.useSpinnerSnap" "preferences.useLargeVertexDots" "preferences.useTransformGizmos" "preferences.useVertexDots"
       "realTimePlayback" "renderer" "renderDisplacements" "renderEffects" "renderHeight" "renderPixelAspect" "renderWidth" "rendOutputFilename" "rendSimplifyAreaLights" "rootNode"
       "sceneMaterials" "scriptsPath" "selectionSets" "showEndResult" "skipRenderedFrames" "sliderTime" "snapMode.active" "snapMode.type" "subObjectLevel" "sysInfo.DesktopSize" "sysInfo.DesktopBPP" "sysInfo.MAXPriority"
       "ticksPerFrame" "timeConfiguration.playActiveOnly" "timeConfiguration.realTimePlayback" "timeConfiguration.PlaybackSpeed" "timeConfiguration.PlaybackLoop" "timeConfiguration.useTrackBar" "toolMode.coordSysNode" "trackbar.filter" "trackbar.visible" "trackViewNodes"
       "units.DisplayType" "units.MetricType" "units.USType" "units.USFrac" "units.CustomName" "units.CustomValue" "units.CustomUnit" "units.SystemScale" "units.SystemType" "useEnvironmentMap"
       "videoPostTracks" "viewport.activeViewport" "viewport.numViews"
       "scanlineRender.antiAliasFilter" "scanlineRender.antiAliasFilterSize" "scanlineRender.enablePixelSampler"
       ;;---
       "currentTime" "editorFont" "editorFontSize" "editorTabWidth" "escapeEnable" "heapFree" "heapSize" "inputTextColor" "messageTextColor" "outputTextColor" "options.oldPrintStyles" "options.showGCStatus" "stackLimit" "?'"
       ;;--- collections
       "objects" "geometry" "lights" "cameras" "helpers" "shapes" "systems" "spacewarps" "selection" ;ObjectSet
       "selectionSets" ;SelectionSetArray
       "currentMaterialLibrary" "sceneMaterials" "meditMaterials" ;MaterialLibrary

       )
    "MAXScript global variables.")
  ) ; eval-and-compile

(eval-when-compile
  (defun mxs-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defconst maxscript-font-lock-keywords
  (list
   '("\\(--.*$\\)"
     1 'font-lock-comment-face)
   '("\\(\"[^\"]*\"\\)"
     1 'font-lock-string-face)
   ;; raw strings starting with @, highlight the @
   '("\\(@\\)\""
     1 'font-lock-emphasized-face)
   '("\\(\\/\\*[a-z0-9 	\n]+?\\*\\/\\)"
     1 'font-lock-comment-face)
   '("\\(\\/\\*\\*\\*[a-z0-9 	\n]+?\\*\\*\\*\\/\\)"
     1 'font-lock-doc-string-face)
   '("\\(\\#\\sw+\\)"
     1 'font-lock-preprocessor-face)
   '("\\(\\$\\sw+\\)"
     1 'font-lock-preprocessor-face)
   (cons (eval-when-compile
	   (mxs-ppre maxscript-keywords))
	 font-lock-keyword-face)
  (cons (eval-when-compile
	  (mxs-ppre maxscript-constants))
	font-lock-constant-face)
  (cons (eval-when-compile
	  (mxs-ppre maxscript-global-variables))
	;font-lock-builtin-face)
	font-lock-variable-name-face)
   ))


(defun maxscript-mode-calc-indent-level ()
  "Calculate the indent level for the current line."
  (save-excursion
    (let* ((indent-data (parse-partial-sexp (point-min)
                                            (line-beginning-position)))
           (indent (car indent-data))
           (in-comment (nth 4 indent-data))
           close-block
           close-comment
           pos)
      (back-to-indentation)
      (setq
       first-char (buffer-substring (point) (+ 1 (point)))
       close-block (looking-at ")")
       )
      (when close-block
        (setq indent (1- indent)))
      (setq pos (* indent maxscript-indent-level))
      )
    ))


(defun maxscript-indent-line (&optional indent)
  "Indents the current line."
  (interactive)
  (let ((indent (or indent (maxscript-mode-calc-indent-level)))
        pos)
    (save-excursion
      (back-to-indentation)
      (delete-region (point-at-bol) (point))
      (indent-to indent)
      (setq pos (point)))
    (when (> pos (point))
      (goto-char pos))))

(defun maxscript-mode-electric-insert-close-brace ()
  "Insert a closing brace }."
  (interactive)
  (insert ")")
  (maxscript-indent-line)
  )


(define-derived-mode maxscript-mode prog-mode "MXS"
  "Major mode for editing Maxscript script files."
  
  (use-local-map maxscript-mode-map)
  (setq local-abbrev-table maxscript-mode-abbrev-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'maxscript-indent-line)
  (setq indent-tabs-mode t)
  (setq tab-width 4) ; a tab is 4 spaces wide
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")

  ;;(add-to-list 'align-c++-modes 'maxscript-mode)

  (set (make-local-variable 'font-lock-defaults) '(maxscript-font-lock-keywords nil t))
  
  (run-hooks 'maxscript-mode-hook)
  )

(provide 'maxscript-mode)

;;; maxscript-mode.el ends here
