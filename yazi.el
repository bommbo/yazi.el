;;; yazi.el --- Yazi file manager integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: bommbo
;; Version: 0.2
;; Package-Requires: ((emacs "30.0"))
;; Keywords: convenience, files, yazi

;;; Commentary:

;; Yazi file manager integration for Emacs using term.el.
;; Opens yazi in the current file's directory, selecting the current file.

;;; Code:

(require 'term)

(defgroup yazi nil
  "Yazi file manager integration."
  :group 'applications
  :prefix "yazi-")

;;; Internal Variables

(defvar yazi--buffer nil
  "Current yazi buffer.")

(defvar yazi--output-file nil
  "Temporary file for yazi output.")

(defvar yazi--window-config nil
  "Window configuration before yazi was opened.")

;;; Core Functions

(defun yazi--process-sentinel (proc event)
  "Process sentinel to detect when yazi exits."
  (when (memq (process-status proc) '(exit signal))
    (run-at-time 0.1 nil #'yazi--process-output)))

(defun yazi--setup-environment ()
  "Setup environment variables for yazi."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "COLORTERM" "truecolor")
    (setenv "EDITOR" "emacsclient")
    (setenv "VISUAL" "emacsclient")
    (unless (getenv "LANG")
      (setenv "LANG" "en_US.UTF-8"))
    process-environment))

(defun yazi--start (directory start-file)
  "Start yazi in DIRECTORY, optionally at START-FILE."
  (setq yazi--output-file (make-temp-file "yazi-emacs-" nil ".tmp"))
  
  (let* ((default-directory (expand-file-name directory))
         (process-environment (yazi--setup-environment))
         (buf-name "*yazi*")
         (args (list "--chooser-file" yazi--output-file)))
    
    ;; Add start file if provided and exists
    (when (and start-file (file-exists-p start-file))
      (setq args (append args (list (expand-file-name start-file)))))
    
    ;; Kill old buffer if exists
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    
    ;; Create and setup term buffer
    (let ((buf (apply #'make-term "yazi" "yazi" nil args)))
      (with-current-buffer buf
        ;; Disable interfering modes
        (when (fboundp 'meow-mode)
          (meow-mode -1))
        
        ;; Enter char mode for direct key input
        (term-char-mode)
        
        ;; Setup process
        (let ((proc (get-buffer-process buf)))
          (when proc
            (set-process-sentinel proc #'yazi--process-sentinel)
            (set-process-window-size proc (window-body-height) (window-body-width))))
        
        ;; Hide UI elements
        (setq mode-line-format nil
              cursor-type nil)
        (internal-show-cursor nil nil)
        
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1)))
      
      buf)))

(defun yazi--show-buffer (buf)
  "Display yazi BUF in full screen."
  (setq yazi--window-config (current-window-configuration))
  (delete-other-windows)
  (switch-to-buffer buf)
  (term-char-mode))

(defun yazi--process-output ()
  "Process yazi output and jump to selected file."
  ;; Restore cursor
  (internal-show-cursor nil t)
  
  ;; Restore window config
  (when yazi--window-config
    (set-window-configuration yazi--window-config)
    (setq yazi--window-config nil))
  
  ;; Kill buffer
  (when (and yazi--buffer (buffer-live-p yazi--buffer))
    (kill-buffer yazi--buffer))
  (setq yazi--buffer nil)
  
  ;; Read output file and jump to selected file
  (when (and yazi--output-file (file-exists-p yazi--output-file))
    (let ((file (with-temp-buffer
                  (insert-file-contents yazi--output-file)
                  (string-trim (buffer-string)))))
      (delete-file yazi--output-file)
      (setq yazi--output-file nil)
      
      (when (and (not (string-empty-p file))
                 (file-exists-p file))
        (find-file file)))))

(defun yazi--jump-to-file (file)
  "Jump to FILE selected in yazi."
  (when (file-exists-p file)
    (find-file file)))

(defun yazi--cleanup ()
  "Clean up existing yazi session."
  (when (and yazi--buffer (buffer-live-p yazi--buffer))
    (let ((proc (get-buffer-process yazi--buffer)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))
    (kill-buffer yazi--buffer))
  (when (and yazi--output-file (file-exists-p yazi--output-file))
    (ignore-errors (delete-file yazi--output-file)))
  (setq yazi--buffer nil
        yazi--output-file nil))

;;; Interactive Commands

;;;###autoload
(defun yazi ()
  "Open yazi in current file's directory, selecting current file.
If current buffer has no file, opens yazi in `default-directory`."
  (interactive)
  ;; Clean up any existing session
  (yazi--cleanup)
  
  ;; Determine directory and file
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (buf (yazi--start dir buffer-file-name)))
    
    (setq yazi--buffer buf)
    (yazi--show-buffer buf)))

(provide 'yazi)
;;; yazi.el ends here
