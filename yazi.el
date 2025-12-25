;;; yazi.el --- Yazi file manager integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: bommbo
;; Version: 0.1
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

(defvar yazi--callback nil
  "Callback function to call after yazi exits.")

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

(defun yazi--start-process (directory start-file)
  "Start yazi process in DIRECTORY, optionally at START-FILE."
  ;; Create temp file
  (setq yazi--output-file (make-temp-file "yazi-emacs-" nil ".tmp"))
  
  (let* ((default-directory (expand-file-name directory))
         (process-environment (yazi--setup-environment))
         (buf-name "*yazi*")
         (args (list "--chooser-file" yazi--output-file)))
    
    (when start-file
      (setq args (append args (list (expand-file-name start-file)))))
    
    ;; Kill old buffer if exists
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    
    ;; Create term buffer
    (let ((buf (apply #'make-term "yazi" "yazi" nil args)))
  (with-current-buffer buf
    (when (fboundp 'meow-mode)
      (meow-mode -1))
    
    (term-char-mode)
    
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-sentinel proc #'yazi--process-sentinel)
        (set-process-window-size proc (window-body-height) (window-body-width))))
    
    (setq mode-line-format nil
          cursor-type nil)
    (internal-show-cursor nil nil)
    
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)))
  
  (setq yazi--buffer buf)
  buf)))

(defun yazi--show-buffer (buf)
  "Display yazi BUF in full screen."
  (setq yazi--window-config (current-window-configuration))
  (delete-other-windows)
  (switch-to-buffer buf)
  (term-char-mode))

(defun yazi--process-output ()
  "Process yazi output: either call callback (chooser mode) or change directory (cwd mode)."
  ;; Restore window config
  (when yazi--window-config
    (set-window-configuration yazi--window-config)
    (setq yazi--window-config nil))
  
  ;; Kill buffer
  (when (and yazi--buffer (buffer-live-p yazi--buffer))
    (kill-buffer yazi--buffer))
  (setq yazi--buffer nil)
  
  ;; Read output file
  (let ((output (when (and yazi--output-file (file-exists-p yazi--output-file))
                  (with-temp-buffer
                    (insert-file-contents yazi--output-file)
                    (string-trim (buffer-string))))))
    
    ;; Clean up temp file
    (when (and yazi--output-file (file-exists-p yazi--output-file))
      (delete-file yazi--output-file))
    (setq yazi--output-file nil)
    
    ;; Two modes:
    (cond
     ;; 1. Callback mode (original `yazi`): jump to selected file(s)
     (yazi--callback
      (when (and output (not (string-empty-p output)))
        (funcall yazi--callback output)))
     ;; 2. Cwd mode (`yazi-cd`): change directory
     (output
      (let ((cwd output))
        (when (and (stringp cwd)
                   (> (length cwd) 0)
                   (file-directory-p cwd)
                   (not (string= (file-truename (expand-file-name cwd))
                                 (file-truename default-directory))))
          (cd cwd)
          (message "yazi-cd: Changed directory to %s" cwd)))))
    
    (setq yazi--callback nil)))

(defun yazi--jump-to-file (file)
  "Jump to FILE selected in yazi."
  (when (file-exists-p file)
    (find-file file)))

;;; Interactive Command

;;;###autoload
(defun yazi ()
  "Open yazi in current file's directory, selecting current file.
If current buffer has no file, opens yazi in `default-directory`."
  (interactive)
  ;; Quit any existing yazi
  (when (or yazi--buffer yazi--output-file)
    (when (and yazi--buffer (buffer-live-p yazi--buffer))
      (with-current-buffer yazi--buffer
        (let ((proc (get-buffer-process yazi--buffer)))
          (when (and proc (process-live-p proc))
            (delete-process proc))))
      (kill-buffer yazi--buffer))
    (when (and yazi--output-file (file-exists-p yazi--output-file))
      (delete-file yazi--output-file))
    (setq yazi--buffer nil
          yazi--output-file nil
          yazi--callback nil))

  ;; Determine dir and file
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (file buffer-file-name)
         (buf (yazi--start-process dir file)))
    (setq yazi--callback #'yazi--jump-to-file)
    (yazi--show-buffer buf)))

;;;###autoload
(defun yazi-cd ()
  "Open yazi in current directory. On exit, change Emacs's `default-directory`
if yazi's working directory has changed (just like Zsh function `y()`)."
  (interactive)
  
  ;; Clean up any existing yazi process/buffer
  (when (and yazi--buffer (buffer-live-p yazi--buffer))
    (let ((proc (get-buffer-process yazi--buffer)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))
    (kill-buffer yazi--buffer))
  (when (and yazi--output-file (file-exists-p yazi--output-file))
    (delete-file yazi--output-file))
  (setq yazi--buffer nil
        yazi--output-file nil
        yazi--callback nil)

  ;; Create temp file for --cwd-file
  (setq yazi--output-file (make-temp-file "yazi-emacs-cwd-" nil ".tmp"))

  (let* ((default-directory (expand-file-name default-directory))
         (process-environment (yazi--setup-environment))
         (buf-name "*yazi*")
         (args (list "--cwd-file" yazi--output-file)))
    
    ;; Optional: highlight current file if exists
    (when buffer-file-name
      (push (file-relative-name buffer-file-name default-directory) args)
      (push "--on-file" args))
    
    ;; Kill old buffer
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    
    ;; Start yazi in term
    (let ((buf (apply #'make-term "yazi" "yazi" nil args)))
      (with-current-buffer buf
        (when (fboundp 'meow-mode) (meow-mode -1))
        (term-char-mode)
        (let ((proc (get-buffer-process buf)))
          (when proc
            (set-process-sentinel proc #'yazi--process-sentinel)
            (set-process-window-size proc (window-body-height) (window-body-width))))
        (setq mode-line-format nil
              cursor-type nil)
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1)))
      (setq yazi--buffer buf)
      (yazi--show-buffer buf))))

(provide 'yazi)
;;; yazi.el ends here
