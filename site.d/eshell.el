;;; eshell.el -*- lexical-binding: t; -*-
;; Keywords: eshell, emacs, shell, commands, utilities, configuration

(require 'dash)
(require 'cl-lib)
(require 'message) ; For logging
(require 's)
(require 'cl-lib)

(require 'esh-util)

;;                     _
;;                    | |
;;   ___ _ __ ___   __| |___
;;  / __| '_ ` _ \ / _` / __|
;; | (__| | | | | | (_| \__ \
;;  \___|_| |_| |_|\__,_|___/
;;


(defun eshell/ff (file)
  (if (file-exists-p file)
      (progn
        (message "Opening file: %s" file)
        (find-file file))
    (message "File not found: %s" file)))

(defun eshell/x ()
  "Exit eshell and close the window."
  (insert "exit")
  (eshell-send-input)
  (delete-window)
  (message "Exited eshell and closed window"))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/edit (file)
  (if (file-exists-p file)
      (progn
        (message "Editing file: %s" file)
        (find-file file))
    (message "File not found: %s" file)))

(defun str-empty-p (str)
  (string= str ""))

(defun get-last-line-from-output (output)
  (let ((out (split-string output "\n")))
    (last (remove-if #'str-empty-p out))))

(defun eshell/g4d (&rest args)
  (let* ((command (mapconcat 'identity (cons "p4 g4d" args) " "))
         (output (shell-command-to-string command))
         (newpath (get-last-line-from-output output)))
    (if newpath
        (progn
          (message "Changing directory to: %s" (first newpath))
          (eshell/cd (first newpath)))
      (message "Failed to change directory. Command output: %s" output))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
(defun eshell-here ()
  "Open a new eshell in the current buffer's directory."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (message "Opening eshell in directory: %s" parent)
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert "ls")
    (eshell-send-input)))
    (insert (concat "ls"))
    (eshell-send-input)))



;;                                  _
;;                                 | |
;;  _ __  _ __ ___  _ __ ___  _ __ | |_
;; | '_ \| '__/ _ \| '_ ` _ \| '_ \| __|
;; | |_) | | | (_) | | | | | | |_) | |_
;; | .__/|_|  \___/|_| |_| |_| .__/ \__|
(setenv "P4DIFF" "/usr/bin/ediff")
(setenv "P4MERGE" "/usr/bin/ediff_merge")
(defalias 'contracts_cli
  "/google/data/ro/teams/resource-management/contracts_cli.par $*")

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient -t")

(local-set-key "\M-p" 'helm-eshell-history)

(add-to-list 'eshell-visual-commands "ssh" "tail")

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to t."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))
(add-hook 'eshell-pre-command-hook 'eshell-append-history)

(setq eshell-history-size 10000)

;;; eshell.el ends here
