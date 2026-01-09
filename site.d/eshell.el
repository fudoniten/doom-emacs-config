;;; eshell.el -*- lexical-binding: t; -*-
;; Keywords: eshell, emacs, shell, commands, utilities, configuration

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'message) ; For logging
(require 's)
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
    (last (cl-remove-if #'str-empty-p out))))

;;                                  _
;;                                 | |
;;  _ __  _ __ ___  _ __ ___  _ __ | |_
;; | '_ \| '__/ _ \| '_ ` _ \| '_ \| __|
;; | |_) | | | (_) | | | | | | |_) | |_
;; | .__/|_|  \___/|_| |_| |_| .__/ \__|

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient -t")

(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to t."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(add-hook 'eshell-pre-command-hook 'eshell-append-history)

(setq eshell-history-size 10000)

(provide 'eshell-config)
;;; eshell.el ends here
