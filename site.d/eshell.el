;;; eshell.el -*- lexical-binding: t; -*-
;; Keywords: eshell, emacs, shell, commands, utilities, configuration

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'esh-util)

;;                     _
;;                    | |
;;   ___ _ __ ___   __| |___
;;  / __| '_ ` _ \ / _` / __|
;; | (__| | | | | | (_| \__ \
;;  \___|_| |_| |_|\__,_|___/
;;

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

;;                                  _
;;                                 | |
;;  _ __  _ __ ___  _ __ ___  _ __ | |_
;; | '_ \| '__/ _ \| '_ ` _ \| '_ \| __|
;; | |_) | | | (_) | | | | | | |_) | |_
;; | .__/|_|  \___/|_| |_| |_| .__/ \__|

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient -t")

;; Inside eshell, EDITOR opens files in the current frame instead.
;; (`process-environment' is buffer-local in eshell, so this doesn't leak.)
(add-hook 'eshell-mode-hook
          (lambda () (setenv "EDITOR" "emacsclient")))

;; `eshell-visual-commands' lives in em-term, which isn't loaded at startup;
;; touching it earlier signals void-variable and aborts the rest of this file.
(after! em-term
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail"))

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to t."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(add-hook 'eshell-pre-command-hook 'eshell-append-history)

(setq eshell-history-size 10000)

;; capf-autosuggest: fish-like right-arrow completion suggestions
;; Docs: https://github.com/emacs-straight/capf-autosuggest
(use-package capf-autosuggest
  :hook (eshell-mode . capf-autosuggest-mode))

;; bash-completion: complete via a real bash in M-x shell and eshell.
(defun my/eshell-mode-completion-hook ()
  (add-hook 'completion-at-point-functions
            #'bash-completion-capf-nonexclusive nil t))

(use-package bash-completion
  :commands (bash-completion-dynamic-complete bash-completion-capf-nonexclusive)
  :hook ((shell-dynamic-complete-functions . bash-completion-dynamic-complete)
         (eshell-mode . my/eshell-mode-completion-hook)))

(provide 'eshell-config)
;;; eshell.el ends here
