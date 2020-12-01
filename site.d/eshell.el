;;; eshell.el -*- lexical-binding: t; -*-

(require 'dash)
(require 's)
(require 'cl-lib)
;; (eval-when-compile
;;   ;; (require 'eshell)
;;   (require 'cl-lib)
;;   (require 'esh-mode))

(require 'esh-util)

;;                     _
;;                    | |
;;   ___ _ __ ___   __| |___
;;  / __| '_ ` _ \ / _` / __|
;; | (__| | | | | | (_| \__ \
;;  \___|_| |_| |_|\__,_|___/
;;

;; Doesn't work over tramp...
;; (defun eshell/ws (&optional workspace)
;;   (if workspace
;;       (let ((workspace-path (concat "/google/src/cloud/"
;;                                     (user-login-name)
;;                                     "/"
;;                                     workspace
;;                                     "/google3")))
;;         (if (file-directory-p workspace-path)
;;             (eshell/cd workspace-path)
;;           (format "Not a valid workspace: %s\n"
;;                   workspace)))
;;     (p4-current-client)))

;; (defun eshell/ws? ()
;;   (eshell/ls (concat "/google/src/cloud/"
;;                      (user-login-name))))

(defun eshell/ff (file)
  (find-file file))

;;(defun eshell/ffow (file)
;;  (find-file-other-window file))

(defun eshell/edit (file)
  (find-file file))

(defun str-empty-p (str)
  (string= str ""))

(defun get-last-line-from-output (output)
  (let ((out (split-string output "\n")))
    (last (remove-if #'str-empty-p out))))

(defun eshell/g4d (&rest args)
  (let* ((command (mapconcat 'identity (cons "p4 g4d" args) " "))
         (newpath (get-last-line-from-output
                   (shell-command-to-string command))))
    (when newpath
      (eshell/cd (first newpath)))))

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
    (insert (concat "ls"))
    (eshell-send-input)))



;;                                  _
;;                                 | |
;;  _ __  _ __ ___  _ __ ___  _ __ | |_
;; | '_ \| '__/ _ \| '_ ` _ \| '_ \| __|
;; | |_) | | | (_) | | | | | | |_) | |_
;; | .__/|_|  \___/|_| |_| |_| .__/ \__|
;; | |                       | |
;; |_|                       |_|
;;

;; http://www.modernemacs.com/post/custom-eshell/

;; (defmacro with-face (STR &rest PROPS)
;;   `(propertize ,STR 'face (list ,@PROPS)))

;; (defmacro esh-section (NAME ICON FORM &rest PROPS)
;;   "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
;;   `(setq ,NAME
;;          (lambda () (when ,FORM
;;                       (-> ,ICON
;;                           (concat esh-section-delim ,FORM)
;;                           (with-face ,@PROPS))))))

;; (defun esh-acc (acc x)
;;   "Accumulator for evaluating and concatenating esh-sections."
;;   (--if-let (funcall x)
;;       (if (s-blank? acc)
;;           it
;;         (concat acc esh-sep it))
;;     acc))

;; (defun esh-prompt-func ()
;;   "Build `eshell-prompt-function'"
;;   (let ((exit-code (eshell-exit-code)))
;;     (concat (eshell-dynamic-header-string exit-code)
;;             (-reduce-from 'esh-acc "" eshell-funcs)
;;             "\n"
;;             (eshell-dynamic-prompt-string exit-code))))

;; (defun p4-extract-client-name (client-str)
;;   "Pull just the workspace name from the citc string."
;;   (when client-str (nth 1 (split-string client-str ":"))))

;; (defun string-blank-p (str)
;;   (or (equal str "")
;;       (equal str '())))

;; (defun fig-client-name ()
;;   "Take the client name from fig--root."
;;   (let ((root (fig--root)))
;;     (when root
;;       (first (reverse (filter (lambda (s) (not (string-blank-p s)))
;;                               (split-string root "/")))))))

;; (defun shorten-path-if-necessary (path max)
;;   (if (<= (string-width path) max)
;;       path
;;     (let* ((elems (split-string path "/"))
;;            (head (nth 1 elems))
;;            (tail (last elems 3)))
;;       (mapconcat 'identity
;;                  (cons "" (cons head (cons ".." tail)))
;;                  "/"))))

;; (setq esh-sep "  ")
;; (setq esh-section-delim " ")

;; (defun eshell-exit-code ()
;;   "Return non-nil if the last command was successful.
;; Based on eshell-exit-success-p, but return the code if available."
;;   eshell-last-command-status)

;; (defun eshell-color-from-code (exit-code string-to-facify)
;;   "Based on the exit code of the last command, color red or green.
;; EXIT-CODE: numeric value where 0 is success
;; STRING-TO-FACIFY: string to which we apply the new face"
;;   (if (= 0 exit-code)
;;       (-> string-to-facify
;;           (with-face :foreground "green" :weight 'bold))
;;     (-> string-to-facify
;;         (with-face :foreground "red" :weight 'bold))))

;; (defun eshell-dynamic-prompt-string (exit-code)
;;   "Color the prompt based on the exit code of the last command.
;; EXIT-CODE: exit code of the last-run command"
;;   (let ((prompt (concat "└─[" (number-to-string exit-code) "]─»")))
;;     (setq eshell-prompt-regexp (concat "^" (regexp-quote prompt) " +"))
;;     (concat (eshell-color-from-code exit-code prompt) " ")))

;; (defun eshell-dynamic-header-string (exit-code)
;;   "Color the header based on the exit code of the last command.
;; EXIT-CODE: exit code of the last-run command"
;;   (let ((header-string "\n┌─"))
;;     (eshell-color-from-code exit-code header-string)))

;; (esh-section esh-dir
;;              "\xf07c" ; folder
;;              (shorten-path-if-necessary (eshell/pwd) 50)
;;              '(:foreground "gold" :weight ultra-bold))

;; (esh-section esh-p4
;;              "\x26d6"
;;              (p4-extract-client-name (p4-current-client))
;;              '(:foreground "pink"))

;; (esh-section esh-fig
;;              "\x26d6"
;;              (fig-client-name)
;;              '(:foreground "pink"))

;; (esh-section esh-clock
;;              "\x25d0" ;"\xf550" ; clock
;;              (format-time-string "%H:%M" (current-time))
;;              '(:foreground "forest green"))

;; (setq eshell-funcs (list esh-dir esh-p4 esh-fig esh-clock))

;; (setq eshell-prompt-function 'esh-prompt-func)


;;   ___ _ ____   __
;;  / _ \ '_ \ \ / /
;; |  __/ | | \ V /
;;  \___|_| |_|\_/

(setenv "P4DIFF" "/usr/bin/ediff")
(setenv "P4MERGE" "/usr/bin/ediff_merge")
(defalias 'contracts_cli
  "/google/data/ro/teams/resource-management/contracts_cli.par $*")

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient -t")

(local-set-key "\M-p" 'helm-eshell-history)

;; (add-to-list 'eshell-visual-commands
;;              "ssh"
;;              "tail")

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to t."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))
;; (add-hook eshell-pre-command-hook 'eshell-append-history)

(setq eshell-history-size 10000)

;;; eshell.el ends here
