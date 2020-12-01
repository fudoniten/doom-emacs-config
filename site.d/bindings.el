;; -*-emacs-lisp-*-

(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-xb" 'ivy-switch-buffer)
(global-set-key "\C-cb" '+ivy/switch-workspace-buffer)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-cj" 'org-journal-entry)

(global-set-key "\C-e" 'end-of-line)

(global-set-key [?\C-*] 'er/expand-region)

(global-set-key (kbd "C-.") 'ace-jump-mode)
(global-set-key (kbd "M-.") 'avy-goto-char)

(defalias 'qrr 'query-replace-regexp)

(global-set-key [M-right] 'forward-word)
(global-set-key [M-left] 'backward-word)

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

(global-set-key "\C-c\C-f" 'find-file-at-point)

(global-set-key "\C-a" 'beginning-of-line-text)

(global-set-key "\C-c\C-u" 'untabify-this-file)

(global-set-key "\C-e" 'end-of-line)

(global-set-key (kbd "C-;") 'kill-whitespace)
(global-set-key (kbd "C-c ;") 'kill-whitespace)
(global-set-key (kbd "C-,") 'split-line)
(global-set-key (kbd "C-c ,") 'split-line)

(global-set-key "\C-cr" 'read-only-mode)

(global-set-key (kbd "C-!") 'eshell-here)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
;;(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-n") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-S-p") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-e") 'mc/mark-all-dwim)

(global-set-key "\C-ct" 'org-capture)

(global-set-key (kbd "C-c C-p") 'open-and-indent-previous-line)
(global-set-key (kbd "C-c C-n") 'open-and-indent-line)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Create & destroy views in ivy (virtual buffers/bookmarks)
(global-set-key (kbd "C-c b") 'ivy-push-view)
(global-set-key (kbd "C-c B") 'ivy-pop-view)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c S") 'isearch-backward-regexp)

(global-set-key (kbd "C-c C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-+") 'text-scale-increase)
