;;; site.d/bindings.el -*- lexical-binding: t; -*-

;;; Code:

;; Sensible default key bindings for non-evil users
(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

;;; Autoloads
(autoload 'org-capture-goto-target "org-capture" nil t)

;;; Leader keys
(map! :leader
      :desc "Search buffer"               "s"   #'counsel-grep-or-swiper
      :desc "Search buffer with swiper"   "s"   #'swiper
      :desc "Search buffer with swiper"   "C-s" #'swiper
      :desc "Evaluate line/region"        "e"   #'+eval/line-or-region
      (:prefix ("l" . "<localleader>")) ; bound locally
      (:prefix ("!" . "checkers"))      ; bound by flycheck
      :desc "Extended command"            "C-m" #'execute-extended-command
      :desc "Kill whitespace"             ";"   #'kill-whitespace
      :desc "Toggle readonly mode"        "R"   #'read-only-mode
      :desc "Open previous line"          "C-p" #'open-and-indent-previous-line
      :desc "Open next line"              "C-n" #'open-and-indent-line
      :desc "Increase font size"          "+"   #'text-scale-increase
      :desc "Decrease font size"          "-"   #'text-scale-decrease
      :desc "Jump to character"           "."   #'avy-goto-char
      :desc "Jump to line"                ","   #'avy-goto-line
      :desc "Open eshell here"            "!"   #'eshell-here

      :desc "Expand region"               "C-+" #'er/expand-region
      :desc "Contract region"             "C--" #'er/contract-region

;;; <leader> V --- views
      (:prefix-map ("V" . "views")
       :desc "Add view"               "v"   #'ivy-push-view
       :desc "Pop view"               "p"   #'ivy-pop-view)

;;; <leader> c --- code
      (:prefix-map ("c" . "code")
       :desc "Comment"                               "c"   #'comment-or-uncomment-region
       :desc "Compile"                               "k"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval/region-and-replace
       :desc "Find file"                             "f"   #'find-file
       :desc "Format buffer/region"                  "F"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
       :desc "Untabify this file"                    "u"   #'untabify-this-file
       (:when (modulep! :checkers syntax)
         :desc "List errors"                         "x"   #'flycheck-list-errors)
       (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
         :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
         :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
         :desc "LSP Rename"                            "r"   #'lsp-rename
         :desc "LSP"                                   "l"   #'+default/lsp-command-map
         (:when (modulep! :completion ivy)
           :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
           :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
         (:when (modulep! :completion helm)
           :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
           :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol))
       (:when (modulep! :tools lsp +eglot)
         :desc "LSP Execute code action"              "a" #'eglot-code-actions
         :desc "LSP Rename"                           "r" #'eglot-rename
         :desc "LSP Find declaration"                 "j" #'eglot-find-declaration))

;;; <leader> f --- file
      (:prefix-map ("f" . "file")
                   (:when (modulep! :tools editorconfig)
                     :desc "Open project editorconfig"  "c"   #'editorconfig-find-current-editorconfig)
                   :desc "Copy this file"              "C"   #'doom/copy-this-file
                   :desc "Find directory"              "d"   #'dired
                   :desc "Delete this file"            "D"   #'doom/delete-this-file
                   :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
                   :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
                   :desc "Find file"                   "f"   #'find-file
                   :desc "Find file from here"         "F"   #'+default/find-file-under-here
                   :desc "Locate file"                 "l"   #'locate
                   :desc "Rename/move this file"       "m"   #'doom/move-this-file
                   :desc "Find file in private config" "p"   #'doom/open-private-config
                   :desc "Browse private config"       "P"   #'doom/open-private-config
                   :desc "Recent files"                "r"   #'recentf-open-files
                   :desc "Recent project files"        "R"   #'projectile-recentf
                   :desc "Sudo this file"              "u"   #'doom/sudo-this-file
                   :desc "Sudo find file"              "U"   #'doom/sudo-find-file
                   :desc "Yank filename"               "y"   #'+default/yank-buffer-filename
                   :desc "Open scratch buffer"         "x"   #'doom/open-scratch-buffer
                   :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer)

;;; <leader> r --- remote
      (:when (modulep! :tools upload)
        (:prefix-map ("r" . "remote")
         :desc "Browse remote"              "b" #'ssh-deploy-browse-remote-base-handler
         :desc "Browse relative"            "B" #'ssh-deploy-browse-remote-handler
         :desc "Download remote"            "d" #'ssh-deploy-download-handler
         :desc "Delete local & remote"      "D" #'ssh-deploy-delete-handler
         :desc "Eshell base terminal"       "e" #'ssh-deploy-remote-terminal-eshell-base-handler
         :desc "Eshell relative terminal"   "E" #'ssh-deploy-remote-terminal-eshell-handler
         :desc "Move/rename local & remote" "m" #'ssh-deploy-rename-handler
         :desc "Open this file on remote"   "o" #'ssh-deploy-open-remote-file-handler
         :desc "Run deploy script"          "s" #'ssh-deploy-run-deploy-script-handler
         :desc "Upload local"               "u" #'ssh-deploy-upload-handler
         :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
         :desc "Diff local & remote"        "x" #'ssh-deploy-diff-handler
         :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
         :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

;;; <leader> S --- search
      (:prefix-map ("S" . "search")
       :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
       :desc "Search buffer"                "b" #'swiper
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Locate file"                  "f" #'+lookup/file
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Jump to visible link"         "l" #'link-hint-open-link
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       :desc "Look up online"               "o" #'+lookup/online
       :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
       :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
       :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Search buffer"                "s" #'+default/search-buffer
       :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point
       :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
       :desc "Thesaurus"                    "T" #'+lookup/synonyms)

;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
       :desc "Emoji"                         "e"   #'emojify-insert-emoji
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "Unicode"                       "u"   #'unicode-chars-list-chars
       :desc "From clipboard"                "y"   #'+default/yank-pop)

;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
       :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
       :desc "Org agenda"                     "a" #'org-agenda
       (:when (modulep! :tools biblio)
         :desc "Bibliographic entries"        "b"
         (cond ((modulep! :completion ivy)   #'ivy-bibtex)
               ((modulep! :completion helm)  #'helm-bibtex)))

       :desc "Toggle last org-clock"          "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"       "C" #'org-clock-cancel
       :desc "Open deft"                      "d" #'deft
       (:when (modulep! :lang org +noter)
         :desc "Org noter"                    "e" #'org-noter)

       :desc "Find file in notes"             "f" #'+default/find-in-notes
       :desc "Browse notes"                   "F" #'+default/browse-notes
       :desc "Org store link"                 "l" #'org-store-link
       :desc "Tags search"                    "m" #'org-tags-view
       :desc "Org capture"                    "n" #'org-capture
       :desc "Goto capture"                   "N" #'org-capture-goto-target
       :desc "Active org-clock"               "o" #'org-clock-goto
       :desc "Todo list"                      "t" #'org-todo-list
       :desc "Search notes"                   "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
       :desc "View search"                    "v" #'org-search-view
       :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
       :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
       (:when (modulep! :lang org +journal)
         (:prefix ("j" . "journal")
          :desc "New Entry"           "j" #'org-journal-new-entry
          :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
          :desc "Search Forever"      "s" #'org-journal-search-forever))
       (:when (modulep! :lang org +roam)
         (:prefix ("r" . "roam")
          :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
          :desc "Org Roam Capture"              "c" #'org-roam-capture
          :desc "Find file"                     "f" #'org-roam-find-file
          :desc "Show graph"                    "g" #'org-roam-graph
          :desc "Insert"                        "i" #'org-roam-insert
          :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
          :desc "Org Roam"                      "r" #'org-roam
          :desc "Tag"                           "t" #'org-roam-tag-add
          :desc "Un-tag"                        "T" #'org-roam-tag-delete
          (:prefix ("d" . "by date")
           :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
           :desc "Today"          "t" #'org-roam-dailies-find-today
           :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
           :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday))))

;;; <leader> o --- open
      "o" nil             ; we need to unbind it first as Org claims this prefix
      (:prefix-map ("o" . "open")
       :desc "Browser"            "b"  #'browse-url-of-file
       :desc "Debugger"           "d"  #'+debugger/start
       :desc "New frame"          "f"  #'make-frame
       :desc "REPL"               "r"  #'+eval/open-repl-other-window
       :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
       :desc "Dired"              "-"  #'dired-jump
       (:when (modulep! :ui neotree)
         :desc "Project sidebar"               "p" #'+neotree/open
         :desc "Find file in project sidebar"  "P" #'+neotree/find-this-file)
       (:when (modulep! :ui treemacs)
         :desc "Project sidebar"               "p" #'+treemacs/toggle
         :desc "Find file in project rsidebar" "P" #'treemacs-find-file)
       (:when (modulep! :term shell)
         :desc "Toggle shell popup"            "t" #'+shell/toggle
         :desc "Open shell here"               "T" #'+shell/here)
       (:when (modulep! :term term)
         :desc "Toggle terminal popup"         "t" #'+term/toggle
         :desc "Open terminal here"            "T" #'+term/here)
       (:when (modulep! :term vterm)
         :desc "Toggle vterm popup"            "t" #'+vterm/toggle
         :desc "Open vterm here"               "T" #'+vterm/here)
       (:when (modulep! :term eshell)
         :desc "Toggle eshell popup"           "e" #'+eshell/toggle
         :desc "Open eshell here"              "E" #'+eshell/here)
       (:when (modulep! :os macos)
         :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
         :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
         :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
         :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
         :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
         :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar
         :desc "Open in iTerm"              "i" #'+macos/open-in-iterm)
       (:when (modulep! :tools docker)
         :desc "Docker" "D" #'docker)
       (:when (modulep! :email mu4e)
         :desc "mu4e" "m" #'=mu4e)
       (:when (modulep! :email notmuch)
         :desc "notmuch" "m" #'=notmuch)
       (:when (modulep! :email wanderlust)
         :desc "wanderlust" "m" #'=wanderlust))


;;; <leader> p --- project
      (:prefix ("p" . "project")
       :desc "Search project for symbol"   "." #'+default/search-project-for-symbol-at-point
       :desc "Find file in other project"  "F" #'doom/find-file-in-other-project
       :desc "Search project"              "s" #'+default/search-project
       :desc "List project todos"          "t" #'magit-todos-list
       :desc "Open project scratch buffer" "x" #'doom/open-project-scratch-buffer
       :desc "Switch to project scratch buffer" "X" #'doom/switch-to-project-scratch-buffer
       (:when (and (modulep! :tools taskrunner)
                   (or (modulep! :completion ivy)
                       (modulep! :completion helm)))
         :desc "List project tasks"         "z" #'+taskrunner/project-tasks)
       ;; later expanded by projectile
       (:prefix ("4" . "in other window"))
       (:prefix ("5" . "in other frame")))

;;; <leader> q --- quit/restart
      (:prefix-map ("q" . "quit/restart")
       :desc "Restart emacs server"         "d" #'+default/restart-server
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'doom/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs"                   "q" #'kill-emacs
       :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
       :desc "Quick save current session"   "s" #'doom/quicksave-session
       :desc "Restore last session"         "l" #'doom/quickload-session
       :desc "Save session to file"         "S" #'doom/save-session
       :desc "Restore session from file"    "L" #'doom/load-session
       :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
       :desc "Restart Emacs"                "R" #'doom/restart)

;;; <leader> a --- aider
      (:prefix-map ("a" . "aider")
       :desc "Function or region refactor" "f" #'aider-function-or-region-refactor
       :desc "Implement TODO" "t" #'aider-implement-todo
       :desc "Write unit test" "u" #'aider-write-unit-test
       :desc "Ask question" "q" #'aider-ask-question
       :desc "Start software planning" "p" #'aider-start-software-planning
       :desc "Add current file or dired marked files" "a" #'aider-add-current-file-or-dired-marked-files)

;;; <leader> & --- snippets
      (:prefix-map ("&" . "snippets")
       :desc "New snippet"           "n" #'yas-new-snippet
       :desc "Insert snippet"        "i" #'yas-insert-snippet
       :desc "Find global snippet"   "/" #'yas-visit-snippet-file
       :desc "Reload snippets"       "r" #'yas-reload-all
       :desc "Create Temp Template"  "c" #'aya-create
       :desc "Use Temp Template"     "e" #'aya-expand)

;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Big mode"                     "b" #'doom-big-font-mode
       (:when (modulep! :ui fill-column)
         :desc "Fill Column Indicator"       "c" #'+fill-column/toggle)
       :desc "Flymake"                      "f" #'flymake-mode
       :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
       :desc "Indent style"                 "I" #'doom/toggle-indent-style
       :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
       :desc "Word-wrap mode"               "w" #'+word-wrap-mode
       (:when (modulep! :checkers syntax)
         :desc "Flycheck"                   "f" #'flycheck-mode)
       (:when (modulep! :ui indent-guides)
         :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
       (:when (modulep! :ui minimap)
         :desc "Minimap mode"               "m" #'minimap-mode)
       (:when (modulep! :lang org +present)
         :desc "org-tree-slide mode"        "p" #'org-tree-slide-mode)
       :desc "Read-only mode"               "r" #'read-only-mode
       (:when (and (modulep! :checkers spell) (not (modulep! :checkers spell +flyspell)))
         :desc "Spell checker"              "s" #'spell-fu-mode)
       (:when (modulep! :checkers spell +flyspell)
         :desc "Spell checker"              "s" #'flyspell-mode)
       (:when (modulep! :lang org +pomodoro)
         :desc "Pomodoro timer"             "t" #'org-pomodoro)
       (:when (modulep! :ui zen)
         :desc "Zen mode"                   "z" #'writeroom-mode))

;;; <leader> v --- versioning
      (:prefix-map ("v" . "versioning")
       :desc "Git revert file"             "R"   #'vc-revert
       :desc "Kill link to remote"         "y"   #'browse-at-remote-kill
       :desc "Kill link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (modulep! :ui vc-gutter)
         :desc "Git revert hunk"            "r"   #'git-gutter:revert-hunk
         :desc "Git stage hunk"             "s"   #'git-gutter:stage-hunk
         :desc "Git time machine"           "t"   #'git-timemachine-toggle
         :desc "Jump to next hunk"          "n"   #'git-gutter:next-hunk
         :desc "Jump to previous hunk"      "p"   #'git-gutter:previous-hunk)
       (:when (modulep! :tools magit)
         :desc "Magit dispatch"             "/"   #'magit-dispatch
         :desc "Magit file dispatch"        "."   #'magit-file-dispatch
         :desc "Forge dispatch"             "'"   #'forge-dispatch
         :desc "Magit status"               "g"   #'magit-status
         :desc "Magit status here"          "G"   #'magit-status-here
         :desc "Magit file delete"          "x"   #'magit-file-delete
         :desc "Magit blame"                "B"   #'magit-blame-addition
         :desc "Magit clone"                "C"   #'magit-clone
         :desc "Magit fetch"                "F"   #'magit-fetch
         :desc "Magit buffer log"           "L"   #'magit-log
         :desc "Git stage file"             "S"   #'magit-stage-file
         :desc "Git unstage file"           "U"   #'magit-unstage-file
         (:prefix ("f" . "find")
          :desc "Find file"                 "f"   #'magit-find-file
          :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
          :desc "Find commit"               "c"   #'magit-show-commit
          :desc "Find issue"                "i"   #'forge-visit-issue
          :desc "Find pull request"         "p"   #'forge-visit-pullreq)
         (:prefix ("o" . "open in browser")
          :desc "Browse file or region"     "."   #'browse-at-remote
          :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
          :desc "Browse remote"             "r"   #'forge-browse-remote
          :desc "Browse commit"             "c"   #'forge-browse-commit
          :desc "Browse an issue"           "i"   #'forge-browse-issue
          :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
          :desc "Browse issues"             "I"   #'forge-browse-issues
          :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
         (:prefix ("l" . "list")
                  (:when (modulep! :tools gist)
                    :desc "List gists"               "g"   #'gist-list)
                  :desc "List repositories"         "r"   #'magit-list-repositories
                  :desc "List submodules"           "s"   #'magit-list-submodules
                  :desc "List issues"               "i"   #'forge-list-issues
                  :desc "List pull requests"        "p"   #'forge-list-pullreqs
                  :desc "List notifications"        "n"   #'forge-list-notifications)
         (:prefix ("c" . "create")
          :desc "Initialize repo"           "r"   #'magit-init
          :desc "Clone repo"                "R"   #'magit-clone
          :desc "Commit"                    "c"   #'magit-commit-create
          :desc "Fixup"                     "f"   #'magit-commit-fixup
          :desc "Issue"                     "i"   #'forge-create-issue
          :desc "Pull request"              "p"   #'forge-create-pullreq)))

;;; <leader> w --- workspaces/windows
      (:prefix-map ("w" . "workspaces/windows")
                   (:when (modulep! :ui workspaces)
                     :desc "Display workspaces"           "d" #'+workspace/display
                     :desc "Rename workspace"             "r" #'+workspace/rename
                     :desc "Create workspace"             "c" #'+workspace/new
                     :desc "Delete workspace"             "k" #'+workspace/delete
                     :desc "Save workspace"               "S" #'+workspace/save
                     :desc "Switch to other workspace"    "o" #'+workspace/other
                     :desc "Switch to left workspace"     "p" #'+workspace/switch-left
                     :desc "Switch to right workspace"    "n" #'+workspace/switch-right
                     :desc "Switch to"                    "w" #'+workspace/switch-to
                     :desc "Switch to workspace 1"        "1" #'+workspace/switch-to-0
                     :desc "Switch to workspace 2"        "2" #'+workspace/switch-to-1
                     :desc "Switch to workspace 3"        "3" #'+workspace/switch-to-2
                     :desc "Switch to workspace 4"        "4" #'+workspace/switch-to-3
                     :desc "Switch to workspace 5"        "5" #'+workspace/switch-to-4
                     :desc "Switch to workspace 6"        "6" #'+workspace/switch-to-5
                     :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
                     :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
                     :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
                     :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final)
                   :desc "Autosave session"             "a" #'doom/quicksave-session
                   :desc "Save session"                 "s" #'doom/save-session
                   :desc "Load session"                 "l" #'doom/load-session
                   :desc "Load last autosaved session"  "L" #'doom/quickload-session
                   :desc "Undo window config"           "u" #'winner-undo
                   :desc "Redo window config"           "U" #'winner-redo)

;;; <leader> m --- multiple cursors
      (:when (modulep! :editor multiple-cursors)
        (:prefix-map ("m" . "multiple-cursors")
         :desc "Edit lines"         "l"         #'mc/edit-lines
         :desc "Mark next"          "n"         #'mc/mark-next-like-this
         :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
         :desc "Mark next word"     "w"         #'mc/mark-next-like-this-word
         :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
         :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
         :desc "Mark all"           "t"         #'mc/mark-all-like-this
         :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
         :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
         :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
         :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
         :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
         :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

      ;; APPs
;;; <leader> M --- mu4e
      (:when (modulep! :email mu4e)
        (:prefix-map ("M" . "mu4e")
         :desc "Open email app" "M" #'=mu4e
         :desc "Compose email"  "c" #'+mu4e/compose))

;;; <leader> I --- IRC
      (:when (modulep! :app irc)
        (:prefix-map ("I" . "irc")
         :desc "Open irc app"       "I" #'=irc
         :desc "Next unread buffer" "a" #'tracking-next-buffer
         :desc "Quit irc"           "q" #'+irc/quit
         :desc "Reconnect all"      "r" #'circe-reconnect-all
         :desc "Send message"       "s" #'+irc/send-message
         (:when (modulep! :completion ivy)
           :desc "Jump to channel"  "j" #'+irc/ivy-jump-to-channel))))


;;
;;; Global & plugin keybinds

(map! "C-'" #'imenu

      ;;; Text scaling
      "M-+" #'doom/reset-font-size
      "M-=" #'doom/increase-font-size
      "M--" #'doom/decrease-font-size

      ;;; search
      (:when (modulep! :completion ivy)
        "C-S-s"        #'swiper
        "C-S-r"        #'ivy-resume)
      (:when (modulep! :completion helm)
        "C-S-s"        #'swiper-helm
        "C-S-r"        #'helm-resume)

      ;;; objed
      (:when (modulep! :editor objed +manual)
        "M-SPC"     #'objed-activate)

      ;;; buffer management
      (:when (modulep! :completion ivy)
        "C-x b"      #'ivy-switch-buffer
        "C-x 4 b"    #'ivy-switch-buffer-other-window)
      (:when (not (modulep! :completion ivy))
        "C-x b"       #'switch-to-buffer
        "C-x 4 b"     #'switch-to-buffer-other-window)

      (:when (modulep! :ui workspaces)
        "C-x B"       #'persp-switch-to-buffer
        "C-x b"       #'switch-to-buffer
        "C-x 4 b"     #'switch-to-buffer-other-window
        (:when (modulep! :completion ivy)
          "C-x 4 B"   #'+ivy/switch-workspace-buffer-other-window))
      "C-x C-b"     #'ibuffer
      "C-x K"       #'doom/kill-this-buffer-in-all-windows

      ;;; company-mode
      "C-:" #'+company/complete
      (:after company
       :map company-active-map
       "C-o"        #'company-search-kill-others
       "C-n"        #'company-select-next
       "C-p"        #'company-select-previous
       "C-h"        #'company-quickhelp-manual-begin
       "C-S-h"      #'company-show-doc-buffer
       "C-s"        #'company-search-candidates
       "M-s"        #'company-filter-candidates
       [C-tab]      #'company-complete-common-or-cycle
       [tab]        #'company-complete-common-or-cycle
       [backtab]    #'company-select-previous
       "C-RET"      #'counsel-company
       :map company-search-map
       "C-n"        #'company-search-repeat-forward
       "C-p"        #'company-search-repeat-backward
       "C-s"        (cmd! (company-search-abort) (company-filter-candidates)))

      ;;; ein notebooks
      (:after ein:notebook-multilang
       :map ein:notebook-multilang-mode-map
       "C-c h" #'+ein/hydra/body)

      ;;; expand-region
      "C-+"  #'er/expand-region
      "C--"  #'er/contract-region

      ;;; flycheck
      (:after flycheck
       :map flycheck-error-list-mode-map
       "C-n" #'flycheck-error-list-next-error
       "C-p" #'flycheck-error-list-previous-error
       "RET" #'flycheck-error-list-goto-error)

      ;;; help and info
      (:after help-mode
       :map help-mode-map
       "o" #'link-hint-open-link
       ">" #'help-go-forward
       "<" #'help-go-back
       "n" #'forward-button
       "p" #'backward-button)
      (:after helpful
       :map helpful-mode-map
       "o" #'link-hint-open-link)
      (:after apropos
       :map apropos-mode-map
       "o" #'link-hint-open-link
       "n" #'forward-button
       "p" #'backward-button)
      (:after info
       :map Info-mode-map
       "o" #'link-hint-open-link)

      ;;; ivy & counsel
      (:when (modulep! :completion ivy)
        (:after ivy
         :map ivy-minibuffer-map
         "TAB"   #'ivy-alt-done
         "C-g"   #'keyboard-escape-quit)
        (:after counsel
         :map counsel-ag-map
         "C-SPC" #'ivy-call-and-recenter ; preview
         "M-RET" #'+ivy/git-grep-other-window-action)
        "C-M-y"   #'counsel-yank-pop)

      ;;; neotree
      (:when (modulep! :ui neotree)
        "<f9>"    #'+neotree/open
        "<C-f9>"  #'+neotree/find-this-file
        (:after neotree
         :map neotree-mode-map
         "q"     #'neotree-hide
         "RET"   #'neotree-enter
         "SPC"   #'neotree-quick-look
         "v"     #'neotree-enter-vertical-split
         "s"     #'neotree-enter-horizontal-split
         "c"     #'neotree-create-node
         "D"     #'neotree-delete-node
         "g"     #'neotree-refresh
         "r"     #'neotree-rename-node
         "R"     #'neotree-refresh
         "h"     #'+neotree/collapse-or-up
         "l"     #'+neotree/expand-or-open
         "n"     #'neotree-next-line
         "p"     #'neotree-previous-line
         "N"     #'neotree-select-next-sibling-node
         "P"     #'neotree-select-previous-sibling-node))

      ;;; popups
      (:when (modulep! :ui popup)
        "C-x p"   #'+popup/other
        "C-`"     #'+popup/toggle
        "C-~"     #'+popup/raise)

      ;;; smartparens
      (:after smartparens
       :map smartparens-mode-map
       "C-M-a"         #'sp-beginning-of-sexp
       "C-M-e"         #'sp-end-of-sexp
       "C-M-f"         #'sp-forward-sexp
       "C-M-b"         #'sp-backward-sexp
       "C-M-d"         #'sp-splice-sexp
       "C-M-k"         #'sp-kill-sexp
       "C-M-t"         #'sp-transpose-sexp
       [C-M-backspace] #'sp-backward-kill-sexp
       )

      ;;; treemacs
      (:when (modulep! :ui treemacs)
        "<f9>"   #'+treemacs/toggle
        "<C-f9>" #'treemacs-find-file))

(map! :leader
      (:when (modulep! :editor fold)
        (:prefix ("C-F" . "fold")
                 "C-d"     #'vimish-fold-delete
                 "C-a C-d" #'vimish-fold-delete-all
                 "C-f"     #'+fold/toggle
                 "C-a C-f" #'+fold/close-all
                 "C-u"     #'+fold/open
                 "C-a C-u" #'+fold/open-all)))

(map! "M-g"     #'goto-line
      "C-s"     #'swiper
      "C-x C-m" #'execute-extended-command
      "C-a"     #'beginning-of-line-text
      "C-A"     #'beginning-of-line
      "C-e"     #'end-of-line
      "C-."     #'avy-goto-char-timer
      "M-."     #'embark-act
      "C-;"     #'kill-whitespace
      "C-!"     #'eshell-here
      "C-M-p"   #'mc/mark-previous-like-this
      "C-c d"   #'duplicate-line
      "C-c t"   #'toggle-comment-on-line
      "C-c o"   #'open-line-below
      "C-c O"   #'open-line-above
      "C-c r"   #'rename-file-and-buffer
      "C-c m"   #'move-buffer-file
      "C-c u"   #'untabify-this-file
      "C-c y"   #'yank-buffer-path
      "C-c c"   #'copy-line
      "C-c w"   #'copy-word
      "C-c n"   #'open-and-indent-line
      "C-c N"   #'open-and-indent-previous-line
      "C-M-n"   #'mc/mark-next-like-this
      "M-p"     #'mc/mark-previous-lines
      "M-n"     #'mc/mark-next-lines
      "C-x b"   #'ivy-switch-buffer
      "C-M-SPC" #'mark-whole-sexp
      [C-M-backspace] #'backward-kill-sexp)

(global-unset-key [down-mouse-1])
(global-unset-key [mouse-1])
(global-unset-key [down-mouse-2])
(global-unset-key [mouse-2])

(map! :map paredit-mode-map
      "C-c }" #'paredit-forward-barf-sexp
      "C-c {" #'paredit-backward-barf-sexp
      "C-}"  #'paredit-forward-barf-sexp
      "C-{"  #'paredit-backward-barf-sexp
      "C-c )" #'paredit-forward-slurp-sexp
      "C-c (" #'paredit-backward-slurp-sexp
      "C-)"  #'paredit-forward-slurp-sexp
      "C-("  #'paredit-backward-slurp-sexp)

(provide 'bindings)
;;; bindings.el ends here
