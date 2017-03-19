(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")) ; Org-mode's repository
(package-initialize)

(add-to-list 'load-path "~/myprojects/scripts/emacs/")

(setq-default tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black"
    "#d55e00"
    "#009e73"
    "#f8ec59"
    "#0072b2"
    "#cc79a7"
    "#56b4e9"
    "white"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-backends 
   (quote
    (company-emacs-eclim
     company-elisp
     company-bbdb
     company-css
     company-semantic
     company-clang
     company-xcode
     company-cmake
     company-go
     company-capf
     (company-dabbrev-code
      company-gtags
      company-etags
      company-keywords)
     company-oddmuse
     company-files
     company-dabbrev
     company-abbrev)))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-options-version "2.40")
 '(eclim-eclipse-dirs jai-eclim-executable)
 '(eclim-executable jai-eclim-executable)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "american")
 '(menu-bar-no-scroll-bar t)
 '(org-startup-folded (quote content))
 '(python-guess-indent nil)
 '(python-indent 2 t)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(text-scale-mode-step 1.2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
;; '(tramp-syntax (quote url))
 '(truncate-lines nil))


(global-auto-revert-mode t)
(set-frame-font "Liberation Mono-14:antialias=1")

(require 'ace-jump-mode)
(global-set-key "\C-cs" 'ace-jump-mode)

(column-number-mode t)
(setq js-indent-level 2)
;; For starting multiple emacs daemon
;; (setq server-use-tcp t)
;; Currently using setup for only one daemon
;;(server-start)
;; Now start server using emacs --daemon=<server-name>
;; connect to the server using emacsclient --server-file=<server-name> -c -n


;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m a") 'mc/edit-lines)
(global-set-key (kbd "C-c m >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m !") 'mc/mark-all-like-this)

;; highlight matching parenthesis
(show-paren-mode)

;; org-mode
(require 'org)
(setq org-support-shift-select 'always)

;; highlight word on idle
(require 'idle-highlight-mode)
(idle-highlight-mode 1)

;; Please backups should not be in my directories.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ido-mode
(require 'ido)
(require 'ido-ubiquitous)
(require 'smex)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere nil)
;; Smex (autocomplete for emacs commands) should be initilized on first call instead.
;; (smex-initialize)
;; Auto update smex's command index when emacs has been inactive for 300s
(smex-auto-update 300)
(global-set-key (kbd "M-x") 'smex)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; (require 'flex-isearch)
;; (require 'flx-ido)
;; (flx-ido-mode t)

;; vertical mode for ido
(require 'ido-vertical-mode)
(ido-vertical-mode t)

;; ido should handle yes or no questions
(ido-yes-or-no-mode t)

;; popups for trying things
(require 'popup)
;; (popup-menu '(0 0) '("title" ("v1" . 1) ("v2" . 2)))
;; (easy-menu-define my-menu nil "My own menu"
;;   '("My Stuff"
;;     ["ERC" erc t]
;;     ["Tetris" tetris nil]))
;; (popup-menu* my-menu)

;; SAVE packages for later
;; save packages in scripts/settings/emacs-packages
;; commands: save-packages, install-saved-packages
(require 'save-packages)

;; Using tabs for indentation is a blunder
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(require 'expand-region)
(global-set-key (kbd "C-x SPC") 'er/expand-region)
(global-set-key (kbd "C-@") 'set-mark-command)

;; I dont like C-x C-s for saving
;; Many editors and browsers kill line/text with C-x.
;; This is painful when you actually meant to save file.
;; C-c is copy. So does not have any significant side effect.
(global-set-key (kbd "C-c C-s") 'save-buffer)

;; highlight words after certain period of time.
(require 'idle-highlight-mode)

;; Merge the clipboard with system clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Use Common user interface bindings
;; To enter an Emacs command like C-x C-f while the mark is active, use one of the following
;; methods: either hold Shift together with the prefix key, e.g., S-C-x C-f, or quickly type
;; the prefix key twice, e.g., C-x C-x C-f.

;; (cua-mode t)

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'js-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'borg-mode (font-lock-width-keyword 80))

;; list-buffers doesn't switch to buffer window.
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

;; Better names for duplicate buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; InteractivelyDoThings - buffer and command fuzzy matching
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)


(global-set-key [f9] 'compile)
(global-set-key [f10] 'toggle-truncate-lines)

;; (global-set-key (kbd "<mouse-6>") 'ignore)
;; (global-set-key (kbd "<mouse-7>") 'ignore)
;; (global-set-key [mode-line mouse-2] 'ignore)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; save history between sessions
(setq savehist-additional-variables                        ;; also save...
      '(search-ring regexp-search-ring compile-history)    ;; search and history
      savehist-file "~/.emacs.d/savehist")                 ;; where to save
(savehist-mode t)                                          ;; do customization before activate

;; enable multi buffer interactive search
;; (load "far-search")

(setq visible-bell nil)
(setq mouse-autoselect-window t)

(setq fill-column 80)               ;; 80 columns
(setq c-basic-indent 2)             ;; c indents are 4 chars wide
(setq c-basic-offset 2)             ;; c offsets are 4 chars wide
(setq-default python-indent 2)
(setq-default py-indent-offset 2)

;; F12 switch to ansi-term
(defun visit-ansi-term ()
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash")))
(defun switch-to-ansi-term ()
  "Switch to ansi-term buffer (start new session if required)"
  (interactive)
  (let ((ansibuf (get-buffer-window "*ansi-term*")))
    (if ansibuf
        (select-window ansibuf)
      (visit-ansi-term)
      )))
(global-set-key [f12] 'switch-to-ansi-term)

;; Shift-F12 switch to terminal and cd to current buffer
(defun switch-to-ansi-term-cd ()
  (interactive)
  (let ((d (file-name-directory buffer-file-name)))
    (switch-to-ansi-term)
    (term-send-raw-string (concat "cd " d "\n"))))
(global-set-key [C-f12] 'switch-to-ansi-term-cd)

(if (eq system-type 'darwin) (set-face-attribute 'default nil :height 130))

(setq show-ws-toggle-show-trailing-whitespace t)
(setq show-ws-toggle-show-tabs t)

;; Delete extra blank lines at the end of a file
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))


;; Add a hook to delete extra blank lines when a file is written out.
(add-hook 'write-file-hooks 'delete-trailing-blank-lines)

;; choose google chrome as my browser. use M-x google-search for search
(setq browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "google-chrome")

(require 'eclim)
(global-eclim-mode)

(require 'eclimd)
;; (require 'eclim-ido)
;; (require 'eclim-postip)

;; enable viewing local help in status. You can also call display-local-help
;; manually every time but its better to see the help automatically.
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/elpa/yasnippet-20150415.244/snippets"
        "~/.emacs.d/snippets"
       ))
(yas-global-mode 1)

(require 'company)
(global-company-mode t)
(require 'company-go)
(require 'company-dabbrev)
(require 'company-emacs-eclim)

(setq-default company-minimum-prefix-length 1)
(global-set-key "\M-/" 'company-complete)

(require 'color)

;; (add-hook
;;  'python-mode-hook
;;  (lambda ()
;;    (unless (eq major-mode 'google3-build-mode)
;;      (add-hook 'before-save-hook 'google-pyformat nil t))))

;; (when window-system ;; not just X
;;     (speedbar 1))

;; (require 'ecb)
;; (require 'ecb-autoloads)

;; (setq ecb-layout-name "left3")

;; (setq ecb-show-sources-in-directories-buffer 'always)

;; IBuffer grouping
(setq ibuffer-saved-filter-groups
          (quote (("default"
                   ;; ("dired" (mode . dired-mode))
                   ;; ("perl" (mode . cperl-mode))
                   ("java" (mode . java-mode))
                   ("python" (mode . python-mode))
                   ;; ("erc" (mode . erc-mode))
                   ("planner" (or
                               (name . "^\\*Calendar\\*$")
                               (name . "^diary$")
                               (mode . muse-mode)
                               (mode . org-mode)))
                   ("emacs" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")
                             (name . "^\\*.*$")))
                   ("gnus" (or
                            (mode . message-mode)
                            (mode . bbdb-mode)
                            (mode . mail-mode)
                            (mode . gnus-group-mode)
                            (mode . gnus-summary-mode)
                            (mode . gnus-article-mode)
                            (name . "^\\.bbdb$")
                            (name . "^\\.newsrc-dribble")))))))

(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

(require 'evil-setup)

;; desktop.el's default sessions sucks!
(require 'desktop)
(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)

(defun my-desktop-switch-session ()
  "Open a desktop session and save it so that we dont block the file."
  (interactive)
  (my-desktop-save-and-clear)
  (my-desktop-read))

(defun my-desktop-free-session ()
  "Free the lock file for an existing session."
  (interactive)
  (let ((session-to-claim (my-desktop-get-session-name "Session to claim" t)))
    (setq session-directory (concat my-desktop-session-dir session-to-claim))
    (desktop-release-lock session-directory)))

(defun my-desktop-claim-session ()
  "Claim the lock file for an existing session."
  (interactive)
  (let ((session-to-claim (my-desktop-get-session-name "Session to claim" t)))
    (setq session-directory (concat my-desktop-session-dir session-to-claim))
    (desktop-release-lock session-directory)
    (my-desktop-read session-to-claim)))

;; Some files should just stay here and not close with session.
(setq desktop-clear-preserve-buffers
      (append '("\\*.*\\..*\\*" "#.*" ".*@.*")
              '("\\*-jabber.*\\*")
              '("^.*\\.org$") ;; Org files should not belong to sessions.
              '("^.emacs$")
              '("^.zshrc$")
              desktop-clear-preserve-buffers))

;; Remove the scroll bars; they are useless
(scroll-bar-mode -1)

(require 'yafolding)
;; Context menu discovery
(require 'discover)

(add-hook 'prog-mode-hook (lambda() (yafolding-mode)))

(global-set-key (kbd "<C-return>") 'yafolding-toggle-element)

(global-set-key (kbd "C-c e c") 'eclim-problems-correct)
(global-set-key (kbd "C-c e p") 'eclim-problems-buffer-refresh)
(global-set-key (kbd "C-c e o") 'eclim-java-import-organize)
(global-set-key (kbd "C-c e r") 'eclim-java-refactor-rename-symbol-at-point)
(global-set-key (kbd "C-c e d") 'eclim-java-show-documentation-for-current-element)
(global-set-key (kbd "C-c e f") 'eclim-file-locate)

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun run-on-zsh (&optional command)
  (interactive)
  (if command nil
    (setq command (read-string "zsh: $ ")))
  (async-shell-command (concat "zsh -c 'source ~/.zshrc; " command  "'")))

(defun close-other-window ()
  (interactive)
  (other-window 1)
  (delete-window))

(global-set-key (kbd "\C-x !") 'close-other-window)

;; YAS expand advice add.
(defun yas-advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas-try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas-fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas-expand))
               ad-do-it)))))

(yas-advise-indent-function 'c-indent-line-or-region)
(yas-advise-indent-function 'indent-for-tab-command)

(define-key company-active-map "\t" 'company-yasnippet-or-completion)
 
(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))
 
(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
    (first (yas/current-key)))

(defun java-mode-macros ()
  (fset 'm_pf
        "\C-a\C-iprivate final \C-e")
  (fset 'm_psf
        "\C-a\C-iprivate static final \C-e"))

(add-hook 'java-mode-hook 'java-mode-macros)
(add-hook 'java-mode-hook 'eclim-mode)

(setq vc-follow-symlinks t)
(setq tabbar-background-color "#888888") ;; the color of the tabbar background
(require 'powerline)
;; (powerline-default-theme)
(powerline-center-evil-theme)

(menu-bar-mode 0)

(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
;; (blink-cursor-mode 0)

;; Smex should insert '-' when space is pressed.
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

(require 'pos-tip)

(require 'helm)
(require 'helm-spotify)
(helm-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(defun spotify-patched ()
  "wrapper for calling spotify from keyboard shortcut and removing possibility for error"
  (interactive)
  (setq debug-on-error t)
  (helm-spotify)
  (setq debug-on-error nil))

;; ERC using SSL: see http://www.emacswiki.org/emacs/ErcSSL
(require 'tls)

;; M-x start-irc
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.corp.google.com" :port 6697))

(require 'recentf)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(setq interprogram-paste-function 'x-selection-value)

;; Open some useful buffers by default
(find-file "~/.emacs")
