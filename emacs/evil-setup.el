;;; evil-leader - ViM leader key for evil-mode
;;; https://github.com/cofi/evil-leader
(require 'evil-leader)
(evil-leader/set-leader ",")

;; Evil mode tries to take away my tab key
(setq evil-want-C-i-jump nil)
(require 'evil)
(evil-mode 1)
(global-set-key (kbd "C-l") 'evil-mode)
(global-set-key (kbd "C-L") 'evil-normal-state)

(require 'key-chord)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-jumper)
(require 'evil-matchit)
(global-evil-matchit-mode 1)
;; (require 'evil-rsi)
(require 'evil-nerd-commenter)
(require 'evil-little-word)
;; (require 'evil-mode-line)
(require 'evil-escape)
(require 'evil-leader)
(require 'evil-god-state)

(global-evil-leader-mode 1)
;; keyboard shortcuts
(evil-leader/set-key
  "a" 'ag-project
  "A" 'ag
  "b" 'ido-switch-buffer
  "B" 'ido-switch-buffer-other-window
  "c" 'mc/mark-next-like-this
  "C" 'mc/mark-all-like-this
  "d" 'dired-jump
  "e" 'er/expand-region
  "E" 'mc/edit-lines
  "f" 'ido-find-file
  "G" 'magit-blame-mode
  "i" 'idomenu
  "j" 'ace-jump-mode
  "J" 'ace-jump-word-mode
  "k" 'kill-this-buffer
  "K" 'kill-buffer
  "l" 'linum-mode
  "o" 'occur
  "O" 'browse-url
  "ge" 'g4-edit
  "gd" 'g4-diff
  "go" 'g4-opened-ido
  "gO" 'g4-opened
  "gp" 'g4-pending
  "gC" 'g4-client
  "P" 'popwin:popup-last-buffer
  "r" 'bw/recentf-ido-find-file
  "R" 'bookmark-jump
  "s" 'ag-project
  "t" 'bw/open-term
  "T" 'eshell
  "w" 'save-buffer
  "xx" 'smex
  "x1" 'delete-other-windows
  "x0" 'delete-window
  "x3" 'split-window-right
  "x2" 'split-window-below
  "xo" 'other-window
  "xk" 'kill-buffer
  "xK" 'kill-buffer-and-window
  "y" 'bury-buffer)

;; TODO(jaigupta): I should be moving some of the mappings from evil leader mode to
;; evil normal state so that i can use them easily.

(setq
 ;; this stops evil from overwriting the cursor color
 evil-default-cursor t
 ;; h/l wrap around to next lines
 evil-cross-lines t
 evil-default-state 'normal
 ;; include first/last character when moving to e/bol
 evil-want-visual-char-semi-exclusive t
 ;; don't move the cursor around like Vim
 evil-move-cursor-back nil
 )

;; use ido to open files
(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)

;; Make C-g work like <esc>
(define-key evil-normal-state-map "\C-g" 'evil-normal-state)
(define-key evil-visual-state-map "\C-g" 'evil-normal-state)
(define-key evil-insert-state-map "\C-g" 'evil-normal-state)

;; TODO(jaigupta): Probably the existing evil-escape package already
;; provides for all these bindings.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)


;; evil integration for indirect regions
;; from: http://demonastery.org/2013/04/emacs-evil-narrow-region/
(evil-define-operator evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region-indirect beg end))
(define-key evil-normal-state-map "m" 'evil-narrow-indirect)
(define-key evil-visual-state-map "m" 'evil-narrow-indirect)

;; on OSX, stop copying each visual state move to the clipboard:
;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
;; Most of this code grokked from:
;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
(defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
  (unless (featurep 'ns)
    ad-do-it))

;; modes to map to different default states
(dolist (mode-map '((ag-mode . emacs)
                    (cider-repl-mode . emacs)
                    (comint-mode . emacs)
                    (eshell-mode . emacs)
                    ;; (fundamental-mode . emacs)
                    (git-commit-mode . insert)
                    (git-rebase-mode . emacs)
                    (help-mode . emacs)
                    (paradox-menu-mode . emacs)
                    (term-mode . emacs)))
  (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

(autoload 'evil-mode "evil" "Emacs Vi emuLation" t)

;;; linum-relative - makes Evil behave like:
;;; https://github.com/myusuf3/numbers.vim
;;; https://github.com/coldnew/linum-relative
(require 'linum-relative)
(defun bw/linum-non-relative (line-number)
  "Linum formatter that copies the format"
  (propertize (format linum-relative-format line-number)
              'face 'linum))

(defun bw/linum-relative-formatting ()
  "Turn on relative formatting"
  (setq-local linum-format 'linum-relative))

(defun bw/linum-normal-formatting ()
  "Turn on non-relative formatting"
  (setq-local linum-format 'bw/linum-non-relative))

;; I never use linum-mode except for this, so it's okay to
;; clobber it
(setq linum-format 'bw/linum-non-relative
      ;; show >> on line where cursor is
      linum-relative-current-symbol ">>")

;; in Normal mode, use relative numbering
(add-hook 'evil-normal-state-entry-hook 'bw/linum-relative-formatting)
;; in Insert mode, use normal line numbering
(add-hook 'evil-insert-state-entry-hook 'bw/linum-normal-formatting)
;; turn off linum mode automatically when entering Emacs mode
;; (add-hook 'evil-emacs-state-entry-hook 'bw/disable-linum-mode)
;; turn off linum mode when entering Emacs
(add-hook 'evil-emacs-state-entry-hook 'bw/linum-normal-formatting)

;; copy linum face so it doesn't look weird
(set-face-attribute 'linum-relative-current-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground) :background nil :inherit 'linum :bold t)

(require 'linum-relative)

;; abbrev comes with evil
(diminish 'abbrev-mode)

;; evil-surround - Emacs version of surround.vim
;; https://github.com/timcharper/evil-surround
(require 'evil-surround)

(require 'evil-org)

;; evil-god-state - One-shot god-mode from a leader key in evil
;; https://github.com/gridaphobe/evil-god-state
(require 'evil-god-state)
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)

(global-evil-leader-mode 1)

(provide 'evil-setup)
