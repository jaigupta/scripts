;;; evil-leader - ViM leader key for evil-mode
;;; https://github.com/cofi/evil-leader
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")

;; Evil mode tries to take away my tab key
(setq evil-want-C-i-jump nil)
;; C-u should scroll up like vim
(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)

(require 'evil-escape)
(require 'evil-god-state)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-little-word)
(require 'evil-matchit)
(require 'evil-nerd-commenter)
(require 'evil-org)
(require 'evil-surround)
(require 'key-chord)
(require 'linum-relative)

;; I need emacs at times. C-e to switch between the two modes.
(global-set-key (kbd "C-e") 'evil-mode)
(define-key evil-normal-state-map (kbd "C-e") 'evil-mode)


(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)

(global-evil-leader-mode 1)
;; keyboard shortcuts
(evil-leader/set-key
  "a" 'ag-project
  "A" 'ag
  "bb" 'ido-switch-buffer
  "bc" 'kill-buffer ;; buffer close
  "bd" 'kill-buffer-and-window ;; buffer delete
  "bj" 'previous-buffer
  "bk" 'next-buffer
  "bo" 'ido-switch-buffer-other-window
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
  "xx" 'smex
  "w1" 'delete-other-windows
  "w0" 'delete-window
  "w3" 'split-window-right
  "w2" 'split-window-below
  "wo" 'other-window
  "y" 'bury-buffer
)

;; TODO(jaigupta): I should be moving some of the mappings from evil leader mode to
;; evil normal state so that i can use them easily.

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

;; Window movement keys
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "B") 'ido-switch-buffer)

(setq
 ;; this stops evil from overwriting the cursor color
 evil-default-cursor t
 ;; h/l wrap around to next lines
 ;; evil-cross-lines t
 evil-default-state 'normal
 ;; include first/last character when moving to e/bol
 evil-want-visual-char-semi-exclusive t
 ;; don't move the cursor around like Vim
 ;; evil-move-cursor-back nil
 )

;; This is wonderful! Pretty sure this is not something that can
;; can be configured in Vim.
(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "x " 'smex)

;; Make C-g work like <esc>
(define-key evil-normal-state-map "\C-g" 'evil-normal-state)
(define-key evil-visual-state-map "\C-g" 'evil-normal-state)
(define-key evil-insert-state-map "\C-g" 'evil-normal-state)

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
                    ;; (eshell-mode . emacs)
                    (fundamental-mode . insert)
                    (git-commit-mode . insert)
                    (git-rebase-mode . emacs)
                    ;; (help-mode . emacs)
                    (paradox-menu-mode . emacs)
                    (term-mode . insert)))
  (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

(autoload 'evil-mode "evil" "Emacs Vi emuLation" t)

(add-hook 'text-mode 'linum-mode)
(defun bw/linum-relative-formatting ()
  "Turn on relative formatting"
  (setq-local linum-format 'linum-relative))

(defun bw/linum-normal-formatting ()
  "Turn on non-relative formatting"
  (setq-local linum-format 'bw/linum-non-relative))

(setq linum-format 'bw/linum-non-relative
      ;; show the actual line number at the current line.
      linum-relative-current-symbol "")

;; in Normal mode, use relative numbering
(add-hook 'evil-normal-state-entry-hook 'bw/linum-relative-formatting)
;; In Insert mode, use normal line numbering
(add-hook 'evil-insert-state-entry-hook 'bw/linum-normal-formatting)
;; turn on linum mode when entering Emacs
(add-hook 'evil-emacs-state-entry-hook 'bw/linum-normal-formatting)

(global-evil-leader-mode 1)

(provide 'evil-setup)
