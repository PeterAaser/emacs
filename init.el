;;; package --- Anus.
;;; Commentary:

;;; -*- lexical-binding: t; -*-

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:

;; environment
(use-package better-defaults
  :straight (better-defaults :type git :host nil :repo "https://git.sr.ht/~technomancy/better-defaults")
  :demand t)

(setq default-directory "~/"
      ;; always follow symlinks when opening files
      vc-follow-symlinks t
      ;; overwrite text when selected, like we expect.
      delete-seleciton-mode t
      ;; quiet startup
      inhibit-startup-message t
      initial-scratch-message nil
      ;; hopefully all themes we install are safe
      custom-safe-themes t
      ;; simple lock/backup file management
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      ;; when quiting emacs, just kill processes
      confirm-kill-processes nil
      ;; ask if local variables are safe once.
      enable-local-variables t
      ;; life is too short to type yes or no
      use-short-answers t

      ;; clean up dired buffers
      dired-kill-when-opening-new-dired-buffer t)

;; use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; always highlight code
(global-font-lock-mode 1)
;; refresh a buffer if changed on disk
(global-auto-revert-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(use-package no-littering
  :demand t
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Vim!
(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode))
(use-package evil
  :demand t
  :after undo-tree
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :demand t
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :demand t
  :after evil org
  :hook ((org-mode . evil-org-mode))
  :config
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package helm
  :demand t
  :straight t)
(global-set-key (kbd "C-S-p") 'helm-M-x)

(use-package helm-projectile
  :demand t
  :straight t)

;; which-key pops up a nice window whenever we hesitate about a keyboard shortcut, 
;; and shows all the possible keys we can press. Popularized by Spacemacs and Doom-Emacs, 
;; we can now configure absurd key combinations, forget about them, and then be 
;; delighted to discover them again!
(use-package which-key
  :demand t
  :after evil
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (set-face-attribute
    'which-key-local-map-description-face nil :weight 'bold))

(use-package general
  :demand t
  :config
  (general-evil-setup t))
  (general-create-definer leader-def
    :states '(normal motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

(leader-def
  "" '(:ignore t :wk "leader")
  "f" '(:ignore t :wk "file")
  "ff" 'helm-find-files
  "c" '(:ignore t :wk "checks")
  "t" '(:ignore t :wk "toggle")
  "b" '(:ignore t :wk "buffer")
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bB" 'me/scratch-new
  "s" '(:ignore t :wk "straight")
  "sf" 'straight-x-fetch-all
  "sp" 'straight-x-pull-all
  "sr" 'straight-remove-unused-repos
  "ss" 'straight-get-recipe)

(general-define-key
 :states '(normal visual)
 "a" 'evil-backward-char
 "h" 'evil-forward-char
 "e" 'evil-next-line
 "o" 'evil-previous-line
 "C-h" 'next-window-any-frame
 "C-a" 'previous-window-any-frame)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rice trash
(set-face-attribute 'default nil :height 180)

(global-linum-mode 1)

(use-package doom-themes
  :defer t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (doom-themes-set-faces nil
    ;; extending faces breaks orgmode collapsing for now
   '(org-block-begin-line :extend nil)
   '(org-block-end-line :extend nil)
    ;; different sized headings are nice.
   '(outline-1 :height 1.3)
   '(outline-2 :height 1.1)
   '(outline-3 :height 1.0)))

(defun me/init-theme ()
  "."
  (load-theme 'doom-gruvbox t))

(add-hook 'emacs-startup-hook #'me/init-theme)

(use-package all-the-icons
  :demand t)

; (use-package all-the-icons-dired
;   :defer 1
;   :after all-the-icons
;   :hook (dired-mode . all-the-icons-dired-mode))
(use-package treemacs-all-the-icons
  :defer 1
  :after all-the-icons treemacs
  :config
  (treemacs-load-theme "all-the-icons"))
(use-package all-the-icons-completion
  :defer 1
  :after all-the-icons
  :config
  (add-hook 'marginalia-mode-hook
            #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode 1))

(use-package dashboard
  :demand t
  :after all-the-icons projectile
  :if (< (length command-line-args) 2)
  :custom
  ;; show in `emacsclient -c`
  (initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*")))
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :demand t
  :custom
  ;; (inhibit-compacting-font-caches t)
  ;; (doom-modeline-height 28)
  ;; 1 minor mode will be shown thanks to minions
  (doom-modeline-minor-modes t)
  (doom-modeline-hud t)
  :config
  (doom-modeline-mode 1))

(setq fast-but-imprecise-scrolling t
      jit-lock-defer-time 0)

(use-package fast-scroll
  :defer 1
  :hook
  (fast-scroll-start . (lambda () (flycheck-mode -1)))
  (fast-scroll-end . (lambda () (flycheck-mode 1)))
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(defun me/reload-init ()
  "Reload init.el."
  (interactive)
  (message "Reloading init.el...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init.el... done."))

(use-package restart-emacs
  :commands restart-emacs
  :general
  (leader-def
    "q" '(:ignore t :wk "exit emacs")
    "qR" 'restart-emacs
    "qn" 'restart-emacs-start-new-emacs
    "qr" 'me/reload-init))

(use-package projectile
  :demand t
  :general
  (leader-def
    "p" '(:ignore t :wk "project")
    "pP" 'projectile-switch-project
    "pd" 'projectile-dired
    "pb" 'projectile-switch-to-buffer
    "pf" 'projectile-find-file
    "p/" 'projectile-ripgrep)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-sort-order 'recently-active)
  (projectile-indexing-method 'native)
  :config
  (projectile-mode +1))

(use-package treemacs
  :defer 2
  :commands treemacs treemacs-find-file
  :general
  (leader-def
    "tt" 'treemacs
    "tf" 'treemacs-find-file))
(use-package treemacs-evil
  :defer 1
  :after treemacs evil)
(use-package treemacs-projectile
  :defer 1
  :after treemacs projectile)
(use-package treemacs-magit
  :defer 1
  :after treemacs-magit)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; treat camel-cased words as individual words.
(add-hook 'prog-mode-hook 'subword-mode)
;; don't assume sentences end with two spaces after a period.
(setq sentence-end-double-space nil)
;; show matching parens
(show-paren-mode t)
(setq show-paren-delay 0.0)
;; limit files to 80 columns. Controversial, I know.
(setq-default fill-column 80)
;; handle very long lines without hurting emacs
(global-so-long-mode)

(use-package key-chord)

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)

(key-chord-define evil-insert-state-map "vk" 'evil-normal-state)

(use-package helpful
  :defer 1
  :general
  (leader-def
    "h" '(:ignore t :wk "help")
    "hh" 'helpful-symbol
    "hf" 'helpful-function
    "hv" 'helpful-variable
    "hk" 'helpful-key
    "ho" 'helpful-at-point)
  :config
  (add-to-list 'display-buffer-alist
               '("*[Hh]elp"
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window))))

(use-package info-colors
  :defer 1
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; Scratch buffer
(defun me/scratch-new ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
  (switch-to-buffer (get-buffer-create bufname))
  (if (= n 1) initial-major-mode))) ; 1, because n was incremented


(provide 'init)

;; Where the fuck else would it have ended??
;;; init.el ends here

