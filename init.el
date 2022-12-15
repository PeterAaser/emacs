;; environment
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-unix* (not *is-windows*))

;; fonts
(defconst *mono-font-family*
  (if *is-windows* "JetBrainsMono NF" "GoMono Nerd Font"))
(defconst *mono-font-height*
  (if *is-windows* 90 90))
(defconst *serif-font-family*
  (if *is-windows* "Georgia" "IBM Plex Serif"))
(defconst *serif-font-height*
  (if *is-windows* 110 110))
(defconst *project-dir* (expand-file-name "~/git"))

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
(if *is-windows*
  (set-w32-system-coding-system 'utf-8))
(set-buffer-file-coding-system 'utf-8)

;; no ~#hurr.swp shit por favor
(use-package no-littering
  :demand t
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))


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
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-create-definer leader-def
    :states '(normal motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (leader-def
    "" '(:ignore t :wk "leader")
    "f" '(:ignore t :wk "file")
    "c" '(:ignore t :wk "checks")
    "t" '(:ignore t :wk "toggle")
    "b" '(:ignore t :wk "buffer")
    "bd" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bx" 'kill-buffer-and-window
    "s" '(:ignore t :wk "straight")
    "sf" 'straight-x-fetch-all
    "sp" 'straight-x-pull-all
    "sr" 'straight-remove-unused-repos
    "ss" 'straight-get-recipe)

(general-create-definer localleader-def
  :states '(normal motion emacs)
  :keymaps 'override
  :prefix "SPC m"
  :non-normal-prefix "C-SPC m")
(localleader-def "" '(:ignore t :wk "mode")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rice trash

(use-package leuven-theme
  :defer t)
;
;(use-package vivid-theme
;  :straight (:host github :repo "websymphony/vivid-theme")
;  :defer t)
;
;(use-package doom-themes
;  :defer t
;  :config
;  (doom-themes-visual-bell-config)
;  (doom-themes-treemacs-config)
;  (doom-themes-org-config)
;  (doom-themes-set-faces nil
;    ;; extending faces breaks orgmode collapsing for now
;   '(org-block-begin-line :extend nil)
;   '(org-block-end-line :extend nil)
;    ;; different sized headings are nice.
;   '(outline-1 :height 1.3)
;   '(outline-2 :height 1.1)
;   '(outline-3 :height 1.0)))
;
;(use-package modus-themes
;  :defer t
;  :custom
;  (modus-themes-italic-constructs t)
;  (modus-themes-intense-markup t)
;  (modus-themes-mode-line '(borderless moody))
;  (modus-themes-tabs-accented t)
;  (modus-themes-completions
;   '((matches . (extrabold background intense))
;     (selection . (semibold accented intense))
;     (popup . (accented))
;     (t . (extrabold intense))))
;  (modus-themes-org-blocks 'tinted-background)
;  (modus-themes-mixed-fonts t)
;  (modus-themes-headings
;      '((1 . (rainbow))
;        (2 . (rainbow))
;        (3 . (rainbow))
;        (t . (monochrome)))))
;
;(defun me/init-theme ()
;  (load-theme 'modus-operandi t))
;
;(add-hook 'emacs-startup-hook #'me/init-theme)
;
;;; fonts something-something
;(use-package persistent-soft
;  :demand t)
;(use-package unicode-fonts
;  :demand t
;  :after persistent-soft
;  :config
;  (unicode-fonts-setup)
;  (custom-set-faces
;   `(default ((t (:family ,*mono-font-family*
;                  :height ,*mono-font-height*))))
;   `(variable-pitch ((t (:family ,*serif-font-family*
;                         :height ,*serif-font-height*))))))


(use-package all-the-icons
  :demand t)
(use-package all-the-icons-dired
  :defer 1
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
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

(use-package anzu
  :defer 1
  :after isearch
  :config
  (global-anzu-mode 1))

(use-package minions
  :defer 1
  :config
  (minions-mode 1))

(use-package doom-modeline
  :demand t
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-height 28)
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

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer 1
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))
  (global-ligature-mode t))

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
