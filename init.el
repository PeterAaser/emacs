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
;(use-package evil-org
;  :demand t
;  :after evil org
;  :hook (org-mode . evil-org-mode)
;  :config
;  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
;  (require 'evil-org-agenda)
;  (evil-org-agenda-set-keys))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VIM BINDS
(general-define-key
 :states 'motion
 ;"a" 'evil-forward-char
 "e" 'evil-next-line
 "o" 'evil-previous-line
                                        ;"h" 'evil-backward-char
 )


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

(use-package vivid-theme
  :straight (:host github :repo "websymphony/vivid-theme")
  :defer t)

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

(use-package modus-themes
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-intense-markup t)
  (modus-themes-mode-line '(borderless moody))
  (modus-themes-tabs-accented t)
  (modus-themes-completions
   '((matches . (extrabold background intense))
     (selection . (semibold accented intense))
     (popup . (accented))
     (t . (extrabold intense))))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings
      '((1 . (rainbow))
        (2 . (rainbow))
        (3 . (rainbow))
        (t . (monochrome)))))

(defun me/init-theme ()
  "."
  (load-theme 'doom-gruvbox t))

(add-hook 'emacs-startup-hook #'me/init-theme)

;;; fonts something-something
(use-package persistent-soft
  :demand t)

; (use-package unicode-fonts
;   :demand t
;   :after persistent-soft
;   :config
;   (unicode-fonts-setup)
;   (custom-set-faces
;    `(default ((t (:family ,*mono-font-family*
;                   :height ,*mono-font-height*))))
;    `(variable-pitch ((t (:family ,*serif-font-family*
;                          :height ,*serif-font-height*))))))


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

;; A more complex, more lazy-loaded config
(use-package solaire-mode
  :defer 1
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  ((change-major-mode . turn-on-solaire-mode))
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  ((after-revert . turn-on-solaire-mode))
  ;; To enable solaire-mode unconditionally for certain modes:
  ((ediff-prepare-buffer . solaire-mode))
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))

(use-package company
  :defer 1
  :config
  (global-company-mode 1))
(use-package company-prescient
  :defer 1
  :after company prescient
  :config
  (company-prescient-mode 1))
(use-package company-posframe
  :defer 1
  :after company
  :custom
  (company-posframe-quickhelp-delay nil)
  :config
  (company-posframe-mode 1))

(use-package magit
  :commands magit
  :general
  (leader-def
    "g"  '(:ignore t :wk "git")
    "gs" '(magit :wk "git status")
    "gg" '(magit :wk "git status"))
  :custom
  (magit-repository-directories `((,*project-dir* . 3)))
  :config
  ;; speed up magit for large repos
  (dir-locals-set-class-variables 'huge-git-repository
   '((magit-status-mode
      . ((eval . (magit-disable-section-inserter 'magit-insert-tags-header))))))

  ;; clasify by repo-name as detected by magit.
  ;; .dir-locals.el isn't portable across machines.
  (let ((large-dirs '("nixpkgs")))
    (dolist
        (dir large-dirs)
      (dir-locals-set-directory-class
       (cdr (assoc dir (magit-repos-alist)))
       'huge-git-repository))))

(use-package magit-todos
  :after magit
  :commands magit-todos-list magit-todos-mode
  :general
  (leader-def
    "gt" 'magit-todos-list)
  :init
  (if *is-windows* (setq magit-todos-nice nil)))

(use-package magit-delta
  :if *is-unix*
  :after magit
  :commands magit-delta-mode
  :hook ((magit-mode . magit-delta-mode)))

(defun me/expand-git-project-dirs (root)
  "Return a list of all project directories 2 levels deep in ROOT.

Given my git projects directory ROOT, with a layout like =git/{hub,lab}/<user>/project=, return a list of 'user' directories that are part of the ROOT."
  (mapcan #'(lambda (d) (cddr (directory-files d t)))
          (cddr (directory-files root t))))

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
  (projectile-indexing-method (if *is-unix* 'hybrid 'native))
  (projectile-project-search-path `((,*project-dir* . 3)))
  :config
  (projectile-save-known-projects)
  (projectile-mode +1))

(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode t))
(use-package flycheck-posframe
  :defer 1
  :after flycheck
  :hook ((flycheck-mode . flycheck-posframe-mode))
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(use-package diff-hl
  :defer 1
  :hook
  ((dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  (global-diff-hl-mode 1))

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

(use-package lsp-mode
  :defer 1
  :commands lsp lsp-deferred
  :hook
  ((prog-mode . lsp-deferred))
  ((lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-completion-provider :capf
        lsp-keymap-prefix nil)

  :general
  (local-leader-def
    :keymaps 'lsp-mode-map
    "l" '(lsp-command-map :wk "LSP")))

(use-package company-lsp
  :after company lsp-mode
  :config
  (add-to-list 'company-backends 'company-lsp))

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

(use-package savehist
  :demand t
  :config
  (savehist-mode))

(use-package vertico
  :demand t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package icomplete
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)

  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))))

  (completion-group t)
  (completions-group-format
        (concat
         (propertize "    " 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator
                     'display '(space :align-to right)))))
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless))
  :config
  (defun prefix-if-tilde (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-prefixes . ,(substring pattern 0 -1))))

  (defun regexp-if-slash (pattern _index _total)
    (when (string-prefix-p "/" pattern)
      `(orderless-regexp . ,(substring pattern 1))))

  (defun literal-if-equal (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-flex))
  (setq orderless-style-dispatchers
        '(prefix-if-tilde
          regexp-if-slash
          literal-if-equal
          without-if-bang)))

(use-package consult
  :defer 1
  :general
  (leader-def
    "ff" 'find-file
    "fr" 'consult-recent-file
    "bb" 'consult-buffer
    "tc" 'consult-theme
    "/"  'consult-ripgrep
    "g/" 'consult-git-grep)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key "<"))

(use-package consult-projectile
  :after consult projectile
  :demand t
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :general
  (leader-def
    "pp" 'consult-projectile))

(use-package consult-flycheck
  :after (consult flycheck)
  :demand t
  :general
  (leader-def
    "ee" 'consult-flycheck))

(use-package marginalia
  :defer 1
  :config
  (marginalia-mode 1))


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

(use-package git-modes)

(provide 'init)

;; Where the fuck else would it have ended??
;;; init.el ends here

