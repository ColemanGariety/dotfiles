;;;;;;;;;;;;;;;;;
;; Performance ;;
;;;;;;;;;;;;;;;;;

;; in spite of this:
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; ...i've found that the only way to reliably prevent emacs from
;; skipping/jumping when scrolling is to set an ungodly high gc threshold.
;; yeah... high number
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer t)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(unless noninteractive
  (setq file-name-handler-alist nil))

;; Emacs "updates" its ui more often than it needs to.
(setq idle-update-delay 1)

;; Disable bidirectional text rendering for a modest performance boost.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; More performant rapid scrolling over unfontified regions.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font.
(setq frame-inhibit-implied-resize t)

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later.
(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; ;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;;;;;;;;;;;;;;;;;;;
;; Inhibit stuff ;;
;;;;;;;;;;;;;;;;;;;

(remove-hook 'find-file-hook 'vc-find-file-hook)

(setq inhibit-default-init t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode nil))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(setq inhibit-startup-screen            t
      menu-bar-mode                     nil
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init              t
      initial-major-mode                'fundamental-mode
      default-major-mode                'fundamental-mode
      initial-scratch-message           nil)
(fset #'display-startup-echo-area-message #'ignore)
(add-hook 'emacs-startup-hook (lambda () (message "")))

;; ;; trying to stop it from loading with doom-modeline
;; (setq package-load-list '(all
;;                           (all-the-icons nil)))

;; (unload-feature 'tramp)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(unless (and (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
             (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory)))
  (package-refresh-contents))

(defvar my-package-list '(no-littering use-package))

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (message "Pre-installing %s" (symbol-name package))
    (package-install package)))

(dolist (package my-package-list)
  (message "Pre-loading %s" (symbol-name package))
  (require package))

(defvar use-package-verbose)
(defvar use-package-always-ensure)
(setq use-package-verbose t
      use-package-always-ensure t)

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (paradox-enable))

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

;; ;; Used to show some useful evil stuff one startup.
;; ;; Now I just keep it here:
;; (setq initial-scratch-message ";; ^    first non-whitespace
;; 'x   go to mark
;; mx   set mark
;; '.   last changed line
;; zz   center cursor line
;; C-o  previous location
;; C-i  next location
;; #    previous token under cursor
;; *    next token under cursor
;; cc   replace line
;; C    change to end of line
;; D    delete to end of line
;; cib  change in block (or symbol)
;; cil change in line
;; cia change in argument
;; I    insert at first non-whitespace"
;; g d  jump to definition
;; C-x C-e  iedit
;; L    next argument
;; H    previous argument
;; C-x RET evil marks)
;; General

;; (global-font-lock-mode +1)   ;; for all buffers
(global-visual-line-mode +1) ;; word-wrap
(show-paren-mode +1)         ;; show matching parentheses
(display-time) ;; in mode-line
(size-indication-mode +1)
(column-number-mode +1) ;; Column numbers in modeline
(delete-selection-mode +1) ;; Replace selection
(fset 'yes-or-no-p 'y-or-n-p) ;; Changes all yes/no questions to y/n type
(save-place-mode +1)
(blink-cursor-mode 0)
(window-divider-mode +1)
(electric-pair-mode +1)
;; (set-locale-environment "en_US.UTF-8")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(set-face-inverse-video 'vertical-border nil)
(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(setq-default fill-column 80)
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; ;; UTF-8 as the default coding system
;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
;; (prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;
;; Sane defaults ;;
;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq frame-background-mode (quote dark))
(defvar apropos-do-all)
(defvar compilation-message-face)
(setq echo-keystrokes                 0.15
      user-full-name                  "Coleman Gariety"
      ;; initial-buffer-choice           t
      sentence-end-double-space       nil
      scroll-conservatively           most-positive-fixnum
      vc-follow-symlinks              t
      ls-lisp-use-insert-directory-program nil
      ls-lisp-dirs-first              t
      vc-make-backup-files            t
      delete-by-moving-to-trash       t
      visible-bell                    t
      visible-cursor                  t
      ring-bell-function              'ignore
      scroll-error-top-bottom         t ;; Pgdn & Pgup work properly
      large-file-warning-threshold    (* 1024 1024) ;; 1mb
      mode-require-final-newline      t ;; Newlines
      create-lockfiles                nil ;; no lockfiles in server mode
      apropos-do-all                  t
      auto-window-vscroll             nil
      auto-mode-case-fold             nil)

;; Indentation

(setq-default indent-tabs-mode nil
              c-basic-indent   2
              c-basic-offset   2
              tab-width        2)
(defvar tab-width)
(defvar js-indent-level)
(defvar css-indent-offset)
(defvar c-basic-offset)
(setq css-indent-offset 2
      tab-width         2
      js-indent-level   2
      c-basic-offset    2)

;; Inhibit annoying "when done with this frame..." message
(add-hook 'after-make-frame-functions
          (lambda (&optional frame)
            (with-selected-frame (or frame (selected-frame))
              (setq inhibit-message t)
              (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))))

(use-package dired
  :ensure nil
  :hook (dired-mode . hl-line-mode))

;;;;;;;;;;;;;;;
;; Dashboard ;;
;;;;;;;;;;;;;;;

;; can't defer
(use-package dashboard
  :init
  (setq initial-buffer-choice       (lambda ()
                                      (get-buffer "*dashboard*"))
        dashboard-center-content    t
        dashboard-set-file-icons    t
        dashboard-set-heading-icons t
        dashboard-startup-banner    3
        dashboard-page-separator    "\n\n"
        dashboard-set-init-info     nil
        dashboard-items             '((recents   . 5)
                                      (projects  . 5)))
  :config
  (dashboard-setup-startup-hook))

;;:;;;;;;;;;;;;:
;; Move lines ;;
;;;;;;;;;;;;;;;;

;; ;; can't find a good key for these

;; (defun move-line-or-region (arg)
;;   "ARG: See https://github.com/syl20bnr/spacemacs/issues/5365."
;;   (interactive "P")
;;   (if (or (not arg) (>= arg 0))
;;       (let ((reg-or-lin (if (region-active-p) "'>" "."))
;;             (reactivate-region (if (region-active-p) "gv=gv" ""))
;;             (num (if arg arg 1)))
;;         (execute-kbd-macro
;;          (concat ":m" reg-or-lin "+" (number-to-string num) (kbd "RET") reactivate-region)))
;;     (backward-move-line-or-region (- arg))))

;; (defun backward-move-line-or-region (arg)
;;   "ARG: See https://github.com/syl20bnr/spacemacs/issues/5365."
;;   (interactive "P")
;;   (if (or (not arg) (>= arg 0))
;;       (let ((reg-or-lin (if (region-active-p) "'<" "."))
;;             (reactivate-region (if (region-active-p) "gv=gv" ""))
;;             (num (if arg (+ arg 1) 2)))
;;         (execute-kbd-macro
;;          (concat ":m" reg-or-lin "-" (number-to-string num) (kbd "RET") reactivate-region)))
;;     (move-line-or-region (- arg))))

;;;;;;;;;;;;;;;
;; Mode line ;;
;;;;;;;;;;;;;;;

;; can't defer
(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-encoding   nil
        doom-modeline-github            nil
        doom-modeline-icon              nil
        doom-modeline-project-detection 'project)
  (unless after-init-time
    ;; prevent flash of unstyles modeline at startup
    (setq-default mode-line-format nil))
  :config
  (doom-modeline-mode +1))

;;;;;;;;;;
;; Evil ;;
;;;;;;;;;;

;; can't defer
(use-package evil
  :preface
  (setq evil-want-keybinding                 nil
        evil-cross-lines                     t
        evil-symbol-word-search              t
        evil-shift-width                     2
        evil-ex-interactive-search-highlight 'selected-window)
  (setq-default evil-kill-on-visual-paste nil) ;; doesn't work :(
  :config
  (general-define-key
   :keymaps '(normal motion)
   "<remap> <evil-next-line>" 'evil-next-visual-line
   "<remap> <evil-previous-line>" 'evil-previous-visual-line
   "<remap> <evil-next-line>" 'evil-next-visual-line
   "<remap> <evil-previous-line>" 'evil-previous-visual-line)
  (general-define-key
   :keymaps '(normal insert visual)
   ;; "C-k" 'backward-move-line-or-region ;; too easy
   ;; "C-j" 'move-line-or-region          ;; to mess up
   "C-n" 'evil-next-line
   "C-p" 'evil-previous-line
   "C-a" 'evil-beginning-of-line
   "C-e" 'evil-end-of-line)
  (general-define-key
   :keymaps 'global
   "M-k" 'evil-window-up
   "M-j" 'evil-window-down
   "M-h" 'evil-window-left
   "M-l" 'evil-window-right
   "M-K" 'evil-window-split
   "M-J" 'evil-window-split
   "M-H" 'evil-window-vsplit
   "M-L" 'evil-window-vsplit
   "C-q" 'delete-window)
  (evil-mode +1))

;; ;; old evil config
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)

;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
;; (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
;; (define-key evil-visual-state-map (kbd "C-n") 'evil-next-line)
;; (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
;; (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
;; (define-key evil-visual-state-map (kbd "C-p") 'evil-previous-line)
;; (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
;; (define-key evil-visual-state-map (kbd "C-a") 'evil-beginning-of-line)
;; (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
;; (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
;; (define-key global-map (kbd "M-k") 'evil-window-up)
;; (define-key global-map (kbd "M-j") 'evil-window-down)
;; (define-key global-map (kbd "M-h") 'evil-window-left)
;; (define-key global-map (kbd "M-l") 'evil-window-right)
;; (define-key global-map (kbd "M-K") 'evil-window-split)
;; (define-key global-map (kbd "M-J") 'evil-window-split)
;; (define-key global-map (kbd "M-H") 'evil-window-vsplit)
;; (define-key global-map (kbd "M-L") 'evil-window-vsplit)
;; (define-key global-map (kbd "C-q") 'delete-window)

;; can't defer
;; TODO replace with my own version
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :init
    (setq evil-normal-state-cursor 'box
          evil-motion-state-cursor 'box
          evil-visual-state-cursor 'hollow ;; does not work in kitty
          evil-insert-state-cursor 'bar
          evil-emacs-state-cursor  'hbar)
    :config
    (etcc-on)))

;; (use-package evil-string-inflection
;;   :defer 1
;;   :config
;;   (define-key evil-normal-state-map "g~" 'evil-operator-string-inflection))

;; ;; couldn't get it to work I think
;; (use-package rotate-text
;;   :load-path "~/Git/rotate-text.el"
;;   :init
;;   (autoload 'rotate-text "rotate-text" nil t)
;;   (autoload 'rotate-text-backward "rotate-text" nil t)
;;   (push '("true" "false") rotate-text-words))

;; (use-package evil-textobj-column :defer 2)
;; (use-package evil-textobj-line :defer 2)
;; (use-package evil-textobj-syntax :defer 2)
;; (use-package evil-indent-plus :defer 2)
;; (use-package evil-embrace :defer 2)
;; (use-package exato :defer 2)

;; ;; just don't know how to work it
;; (use-package targets
;;   :load-path "~/Git/targets.el/targets.el"
;;   :init
;;   (setq targets-user-text-objects '((pipe "|" nil separator)
;;                                     (paren "(" ")" pair :more-keys "b")
;;                                     (bracket "[" "]" pair :more-keys "r")
;;                                     (curly "{" "}" pair :more-keys "c")))
;;   :config
;;   (targets-setup t
;;                  :inside-key nil
;;                  :around-key nil
;;                  :remote-key nil))

;; (use-package evil-exchange
;;   :defer 1
;;   :config
;;   (evil-exchange-install))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

(use-package evil-mc
  :config
  (global-evil-mc-mode +1))

;; show # of candidates in isearch
(use-package evil-anzu
  :after evil)

;; (use-package evil-args
;;   :config
;;   (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
;;   (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;;   (define-key evil-normal-state-map "L" 'evil-forward-arg)
;;   (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;   (define-key evil-motion-state-map "L" 'evil-forward-arg)
;;   (define-key evil-motion-state-map "H" 'evil-backward-arg)
;;   (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;   (define-key evil-motion-state-map "L" 'evil-forward-arg))

;; ;; Performance is REALLY bad
;; (use-package vim-empty-lines-mode
;;   :hook (prog-mode . vim-empty-lines-mode))

;; ;;;;;;;;;;;;;;;;;
;; Auto-compie ;;
;;;;;;;;;;;;;;;;;

(use-package auto-compile
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1)
  (setq auto-compile-display-buffer               nil
        auto-compile-mode-line-counter            t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest   t
        auto-compile-update-autoloads             t))

;;;;;;;;;;;;;;;
;; which-key ;;
;;;;;;;;;;;;;;;

(use-package which-key
  :defer 2
  :config
  (which-key-mode +1))

;;:;;;;;;;;;;;;;;;;;;
;; Misc. Languages ;;
;;;;;;;;;;;;;;;:;;;;;

(use-package gitattributes-mode
  :mode ("\\.gitattributes\\'" . gitattributes-mode))
(use-package gitconfig-mode
  :mode ("\\.gitconfig\\'" . gitconfig-mode))
(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))
(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :init
  (setq json-reformat:indent-width 2))
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))
(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))
;; (use-package psc-ide)
(use-package purescript-mode
  :mode ("\\.psc\\'" . purescript-mode))
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package haskell-mode
  :mode "\\.hs\\'")
(use-package typescript-mode
  ;; :mode (("\\.tsx\\'" . typescript-mode)
  ;;        ("\\.ts\\'" . typescript-mode))
  :init
  (setq typescript-indent-level 2))

;; (use-package rjsx-mode
;;   :mode ("\\.jsx?\\'" "\\.mjs\\'"))

(use-package web-mode
  :mode (("\\.tsx\\'"  . web-mode)
         ("\\.ts\\'"  . web-mode)
         ("\\.jsx\\'"  . web-mode)
         ("\\.js\\'"  . web-mode))
  :init
  (setq web-mode-code-indent-offset                   2
        web-mode-markup-indent-offset                 2
        web-mode-css-indent-offset                    2
        web-mode-enable-html-entities-fontification   nil
        web-mode-enable-block-face                    nil
        web-mode-enable-comment-annotation            nil
        web-mode-enable-comment-interpolation         nil
        web-mode-enable-control-block-indentation     nil
        web-mode-enable-css-colorization              nil
        web-mode-enable-current-column-highlight      nil
        web-mode-enable-current-element-highlight     nil
        web-mode-enable-element-content-fontification nil
        web-mode-enable-heredoc-fontification         nil
        web-mode-enable-inlays                        nil
        web-mode-enable-optional-tags                 nil
        web-mode-enable-part-face                     nil
        web-mode-enable-sexp-functions                nil
        web-mode-enable-sql-detection                 nil
        web-mode-enable-string-interpolation          nil
        web-mode-enable-whitespace-fontification      nil
        web-mode-enable-auto-expanding                nil
        web-mode-enable-auto-indentation              nil
        web-mode-enable-auto-closing                  nil
        web-mode-enable-auto-opening                  nil
        web-mode-enable-auto-pairing                  nil
        web-mode-enable-auto-quoting                  nil
        web-mode-fontification-off                    nil
        web-mode-whitespaces-off                      t)
  :config
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item) (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats))))

;; Assign typescript-mode to .tsx files
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; (use-package mmm-mode
;;   :config
;;   (setq mmm-global-mode t)
;;   (setq mmm-submode-decoration-level 0)
;;   ;; Add submodule for graphql blocks
;;   (mmm-add-classes
;;    '((mmm-graphql-mode
;;       :submode graphql-mode
;;       :front "gr?a?p?h?ql`"
;;       :back "`;")))

;;   (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)

;;   ;; Add JSX submodule, because typescript-mode is not that great at it
;;   (mmm-add-classes
;;    '((mmm-jsx-mode
;;       ;; :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
;;       ;; :back ">\n?\s*)"
;;       :front "<>"
;;       :back "</>"
;;       ;; :front-offset -1
;;       ;; :back-offset 1
;;       :submode web-mode)))

;;   (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)

;;   (defun mmm-reapply ()
;;     (mmm-mode)
;;     (mmm-mode))

;;   (add-hook 'after-save-hook
;;             (lambda ()
;;               (when (string-match-p "\\.tsx?" buffer-file-name)
;;                 (mmm-reapply)))))

(use-package indium
  :commands (indium-connect))

;;;;;;;;;;;;;;;;;;
;; Code Folding ;;
;;;;;;;;;;;;;;;;;;

(use-package origami
  :bind ("M-o" . origami-toggle-node)
  :config
  (global-origami-mode +1)
  (global-set-key (kbd "M-o") 'origami-toggle-node))

;;;;;;;;;;;;;;;;;;;
;; Expand region ;;
;;;;;;;;;;;;;;;;;;;

;; (use-package expand-region
;;   :defer 2
;;   :config
;;   (global-set-key (kbd "C-u") 'er/expand-region))

;; ;; broken
;; (use-package highlight-indent-guides
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-auto-enabled nil)
;;   ;; (add-hook 'prog-mode-hook #'highlight-indent-guides-auto-set-faces)
;;   (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

;; ;; can't use glyphs O_o ??
;; (use-package highlight-indentation
;;   :config
;;   (set-face-attribute 'highlight-indentation-face nil
;;                       :stipple (list 7 4 (string 16 0 0 0))
;;                       :inherit nil)

;;   (set-face-attribute 'highlight-indentation-current-column-face nil
;;                       :stipple (list 7 4 (string 16 0 0 0))
;;                       :inherit nil
;;                       :foreground "yellow")
;;   (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;; ;; moves with cursor ???
;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode))

;;;;;;:;;;;;;;;;
;; Easymotion ;;
;;;;;;;;;;;;;;;;

(use-package avy
  :init
  ;; NOTE See:
  ;; https://github.com/PythonNut/quark-emacs/blob/dev/modules/config-avy-easymotion.el
  (setq avy-keys  (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb"))
        avy-style 'de-bruijn))

(use-package evil-easymotion
  :after avy)

(use-package general
  :after evil-easymotion
  :init
  (defvar general-override-states)
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   ;; there can be no better config
   "f" 'evilem-motion-find-char
   "F" 'evilem-motion-find-char-backward
   "t" 'evilem-motion-find-char-to
   "T" 'evilem-motion-find-char-to-backward
   "C-j" 'evilem-motion-next-line
   "C-k" 'evilem-motion-previous-line)
  (general-define-key
   :states 'insert
   :keymaps 'override
   "C-k" 'kill-line))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

;; (use-package diredfl
;;   :defer 1
;;   :config
;;   (diredfl-global-mode +1))

;; (use-package dired-subtree)

;;;;;;;;;;;;;;;;;
;; Completion  ;;
;;;;;;;;;;;;;;;;;

(use-package company
  :init
  (defvar company-dabbrev-downcase)
  (defvar company-dabbrev-ignore-case)
  (defvar company-dabbrev-code-other-buffers)
  (setq-local completion-ignore-case t)
  (setq company-minimum-prefix-length      3 ;; responsive > cpu cycles
        company-idle-delay                 0
        company-tooltip-limit              10
        company-tooltip-flip-when-above    t
        company-tooltip-align-annotations  t
        ;; ;; nonde of these work O_o
        ;; completion-ignore-case             t
        ;; company-dabbrev-code-ignore-case   t
        ;; company-dabbrev-downcase           t
        ;; company-dabbrev-ignore-case        t
        ;; company-dabbrev-code-other-buffers t
        company-require-match              'never
        company-backends                   '(company-capf)
        company-frontends                  '(company-pseudo-tooltip-frontend
                                             company-echo-metadata-frontend
                                             company-tng-frontend))
  :config
  ;; don't persist company when switching back to normal mode
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (add-hook 'after-init-hook 'global-company-mode)
  (general-define-key
   :keymaps 'company-active-map
   "<backtab>" 'company-select-previous ;; no back-cycle?
   "TAB"       'company-complete-common-or-cycle))

;; (use-package company-flx
;;   :after company
;;   :config
;;   (company-flx-mode +1))

;;;;;;;;;
;; Ivy ;;
;;;;;;;;;

(use-package counsel
  :init
  (let ((base-command "rg -i -M 120 --no-heading --line-number --color never %s"))
    (setq counsel-rg-base-command   base-command
          counsel-grep-base-command base-command))
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x RET") 'counsel-evil-marks)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "M-p") 'counsel-evil-registers)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-r") 'counsel-buffer-or-recentf)
  (counsel-mode +1))

(use-package request :defer t)
(use-package counsel-web
  :commands (counsel-web-search))

(use-package projectile
  :defer 1
  :init
  (setq projectile-enable-caching t))

(use-package counsel-projectile
  :after projectile
  :init
  (setq counsel-projectile-sort-files t)
  :config
  (counsel-projectile-mode +1)
  (general-define-key
   :keymaps 'projectile-mode-map
   "C-c p" 'projectile-command-map
   "C-x C-j" 'counsel-projectile
   "C-x C-g" 'counsel-projectile-rg))

(use-package ivy
  :init
  (setq ivy-wrap                         t
        enable-recursive-minibuffers     t
        projectile-completion-system     'ivy
        ivy-magic-slash-non-match-action nil
        ivy-on-del-error-function        #'ignore
        ivy-use-virtual-buffers          t
        ivy-use-selectable-prompt        t
        ivy-count-format                 "[%d/%d] ")
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line)
  :config
  (ivy-mode +1)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (defun ivy-resize--minibuffer-setup-hook ()
    (add-hook 'post-command-hook #'ivy-resize--post-command-hook nil t))
  (defun ivy-resize--post-command-hook ()
    (when ivy-mode
      (shrink-window (1+ ivy-height))))
  (add-hook 'minibuffer-setup-hook 'ivy-resize--minibuffer-setup-hook)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "<ESC>" 'minibuffer-keyboard-quit
   "TAB" 'ivy-alt-done
   "C-l" 'ivy-call
   "C-b" 'counsel-up-directory)
  (general-define-key
   :keymaps '(counsel-imenu-map counsel-ag-map)
   "C-n" 'ivy-next-line-and-call
   "C-p" 'ivy-previous-line-and-call)
  (general-define-key
   :keymaps '(normal insert visual)
   "M-r" 'counsel-imenu))


;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;   (setq ivy-rich-path-style 'abbrev)
;;   (setq ivy-rich-parse-remote-buffer nil)
;;   (setq ivy-switch-buffer-faces-alist nil)
;;   :config
;;   ;; Highlight buffers differently based on whether they're in the same project
;;   ;; as the current project or not.
;;   (let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
;;          (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
;;     (when switch-buffer-alist
;;       (setcar switch-buffer-alist '+ivy-rich-buffer-name)))
;;   (ivy-set-display-transformer 'internal-complete-buffer nil)
;;   (ivy-rich-mode +1))

;; ;; doesn't match spaces for dashes why ???
;; (use-package flx
;;   :init
;;   (setq ivy-flx-limit 1000)
;;   :config
;;   ;; fuzzy matching in ivy
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-re-builders-alist
;;         '((counsel-find-file  . ivy--regex-fuzzy)
;;           (counsel-projectile . ivy--regex-fuzzy)
;;           (t                  . ivy--regex-plus))))

;; we add this for sort-by-frequency
(use-package smex :defer 1)

(use-package swiper
  :defer 1
  :init
  (setq swiper-action-recenter t)
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-x C-a") 'swiper-all))

;;;;;;;;;;;;;;;;
;; Copy/paste ;;
;;;;;;;;;;;;;;;;

(require 'cl)
(case system-type
    ('darwin (unless window-system
               (setq interprogram-cut-function
                     (lambda (text &optional push)
                       (let* ((process-connection-type nil)
                              (pbproxy (start-process "pbcopy"
                                                      "pbcopy"
                                                      "/usr/bin/pbcopy")))
                         (process-send-string pbproxy text)
                         (process-send-eof pbproxy))))))
    ('gnu/linux (progn
                  (setq select-enable-clipboard t)
                  (defun xsel-cut-function (text &optional push)
                    (with-temp-buffer
                      (insert text)
                      (call-process-region (point-min)
                                           (point-max)
                                           "xsel"
                                           nil
                                           0
                                           nil
                                           "--clipboard"
                                           "--input")))
                  (defun xsel-paste-function()
                    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))

                      (unless (string= (car kill-ring) xsel-output)
                        xsel-output)))
                  (setq interprogram-cut-function 'xsel-cut-function)
                  (setq interprogram-paste-function 'xsel-paste-function))))

;;;;;;;;;;;
;; Color ;;
;;;;;;;;;;;

;; can't defer
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :defer t
  :delight " ✓"
  :init
  (setq flycheck-check-syntax-automatically '(save))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             javascript-jshint))
  (setq flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             javascript-jshint))
  :config
  ;; (global-flycheck-mode +1)
  ;; disable jshint since we prefer eslint checking
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))


;;;;;;;;:
;; LSP ;;
;;;;;;;;;

(use-package lsp-mode
  :commands lsp
  :hook ((rust-mode       . lsp)
         (typescript-mode . lsp)
          (web-mode       . lsp)
          (lsp-mode       . lsp-enable-which-key-integration))
  :init
  (defvar read-process-output-max)
  (setq read-process-output-max               3145728 ;; 3mb max packet size
        lsp-auto-guess-root                   nil
        lsp-keep-workspace-alive              nil
        lsp-keymap-prefix                     "C-l"
        lsp-prefer-capf                       nil
        lsp-enable-completion-at-point        nil
        lsp-enable-folding                    nil
        lsp-enable-file-watchers              nil
        lsp-enable-text-document-color        nil
        lsp-enable-semantic-highlighting      nil
        lsp-enable-indentation                nil
        lsp-enable-on-type-formatting         nil
        lsp-flycheck-live-reporting           nil ;; was causing seizures
        lsp-signature-auto-activate           nil
        lsp-signature-render-documentation    nil
        lsp-ui-doc-enable                     nil ;; too big
        lsp-ui-sideline-ignore-duplicate      t
        lsp-enable-symbol-highlighting        t
        lsp-idle-delay                       0.5
        lsp-clients-typescript-log-verbosity "debug"))
        ;; lsp-clients-typescript-plugins
        ;; (vector
        ;;  (list :name "@vsintellicode/typescript-intellicode-plugin"
        ;;        :location "~/.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.2.6/"))))

(use-package lsp-ui
  :after lsp-mode)

;; (use-package lsp-ivy
;;   :requires lsp-mode)

;; ;; So slow!
(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends))

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package treemacs
;;   :after lsp-mode)
;; (use-package lsp-treemacs
;;   :after lsp-mode)
;; (use-package dap-mode)
;; (use-package dap-chrome
;;   :ensure nil
;;   :config
;;   (dap-register-debug-template
;;    "default"
;;    (list :type "chrome"
;;          :cwd nil
;;          :mode "url"
;;          :request "launch"
;;          :webRoot nil
;;          :url "http://localhost:3000"
;;          :name "Chrome Browse URL"
;;          :runtimeExecutable "/usr/bin/google-chrome-stable")))

;; (use-package dap-node
;;   :ensure nil
;;   :requires dap-mode)

;;;;;;;;;;;;;;;;;;;;;
;; Highlight stuff ;;
;;;;;;;;;;;;;;;;;;;;;

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :defer 2
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"   warning bold italic)
          ("HACK"   warning bold italic)
          ("FIXME"  error bold italic)
          ("NOTE"   warning bold italic)
          ("GOTCHA" warning bold italic)))
  :config
  (global-hl-todo-mode +1))

(use-package highlight-numbers
  :defer 1
  :hook (prog-mode . highlight-numbers-mode))

;;;;;;;;;
;; Git ;;
;;;;;;;;;

;; (use-package magit)
(use-package git-timemachine
  :commands (git-timemachine))

;;;;;;;;;
;; IRC ;;
;;;;;;;;;

(use-package circe
  :commands (circe)
  :config
  (setq my-credentials-file "~/.private.el")

  (defun my-nickserv-password (server)
    (with-temp-buffer
      (insert-file-contents-literally my-credentials-file)
      (plist-get (read (buffer-string)) :nickserv-password)))

  (setq circe-network-options
        '(("Freenode"
           :tls t
           :nick "clmg"
           :sasl-username "clmg"
           :sasl-password my-nickserv-password))))

(use-package circe-notifications
  :after circe
  :config
  ;; (eval-after-load "circe-notifications"
  ;;   '(setq circe-notifications-watch-strings
  ;;       '("people" "you" "like" "to" "hear" "from")))
  (autoload 'enable-circe-notifications "circe-notifications" nil t)
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

;;;;;;;;;;;;;;
;; Hardcore ;;
;;;;;;;;;;;;;;

;; (use-package jammer
;;   :hook (prog-mode . jammer-mode)
;;   :init
;;   (setq jammer-repeat-type                'linear
;;         jammer-repeat-allowed-repetitions 7))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

;; (use-package org-brain)

;;;;;;;;;;;;;;;;;;
;; Smart Parens ;;
;;;;;;;;;;;;;;;;;;

;; (use-package smartparens
;;   :config
;;   (smartparens-global-mode))

;; (use-package smart-jump
;;   :config
;;   (smart-jump-setup-default-registers))

;;;;;;;;;
;; RSS ;;
;;;;;;;;;

;; (use-package elfeed
;;   :config
;;   (setq elfeed-feeds
;;         '("https://oremacs.com/atom.xml"
;;           "https://medium.com/feed/@gcanti")))

;;;;;;;;;;;;;;;;;;;
;; GC Magic Hack ;;
;;;;;;;;;;;;;;;;;;;

(use-package gcmh
  :config
  (when (not noninteractive)
    (add-hook 'pre-command-hook (gcmh-mode +1))
    (with-eval-after-load 'gcmh
      (setq gcmh-idle-delay 10
            gcmh-verbose nil
            gcmh-high-cons-threshold 16777216
            gc-cons-percentage 0.1)
      (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))))
