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

;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
;; OBSOLETE
;; (setq redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;;
;; Inhibit stuff ;;
;;;;;;;;;;;;;;;;;;;

(setq inhibit-default-init t)
(setq inhibit-compacting-font-caches t)
(setq inhibit-bidi-mirroring t)
(setq inhibit-free-realized-faces t)
(setq inhibit-menubar-update t)
(setq inhibit-x-resources t)
(setq inhibit-face-set-after-default t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode nil))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(setq inhibit-startup-screen            t
      menu-bar-mode                     nil
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode                'fundamental-mode
      default-major-mode                'fundamental-mode
      initial-scratch-message           nil)
(fset #'display-startup-echo-area-message #'ignore)
(add-hook 'emacs-startup-hook (lambda () (message "")))

;; (unload-feature 'tramp)

;(defadvice save-buffers-kill-terminal (before update-mod-flag activate)
	;(shell-command "emacswindow")
                                        ;(shell-command "tmux detach"))

(defvar my/terminal-initted nil)

;; HACK: this function does a lot of unnecessary work, and it runs every time a
;; terminal frame is made so let's modify it a bit
(defun my/tty-create-frame-with-faces (orig-fun &rest args)
  (let* ((parameters (car args))
				(frame (make-terminal-frame parameters))
        ;; success
        )
		;; (unwind-protect
			(with-selected-frame
          frame
          (unless my/terminal-initted
            (tty-handle-reverse-video frame (frame-parameters frame))
            (set-locale-environment nil frame)
            (xterm-register-default-colors xterm-standard-colors)
            (setq my/terminal-initted t))
          ;; (setq success t)
          )
      ;; (unless success
      ;;   (delete-frame frame)))
    frame))

;; HACK: load this once instead of with every new frame
(load (concat term-file-prefix "xterm"))

;; ;; just a util
;; (defun print-elements-of-list (list)
;;   "Print each element of LIST on a line of its own."
;;   (while list
;;     (print (car list))
;;     (setq list (cdr list))))

;; HACK: initialize xterm colors manually for speed
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame (or frame (selected-frame))
              (frame-set-background-mode frame t)
              (face-set-after-frame-default frame)
              (setq inhibit-message t)
              (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))))

(advice-add 'tty-create-frame-with-faces :around 'my/tty-create-frame-with-faces)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; (setq package-enable-at-startup nil)

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

;; (use-package paradox
;;   :commands (paradox-list-packages)
;; 	:defer 5
;;   :init
;;   (setq paradox-github-token t)
;;   :config
;; 	(paradox-enable))

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
;; I    insert at first non-whitespace"
;; g d  jump to definition
;; C-x RET evil marks)

;; (global-font-lock-mode +1)   ;; for all buffers
(global-visual-line-mode +1) ;; word-wrap
(setq show-paren-delay 0.25)
(show-paren-mode +1)         ;; show matching parentheses
;; (display-time) ;; in mode-line
(size-indication-mode +1)
(column-number-mode +1) ;; Column numbers in modeline
(delete-selection-mode +1) ;; Replace selection
(fset 'yes-or-no-p 'y-or-n-p) ;; Changes all yes/no questions to y/n type
(save-place-mode +1)
(blink-cursor-mode 0)
(window-divider-mode +1)
(electric-pair-mode +1)
(global-display-line-numbers-mode +1)
(global-hl-line-mode +1)

;; NOTE: so that I can easily resize my terminal window to the fill column width
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
;; (global-display-fill-column-indicator-mode +1)

;; HACK: this runs on frame creation for some reason. don't, please.
(advice-add #'evil-set-jump :override #'ignore)

(set-locale-environment "en_US.UTF-8")
(advice-add #'set-locale-environment :override #'ignore)
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

(setq-default frame-title-format '("%b"))

(use-package xterm-frobs
  :load-path "~/Git/emacs-xterm"
  :config
  (autoload 'xterm-frobs "xterm-frobs" nil t)
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (let ((title (buffer-name)))
                (unless (string-match-p (regexp-quote "Minibuf") title)
                  (xterm-set-all-titles (concat title " - Emacs")))))))

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
(setq echo-keystrokes                 show-paren-delay
      scroll-preserve-screen-position t
      user-full-name                  "Coleman Gariety"
      initial-buffer-choice           t
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
      ;; scroll-error-top-bottom         t ;; change Pgdn & Pgup function ?
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

(use-package dired
  :ensure nil
  :hook (dired-mode . (lambda () (display-line-numbers-mode -1))))

;;;;;;;;;;;;;;;
;; Mode-line ;;
;;;;;;;;;;;;;;;

(setq-default mode-line-format
              '(("-"
                  mode-line-mule-info
                  mode-line-modified
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "   "
                  mode-name)))

;;;;;;;;;;;;;;;
;; Dashboard ;;
;;;;;;;;;;;;;;;

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
                                      (projects  . 5))
        dashboard-footer-messages '("Just as little is seen in pure light as in pure darkness."
                                    "Freedom is the truth of necessity."
                                    "Only what is living feels a lack."
                                    "We learn from history that we do not learn from history."
                                    "Nothing great in the world has ever been accomplished without passion."
                                    "If you want to love you must serve, if you want freedom you must die."
                                    "To be independent of public opinion is the first formal condition of achieving anything great."
                                    "Evil resides in the very gaze which perceives evil all around itself."
                                    "The owl of Minerva begins its flight only with the coming of the dusk."
                                    "History is not the soil in which happiness grows; the periods of happiness in it are the blank pages of history."
                                    "The image of true infinity becomes the circle, the line that has reached itself."))
  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;
;; Centered cursor ;;
;;;;;;;;;;;;;;;;;;;;;

;; (use-package centered-cursor-mode
;;   :config
;;   (defun ccm-remap-evil-keys ()
;;     (when (and centered-cursor-mode evil-mode)
;;       ;; up is down, evil doesn't follow the convention
;;       (local-set-key [remap evil-scroll-page-up] 'ccm-scroll-down)
;;       (local-set-key [remap evil-scroll-page-down] 'ccm-scroll-up)))

;;   (add-hook 'evil-mode-hook 'ccm-remap-evil-keys)
;;   (add-hook 'centered-cursor-mode-hook 'ccm-remap-evil-keys)
;;   (global-centered-cursor-mode))

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

;; (use-package doom-modeline
;;   :init
;;   (setq doom-modeline-buffer-encoding        nil
;;         doom-modeline-github                 nil
;;         doom-modeline-icon                   nil
;;         doom-modeline-project-detection      'project
;;         doom-modeline-buffer-file-name-style 'file-name)
;;   (unless after-init-time
;;     ;; prevent flash of unstyled modeline at startup
;;     (setq-default mode-line-format nil))
;;   :config
;;   (doom-modeline-mode +1))

;;;;;;;;;;
;; Evil ;;
;;;;;;;;;;

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :preface
  (setq evil-want-keybinding                 nil
        evil-cross-lines                     t
        evil-symbol-word-search              t
        evil-shift-width                     2
        evil-undo-system                     'undo-tree
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
;;   :config
;;   (define-key evil-normal-state-map "g~" 'evil-operator-string-inflection))

;; ;; couldn't get it to work I think
;; (use-package rotate-text
;;   :load-path "~/Git/rotate-text.el"
;;   :init
;;   (autoload 'rotate-text "rotate-text" nil t)
;;   (autoload 'rotate-text-backward "rotate-text" nil t)
;;   (push '("true" "false") rotate-text-words))

;; (use-package evil-textobj-column)
;; (use-package evil-textobj-line)
;; (use-package evil-textobj-syntax)
;; (use-package evil-indent-plus)
;; (use-package evil-embrace)
;; (use-package exato)

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
  (evil-collection-init)
  (evil-collection-define-key 'normal 'package-menu-mode-map
   "gr" 'revert-buffer))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

;; show # of candidates in isearch
(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

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

;(use-package auto-compile
  ;:init
  ;(setq auto-compile-display-buffer nil
        ;auto-compile-mode-line-counter t)
  ;:config
  ;(auto-compile-on-load-mode +1)
  ;(auto-compile-on-save-mode +1)
  ;(setq auto-compile-display-buffer               nil
        ;auto-compile-mode-line-counter            t
        ;auto-compile-source-recreate-deletes-dest t
        ;auto-compile-toggle-deletes-nonlib-dest   t
        ;auto-compile-update-autoloads             t))

;;;;;;;;;;;;;;;
;; which-key ;;
;;;;;;;;;;;;;;;

(use-package which-key
  :config
  (which-key-mode +1))

;;:;;;;;;;;;;;;;;;;;;
;; Misc. Languages ;;
;;;;;;;;;;;;;;;:;;;;;

;; (use-package gitattributes-mode
;;   :mode ("\\.gitattributes\\'" . gitattributes-mode))

;; (use-package gitconfig-mode
;;   :mode ("\\.gitconfig\\'" . gitconfig-mode))

;; (use-package gitignore-mode
;;   :mode ("\\.gitignore\\'" . gitignore-mode))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :init
  (setq json-reformat:indent-width 2))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; (use-package dhall-mode
;;   :mode ("\\.dhall\\'" . dhall-mode))

;; (use-package graphql-mode
;;   :mode ("\\.graphql\\'" . graphql-mode))

;; (use-package purescript-mode
;;   :mode ("\\.psc\\'" . purescript-mode))

;; (use-package psc-ide
;;   :config
;;   (add-hook 'purescript-mode-hook
;;             (lambda ()
;;               (push 'company-psc-ide-backend company-backends)
;;               (psc-ide-mode)
;;               (company-mode)
;;               (flycheck-mode)
;;               (turn-on-purescript-indentation))))

;; (use-package rust-mode
;;   :mode "\\.rs\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

;; (use-package elm-mode
;;   :mode "\\.elm\\'")

;; (use-package typescript-mode
;;   :mode (("\\.tsx\\'" . typescript-mode)
;;          ("\\.ts\\'" . typescript-mode))
;;   :init
;;   (setq typescript-indent-level 2))

;; (use-package rjsx-mode
;;   :mode ("\\.jsx?\\'" "\\.mjs\\'"))

(use-package web-mode
  :mode (("\\.tsx\\'"  . web-mode)
         ("\\.ts\\'"  . web-mode)
         ("\\.jsx\\'"  . web-mode)
         ("\\.js\\'"  . web-mode))
  :init
  ;; (setq web-mode-content-types-alist
  ;;       '(("javascript"  . "\\.tsx\\'")))
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
        web-mode-whitespaces-off                      t
        web-mode-comment-formats                      '(("javascript" . "//")
                                                        ("typescript" . "//"))))

  ;; :config
  ;; (defun eslint-fix-file ()
  ;;   (interactive)
  ;;   (message "eslint --fixing the file" (buffer-file-name))
  ;;   (shell-command (concat "eslint --fix " (buffer-file-name))))

;;   ;; (defun eslint-fix-file-and-revert ()
;;   ;;   (interactive)
;;   ;;   (eslint-fix-file)
;;   ;;   (revert-buffer t t))
;;   ;; (add-hook 'web-mode-hook (lambda ()
;;   ;;                            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))
;;   (let ((types '("javascript" "jsx")))
;;     (setq web-mode-comment-formats
;;           (cl-remove-if (lambda (item) (member (car item) types))
;;                         web-mode-comment-formats))
;;     (dolist (type types)
;;       (push (cons type "//") web-mode-comment-formats))))

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package prettier-js
  :after web-mode
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--single-quote"   "true"
                           "--trailing-comma" "all"
                           "--prose-wrap"     "never")))

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

;; (use-package indium
;;   :commands (indium-connect))

;;;;;;;;;;;;;;;;;;
;; Code Folding ;;
;;;;;;;;;;;;;;;;;;

;; (use-package origami
;;   :bind ("M-o" . origami-toggle-node)
;;   :config
;;   (global-origami-mode +1)
;;   (global-set-key (kbd "M-o") 'origami-toggle-node))

;;;;;;;;;;;;;;;;;;;
;; Expand region ;;
;;;;;;;;;;;;;;;;;;;

;; (use-package expand-region
;;   :config
;;   (global-set-key (kbd "C-u") 'er/expand-region))

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
  (setq avy-keys       (eval-when-compile (string-to-list "jfkdls;anviroe"))
        avy-style      'de-bruijn
        avy-background t))

(use-package evil-easymotion
  :after avy
  :config
  (evilem-default-keybindings "SPC"))

(use-package general)

;; (use-package general
;;   :after evil-easymotion
;;   :init
;;   (defvar general-override-states)
;;   (setq general-override-states '(insert
;;                                   emacs
;;                                   hybrid
;;                                   normal
;;                                   visual
;;                                   motion
;;                                   operator
;;                                   replace)))
  ;; :config
  ;; (general-define-key
  ;;  :states '(normal visual motion)
  ;;  :keymaps 'override
  ;;  :prefix "SPC"
  ;;  "f" 'evilem-motion-find-char
  ;;  "F" 'evilem-motion-find-char-backward
  ;;  "t" 'evilem-motion-find-char-to
  ;;  "T" 'evilem-motion-find-char-to-backward
  ;;  "j" 'evilem-motion-next-line)
  ;; (general-define-key
  ;;  :states '(normal visual motion)
  ;;  :keymaps 'override
  ;;  "C-j" 
  ;;  "C-k" 'evilem-motion-previous-line))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

;; ;; adds tons of faces to face-list
;; (use-package diredfl
;;   :config
;;   (diredfl-global-mode +1))

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
        ;; none of these work O_o
        completion-ignore-case             t
        company-dabbrev-code-ignore-case   t
        company-dabbrev-downcase           nil
        company-dabbrev-ignore-case        t
        company-dabbrev-code-other-buffers t
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

;; (use-package request)
;; (use-package counsel-web
;;   :commands (counsel-web-search))

(use-package projectile
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
   "C-x C-j" 'counsel-projectile-find-file
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

;; we add this for sort-by-frequency
(use-package smex)

(use-package swiper
  :init
  (setq swiper-action-recenter t)
  :config
  (global-set-key (kbd "C-s") 'swiper)
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

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-horizon t))

(use-package horizon-theme
  :config
  (load-theme 'horizon t))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
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


;; ;;;;;;;;:
;; ;; LSP ;;
;; ;;;;;;;;;

(use-package lsp-mode
  :commands lsp
  :hook ((rust-mode       . lsp)
         (web-mode . lsp)
         ;; (purescript-mode . lsp) ;; NOTE: missing lots of features (use
         ;; psc-ide instead)
         ;; (web-mode       . lsp)
         (lsp-mode       . lsp-enable-which-key-integration))
  :init
  (defvar read-process-output-max)
  (setq read-process-output-max               3145728 ;; 3mb max packet size
        lsp-auto-guess-root                   nil
        lsp-keep-workspace-alive              nil
        lsp-keymap-prefix                     "C-l"
        lsp-completion-provider               :capf
        lsp-completion-enable                 t
        lsp-enable-folding                    nil
        lsp-enable-file-watchers              nil
        lsp-enable-text-document-color        nil
        lsp-semantic-tokens-enable            nil
        lsp-enable-indentation                nil
        lsp-enable-on-type-formatting         nil
        lsp-flycheck-live-reporting           nil ;; was causing seizures
        lsp-signature-auto-activate           nil
        lsp-signature-render-documentation    nil
        lsp-ui-doc-enable                     nil ;; too big
        lsp-ui-sideline-ignore-duplicate      t
        lsp-ui-sideline-show-code-actions     nil
        lsp-enable-symbol-highlighting        t
        lsp-idle-delay                        show-paren-delay
        lsp-headerline-breadcrumb-enable      nil))

;; ;; ;; Intellicode
;; ;; lsp-clients-typescript-plugins
;; ;; max-mini-window-height                1
;; ;; (vector
;; ;;  (list :name "@vsintellicode/typescript-intellicode-plugin"
;; ;;        :location
;; ;; "~/.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.2.6/"))))

(use-package lsp-haskell
  :hook (haskell-mode . lsp)
  :after lsp-mode)

(use-package lsp-ui
  :after lsp-mode)

;; (use-package lsp-ivy
;;   :requires lsp-mode)

;; ;; So slow!
;; (use-package company-lsp
;;   :after lsp-mode
;;   :config
;;   (push 'company-lsp company-backends))

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
  :hook (prog-mode . highlight-numbers-mode))

;;;;;;;;;
;; Git ;;
;;;;;;;;;

;; (use-package magit
;;   :commands (magit))
;; (use-package magit-todos
;;   :after magit)
;; (use-package evil-magit
;;   :after magit)
;; (use-package git-timemachine
;;   :commands (git-timemachine))

;;;;;;;;;;;;;;
;; Hardcore ;;
;;;;;;;;;;;;;;

;; (use-package jammer
;;   :hook (prog-mode . jammer-mode)
;;   :init
;;   (setq jammer-repeat-type                'linear
;;         jammer-repeat-allowed-repetitions 3))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

;; (use-package org-brain
;;   :init
;;   (setq org-brain-path "~/org/")
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;;   (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 12)
;;   (setq org-brain-include-file-entries nil
;;         org-brain-file-entries-use-title nil))

;;;;;;;;;;;;;;;;;;
;; Smart Parens ;;
;;;;;;;;;;;;;;;;;;

;; (use-package smartparens
;;   :config
;;   (smartparens-global-mode))

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

;; (use-package gcmh
;;   :config
;;   (when (not noninteractive)
;;     (add-hook 'pre-command-hook (gcmh-mode +1))
;;     (with-eval-after-load 'gcmh
;;       (setq gcmh-idle-delay 10
;;             gcmh-verbose nil
;;             gcmh-high-cons-threshold most-positive-fixnum
;;             gc-cons-percentage 0.1)
;;       (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))))
