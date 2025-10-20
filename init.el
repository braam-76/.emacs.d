;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; My Emacs config

;;; Code:

(setq gc-cons-threshold (* 50 1000 1000))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; fonts
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(let ((mono-spaced-font "ZedMono Nerd Font")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-12"))
(setq-default line-spacing 0.12)

(use-package emacs
  :custom
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)

  (delete-selection-mode t)
  (electric-indent-mode nil)
  (electric-pair-mode t)

  (ring-bell-function 'ignore)

  (blink-cursor-mode nil)
  (global-auto-revert-mode t)

  (global-display-line-numbers-mode t)

  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 10)

  (tab-width 2)

  (auto-save-default nil)
  (make-backup-files nil)

  :hook (prog-mode . (lambda () (hs-minor-mode t)))
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)

  (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (setq backup-by-copying-when-linked t)
  (setq backup-by-copying             t) ; Backup by copying rather renaming
  
  :bind (([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         ;; Zooming In/Out
         ("C-+" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease))
  )

;;; turn off warnings when installing packages
(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

(use-package general
	:straight t
  :config
  (general-emacs-define-key 'global :prefix "C-c"
   "." '(find-file :wk "Find file")
   "TAB" '(comment-line :wk "Comment lines")
   "t" '(eat :wk "Eat terminal")
	 ))

(use-package multiple-cursors
	:straight t
	:config 
	(general-emacs-define-key 'global
		"C-." (mc/mark-next-like-this :wk "Mark next like this")
		"C-," (mc/mark-previous-like-this :wk "Mark previous like this")
		"C-'" (mc/mark-all-like-this :wk "Mark all like this")
		)
	:init
	(define-key mc/keymap (kbd "<return>") nil) ;; so i can insert newline while using multiple cursors
	)

;;; themes
(use-package kanagawa-themes :straight t)
(use-package eziam-themes :straight t)
(use-package goose-theme :straight t)
(use-package badger-theme :straight t)


(use-package auto-dark
	:straight t
  :custom
  (auto-dark-themes '((badger) (goose)))
  (auto-dark-polling-interval-seconds 5)
  :init (auto-dark-mode))

;;; status line
(use-package doom-modeline
	:straight t
  :custom
  (doom-modeline-height 25) ;; Set modeline height
  :hook (after-init . doom-modeline-mode))

;;; nerd-icons
(use-package nerd-icons
	:straight t
  :if (display-graphic-p))

;;; Use $PATH from shell to find lsp's and any other programs
(use-package exec-path-from-shell
	:straight t
  :init
  (exec-path-from-shell-initialize))

;;; eglot
(use-package eglot
  :straight nil
  :hook (c-mode . eglot-ensure)
	(c++-mode . eglot-ensure)
	(java-mode . eglot-ensure)
	(js-ts-mode . eglot-ensure)
	(tsx-ts-mode . eglot-ensure)
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil))

;;; treesitter (automatically installs treesitter grammars)
(use-package treesit-auto
	:straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; yasnippet
(use-package yasnippet-snippets :straight t :hook (prog-mode . yas-minor-mode))

;;; which-key
(use-package which-key :straight t :hook (prog-mode . which-key-mode))

;;; eat
(use-package eat
	:straight t
  :hook ('eshell-load-hook #'eat-eshell-mode))

;;; magit
(use-package magit
	:straight t
  :defer
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one) ;; Make escape quit magit prompts
  )

;;; corfu
(use-package corfu
	:straight t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popup info delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
(use-package nerd-icons-corfu
	:straight t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
	:straight t
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  ;; The functions that are added later will be the first in the list
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion

  ;;(add-hook 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-hook 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-hook 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-hook 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

;;; orderless
(use-package orderless
	:straight t 
  :config
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;;; vertico
(use-package vertico
	:straight t
  :custom (vertico-cycle t)
  :init (vertico-mode 1))

;;; marginalia
(use-package marginalia :straight t :init (marginalia-mode 1))

;;; consult
(use-package consult
	:straight t
  :defer t
  :init (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; sudo-edit
(use-package sudo-edit :straight t)

;;; embark
(use-package embark :straight t :defer t)

;;; embark-consult
(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package sly :straight t)

(use-package geiser-chez :straight t)
(use-package geiser 
	:straight t
	:init
	(setq geizer-chez-binary "/usr/bin/scheme"))

(provide 'init)
;;; init.el ends here
