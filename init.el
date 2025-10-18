;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs config

;;; Code:



;(setenv "PATH" "/home/braam/.local/share/pnpm:/home/linuxbrew/.linuxbrew/sbin:/home/braam/.local/bin:/home/braam/.local/share/bob/nvim-bin:/home/linuxbrew/.linuxbrew/bin:/home/braam/.config/emacs/bin:/home/braam/.cargo/bin:/home/braam/.sdkman/candidates/maven/current/bin:/home/braam/.sdkman/candidates/java/current/bin:/home/braam/.sdkman/candidates/gradle/current/bin:/home/braam/.local/share/nvm/v22.19.0/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/braam/.local/bin")

;;; SLIME installed with quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;;; set FISH as default shell instead of BASH
(setq-default explicit-shell-file-name "/usr/bin/fish")

;;; ui stuff
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore) ;; turn of that goddamn beep!

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(global-hl-line-mode t)
(electric-pair-mode 1)

;;; fonts
(let ((mono-spaced-font "ZedMono Nerd Font")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-12"))
(setq-default line-spacing 0.12)

;;; backup and custom files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq backup-by-copying-when-linked t)
(setq backup-by-copying             t) ; Backup by copying rather renaming

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; turn off warnings when installing packages
(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

;;; PACKAGES
;;; use-package
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu"      . "http://elpa.gnu.org/packages/")
        ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ("melpa"    . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil
      package-check-signature   nil)
(package-initialize)

;; Refresh package archives if needed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Now load use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; Automatically ensure packages

;;; themes
(use-package kanagawa-themes :ensure t)
(use-package eziam-themes :ensure t)

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((kanagawa-wave) (eziam-light)))
  (auto-dark-polling-interval-seconds 5)
  :init (auto-dark-mode))

;;; which-key
(use-package which-key :ensure t :init (which-key-mode 1))

;;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;;; yasnippet
(use-package yasnippet :ensure t :init (yas-global-mode 1))

;;; sudo-edit
(use-package sudo-edit :ensure t)

;;; vertico
(use-package vertico
  :ensure t
  :custom (vertico-cycle t)
  :init (vertico-mode 1))

;;; marginalia
(use-package marginalia :ensure t :init (marginalia-mode 1))

;;; consult
(use-package consult
  :ensure t
  :defer t
  :init (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; embark
(use-package embark
  :ensure t
  :defer t)

;;; embark-consult
(use-package embark-consult
  :ensure t
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; treesitter-auto
(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; corfu
(use-package corfu
  :ensure t
  :straight t
  :defer t
  :custom
  (corfu-auto nil)                        ;; Only completes when hitting TAB
  ;; (corfu-auto-delay 0)                ;; Delay before popup (enable if corfu-auto is t)
  (corfu-auto-prefix 1)                  ;; Trigger completion after typing 1 character
  (corfu-quit-no-match t)                ;; Quit popup if no match
  (corfu-scroll-margin 5)                ;; Margin when scrolling completions
  (corfu-max-width 50)                   ;; Maximum width of completion popup
  (corfu-min-width 50)                   ;; Minimum width of completion popup
  (corfu-popupinfo-delay 0.5)            ;; Delay before showing documentation popup
  :config
  (if ek-use-nerd-fonts
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t))

;;; Flymake
(use-package flymake
  :ensure nil          ;; This is built-in, no need to fetch it.
  :defer t
  :init (flymake-mode 1)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

(use-package company
  :ensure t
  :init (global-company-mode 1))

;;; eglot
(package-install 'eglot)
(require 'eglot)
(define-key eglot-mode-map (kbd "C-c e f n")  flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c e f p")  flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "C-c e r")    eglot-rename)

;;; PATH
(use-package exec-path-from-shell
  :ensure t)
(when (daemonp)
  (exec-path-from-shell-initialize))

(provide 'init)
;;; init.el ends here
