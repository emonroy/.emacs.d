;;; init.el --- Edu's Emacs configuration with use-package -*- lexical-binding: t; -*-

;;; Commentary:
;; Author: E. Monroy
;; URL: https://github.com/emonroy/.emacs.d

;;; Code:
;;; Configuration constants ----------------------------------------------------

(defconst emonroy--frame-font "fira mono")
(defconst emonroy--default-face-height 100)
(defconst emonroy--default-face-height-hd 120)
(defconst emonroy--default-face-height-2k 140)

;;; Package management ---------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package paradox
  :ensure t
  :config
  (setq-default paradox-github-token -1))

;;; Emacs configuration --------------------------------------------------------

(use-package emonroy--emacs-config
  :bind (("C-z" . undo)
         ("M-SPC" . cycle-spacing)
         ("M-<up>" . windmove-up)
         ("M-<right>" . windmove-right)
         ("M-<down>" . windmove-down)
         ("M-<left>" . windmove-left))
  :init
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil
                initial-major-mode 'text-mode
                custom-file "~/.emacs.d/custom.el"
                backup-directory-alist `((".*" . ,temporary-file-directory))
                auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
                create-lockfiles nil
                dired-listing-switches "-alh"
                scroll-conservatively 101
                mouse-wheel-scroll-amount '(1)
                mouse-wheel-progressive-speed nil)
  (load custom-file t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (winner-mode)
  (line-number-mode)
  (column-number-mode)
  (electric-pair-mode)
  (show-paren-mode)
  (global-hl-line-mode)
  (delete-selection-mode)
  (ido-mode)
  (global-auto-revert-mode))

(use-package expand-region
  :ensure t
  :bind (("C-+" . er/expand-region)))

(use-package default-text-scale
  :ensure t
  :bind (("C-c +" . default-text-scale-increase)
         ("C-c -" . default-text-scale-decrease)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

;;; Theme ----------------------------------------------------------------------

(use-package monokai-theme
  :ensure t
  :config
  (setq-default monokai-user-variable-pitch t)
  (load-theme 'monokai t))

(defun emonroy--available-font-p (font)
  "Check font availability.
Returns t when FONT is available."
  (when (find-font (font-spec :name font))
    t))

(defun emonroy--display-width ()
  "Get display width.
Returns the display width in pixels."
  (if (display-graphic-p)
      (display-pixel-width)
    0))

(use-package emonroy--font-config
  :init
  (when (emonroy--available-font-p emonroy--frame-font)
    (set-frame-font emonroy--frame-font))
  (let ((default-face-height emonroy--default-face-height))
    (cond ((>= (emonroy--display-width) 2560)
           (setq default-face-height emonroy--default-face-height-2k))
          ((>= (emonroy--display-width) 1920)
           (setq default-face-height emonroy--default-face-height-hd)))
    (set-face-attribute 'default nil
                        :height default-face-height)))

;;; Ido ------------------------------------------------------------------------

(use-package flx-ido
  :ensure t
  :config
  (setq-default ido-enable-dot-prefix t
                ido-enable-flex-matching t)
  (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq-default ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-vertical-mode))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode))

;;; Helm   ---------------------------------------------------------------------

(use-package helm
  :ensure t
  :bind (("C-c x" . helm-M-x)
         ("C-c b" . helm-mini))
  :config
  (setq-default helm-split-window-in-side-p t
                helm-M-x-fuzzy-match t
                helm-candidate-number-limit 10)
  (set-face-attribute 'helm-selection nil
                      :underline nil)
  (helm-autoresize-mode))

(use-package helm-ag
  :ensure t
  :bind (("C-c a" . helm-do-ag)
         ("C-c r" . helm-do-ag-project-root))
  :config
  (setq-default helm-ag-command-option "--ignore-case"
                helm-ag-use-agignore t
                helm-ag-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . emonroy--helm-swoop-multiline-4)
         ("C-c S" . helm-multi-swoop-all))
  :config
  (setq-default helm-swoop-move-to-line-cycle nil
                helm-swoop-speed-or-color nil)
  (defun emonroy--helm-swoop-multiline-4 ()
    (interactive)
    (setq current-prefix-arg 4)
    (helm-swoop)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c f" . helm-projectile-find-file)))

;;; Minor modes ----------------------------------------------------------------

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq-default indent-tabs-mode nil
                whitespace-style '(empty trailing tab-mark)
                whitespace-action '(auto-cleanup))
  (global-whitespace-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package anzu
  :ensure t
  :demand t
  :diminish anzu-mode
  :bind (("C-c %" . anzu-query-replace))
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :inherit 'mode-line
                      :foreground nil)
  (global-anzu-mode))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package dumb-jump
  :ensure t
  :bind ("C-c g" . dumb-jump-go))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq-default projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (setq-default projectile-enable-caching t
                projectile-completion-system 'helm
                projectile-indexing-method 'alien)
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (require 'auto-complete-config)
  (setq-default ac-use-menu-map t
                ac-sources '(ac-source-words-in-buffer
                             ac-source-words-in-same-mode-buffers))
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  (ac-linum-workaround)
  (add-hook 'prog-mode-hook 'auto-complete-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq-default yas-also-auto-indent-first-line t)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;;; Major modes ----------------------------------------------------------------

(use-package emonroy--emacs-lisp-mode
  :init
  (defun emonroy--emacs-lisp-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-functions
                                          ac-source-variables
                                          ac-source-yasnippet
                                          ac-source-symbols
                                          ac-source-features)))
    (rainbow-mode)
    (linum-mode))
  (add-hook 'emacs-lisp-mode-hook 'emonroy--emacs-lisp-mode-hook))

(use-package dummy-h-mode
  :ensure t
  :mode "\\.h\\'"
  :config
  (setq-default dummy-h-mode-default-major-mode 'c++-mode))

(use-package emonroy--c-mode
  :init
  (setq-default c-default-style "k&r"
                c-basic-offset 4)
  (defun emonroy--c-mode-hook ()
    (linum-mode))
  (add-hook 'c-mode-hook 'emonroy--c-mode-hook))

(use-package emonroy--c++-mode
  :init
  (c-set-offset 'inline-open 0)
  (defun init-c++-mode ()
    (setq flycheck-gcc-language-standard "c++11")
    (linum-mode))
  (add-hook 'c++-mode-hook 'init-c++-mode))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less\\'"
         "\\.css\\'")
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  (defun emonroy--less-css-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-css-property
                                          ac-source-words-in-all-buffer)))
    (rainbow-mode))
  (add-hook 'less-css-mode-hook 'emonroy--less-css-mode-hook))

(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq-default web-mode-engines-alist '(("django" . "\\.html?\\'"))
                web-mode-css-indent-offset 4
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 4
                web-mode-style-padding 2
                web-mode-script-padding 2
                web-mode-block-padding 0
                web-mode-enable-auto-quoting nil
                web-mode-enable-auto-pairing nil
                web-mode-enable-current-element-highlight t)
  (defun emonroy--web-mode-hook()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer))))
  (add-hook 'web-mode-hook 'emonroy--web-mode-hook))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("$"
                                     "_")
                js2-include-node-externs t
                js2-strict-trailing-comma-warning nil)
  (defun emonroy--js2-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer)))
    (linum-mode))
  (add-hook 'js2-mode-hook 'emonroy--js2-mode-hook))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq-default lua-indent-level 2))

(provide 'init)
;;; init.el ends here
