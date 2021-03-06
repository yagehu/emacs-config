(setq whitespace-style '(
  face
  trailing
  tabs
  spaces
  empty
  indentation
  space-after-tab
  space-before-tab
  space-mark
  tab-mark))
(global-whitespace-mode)

(global-auto-revert-mode t)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 95)

(setq initial-frame-alist '((fullscreen . maximized)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

;; Sort apropos results by relevancy.
(setq apropos-sort-by-scores t)

;; Restart Emacs.
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let (
    (kill-emacs-hook (append
      kill-emacs-hook
      (list (if
        (display-graphic-p)
        #'launch-separate-emacs-under-x
        #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;; Directional window selection
;; S-<up, down, right, left>
(windmove-default-keybindings)


;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

(use-package doom-themes
  :config
    (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :config
    (doom-modeline-mode 1))

(use-package counsel
  :config
    (counsel-mode t)
  :bind
    (("C-x b" . counsel-switch-buffer)))

(use-package vterm
  :hook
    (vterm-mode . (lambda () (display-line-numbers-mode 0))))

(use-package which-key
  :init
    (setq which-key-idle-delay 1.0)
  :config
    (which-key-mode))

(use-package magit)

(use-package treemacs
  :commands treemacs
  :hook
    (treemacs-mode . (lambda () (display-line-numbers-mode 0))))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-mode
  :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-signature-render-documentation nil)
  :hook
    ((yaml-mode . lsp)
     (lsp-mode . lsp-enable-which-key-integration))
  :commands
    lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config (lsp-treemacs-sync-mode 1))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package company
  :init
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0.0)
    (add-hook 'after-init-hook 'global-company-mode))

(use-package rustic
  :defer t
  :init
    ;; Don't display rustfmt buffer.
    (setq rustic-format-display-method 'ignore)
    (setq rustic-format-on-save t)
    (setq rustic-lsp-server 'rust-analyzer)
    (setq rustic-rustfmt-args "--unstable-features")
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (setq lsp-rust-analyzer-proc-macro-enable t))

(use-package yaml-mode)

(use-package fzf
  :bind
    (("C-c C-f C-g" . fzf-git)))
