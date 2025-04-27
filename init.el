(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Critical for use-package to auto-install!
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(menu-bar-mode -1)    ;; Disable the top "File/Edit/Options/Help" menu
(tool-bar-mode -1)    ;; Disable the icon toolbar (save, open, scissors icons)
(scroll-bar-mode -1)  ;; Disable the scrollbars on the side
(set-fringe-mode 0)   ;; Disable side fringes (optional, for absolute minimalism)

(setq frame-title-format nil) 
(setq ns-use-proxy-icon nil)

(setq ring-bell-function 'ignore)

(setq auto-save-default nil)

(setq make-backup-files nil)

(setq create-lockfiles nil)

(set-fringe-mode 0)

(visual-line-mode t)

(use-package nord-theme
  :ensure t)

(load-theme 'nord t)

(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq-default line-spacing 2)

(use-package org
  :config
  ;; Make sure source blocks are fontified
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Set background color for code block contents
  (set-face-attribute 'org-block nil :background "#1E222A")

  ;; Optionally set the block marker lines too (begin_src, end_src)
  (set-face-attribute 'org-block-begin-line nil :background "#2E3440" :foreground "#5E81AC" :extend t)
  (set-face-attribute 'org-block-end-line   nil :background "#2E3440" :foreground "#5E81AC" :extend t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Vertico - Vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; Marginalia - Show annotations (like file size, buffer info)
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode 1))

;; Consult - Search, M-x, switch buffers, find files, ripgrep, etc.
(use-package consult
  :ensure t
  :after vertico)

;; Orderless - Advanced flexible matching
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-l") #'vertico-directory-up))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)             ;; Allow cycling through candidates
  (vertico-resize nil)          ;; <<< Disable auto resize
  (vertico-count 20))           ;; <<< Show 20 items always

(with-eval-after-load 'vertico
  ;; When inside find-file
  (define-key vertico-map (kbd "C-l") #'vertico-directory-up) ;; Go up directory
  (define-key vertico-map (kbd "C-h") #'vertico-directory-delete-char) ;; Delete char
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)) ;; Enter directory

(defun vertico-directory-enter ()
  "Enter the selected directory or open file."
  (interactive)
  (if (file-directory-p (vertico--candidate))
      (vertico-insert (concat (vertico--candidate) "/"))
    (vertico-exit)))

;; Evil Core
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil Collection (extra Evil bindings for other modes)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after (evil consult)
  :config
  (general-evil-setup t)

  ;; Main Leader Key
  (general-create-definer jonroby/leader-keys
    :prefix "SPC"
    :keymaps 'override
    :states '(normal visual motion))

  ;; Reserve sub-prefixes
  (jonroby/leader-keys
    "e" '(:ignore t :which-key "emacs commands")
    "w" '(:ignore t :which-key "window management")
    "c" '(:ignore t :which-key "code folding")
    "b" '(:ignore t :which-key "buffer management"))

  ;; Top-level SPC bindings
  (jonroby/leader-keys
    "a" 'save-buffer 
    "." 'consult-buffer
    "/" 'find-file 
    "s" 'consult-line
    "p" 'project-switch-project
    "f" 'project-find-file) 

  ;; Code folding under SPC c
  (jonroby/leader-keys
    "c h" 'hs-hide-block
    "c s" 'hs-show-block)

  ;; Window management under SPC w
  (jonroby/leader-keys
    "w j" 'split-window-below
    "w l" 'split-window-right
    "w d" 'delete-window)

  ;; Define switch-to-last-buffer function
  (defun switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))

  ;; Buffer navigation under SPC b
  (jonroby/leader-keys
    "b a" 'save-buffer
    "b j" 'previous-buffer
    "b k" 'next-buffer
    "b l" 'switch-to-last-buffer
    "b b" 'switch-to-buffer
    "b d" 'kill-buffer)

  ;; Emacs commands under SPC e
  (jonroby/leader-keys
    "e q" 'save-buffers-kill-terminal
    "e e" 'execute-extended-command
    "e y" 'consult-yank-pop
    "e r" 'eval-last-sexp
    "e i" '(lambda () (interactive) (find-file "~/.emacs.d/emacs.org"))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(evil-define-key '(normal visual) 'global (kbd ",") 'evil-scroll-down)
(evil-define-key '(normal visual) 'global (kbd ".") 'evil-scroll-up)

(evil-define-key '(normal) 'global (kbd "<DEL>") 'delete-backward-char)
(evil-define-key '(normal) 'global (kbd "M-n") 'drag-stuff-down)
(evil-define-key '(normal) 'global (kbd "M-p") 'drag-stuff-up)
(evil-define-key '(normal) 'global (kbd "C-o") 'open-line)
(evil-define-key '(normal) 'global (kbd "M-o") 'delete-blank-lines)

(setq evil-move-beyond-eol ())

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character
  on the current line. If COUNT is given, move COUNT - 1
  lines downward first."
  :type inclusive
  (evil-end-of-line count)
  (re-search-backward "^\\|[^[:space:]]")
  (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))

;; (define-key evil-motion-state-map "g-" 'evil-end-of-line) Delete by Oct 31 2022 if not used
;; (define-key evil-motion-state-map "-" 'evil-last-non-blank)
;; (define-key evil-motion-state-map "-" 'evil-append-line)

;; Bind the custom function directly
(define-key evil-motion-state-map "-" 'evil-append-line)
(define-key evil-visual-state-map "-" 'evil-last-non-blank)

(define-key evil-motion-state-map "1" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map ";" 'other-window)
(define-key evil-motion-state-map "f" 'avy-goto-word-1)

(setq avy-timeout-seconds 0.3)
(define-key evil-motion-state-map "3" 'avy-goto-char-timer)

;; Evil Surround (surround text objects with parens, quotes, etc.)
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1))

(use-package vterm
  :ensure t)

(defun my/vterm-execute-current-line ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (string-trim (buffer-substring
                               (save-excursion (beginning-of-line) (point))
                               (save-excursion (end-of-line) (point))))))
    (let ((buf (current-buffer)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window buf))))

(use-package multi-vterm
  :ensure t)

(use-package lean4-mode
  :straight (lean4-mode :type git :host github :repo "leanprover-community/lean4-mode")
  :mode ("\\.lean\\'" . lean4-mode))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package clang-format
  :ensure t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))

(use-package python
  :hook ((python-mode . highlight-indent-guides-mode)
         (python-mode . display-line-numbers-mode)
         (python-mode . hs-minor-mode)))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clang-format company consult corfu elixir-mode evil-collection
		  evil-surround exec-path-from-shell flycheck
		  geiser-racket general haskell-mode helm-ag
		  helm-projectile helm-swoop lean4-mode lsp-treemacs
		  lsp-ui magit marginalia multi-vterm nord-theme
		  orderless org-bullets prettier py-autopep8
		  racket-mode rust-mode typescript-mode vertico))
 '(package-vc-selected-packages
   '((lean4-mode :url
		 "https://github.com/leanprover-community/lean4-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
