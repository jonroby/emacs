(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))


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

(use-package mini-frame
 :ensure t) 



(customize-set-variable 'mini-frame-resize 'fit-content)

(add-to-list 'load-path "~/.emacs.d/nano-emacs") 

  (require 'nano)
  (require 'nano-theme-dark)

  (require 'nano-colors) 

  (require 'nano-faces)


  (nano-faces)
  (nano-theme) 


  ;;(defface nano-faded
   ;; '((t (:foreground "black")))  ;; A typical faded gray color

    ;; :group 'nano-faces) 
  ;; (require 'nano-minibuffer) 


;; (setq mini-frame-default-height 20)  ;; Taller frame for more completions
;; (setq mini-frame-create-lazy t)
;; (setq mini-frame-resize t)

;; Add padding and border improvements
;; (customize-set-variable 'mini-frame-internal-border-width 10)
;; (setq mini-frame-resize-min-height 10)

;; Custom faces for better appearance
(custom-set-faces
 '(nano-minibuffer-face ((t (:background "#2a2f38" :foreground "#e6e6e6"))))
 '(nano-minibuffer-header-face ((t (:background "#343d46" :foreground "#a7cfa3" :height 1.1 :weight bold))))
 '(completions-common-part ((t (:foreground "#7cb7ff"))))
 '(file-name-shadow ((t (:foreground "#889099")))))
  
 ;;  (nano-minibuffer-mode 1)

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
  ;; (setq org-src-fontify-natively t)
  ;; (setq org-src-tab-acts-natively t)

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

;; (setq org-src-tab-acts-natively nil)

(setq org-src-preserve-indentation t)

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

(use-package avy
  :ensure t
  :config
  (avy-setup-default))

(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-mode drag-stuff-global-mode)
  :config
  ;; Enable if you want globally
  ;; (drag-stuff-global-mode 1)
  )

(use-package iedit
  :ensure t)

(use-package evil-multiedit
  :ensure t
  :after (evil iedit)
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

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
    "p" '(:ignore t :which-key "project management") 
    "b" '(:ignore t :which-key "buffer management")) 

  ;; Top-level SPC bindings
  (jonroby/leader-keys
    "a" 'save-buffer 
    "." 'consult-buffer
    "/" 'find-file 
    "s" 'consult-line
   ) 

  ;; Code folding under SPC c
  (jonroby/leader-keys
    "c h" 'hs-hide-block
    "c s" 'hs-show-block)

  ;; Window management under SPC w
  (jonroby/leader-keys
    "w j" 'split-window-below
    "w l" 'split-window-right
    "w d" 'delete-window)

  ;; Window management under SPC w
  (jonroby/leader-keys
    "p s" 'project-find-regexp
    "p f" 'project-find-file
    ) 

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

(jonroby/leader-keys
    "l" '(:ignore t :which-key "language server")
    "l h" '(eglot-hover :which-key "hover info")
    "l d" '(flymake-show-diagnostics-buffer :which-key "diagnostics")
    "l g" '(xref-find-definitions :which-key "go to definition")
    "l r" '(xref-find-references :which-key "find references"))

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
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
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

(setq treesit-language-source-alist
        '((python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        ;; (haskell  . ("https://github.com/tree-sitter/tree-sitter-haskell"))
        ;; (scheme   . ("https://github.com/6cdh/tree-sitter-scheme"))
        ;; (lean     . ("https://github.com/Julian/tree-sitter-lean"))
        ;; (rust     . ("https://github.com/tree-sitter/tree-sitter-rust"))
        ;; (elixir   . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" :subdir "typescript/src"))
        ;; (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" :subdir "tsx/src"))
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src") 
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src") 
        ;; (bash     . ("https://github.com/tree-sitter/tree-sitter-bash"))
        ;; (c        . ("https://github.com/tree-sitter/tree-sitter-c"))
        ;; (cpp      . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        ;; (json     . ("https://github.com/tree-sitter/tree-sitter-json"))
        ;; (html     . ("https://github.com/tree-sitter/tree-sitter-html"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css")))) 


;; (treesit-install-language-grammar 'typescript) 
;; (treesit-install-language-grammar 'tsx)

(dolist (lang '(python typescript tsx css))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang))) 
 

        ;; haskell scheme lean rust elixir javascript typescript bash c cpp json html css

;; (defun my/tab-indent-or-complete ()
 ;;  "Indent line or trigger completion."
 ;;  (interactive)
 ;;  (if (or (not (boundp 'completion-at-point-functions))
 ;;          (null (completion-at-point)))
 ;;      (indent-for-tab-command))) 


;; If you're using Evil
;; (define-key evil-insert-state-map (kbd "TAB") #'my/tab-indent-or-complete)



(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        ;; (haskell-mode . haskell-ts-mode)
        ;; (scheme-mode . scheme-ts-mode)
        ;; (rust-mode . rust-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . tsx-ts-mode)
        ;; Add more here as needed
        ))

(use-package lean4-mode
 :commands lean4-mode
 :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
      :rev :last-release
      ;; Or, if you prefer the bleeding edge version of Lean4-Mode:
      ;; :rev :newest
      ))

(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs
              '(lean4-mode . ("lake" "serve"))))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package clang-format
  :ensure t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package highlight-indent-guides
  :ensure t
  :hook ((haskell-mode . highlight-indent-guides-mode)
         (lean4-mode . highlight-indent-guides-mode)
         (python-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(global-set-key (kbd "C-c y") 'copy-full-path-to-kill-ring)

(electric-pair-mode 1)


