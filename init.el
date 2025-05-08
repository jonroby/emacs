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
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY"))

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
                    :family "Fira Code"
                    :height 140
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

 ;; For partial-completion

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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
      "b" '(:ignore t :which-key "buffer management") 
      "i" '(:ignore t :which-key "aider management")
      "l" '(:ignore t :which-key "eglot")) 

    ;; Top-level SPC bindings
    (jonroby/leader-keys
      "a" 'save-buffer 
      "." 'consult-buffer
      "/" 'find-file 
      "s" 'consult-line
     ) 

    ;; Code folding under SPC c
    (jonroby/leader-keys
      "i i" 'aidermacs-transient-menu) 

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
      "p ." 'consult-project-buffer
      "p t" 'consult-ripgrep
      "p p" 'project-switch-project
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

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref) 

  (jonroby/leader-keys
      "l l" 'eglot
      "l h" '(eglot-hover :which-key "hover info")
      "l d" '(flymake-show-diagnostics-buffer :which-key "diagnostics")
      "l g" '(xref-find-definitions :which-key "go to definition")
      "l r" '(xref-find-references :which-key "find references")
      "l a" '(eglot-code-actions :which-key "code actions")
      "l s" '(eglot-rename :which-key "rename symbol"))

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

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode)))



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

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(electric-pair-mode 1)

(global-display-line-numbers-mode 1)

;; (set-face-attribute 'minibuffer-prompt nil :inherit 'default :extend t :box '(:line-width (2 . 2) :color "#1C1E28"))

(use-package treesit-fold
  :load-path "~/.emacs.d/treesit-fold") 


(defun my/tsx-string-fragment-fold-only ()
  (when (eq major-mode 'tsx-ts-mode)
    (setq treesit-simple-indent-rules nil) ;; avoid interference
    (setq-local treesit-fold-range-function
      (lambda (node)
        (when (string= (treesit-node-type node) "string_fragment")
          (cons (treesit-node-start node)
                (treesit-node-end node))))))) 

(add-hook 'tsx-ts-mode-hook #'treesit-fold-mode) 
(add-hook 'tsx-ts-mode-hook #'my/tsx-string-fragment-fold-only)

(run-with-idle-timer
 1 nil
 (lambda ()
   (custom-set-faces
    '(default ((t (:foreground "#E4F0FB")))) ;; 
    ;; '(font-lock-variable-name-face ((t (:foreground "#B58EAE")))) 
    '(font-lock-function-name-face ((t (:foreground "#ACD7FF" :weight normal)))))))

(add-hook 'minibuffer-setup-hook
         (lambda ()
           (face-remap-add-relative 'nano-face-default :background nil)))

(setq-default line-spacing 0.32) 
(set-face-attribute 'default nil :height 140)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#81A1C1"))))  ; ;; 81A1C1 for blue
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#81A1C1"))))  ; green
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#81A1C1"))))  ; blue
;; You can continue for depth 4–9
 )

(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                minibuffer-setup-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (use-package indent-bars
;;   :hook ((python-ts-mode tsx-ts-mode) . indent-bars-mode)
;;   :custom
;;   (indent-bars-treesit-support t))

;;    (setq 

;;     indent-bars-color '(highlight :face-bg t :blend .075) 
;;     indent-bars-starting-column 0
;;     indent-bars-pattern "."
;;     indent-bars-width-frac 0.1
;;     indent-bars-pad-frac 0.1
;;     indent-bars-zigzag nil
;;     indent-bars-highlight-current-depth nil 
;;     indent-bars-prefer-character t
;;     indent-bars-display-on-blank-lines t)


(defun show-treesit-node-type ()
  "Print the Tree-sitter node type at point."
  (interactive)
  (message "Node type: %s" (treesit-node-type (treesit-node-at (point)))))

 (use-package ligature
  :config
  ;; Enable ligatures in all modes
  (ligature-set-ligatures 't '("www"))

  ;; EWW-specific ligatures (typographic)
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  ;; Enable Fira Code ligatures in all programming modes
  (ligature-set-ligatures 'prog-mode
    '("**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "!!." "!=="
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "==" "===" "==>" "=!=" "=>>" "=<<" "=/="
      "<=" "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-" "<<<"
      "<>" "<$>" "<|" "<|>" "<:" "<*" "<~" "<~>" "<~~" "<+>" "</>" "<$"
      ">=" ">>" ">>=" ">>>" ">>-" ">->" ">=>" ">:"
      "&&" "||" "||=" "||>" "|>" "|-" "|=" "::" ":>" ":<" ";;"
      "++" "+++" "+>" "?=" "??" "?:" "?."
      "__" "_|_" "~@" "~=" "~>" "~-" "~~" "~~>"
      "%%" ".=" ".-" ".." "..." "..<" ".?"
      "##" "###" "#(" "#?" "#_" "#_(" "#{" "#[" "#:" "#=" "#!"
      "/=" "/>" "/**" "//" "///" "/*"
      "(*" "*)" "$>" "^=")) 

  ;; Enable globally
  (global-ligature-mode t))

(with-eval-after-load 'avy
  (set-face-attribute 'avy-lead-face nil :background "#DA70D6")
  (set-face-attribute 'avy-lead-face-0 nil :background "#88DDFF")
  )

(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :bind (("C-x g" . magit-status)))

(use-package evil-collection
  :after (evil magit)
  :config
  (evil-collection-init))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "haiku"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain) 
(setq ediff-split-window-function 'split-window-horizontally)

;; (setq aidermacs-auto-ediff nil)
