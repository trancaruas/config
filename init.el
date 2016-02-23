;; -*- mode: Emacs-Lisp -*-

;; * Package setup
(require 'package)

(setq package-archives
      '(("GNU" . "http://elpa.gnu.org/packages/")
        ("MARMALADE" . "http://marmalade-repo.org/packages/")
        ("MELPA" . "http://melpa.milkbox.net/packages/")
        ("ELPA" . "http://tromey.com/elpa/")))

;; (unless package-archive-contents
;;   (package-refresh-contents))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)


;; * Initial settings
;; Garbage collection thresold, default 0.76Mb
(setq gc-cons-threshold 50000000)

(setq-default tab-width 8)
(setq enable-local-eval t)
(setq require-final-newline t)
(setq inhibit-startup-screen t)
(setq enable-local-variables t)
(setq-default cursor-type 'bar)
(setq history-delete-duplicates t)
(setq-default indent-tabs-mode nil)
(setq default-indicate-empty-lines t)
(setq large-file-warning-threshold 100000000)
(setq initial-buffer-choice "~/.emacs.d/scratch")

(savehist-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(setq visible-bell t)
(column-number-mode t)
(blink-cursor-mode -1)
(size-indication-mode t)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(setq ring-bell-function 'ignore)
(kill-buffer "*scratch*")

;; * Settings & functions
;; ** Global keys
(global-set-key [(control j)] 'eval-print-last-sexp)
(global-set-key [(control c) (r)] 'replace-regexp)
(global-set-key [(control w)] 'backward-kill-word)
(global-set-key [(meta u)] 'transpose-buffers)
(global-set-key [(meta tab)] 'other-window)
(global-set-key (kbd "C-S-SPC") 'rectangle-mark-mode)

;; ** Mac specific keys
(global-set-key [(super right)] 'end-of-line)
(global-set-key [(super left)] 'beginning-of-line)
(global-set-key [(super w)] 'kill-this-buffer)
;;(global-set-key [(super b)] 'iswitchb-buffer)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)
(bind-key "<s-return>" 'toggle-frame-fullscreen)

;; ** Mac specific settings
;; *** Transparency & fullscreen
(setq transparency-level 90)
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions
          (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
(setq ns-use-native-fullscreen nil)

;; *** Open current dir in Finder
(defun open-dir-in-finder ()
      "Open a new Finder window to the path of the current buffer"
      (interactive)
      (shell-command "open ."))
(bind-key "s-/" 'open-dir-in-finder)


;; ** Global settings & functions
;; *** Macros
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(global-set-key '[(meta f8)] 'toggle-kbd-macro-recording-on)
(global-set-key '[(f8)] 'call-last-kbd-macro)

;; *** Commenting
;; TODO can be replaced with 'comment line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; *** Lines killing & duplicating
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
        (move-end-of-line 1)
        (newline)
        (insert line-text))))
(global-set-key [(control d)] 'duplicate-line)

(defun kill-current-line (&optional n)
  "Implement control-y to kill current line"
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))
(global-set-key [(control y)] 'kill-current-line)

;; *** UUID generation
(defun insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)))))
(global-set-key (kbd "C-x p") 'insert-random-uuid)

;; *** Buffer title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; *** Smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)

;; *** Prevent esc-esc-esc destroying other windows
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;; *** Prevent M-backspace from putting deleted to kill-ring
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "M-<del>") 'backward-delete-word)


;; *** Greek letters mode line
(require 'cl)
(defvar mode-line-cleaner-alist
  `((auto-fill-function         . "")
    (smart-spacing-mode         . "")
    (auto-complete-mode         . " α")
    (company-mode               . " α")
    (yas-minor-mode             . " γ")
    (autopair-mode              . " ρ")
    (eldoc-mode                 . " ε")
    (undo-tree-mode             . " τ")
    (highlight-parentheses-mode . " φ")
    (volatile-highlights-mode   . " υ")
    (elisp-slime-nav-mode       . " δ")
    (workgroups-mode            . " ω")
    (hs-minor-mode              . " χ")
    (flex-autopair-mode         . " ψ")
    (projectile-mode            . " π")
    (outline-minor-mode         . " ο")
    (abbrev-mode                . " αβ")
    (ielm-mode                  . " εζ")
    (auto-revert-mode           . " αρ")
    (cider-mode                 . " ηζ")
    (cider-repl-mode            . " ηζ")
    (cider-interaction-mode     . " ηζ")
    (lisp-mode                  . " ελ")
    
    ;; major modes
    (org-indent-mode            . "η")
    (iimage-mode                . "ι")
    (isearch-mode               . "ς")
    (clojure-test-mode          . "τ")
    (clojure-mode               . "λ")
    (clojurescript-mode         . "ςλ")
    (nrepl-repl-mode            . "ηζ")
    (emacs-lisp-mode            . "ελ")
    (lisp-interaction-mode      . "ελ")
    (inferior-emacs-lisp-mode   . "εζ")
    (flyspell-mode              . "θ ")
    (completion-list-mode       . "ς")
    (debugger-mode              . "δ")
    (compilation-mode           . "κ")
    (help-mode                  . "χ")
    (text-mode                  . "τ")
    (fundamental-mode           . "φ")
    (hi-lock-mode               . "")
    (python-mode                . "π")
    (w3m-mode                   . "ψ")
    (org-mode                   . "ω")
    (org-agenda-mode            . "ωα")
    (calendar-mode              . "ωκ")
    (shell-mode                 . "ς")
    (package-menu-mode          . "π")
    (markdown-mode              . "μδ"))
  "alist for `clean-mode-line'.
 
when you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;; *** Automatic special keywords highlighting
(setq keywords-danger-pattern
      "\\(error\\|error\\|horrible\\|horrible\\)")
(setq keywords-critical-pattern
      "\\(bugs\\|fixme\\|bad\\|todo\\|todo\\|xxx\\|[ii][nn][vv][aa][ll][ii][dd]\\|[ff][aa][ii][ll][ee][dd]\\|[cc][oo][rr][rr][uu][pp][tt][ee][dd]\\)")
(setq keywords-optimal-pattern
      "\\(done\\|done\\|good\\|good\\)")

(make-face 'keywords-danger)
(make-face 'keywords-critical)
(make-face 'keywords-optimal)

(set-face-attribute 'keywords-danger nil :foreground "red" :background nil :weight 'extra-bold)
(set-face-attribute 'keywords-critical nil :foreground "orange" :background nil :weight 'bold)
(set-face-attribute 'keywords-optimal nil :foreground "mediumspringgreen" :background nil :weight 'bold)

;; Set up highlighting of special words for proper selected major modes only
(dolist (mode '(lisp-interaction-mode
                emacs-lisp-mode
                fundamental-mode
                svn-log-view-mode
                text-mode))
  (font-lock-add-keywords mode
        `((,keywords-danger-pattern 1 'keywords-danger prepend)
          (,keywords-critical-pattern 1 'keywords-critical prepend)
          (,keywords-optimal-pattern 1 'keywords-optimal prepend))))

;; Add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
  (font-lock-add-keywords nil
        `((,keywords-danger-pattern 1 'keywords-danger prepend)
          (,keywords-critical-pattern 1 'keywords-critical prepend)
          (,keywords-optimal-pattern 1 'keywords-optimal prepend))))

;; Set up highlighting of special words for selected major modes *and* all
;; major modes derived from them
(dolist (hook '(c++-mode-hook
                c-mode-hook
                change-log-mode-hook
                cperl-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                java-mode-hook
                latex-mode-hook
                lisp-mode-hook
                makefile-mode-hook
                message-mode-hook
                php-mode-hook
                python-mode-hook
                sh-mode-hook
                shell-mode-hook
                ssh-config-mode-hook
                clojure-mode-hook))
  (add-hook hook 'fontify-keywords))


;; * Packages
;; ** Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key [(super @)] 'er/expand-region))

;; ** Multuple cursors 
(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/mark-all-regexp-in-region (beg end)
   "find and mark all the parts in the region matching the given regexp search"
   (interactive "r")
   (let ((search (read-from-minibuffer "mark all regexp in region: "))
         (case-fold-search nil))
     (if (string= search "")
         (message "mark aborted")
       (progn
         (mc/remove-fake-cursors)
         (goto-char beg)
         (while (search-forward-regexp search end t)
           (push-mark (match-beginning 0))
           (mc/create-fake-cursor-at-point))
         (let ((first (mc/furthest-cursor-before-point)))
           (if (not first)
               (error "search failed for %s" search)
             (mc/pop-state-from-overlay first)))
         (if (> (mc/num-cursors) 1)
             (multiple-cursors-mode 1)
           (multiple-cursors-mode 0))))))

  (global-set-key [(super down)] 'mc/mark-next-like-this)
  (global-set-key [(super up)] 'mc/mark-previous-like-this)
  (global-set-key [(super shift m)] 'mc/edit-lines)
  (global-set-key [(control shift m)] 'mc/mark-all-regexp-in-region)
  (global-set-key [(super >)] 'mc/mark-next-like-this)
  (global-set-key [(super <)] 'mc/mark-previous-like-this))

;; ** Manual symbols highlighting
(use-package highlight-symbol
  :ensure t
  :config 
  ;;(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  ;;(setq highlight-symbol-on-navigation-p t)
  (custom-set-faces '(highlight-symbol-face ((t (:background "mediumpurple4")))))

  (global-set-key [(control .)] 'highlight-symbol-at-point)
  (global-set-key [(control >)] 'highlight-symbol-next)
  (global-set-key [(control <)] 'highlight-symbol-prev))

;; ** Autocompletion & popup documentation
(use-package company
  :ensure t
  :config
  (global-company-mode '(not minibuffer-mode))

  (defun indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (looking-at "\\_>")
          (company-complete-common)
        (indent-according-to-mode)))
    (when (outline-on-heading-p)
      (outline-cycle)))
  
  (global-set-key [(tab)] 'indent-or-complete)
  
  (custom-set-faces
   '(tooltip ((t (:background "mediumpurple4" :foreground "wheat"))))
   '(company-preview ((t (:background "mediumpurple4" :foreground "wheat"))))
   '(company-preview-common ((t (:inherit company-preview :background "mediumpurple4" :foreground "lightblue"))))
   '(company-scrollbar-bg ((t (:background "lemonchiffon"))))
   '(company-scrollbar-fg ((t (:background "mediumpurple4"))))
   '(company-tooltip ((t (:inherit default :foreground "gray75" :background "lemonchiffon"))))
   '(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "mediumpurple4"))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :foreground "white" :background "mediumpurple4"))))
   '(company-tooltip-selection
     ((t (:inherit font-lock-function-name-face :foreground "grey75" :background "mediumpurple4"))))))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))
  
(use-package popup
  :ensure t
  :config
  (defun describe-function-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point))
           (description (save-window-excursion
                          (describe-function thing)
                          (switch-to-buffer "*Help*")
                          (buffer-string))))
      (popup-tip description
                 :point (point)
                 :around t
                 :height 30
                 :scroll-bar nil
                 :margin t)))
  (global-set-key (kbd "C-c d") 'describe-function-in-popup))

;; ** UI & UX

(use-package saveplace
  :ensure t
  :config
  (save-place-mode t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace"))

(use-package tramp
  :ensure t
  :config (setq tramp-default-method "scp"))

(use-package ace-jump-mode
  :ensure t
  :config (global-set-key (kbd "C-0") 'ace-jump-char-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package sublime-themes
  :ensure t
  :config
  (defun switch-theme (theme)
    "Disables any currently active theme and loads new theme."
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapc 'symbol-name
                                     (custom-available-themes))))))
    (let ((enabled-themes custom-enabled-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)))

  (defun disable-active-theme ()
    "Disables any currently active theme listed in `custom-enabled-themes'."
    (interactive)
    (message "Unloading current theme")
    (mapc #'disable-theme custom-enabled-themes))
  (load-theme 'charcoal t)
  
  (bind-key "s-<f12>" 'switch-theme)
  (bind-key "s-S-<f12>" 'disable-active-theme))

(use-package powerline
  :ensure t
  :init
  (powerline-center-theme)
  (setq powerline-default-separator (quote utf-8)))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-minor-modes-separator " ")
  (setq spaceline-show-default-input-method nil))

(use-package outshine
  :ensure outline
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

(use-package hideshowvis
  :ensure t
  :config
  ;; https://gist.github.com/jasonm23/514946
  (hideshowvis-symbols)
  (define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 254 124 56 16 0 0])
  (define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32])
  (global-set-key [(control -)] 'hs-toggle-hiding)
  (global-set-key [(control =)] 'hs-toggle-hiding))

(use-package nlinum
  :ensure t
  :config
  (setq nlinum-format " %3d ")
  (global-set-key [(control shift n)] 'nlinum-mode))

(use-package iswitchb
  :config
  (iswitchb-mode)
  (global-set-key [(control x) (control b)] 'iswitchb-display-buffer)
  (defun iswitchb-local-keys ()
    (mapc (lambda (k) 
            (let* ((key (car k)) (fun (cdr k)))
              (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
          '(("<right>" . iswitchb-next-match)
            ("<left>"  . iswitchb-prev-match)
            ("<up>"    . ignore)
            ("<down>"  . ignore))))
  (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys))

(use-package dired-k
  :ensure t
  :config
  (add-hook 'dired-initial-position-hook 'dired-k)
  (define-key dired-mode-map (kbd "k") 'dired-k))

(use-package spinner
  :ensure t)

(use-package paradox
  :ensure t 
  :config
  (setq paradox-automatically-star t)
  (setq paradox-github-token "0e43ca66bba22f85b2afbd9526b1eff567660110"))

(use-package server
  :config
  (setq server-use-tcp t)
  (setq server-host "localhost")
  (server-start)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))


;; ** Customized Aquamacs dholm's tabbar
(use-package tabbar
  :ensure t
  :config
  (setq one-buffer-one-frame-mode t)
  ;; (defun tabbar-buffer-groups ()
;;     "return the list of group names the current buffer belongs to.
;; this function is a custom function for tabbar-mode's tabbar-buffer-groups.
;; this function group all buffers into 3 groups:
;; those dired, those user buffer, and those emacs buffer.
;; emacs buffer are those starting with “*”."
;;     (list
;;      (cond
;;       ((string-equal "*" (substring (buffer-name) 0 1)) "emacs buffer")
;;       ((eq major-mode 'dired-mode) "dired")
;;       (t "user buffer"))))
;;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

  (setq tabbar-buffer-groups-function
          (lambda ()
            (list "All")))
  
  
  (defcustom delete-window-preserve-buffer '("\*messages\*" "\*Help\*")
    "preserve these buffers when deleting window displaying them.
when `one-buffer-one-frame-mode' or `tabbar-mode' are on,
a buffer is killed when the last window displaying it is
deleted by way of user interaction via high-level commands such
as `close-window', unless the buffer name is listed in this
customization variable, or this variable is set to `t'."
    :group 'aquamacs
    :group 'frames
    :type '(choice (repeat string)    
                   (set (const "\*messages\*") (const "\*Help\*"))
                   (const t)))
  
  (defun killable-buffer-p (buf)
    "returns non-nil if buffer buf may be be killed.
customize `delete-window-preserve-buffer' to configure."
    (if (or (eq t delete-window-preserve-buffer)
            (member (get-bufname buf) delete-window-preserve-buffer))
        nil
      t))

  (defun aquamacs-delete-window (&optional window)
    "remove window from the display.  default is `selected-window'.
if window is the only one in its frame, then `delete-frame' too,
even if it's the only visible frame."
    (interactive)
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t)
        (aquamacs-delete-frame)
      (old-delete-window (selected-window))))

  (or (fboundp 'old-delete-window)
      (fset 'old-delete-window (symbol-function 'delete-window)))

  (custom-set-faces
   '(tabbar-button ((t (:inherit tabbar-default :background "grey75" :box nil))))
   '(tabbar-default ((t (:inherit nil :stipple nil :background "grey80" :foreground "black" :box nil :strike-through nil :underline nil :slant normal :weight normal :height 110 :width normal :family "pragmata pro"))))
   '(tabbar-selected ((t (:inherit tabbar-default :stipple nil :background "grey95" :foreground "gray20" :inverse-video nil :box (:line-width 3 :color "grey95")))))
   '(tabbar-selected-highlight ((t (:background "grey95" :foreground "black"))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
   '(tabbar-separator ((t (:inherit tabbar-default :background "grey50" :foreground "grey50"))))
   '(tabbar-unselected-highlight ((t (:background "grey75" :foreground "black"))))
   '(tabbar-unselected-modified ((t (:inherit tabbar-unselected)))))
 
  (add-to-list 'load-path "~/.emacs.d/lisp/tabbar-dholm")
  (require 'aquamacs-tabbar)
  (setq tabbar-key-binding-modifier-list '(super))
  (defvar header-line-inhibit-window-list '())
  (add-to-list 'header-line-inhibit-window-list (selected-window))
  (tabbar-mode)
  (global-set-key [(control tab)] 'tabbar-forward)
  (global-set-key [(control shift tab)] 'tabbar-backward))

;; ** Visual bookmarks
(use-package bm
  :ensure t
  :config
  (setq bm-cycle-all-buffers t)
  (global-set-key (kbd "<f3>") 'bm-toggle)
  (global-set-key (kbd "<m-f3>") 'bm-show-all)
  (global-set-key (kbd "<f4>") 'bm-next)
  (global-set-key (kbd "<m-f4>") 'bm-previous)
  
  (setq-default bm-buffer-persistence t)
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  (custom-set-faces
   '(bm-fringe-persistent-face ((t (:background "darkorange1" :foreground "black"))))
   '(bm-persistent-face ((t (:background "darkorange1" :foreground "black"))))))

;; ** Development
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'outline-minor-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-display-help-banner nil)
  (setq cider-popup-stacktraces nil)
  (setq cider-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  :config
  (defun describe-clojure-function-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point))
           (description (save-window-excursion
                         (cider-doc-lookup (symbol-name thing))
                         (switch-to-buffer "*cider-doc*")
                         (buffer-string))))
     (popup-tip description
                :point (point)
                :around t
                :height 30
                :scroll-bar nil
                :margin t)))

  (defconst cider-clojuredoc-url "https://clojuredocs.org/")

  (defun cider-clojuredoc-replace-special (name)
    "Convert the dashes in NAME to a clojuredoc friendly format."
    (thread-last name
      (replace-regexp-in-string "\\?" "_QMARK_")
      (replace-regexp-in-string "\\." "_DOT_")
      (replace-regexp-in-string "\\/" "_SLASH_")
      (replace-regexp-in-string "\\(\\`_\\)\\|\\(_\\'\\)" "")))

  (defun cider-clojuredoc-url (name ns)
    "Generate a clojuredoc search?q= url from NAME, NS."
    (let ((base-url cider-clojuredoc-url))
      (when (and name ns)
        (concat base-url  "search?q=" name ))))
  ;;      (concat base-url  "search?q=" ns "/" (cider-clojuredoc-replace-special name) "/"))))

  (defun cider-clojuredoc-web-lookup (symbol)
    "Look up the clojuredoc documentation for SYMBOL."
    (if-let ((var-info (cider-var-info symbol)))
        (let ((name (nrepl-dict-get var-info "name"))
              (ns (nrepl-dict-get var-info "ns")))
          (browse-url (cider-clojuredoc-url name ns)))
      (error "Symbol %s not resolved" symbol)))

  (defun cider-clojuredoc-web (&optional arg)
    "Open clojuredoc documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
    (interactive "P")
    (funcall (cider-prompt-for-symbol-function arg)
             "ClojureDoc doc for"
             #'cider-clojuredoc-web-lookup))
  (define-key clojure-mode-map (kbd "C-c C-d C-c") 'cider-clojuredoc-web)
  (define-key cider-mode-map (kbd "C-c C-d C-c") 'cider-clojuredoc-web)

  (define-key clojure-mode-map (kbd "C-c d") 'describe-clojure-function-in-popup)
  (define-key cider-repl-mode-map (kbd "C-c d") 'describe-clojure-function-in-popup)
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer))

(use-package slime
  :ensure slime-company
  :config
  (require 'slime-fancy)
  ;; start slime with M-- M-x
  (setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64" "-K utf-8") :coding-system utf-8-unix)
        (sbcl ("/usr/local/bin/sbcl" "-quiet") :coding-system utf-8-unix)))
  
  ;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
  (add-to-list 'slime-contribs 'slime-fancy)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-company slime-indentation))

  ;; TODO merge all popup describe functions
  (defun slime-describe-function-in-popup ()
    (interactive)
    (let* ((thing (symbol-name (symbol-at-point)))
           (description (save-window-excursion
                          (slime-describe-symbol thing)
                          (switch-to-buffer "*slime-description*")
                          (buffer-string))))
      (popup-tip description
                 :point (point)
                 :around t
                 :height 30
                 :scroll-bar t
                 :margin t)))
  (bind-key "C-c d" 'slime-describe-function-in-popup lisp-mode-map))


;; ** Parentheses management
(use-package flex-autopair
  :ensure t)

(use-package highlight-parentheses
  :ensure paren
  :config
  (show-paren-mode t)
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "white")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "red"))


;; ** Org-mode & documentation
(use-package org
  :config
  (setq org-support-shift-select t))

(use-package ox-latex
  :config
  (setenv "path" (concat (getenv "path") ":/usr/local/texlive/2015/bin/x86_64-darwin"))
  (setq exec-path (append exec-path '("/usr/local/texlive/2015/bin/x86_64-darwin")))
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "cmap"))
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("english,russian" "babel"))
  (add-to-list 'org-latex-packages-alist '("t2a" "fontenc"))
  (add-to-list 'org-latex-packages-alist '("utf8" "inputenc")))

(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode" "major mode for editing markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;; * Custom faces & vars
(custom-set-faces
 ;; custom-set-faces was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 '(default ((t (:font "pragmatapro for powerline"))))
 '(anzu-mode-line ((t (:foreground "purple4" :weight bold))))
 '(bm-fringe-persistent-face ((t (:background "darkorange1" :foreground "black"))))
 '(bm-persistent-face ((t (:background "darkorange1" :foreground "black"))))
 '(company-preview ((t (:background "mediumpurple4" :foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :background "mediumpurple4" :foreground "lightblue"))))
 '(company-scrollbar-bg ((t (:background "lemonchiffon"))))
 '(company-scrollbar-fg ((t (:background "mediumpurple4"))))
 '(company-tooltip ((t (:inherit default :foreground "gray75" :background "lemonchiffon"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "mediumpurple4"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "white" :background "mediumpurple4"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :foreground "grey75" :background "mediumpurple4"))))
 '(cscope-line-number-face ((t (:foreground "dark cyan"))))
 '(cscope-separator-face ((t (:foreground "red" :underline t :weight bold))))
 '(flyspell-duplicate ((t (:inherit nil :underline t))))
 '(highlight-symbol-face ((t (:background "mediumpurple4"))))
 '(hs-face ((t (:foreground "yellow1" :box 1))))
 '(linum ((t (:inherit (shadow default) :foreground "dark slate gray"))))
 '(scroll-bar ((t (:background "red" :foreground "yellow"))))
 '(tabbar-button ((t (:inherit tabbar-default :background "grey75" :box nil))))
 '(tabbar-default ((t (:inherit nil :stipple nil :background "grey80" :foreground "black" :box nil :strike-through nil :underline nil :slant normal :weight normal :height 110 :width normal :family "pragmata pro"))))
 '(tabbar-selected ((t (:inherit tabbar-default :stipple nil :background "grey95" :foreground "gray20" :inverse-video nil :box (:line-width 3 :color "grey95")))))
 '(tabbar-selected-highlight ((t (:background "grey95" :foreground "black"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "grey50" :foreground "grey50"))))
 '(tabbar-unselected-highlight ((t (:background "grey75" :foreground "black"))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected))))
 '(tooltip ((t (:background "mediumpurple4" :foreground "wheat")))))

(custom-set-variables
 ;; custom-set-variables was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "7b4d9b8a6ada8e24ac9eecd057093b0572d7008dbd912328231d0cada776065a"
     "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664"
     default))))

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

