;; -*- mode: Emacs-Lisp; eval: (outline-minor-mode t); -*-

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

;; Garbage collection thresold, default 0.76Mb
(setq gc-cons-threshold 50000000)

(setq-default cursor-type 'bar)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(savehist-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq require-final-newline t)
(delete-selection-mode t)
(setq history-delete-duplicates t)
(setq default-indicate-empty-lines t)
(setq large-file-warning-threshold 100000000)

(setq initial-buffer-choice "~/.emacs.d/scratch")

(setq enable-local-variables t)
(setq enable-local-eval t)



;; * MAC SPECIFIC
;; transparency
(setq transparency-level 90)
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions
          (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))

(global-set-key [(super right)] 'end-of-line)
(global-set-key [(super left)] 'beginning-of-line)
(global-set-key [(super w)] 'kill-this-buffer)
(global-set-key [(super b)] 'iswitchb-buffer)

;; * GLOBAL KEYS
(global-set-key [(control j)] 'eval-print-last-sexp)
(global-set-key [(control c) (r)] 'replace-regexp)
(global-set-key [(control w)] 'backward-kill-word)
(global-set-key [(meta u)] 'transpose-buffers)
(global-set-key [(meta tab)] 'other-window)


;; * CUSTOM FUNCTIONS
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

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

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

;; * CUSTOM SETTINGS
;; ** BUFFER TITLE
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; *** scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)


;; prevent esc-esc-esc destroying other windows
(defadvice keyboard-escape-quit
    (around my-keyboard-escape-quit activate)
  (flet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))

;; ** CLEAN MODE LINE
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (autopair-mode . " ρ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " τ")
    (highlight-parentheses-mode . " φ")
    (volatile-highlights-mode . " υ")
    (elisp-slime-nav-mode . " δ")
    (workgroups-mode . " ω")
    (hs-minor-mode . " χ")
    (nrepl-repl-mode . "ηζ")
    (cider-mode . " ηζ")
    (cider-repl-mode . " ηζ")
    (cider-interaction-mode . " ηζ")
    (ielm-mode . " εζ")
    (lisp-mode . " ελ")
    (auto-fill-function . "")
    (org-indent-mode . "η")
    (smart-spacing-mode . "")
    (iimage-mode . "ι")
    (isearch-mode . "ς")
    (clojure-test-mode . "τ")
    (flex-autopair-mode . " Ψ")
    (company-mode . " α")
    (projectile-mode . " Π")
    (outline-minor-mode . " ")
    
    ;; Major modes
    (flyspell-mode . "θ ")
    (completion-list-mode . "ς")
    (debugger-mode . "Δ")
    (compilation-mode . "κ")
    (help-mode . "Χ")
    (text-mode . "τ")
    (fundamental-mode . "Φ")
    (clojure-mode . "λ")
    (clojurescript-mode . "ςλ")
    (hi-lock-mode . "")
    (python-mode . "π")
    (w3m-mode . "Ψ")
    (org-mode . "Ω")
    (org-agenda-mode . "Ωα")
    (calendar-mode . "Ωκ")
    (shell-mode . "ς")
    (package-menu-mode . "Π")
    (emacs-lisp-mode . "ελ")
    (lisp-interaction-mode . "ελ")
    (inferior-emacs-lisp-mode . "εζ")
    (markdown-mode . "μδ"))
  "Alist for `clean-mode-line'.
 
When you add a new element to the alist, keep in mind that you
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

;; prevent M-backspace from putting deleted to kill-ring
(defadvice backward-kill-word (around fix activate)
  (flet ((kill-region (b e) (delete-region b e)))
    ad-do-it))


;; ** EMACS SERVER
(setq server-use-tcp t)
(setq server-host "localhost")
(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; ** KEYWORDS HIGHLIGHTING SPECIAL WORDS
(setq keywords-danger-pattern
      "\\(ERROR\\|error\\|HORRIBLE\\|horrible\\)")
(setq keywords-critical-pattern
      "\\(BUGS\\|FIXME\\|BAD\\|TODO\\|todo\\|XXX\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
(setq keywords-optimal-pattern
      "\\(DONE\\|Done\\|GOOD\\|Good\\)")

(make-face 'keywords-danger)
(make-face 'keywords-critical)
(make-face 'keywords-optimal)
;; (GNUEmacs (set-face-attribute 'keywords-critical nil
;;                               :foreground ")red" :background "yellow"
;;                               :weight 'bold))

(set-face-attribute 'keywords-danger nil
		    :foreground "red"
		    :background nil
		    :weight 'bold)

(set-face-attribute 'keywords-critical nil
		    :foreground "orange"
		    :background nil
		    :weight 'bold)

(set-face-attribute 'keywords-optimal nil
		    :foreground "MediumSpringGreen"
		    :background nil
		    :weight 'bold)

;; set up highlighting of special words for proper selected major modes only
(dolist (mode '(lisp-interaction-mode
		emacs-lisp-mode
		fundamental-mode
                svn-log-view-mode
                text-mode))  ; no interference with Org mode (which derives
					; from text-mode)
  (font-lock-add-keywords mode
			  `((,keywords-danger-pattern 1 'keywords-danger prepend)
			    (,keywords-critical-pattern 1 'keywords-critical prepend)
			    (,keywords-optimal-pattern 1 'keywords-optimal prepend))))

;; add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
  (font-lock-add-keywords nil
			  `((,keywords-danger-pattern 1 'keywords-danger prepend)
			    (,keywords-critical-pattern 1 'keywords-critical prepend)
			    (,keywords-optimal-pattern 1 'keywords-optimal prepend))))

;; set up highlighting of special words for selected major modes *and* all
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


(setq ring-bell-function 'ignore)

(kill-buffer "*scratch*")

;; ** FACES
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "purple4" :weight bold))))
 '(bm-fringe-persistent-face ((t (:background "DarkOrange1" :foreground "Black"))))
 '(bm-persistent-face ((t (:background "DarkOrange1" :foreground "Black"))))
 '(company-preview ((t (:background "MediumPurple4" :foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :background "mediumpurple4" :foreground "lightblue"))))
 '(company-scrollbar-bg ((t (:background "#ffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffff"))))
 '(company-tooltip ((t (:inherit default :background "#f2f2f2"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "lightblue"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(cscope-line-number-face ((t (:foreground "dark cyan"))))
 '(cscope-separator-face ((t (:foreground "red" :underline t :weight bold))))
 '(flyspell-duplicate ((t (:inherit nil :underline t))))
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :family "Sans Serif"))))
 '(highlight-symbol-face ((t (:background "yellow1"))))
 '(hs-face ((t (:foreground "yellow1" :box 1))))
 '(linum ((t (:inherit (shadow default) :foreground "dark slate gray"))))
 '(powerline-active2 ((t (:inherit mode-line :background "grey40"))))
 '(scroll-bar ((t (:background "red" :foreground "yellow"))))
 '(tabbar-button ((t (:inherit tabbar-default :background "grey75" :box nil))))
 '(tabbar-default ((t (:inherit nil :stipple nil :background "grey80" :foreground "black" :box nil :strike-through nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Pragmata Pro"))))
 '(tabbar-selected ((t (:inherit tabbar-default :stipple nil :background "grey95" :foreground "gray20" :inverse-video nil :box (:line-width 3 :color "grey95")))))
 '(tabbar-selected-highlight ((t (:background "grey95" :foreground "black"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "grey50" :foreground "grey50"))))
 '(tabbar-unselected-highlight ((t (:background "grey75" :foreground "black"))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected)))))


(set-face-attribute 'default nil :font "PragmataPro")


;; * PACKAGES
(use-package spinner
  :ensure t)

(use-package paradox
  :ensure t
  :config
  (setq paradox-github-token "0e43ca66bba22f85b2afbd9526b1eff567660110"))

;; ** MULTUPLE CURSORS & EXPAND REGION
(use-package expand-region
  :ensure t
  :config
  (global-set-key [(super @)] 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/mark-all-regexp-in-region (beg end)
  "Find and mark all the parts in the region matching the given regexp search"
  (interactive "r")
  (let ((search (read-from-minibuffer "Mark all regexp in region: "))
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (while (search-forward-regexp search end t)
          (push-mark (match-beginning 0))
          (mc/create-fake-cursor-at-point))
        (let ((first (mc/furthest-cursor-before-point)))
          (if (not first)
              (error "Search failed for %S" search)
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

(use-package highlight-symbol
  :ensure t
  :config 
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (setq highlight-symbol-on-navigation-p t)
  (global-set-key [(control .)] 'highlight-symbol-at-point)
  (global-set-key [(control >)] 'highlight-symbol-next)
  (global-set-key [(control <)] 'highlight-symbol-prev))

;; ** AUTOCOMPLETION
(use-package company
  :ensure t)

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
               :scroll-bar t
               :margin t)))
  (global-set-key (kbd "C-c d") 'describe-function-in-popup))

;; ** USABILITY
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace"))

(use-package tramp
  :ensure t
  :config (setq tramp-default-method "scp"))

;; ** VISUALS
(use-package color-theme
  :ensure t
  :config
  (color-theme-initialize)
  (color-theme-charcoal-black)
  (set-cursor-color "yellow"))

(use-package powerline
  :ensure t
  :config (powerline-center-theme))

(use-package outshine
  :ensure t
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package hideshowvis
  :ensure t
  :config
  (hideshowvis-symbols)
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
    (mapc (lambda (K) 
            (let* ((key (car K)) (fun (cdr K)))
              (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
          '(("<right>" . iswitchb-next-match)
            ("<left>"  . iswitchb-prev-match)
            ("<up>"    . ignore             )
            ("<down>"  . ignore             ))))
  (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys))

;; ** VISUAL BOOKMARKS
(use-package bm
  :ensure t
  :config
  (setq bm-cycle-all-buffers t)
  (global-set-key (kbd "<f3>") 'bm-toggle)
  (global-set-key (kbd "<M-f3>") 'bm-show-all)
  (global-set-key (kbd "<f4>") 'bm-next)
  (global-set-key (kbd "<M-f4>") 'bm-previous)
  
  (setq-default bm-buffer-persistence t)
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'find-file-hooks 'bm-buffer-restore))

;; ** DEV
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; ** PARENTHESES
(use-package flex-autopair
  :ensure t)

(use-package highlight-parentheses
  :ensure t
  :config
  (show-paren-mode nil)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (highlight-parentheses-mode)))
  (set-face-background 'show-paren-match-face nil)
  (set-face-foreground 'show-paren-match-face "white")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))
