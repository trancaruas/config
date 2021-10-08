;; -*- mode: Emacs-Lisp -*-

;; * PACKAGE SETUP
(setenv "http_proxy" "http://www-proxy.us.oracle.com:80")
(setenv "https_proxy" "http://www-proxy.us.oracle.com:80")
(setq url-proxy-services '(("http" . "www-proxy.us.oracle.com:80")
			   ("https" . "www-proxy.us.oracle.com:80")))

(require 'package)

(setq package-archives
      '(("GNU"       . "http://elpa.gnu.org/packages/")
        ("ELPA"      . "http://tromey.com/elpa/")
        ("MELPA"     . "http://melpa.org/packages/")
        ("ORGMODE"   . "http://orgmode.org/elpa/")
        ;; ("MARMALADE" . "http://marmalade-repo.org/packages/")
        ))

(package-initialize)

(unless package-archive-contents
   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;(require 'benchmark-init)
;(benchmark-init/activate)


;; * BETTER DEFAULTS
;; Garbage collection thresold, default 0.76Mb
; (setq gc-cons-threshold 50000000)
(setq gc-cons-threshold 100000000)

(set-cursor-color "red")
(setq-default tab-width 8)
(setq enable-local-eval t)
(setq require-final-newline t)
(setq inhibit-startup-screen t)
(setq enable-local-variables t)
(setq-default cursor-type 'bar)
(setq history-delete-duplicates t)
(setq-default help-window-select t)
(setq-default indent-tabs-mode nil)
(setq default-indicate-empty-lines t)
(setq emacs-lisp-docstring-fill-column nil)
(setq large-file-warning-threshold 100000000)
(setq initial-buffer-choice "~/.emacs.d/scratch")

;; (kill-buffer "*scratch*")

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
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq ring-bell-function 'ignore)

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; * SETTINGS & FUNCTIONS
;; ** Platform-specific settings
(cond
 ((string-equal system-type "windows-nt")
  (load "win"))
 ((string-equal system-type "darwin")
  (load "osx"))
 ((string-equal system-type "gnu/linux")
  (load "lin")))

;; (cond
;;  ((string-equal window-system "w32")
;;   (load "win"))
;;  ((string-equal window-system "ns")
;;   (load "osx"))
;;  ((string-equal window-system "x")
;;   (load "lin")))

;; ** Global keys
(global-set-key [(control j)] 'eval-print-last-sexp)
(global-set-key [(control c) (r)] 'replace-regexp)
(global-set-key [(control w)] 'backward-kill-word)
(global-set-key [(meta u)] 'transpose-buffers)
(global-set-key [(meta tab)] 'other-window)
(global-set-key [(control shift space)] 'rectangle-mark-mode)

;; ** Mac specific keys
(global-set-key [(super right)] 'end-of-line)
(global-set-key [(super left)] 'beginning-of-line)
(global-set-key [(super w)] 'kill-this-buffer)
(global-set-key [(super b)] 'ido-switch-buffer)
(global-set-key [(super ?-)] 'text-scale-decrease)
(global-set-key [(super ?=)] 'text-scale-increase)
(global-set-key [(super return)] 'toggle-frame-fullscreen)

(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)] 'end-of-line)


;; ** Mac specific settings (seems working in windows too)
;; *** Transparency & fullscreen
(setq transparency-level 95)
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions
          (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
(setq ns-use-native-fullscreen t)
 
;; ** Global settings & functions
;; *** General
(defun read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; w/a for solarized theme absent defun
(defun solarized-color-blend (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 with ALPHA.
COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").
Alpha should be a float between 0 and 1."
  (apply 'color-rgb-to-hex
         (-zip-with '(lambda (it other)
                       (+ (* alpha it) (* other (- 1 alpha))))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))

(eval-after-load "bytecomp"
  '(add-to-list 'byte-compile-not-obsolete-funcs
                'preceding-sexp))

(defun make-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(global-set-key [(control n)] 'make-buffer-frame)

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

;; map to play fn button
(global-set-key '[(meta f8)] 'toggle-kbd-macro-recording-on)
(global-set-key '[(f8)] 'call-last-kbd-macro)

(define-key emacs-lisp-mode-map [(control c) (control c)] 'eval-sexp-fu-eval-sexp-inner-list)


;; *** Commenting
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
(global-set-key (kbd "C-x u") 'insert-random-uuid)


;; *** Buffer title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; *** Prevent esc-esc-esc destroying other windows
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;; *** Prevent dialogs popup
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

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

(global-set-key [(meta delete)] 'backward-delete-word)
(global-set-key [(control w)] 'backward-delete-word)

;; TODO delete one of the below
(global-set-key [(meta delete)] 'backward-delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)


;; *** Automatic special keywords highlighting
;; TODO move fixed faces keywords highligthing to themed faces
;; find app^W package for this
(setq keywords-danger-pattern
      "\\(error\\|ERROR\\|horrible\\|HORRIBLE\\)")
(setq keywords-critical-pattern
      "\\(bugs\\|fixme\\|bad\\|todo\\|TODO\\|xxx\\|[ii][nn][vv][aa][ll][ii][dd]\\|[ff][aa][ii][ll][ee][dd]\\|[cc][oo][rr][rr][uu][pp][tt][ee][dd]\\)")
(setq keywords-optimal-pattern
      "\\(done\\|DONE\\|good\\|GOOD\\)")

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


;; * PACKAGES
;; ** Editing
(use-package visual-regexp
  :ensure t
  :bind
  (([(control c) (r)] . vr/replace)
   ([(control c) (q)] . vr/query-replace)
   ([(control c) (m)] . vr/mc-mark)))

;; TODO relations between expand region / multi-cursor / highlight word under cursor
(use-package expand-region
  :ensure t
  :bind (([(super @)] . er/expand-region)
         ([(control =)] . er/expand-region))
  :config
  ;; to mark whole word with cursor in the middle
  (defadvice er/expand-region (before er/expand-region activate)
    (if (not (= (point) (car (bounds-of-thing-at-point 'word))))
        (backward-word))))

;; ** Multuple cursors 
(use-package multiple-cursors
  :ensure t
  :bind ((     [(super down)] . mc/mark-next-like-this)
         (       [(super up)] . mc/mark-previous-like-this)
         (  [(super shift m)] . mc/edit-lines)
         ([(control shift m)] . mc/mark-all-regexp-in-region)
         (        [(super >)] . mc/mark-next-like-this)
         (        [(super <)] . mc/mark-previous-like-this))
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
           (multiple-cursors-mode 0)))))))

;; ** Manual symbols highlighting
(use-package highlight-symbol
  :ensure t
  :bind
  (([(control .)] . highlight-symbol-or-region-at-point)
   ([(control >)] . highlight-symbol-next)
   ([(control <)] . highlight-symbol-prev))
  :config
  (defun get-region-symbol ()
    (interactive)
    (if mark-active
        (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
          (if (= (length selection) 0)
              nil
            selection))
      nil))
  
  (defun highlight-symbol-or-region-at-point ()
    (interactive)
    (let ((selection (get-region-symbol)))
      (if selection
          (highlight-symbol-at-point selection)
        (highlight-symbol-at-point))))

  (custom-set-faces '(highlight-symbol-face ((t (:background "mediumpurple4")))))
  ;;(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  ;;(setq highlight-symbol-on-navigation-p t)
  )


;; ** Autocompletion & popup documentation
(use-package company
  :ensure t
  :bind (([(tab)] . indent-or-complete))
  :config
  (global-company-mode '(not minibuffer-mode))

  (defun indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (if (looking-at "\\_>")
            (company-complete-common)
          (indent-according-to-mode))))
    (when (outline-on-heading-p)
      (outline-cycle))
    )
  
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

(use-package company-emoji
  :ensure t
  :init
  (add-to-list 'company-backends 'company-emoji)
  (defun set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Noto Sans") frame 'prepend)
      ;; (set-fontset-font t 'symbol (font-spec :family "EmojiOne Color") frame 'prepend)
      ))
;;   (--set-emoji-font nil)
  (add-hook 'after-make-frame-functions '--set-emoji-font))

;; (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
;; (member "Segoe UI" (font-family-list))

;; (member "Noto Sans" (font-family-list))
;; ("EmojiOne Color" "Noto Sans" "Noto Sans UI" "Arial" "Arial Black" "Calibri" "Calibri Light" "Cambria" "Cambria Math" "Candara" "Comic Sans MS" "Consolas" ...)

;; (defun set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (set-fontset-font t 'symbol (font-spec :family "Noto Sans") frame 'prepend))
;; (defun set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (set-fontset-font t 'symbol (font-spec :family "EmojiOne Color") frame 'prepend))
;; (defun set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (set-fontset-font t 'symbol (font-spec :family "Segoe UI") frame 'prepend))
;; (add-hook 'after-make-frame-functions 'set-emoji-font)

;; (set-emoji-font (selected-frame))

;; (setq emojify-company-tooltips-p t)
;; (setq emojify-display-style 'unicode)

;; (set-selection-coding-system 'utf-16-le)
;; (set-default-coding-systems 'utf-16-le)
;; ρ

;; (require 'unicode-fonts)
;; (unicode-fonts-setup)
;; unicode-fonts-blocks
;; nil
;; (scroll-bar-mode -1)


;; (use-package yascroll
;;   :ensure t
;;   :init
;;   (global-yascroll-bar-mode t))

(use-package popup
  :ensure t
  :bind ([(control c) (d)] . describe-function-in-popup)
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
                 :margin t))))


(use-package google-translate
  :bind ([(super shift t)] . google-translate-at-point)
  :config
  (require 'google-translate-default-ui)
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "ru"
        google-translate-output-destination 'popup))


;; ** UI & UX
;; *** Smooth scrolling
;; TODO scrolling totally messed
(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 2)

  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 't
        scroll-step 1)

  (smooth-scrolling-mode 1))

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
  :bind (([(shift super f12)] . disable-active-theme)
         ([(super f12)] . switch-theme))
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
    (mapc #'disable-theme custom-enabled-themes)))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  (setq powerline-default-separator (quote utf-8)))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-minor-modes-separator " ")
  (setq spaceline-show-default-input-method nil))

;; (use-package outshine
;;   :ensure t
;;   ;; :defer t
;;   :config
;;   (require 'outline)
;;   (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;;   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

(use-package hideshowvis
  :ensure t
  :bind
  (([(control -)] . hs-toggle-hiding))
  :config
  ;; https://gist.github.com/jasonm23/514946
  (define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 254 124 56 16 0 0])
  (define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32])
  (hideshowvis-enable)
  (hideshowvis-symbols)
)

(use-package nlinum
  :ensure t
  :bind ([(meta shift n)] . nlinum-mode)
  :config
  (setq nlinum-format " %3d "))

(use-package dired-k
  :ensure t
  :bind (:map dired-mode-map
              (([(k)] . dired-k)))
  :config
  (add-hook 'dired-initial-position-hook 'dired-k))

(use-package spinner
  :ensure t)

;; *** Greek letters mode line
;; TODO swap to diminish
;; (require 'cl)
;; (defvar mode-line-cleaner-alist
;;   `((auto-fill-function         . "")
;;     (smart-spacing-mode         . "")
;;     (auto-complete-mode         . " α")
;;     (company-mode               . " α")
;;     (yas-minor-mode             . " γ")
;;     (autopair-mode              . " ρ")
;;     (eldoc-mode                 . " ε")
;;     (undo-tree-mode             . " τ")
;;     (highlight-parentheses-mode . " φ")
;;     (volatile-highlights-mode   . " υ")
;;     (elisp-slime-nav-mode       . " δ")
;;     (workgroups-mode            . " ω")
;;     (hs-minor-mode              . " χ")
;;     (flex-autopair-mode         . " ψ")
;;     (projectile-mode            . " π")
;;     (outline-minor-mode         . " ο")
;;     (abbrev-mode                . " αβ")
;;     (ielm-mode                  . " εζ")
;;     (auto-revert-mode           . " αρ")
;;     (cider-mode                 . " ηζ")
;;     (cider-repl-mode            . " ηζ")
;;     (cider-interaction-mode     . " ηζ")
;;     (lisp-mode                  . " ελ")
    
;;     ;; major modes
;;     (org-indent-mode            . "η")
;;     (iimage-mode                . "ι")
;;     (isearch-mode               . "ς")
;;     (clojure-test-mode          . "τ")
;;     (clojure-mode               . "λ")
;;     (clojurescript-mode         . "ςλ")
;;     (nrepl-repl-mode            . "ηζ")
;;     (emacs-lisp-mode            . "ελ")
;;     (lisp-interaction-mode      . "ελ")
;;     (inferior-emacs-lisp-mode   . "εζ")
;;     (flyspell-mode              . "θ ")
;;     (completion-list-mode       . "ς")
;;     (debugger-mode              . "δ")
;;     (compilation-mode           . "κ")
;;     (help-mode                  . "χ")
;;     (text-mode                  . "τ")
;;     (fundamental-mode           . "φ")
;;     (hi-lock-mode               . "")
;;     (python-mode                . "π")
;;     (w3m-mode                   . "ψ")
;;     (org-mode                   . "ω")
;;     (org-agenda-mode            . "ωα")
;;     (calendar-mode              . "ωκ")
;;     (shell-mode                 . "ς")
;;     (package-menu-mode          . "π")
;;     (markdown-mode              . "μδ"))
;;   "alist for `clean-mode-line'.
 
;; when you add a new element to the alist, keep in mind that you
;; must pass the correct minor/major mode symbol and a string you
;; want to use in the modeline *in lieu of* the original.")

;; (defun clean-mode-line ()
;;   (interactive)
;;   (loop for cleaner in mode-line-cleaner-alist
;;         do (let* ((mode (car cleaner))
;;                   (mode-str (cdr cleaner))
;;                   (old-mode-str (cdr (assq mode minor-mode-alist))))
;;              (when old-mode-str
;;                (setcar old-mode-str mode-str))
;;              (when (eq mode major-mode)
;;                (setq mode-name mode-str)))))
;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; M-x set-input-method RET TeX

(use-package diminish
  :ensure t
  :config
  (defvar mode-line-clear-alist
    '((emacs-lisp-mode            . "ελ")
      (company-mode               . " α")
      (which-key-mode             . " ω")
      (outline-minor-mode         . " ο")
      (clojure-mode               . "λ")
      (volatile-highlights-mode   . " ƕ")
      (command-log-mode           . " ς")
      (git-gutter+-mode           . " ʒ")

      (org-mode                   . "Ω")))

  (loop for cleaner in mode-line-clear-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner)))
             (diminish mode mode-str)))

  ;; TODO move to alist/plist and loop processing
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ελ")))
  (add-hook 'clojure-mode-hook (lambda () (setq mode-name "λ")))
  (add-hook 'org-mode-hook (lambda () (setq mode-name "Ω")))
  (add-hook 'message-buffer-mode (lambda () (setq mode-name "Μ")))
  
  (diminish 'emacs-lisp-mode "ελ")
  (diminish 'rainbow-delimiters-mode "rb")
  (diminish 'rainbow-mode "rn")

  (diminish 'helm-mode " H")
  (diminish 'outline-minor-mode " ο")
  (diminish 'helm-mode " χ")
  (diminish 'flex-autopair-mode " ψ")
  (diminish 'eldoc-mode " ε")
  (diminish 'abbrev-mode " αβ")
  (diminish 'auto-revert-mode)
  (diminish 'cider-mode " ηζ")
  (diminish 'org-mode "Ω")
  (diminish 'which-key-mode " ωκ")
  (diminish 'emacs-lock-mode " 🔒")
  (diminish 'volatile-highlights-mode " νι")
  (diminish 'auto-revert-mode " αρ")
  (diminish 'yas/minor-mode " γς")
  ;; (diminish 'clojure-mode "λ")
  (diminish 'git-gutter+-mode " ʒ")
  (diminish 'clj-refactor-mode " λρ")
  )

(use-package paradox
  :ensure t
  :config
  (setq paradox-automatically-star t)
  (setq paradox-github-token (read-file (expand-file-name "~/.emacs.d/github.token"))))

(use-package server
  :config
  (setq server-use-tcp t)
  (setq server-host "localhost")
  (server-force-delete)
  (server-start)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;; (use-package edit-server
;;   :if window-system
;;   :config
;;   (add-hook 'after-init-hook 'server-start t)
;;   (add-hook 'after-init-hook 'edit-server-start t))

(use-package eval-sexp-fu
  :ensure t)

(use-package ediff
  :ensure t
  :defer t
  ;; :requires (winner-mode)
  :config
  (winner-mode)
  (defvar ediff-saved-point)
  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq ediff-saved-point (point))
              (set-frame-width (selected-frame) (* (frame-width) 2))))

  (add-hook 'ediff-after-quit-hook-internal
            (lambda ()
              (progn
                (winner-undo)
                (goto-char ediff-saved-point)
                (set-frame-width (selected-frame) (/ (frame-width) 2)))))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package helm-describe-modes
  :ensure t
  :config
  (global-set-key [remap describe-mode] #'helm-describe-modes))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm
  :ensure t
  :bind
  (:map helm-map
        ([remap describe-mode] . helm-describe-modes)
        ([(meta x)] . helm-M-x)
        ([(control x) (f)] . helm-find-files)
        ([(control x) (b)] . helm-mini)
        ([(super b)] . helm-mini)
        ;; ([(meta s) (o)] . helm-occur)
        ([(tab)] . helm-execute-persistent-action)
   :map helm-find-files-map
        ([(tab)] . helm-execute-persistent-action)
        ([(meta p)] . previous-history-element))

  :config
  (require 'highlight)
  (require 'helm-config)
  (require 'helm-describe-modes)

  (ido-mode -1)
  (global-set-key [(meta x)] 'helm-M-x)
  
  (helm-autoresize-mode t)
  (setq-default helm-autoresize-max-height 37)
  (setq-default helm-autoresize-min-height 37)

  (helm-mode t)
  
  (helm-descbinds-mode t)
  (which-key-mode t)

  (defun helm-hidqe-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq helm-display-header-line t)
  (set-face-attribute 'helm-source-header nil :height 1)

  (golden-ratio-mode -1)
  (require 'ido-vertical-mode)

  ;; (ido-vertical-mode t)
  ;; (setq ido-use-virtual-buffers t)
  ;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  ;; (ido-mode t)

  ;; prevent messing with frames
  (require 'shackle)
  (setq helm-display-function #'pop-to-buffer)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.46)))
  (shackle-mode))


;; *** Nice modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-checker-simple-format t))


;; ** Customized Aquamacs dholm's tabbar
(use-package tabbar
  :ensure t
  :load-path "~/.emacs.d/lisp/tabbar-dholm"
  :bind (([(control tab)] . tabbar-forward)
         ([(control shift tab)] . tabbar-backward)
         ([(super \[)] . tabbar-backward)
         ([(super \])] . tabbar-forward)
         ([(super \{)] . tabbar-move-tab-left)
         ([(super \})] . tabbar-move-tab-right)
         ([(control shift n)] . tabbar-move-current-buffer-to-new-frame))
  :config
  (require 'aquamacs-tabbar)
  ;; (require 'one-buffer-one-frame)
  (setq one-buffer-one-frame-mode nil)
  (setq tabbar-key-binding-modifier-list '(super))
  (setq tabbar-buffer-groups-function (lambda () (list "All")))
  (defvar header-line-inhibit-window-list '())
  (add-to-list 'header-line-inhibit-window-list (selected-window))

  ;; W/A for aquamacs native functions
  (defun ns-frame-is-on-active-space-p (frame) t)
  (defun frame-iconified-p (frame) t)

  (defcustom delete-window-preserve-buffer '("\*Messages\*" "\*Help\*")
    "preserve these buffers when deleting window displaying them.
when `one-buffer-one-frame-mode' or `tabbar-mode' are on, a buffer is
killed when the last window displaying it is deleted by way of user
interaction via high-level commands such as `close-window', unless the
buffer name is listed in this customization variable, or this variable
is set to `t'."
    :group 'aquamacs
    :group 'frames
    :type '(choice (repeat string)    
                   (set (const "\*Messages\*") (const "\*Help\*"))
                   (const t)))
  
  (defun killable-buffer-p (buf)
    "returns non-nil if buffer buf may be be killed. customize
`delete-window-preserve-buffer' to configure."
    (if (or (eq t delete-window-preserve-buffer)
            (member (get-bufname buf) delete-window-preserve-buffer))
        nil
      t))
  
  (defun aquamacs-delete-window (&optional window)
    "remove window from the display.  default is `selected-window'.
if window is the only one in its frame, then `delete-frame' too, even
if it's the only visible frame."
    (interactive)
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t)
        (aquamacs-delete-frame)
      (old-delete-window (selected-window))))

  (or (fboundp 'old-delete-window)
      (fset 'old-delete-window (symbol-function 'delete-window)))

  (defun tabbar-move-tab (&optional right)
    "Move current tab to the left or to the right if RIGHT is set."
    (let* ((ctabset nil)
           (ctabs nil)
           (ctab nil)
           (hd nil)
           (tl nil))
      (and 
       (setq ctabset (tabbar-current-tabset 't))
       (setq ctabs (tabbar-tabs ctabset))
       (setq ctab (tabbar-selected-tab ctabset))
       (setq tl ctabs)
       (setq hd '())) ;; nil
      (while (and (cdr tl) (not (eq ctab (car tl))) (not (eq ctab (cadr tl))))
        (setq hd (append hd (list (car tl)))
              tl (cdr tl)))
      (set ctabset
           (cond 
            ((and (not right) (null hd) (eq ctab (car tl)))
             (append (cdr tl) (list (car tl))))
            ((not right)
             (append hd (list (cadr tl)) (list (car tl)) (cddr tl)))
            ((and right (not (cddr tl)))
             (append (list (cadr tl)) hd (list (car tl))))
            ((and right (eq ctab (car tl)))
             (append hd (list (cadr tl)) (list (car tl)) (cddr tl)))
            (right
             (append hd (list (car tl)) (list (caddr tl)) (list (cadr tl)) (cdddr tl)))
            ))
      (put ctabset 'template nil)
      (tabbar-display-update)))

  (defun tabbar-move-tab-left ()
    "Move tab left."
    (interactive)
    (tabbar-move-tab))

  (defun tabbar-move-tab-right ()
    "Move tab right."
    (interactive)
    (tabbar-move-tab t))

  ;; inheritance does not work for those faces
  (set-face-attribute 'tabbar-unselected nil :inherit nil :stipple nil :background "grey80" :foreground "grey50" :inverse-video nil :box '(:line-width 3 :color "grey80") :strike-through nil :overline nil :underline nil :slant 'normal :weight 'bold :height 110 :width 'normal :family "PragmataPro")
  (set-face-attribute 'tabbar-unselected-modified nil :inherit nil :stipple nil :background "grey80" :foreground "grey50" :inverse-video nil :box '(:line-width 3 :color "grey80") :strike-through nil :overline nil :underline nil :slant 'normal :weight 'bold :height 110 :width 'normal :family "PragmataPro")
  (set-face-attribute 'tabbar-unselected-highlight nil :inherit nil :stipple nil :background "grey90" :foreground "grey50" :inverse-video nil :box '(:line-width 3 :color "grey90") :strike-through nil :overline nil :underline nil :slant 'normal :weight 'bold :height 110 :width 'normal :family "PragmataPro")
  
  (custom-set-faces
   '(tabbar-default ((t (:inherit nil :stipple nil :background "grey80" :foreground "black" :box nil :strike-through nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Pragmata Pro"))))
   '(tabbar-button ((t (:inherit tabbar-default :background "grey75" :box nil))))
   '(tabbar-key-binding ((t (:foreground "white" :slant normal :foreground "grey70"))))
   '(tabbar-separator ((t (:inherit tabbar-default :background "grey50" :foreground "grey50"))))

   '(tabbar-selected ((t (:inherit tabbar-default :stipple nil :background "#fbf8ef" :foreground "gray20" :inverse-video nil :box (:line-width 3 :color "grey96")  :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "PragmataPro"))))
   '(tabbar-selected-highlight ((t (:background "grey96" :foreground "black" :slant normal :box nil))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected :slant normal)))))

  (tabbar-mode t))


;; ** Visual bookmarks
(use-package bm
  :ensure t
  :bind (([(f3)] . bm-toggle)
         ([(f4)] . bm-next)
         ([(meta f3)] . bm-show-all)
         ([(meta f4)] . bm-previous))
  :config
  (setq bm-cycle-all-buffers t)  
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
  ;; :requires (magit-gitflow)
  :bind ([(control x) (g)] . magit-status)
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package yasnippet
  :ensure t
  :config
  ;; (yas/initialize)
  )

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-x '"))

(use-package cljr-helm
  :ensure t)

(use-package elpy
  :ensure t
  :mode ( "\\.py\\'" . python-mode)
  :bind (:map elpy-mode-map
              ([(meta up)] . org-previous-visible-heading)
              ([(meta down)] . org-next-visible-heading)
         :map inferior-python-mode-map
         ([(control c) (l)] . my-clear))

  :init
  (defun my-clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  ;; (hideshowvis-enable)
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt")

  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  (setq elpy-rpc-python-command "python3")
  
  :config
  (outline-minor-mode t)

  (defun describe-python-function-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point))
           (description ;; (save-window-excursion
                         (python-describe-at-point)
                         ;; (switch-to-buffer "*Python Doc*")
                         ;; (buffer-string)
                         )
           ;;))
     (popup-tip description
                :point (point)
                :around t
                :height 30
                :scroll-bar nil
                :margin t)))))

(use-package clojure-mode
  :ensure t
  :defer t
  :bind (:map clojure-mode-map
              ([(control x) (\")] . cljr-helm))
  :config
  (defun add-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(("lambda" . 955) ; λ
            ("fn" . 402) ; fn
            ))
    (setq clojure--prettify-symbols-alist
          '(;("fn" . 402)  ; fn
            ("fn" . 2260) ; fn
            ("#" . ?λ)
            ("conso" . "consº")
            )))

  (add-hook 'clojure-mode-hook 'add-pretty-lambda)
  (add-hook 'clojure-mode-hook 'outline-minor-mode)
  (add-hook 'clojure-mode-hook 'hideshowvis-enable)
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)

  (define-clojure-indent
    (match 1)))

(use-package cider
  :ensure t
  ;; :requires (popup)
  :bind (:map cider-repl-mode-map
              ([(control c) (d)] . describe-clojure-function-in-popup)
              ([(control c) (meta o)] . cider-repl-clear-buffer)
              
         :map cider-mode-map
              ([(control c) (control d) (control c)] . cider-clojuredoc-web)
              ([(control j)] . cider-eval-defun-to-comment)
        
         :map clojure-mode-map
              ([(control c) (control d) (control c)] . cider-clojuredoc-web)
              ([(control c) (d)] . describe-clojure-function-in-popup))
  :config
  (setq cider-font-lock-dynamically nil)
  (setq nrepl-log-messages nil)
  (setq cider-pprint-fn 'fipp)
  (setq cider-repl-display-help-banner nil)
  (setq cider-popup-stacktraces nil)
  (setq cider-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)

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
             #'cider-clojuredoc-web-lookup)))

(use-package command-log-mode
  :ensure t
  :bind ([(control x) (l)] . clm/toggle-command-log-buffer)
  :diminish (command-log-mode . " ς")
  :config
  (global-command-log-mode t))

(use-package inf-clojure
  :ensure t
  :config
  (setq inf-clojure-program "planck"))

(use-package slime
  :ensure t
  ;; :requires (slime-company)
  :bind (:map lisp-mode-map
              ([(control c) (d)] . describe-function-in-popup))
  :config
  (require 'popup)
  (require 'slime-fancy)
  ;; start slime with M-- M-x
  (setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64" "-K utf-8") :coding-system utf-8-unix)
        (sbcl ("/usr/local/bin/sbcl" "-quiet") :coding-system utf-8-unix)))
  (setq inferior-lisp-program "/usr/local/bin/sbcl")

  (add-to-list 'slime-contribs 'slime-fancy)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-company slime-indentation))

  ;; TODO merge all popup describe functions
  ;; partially done - works for pre-defined symbols
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
                 :margin t))))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map ([(meta ?.)] . godef-jump))
  ;; :requires (company-go)
  :config
  (add-to-list 'exec-path "~/src/go/bin")
  ;; (setenv "PATH" (concat (getenv "PATH") ":/Users/vader/src/go/bin"))

  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  
  (defun local-go-mode-hook ()
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'local-go-mode-hook)
  (add-hook 'go-mode-hook #'gorepl-mode))

(use-package git-timemachine
  :ensure t
  :bind ([(control x) (t)] . git-timemachine-toggle))

;; ** Parentheses management
(use-package flex-autopair
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  :bind (([(control c) up] . sp-up-sexp)))

(use-package highlight-parentheses
  :ensure t
  :config
  (show-paren-mode t)
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  
  (set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold :foreground "red" :background (face-background 'default))
  ;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "black" :background "lemonchiffon")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "darkgreen" :background (face-background 'default))

  ;; TODO advices not working
  (defadvice load-theme (after load-theme-after activate)
    ;; (set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold :foreground "red" :background "lemonchiffon")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "navyblue" :background (face-background 'default)))

  (advice-add 'load-theme :after #'(lambda () (set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "navyblue" :background "lemonchiffon")))
  )

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package git-gutter-fringe+
  :ensure t
  :config (global-git-gutter+-mode))


;; ** Org-mode & markdown
(use-package org
  :ensure t
  :bind (([(control c) (c)] . org-capture)
  :map org-mode-map
  ([(meta up)] . org-previous-visible-heading)
  ([(meta down)] . org-next-visible-heading)
  ([(control =)] . strike-through-for-org-mode)
  ([(control tab)] . nil))
   ;; ([(meta up)]     . org-backward-element)
   ;;  ([(meta down)]   . org-forward-element)
   ;;  ([(control tab)] . tabbar-forward)
   ;;  ([(control y)]   . kill-current-line)
   ;;  ([(control c) (l)] . org-toggle-link-display))
  :config
  (require 'org-protocol)
  (setq org-directory "~/Sync/")
  (setq org-default-notes-file "~/Sync/refile.org")

  (setq org-support-shift-select 'always)
  ;; (setq org-mode-shift-select-mode 'always)
  (setq-default org-hide-emphasis-markers t)

  (remove-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-off-auto-fill)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (perl . t)
     (python . t)
     (clojure . t)))

  (defun org-toggle-link-display ()
    "Toggle the literal or descriptive display of links."
    (interactive)
    (if org-descriptive-links
        (progn (org-remove-from-invisibility-spec '(org-link))
               (org-restart-font-lock)
               (setq org-descriptive-links nil))
      (progn (add-to-invisibility-spec '(org-link))
             (org-restart-font-lock)
             (setq org-descriptive-links t))))
  
  (setq org-capture-templates
      `(("c" "Link" entry (file+headline ,(concat org-directory "refile.org") "Inbox")
         "* [[%:link][%:description]]\n%i%?\n"
         :immediate-finish 1)
        ("C" "Link prompted" entry (file+headline ,(concat org-directory "refile.org") "Inbox")
         "* [[%:link][%:description]]\n%i%?\n"
         :immediate-finish 1)))

  (setq org-refile-targets (quote (("node.org" :regexp . "LINKS")
                                   ("node.org" :regexp . "COLLECTION$")
                                   ("node.org" :tag . "refile")
                                   ("node.org" :tag . "quotes"))))

  (message (concat "Org init: " (format-time-string "%Y-%m-%d %T"))))

(use-package org-capture-pop-frame
  :ensure t
  :config
  ;; Amend capture frame to lack header that breaks tabbar
  (defun ocpf--org-capture (orig-fun &optional goto keys)
    "Create a new frame and run org-capture."
    (interactive)
    (let ((frame-window-system
           (cond ((eq system-type 'darwin) 'ns)
                 ((eq system-type 'gnu/linux) 'x)
                 ((eq system-type 'windows-nt) 'w32)))
          (after-make-frame-functions
           #'(lambda (frame)
               (progn
                 (select-frame frame)
                 (setq word-wrap nil)
                 (setq truncate-lines nil)
                 (funcall orig-fun goto keys)))))
      (make-frame
       `((window-system . ,frame-window-system)
         ,@ocpf-frame-parameters))))
  (advice-add 'org-capture :around #'ocpf--org-capture)

  ;; TODO convert to advice-add
  (defadvice org-capture-kill (after delete-capture-frame activate) (delete-frame))
  (defadvice org-capture-destroy (after delete-capture-frame activate) (delete-frame))
  (defadvice org-capture-finalize (after delete-capture-frame activate) (delete-frame))
  
  (defadvice org-capture-select-template (around delete-capture-frame activate)
    "Advise org-capture-select-template to close the frame on abort"
    (unless (ignore-errors ad-do-it t)
      (setq ad-return-value "q"))
    (delete-frame))
  
  (message (concat "Org-capture-pop-frame init: " (format-time-string "%Y-%m-%d %T"))))

;; (use-package org-capture-pop-frame
;;   :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; (use-package ox-latex
;;   :config
;;   (setenv "path" (concat (getenv "path") ":/usr/local/texlive/2015/bin/x86_64-darwin"))
;;   (setq exec-path (append exec-path '("/usr/local/texlive/2015/bin/x86_64-darwin")))
;;   (setq org-src-tab-acts-natively t)
;;   (setq org-src-fontify-natively t)

;;   (setq org-latex-listings 'minted)
;;   (add-to-list 'org-latex-packages-alist '("" "minted"))
;;   (add-to-list 'org-latex-packages-alist '("" "cmap"))
;;   (add-to-list 'org-latex-packages-alist '("" "listings"))
;;   (add-to-list 'org-latex-packages-alist '("" "color"))
;;   (add-to-list 'org-latex-packages-alist '("english,russian" "babel"))
;;   (add-to-list 'org-latex-packages-alist '("t2a" "fontenc"))
;;   (add-to-list 'org-latex-packages-alist '("utf8" "inputenc")))

(use-package markdown-mode
  :ensure t
  :diminish (markdown-mode . "Μδ")
  :config
  (autoload 'markdown-mode "markdown-mode" "major mode for editing markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package flyspell-correct
  :after flyspell
    ;; :bind (([(control c) (c)] . org-capture)
  ;; :bind (:map flyspell-mode-map ([(control c) (\;)] . flyspell-popup-correct)))
  :bind (:map flyspell-mode-map ("C-;" . flyspell-popup-correct)
              ("C-." . nil)))

;; (kill-buffer "*Messages*")

;; * CUSTOM FACES & VARS
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-fringe-persistent-face ((t (:background "darkorange1" :foreground "black"))))
 '(bm-persistent-face ((t (:background "darkorange1" :foreground "black"))))
 '(calendar-today ((t (:underline t :weight bold))))
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
 '(git-gutter+-added ((t (:foreground "SpringGreen4" :weight normal))))
 '(git-gutter+-deleted ((t (:foreground "firebrick3" :weight normal))))
 '(git-gutter+-modified ((t (:foreground "MediumOrchid2" :weight bold))))
 '(highlight-symbol-face ((t (:background "mediumpurple4"))))
 '(hs-face ((t (:foreground "dark cyan" :underline (:color "black" :style wave) :weight normal))))
 '(mc/cursor-bar-face ((t (:height 1.0))))
 '(mc/cursor-face ((t nil)))
 '(org-default ((t (:inherit default))))
 '(org-document-info ((t (:foreground "midnight blue" :height 1))))
 '(scroll-bar ((t (:background "red" :foreground "yellow"))))
 '(tabbar-button ((t (:inherit tabbar-default :background "grey75" :box nil))))
 '(tabbar-default ((t (:inherit nil :stipple nil :background "grey80" :foreground "black" :box nil :strike-through nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Pragmata Pro"))))
 '(tabbar-key-binding ((t (:foreground "white" :slant normal :foreground "grey70"))))
 '(tabbar-selected ((t (:inherit tabbar-default :stipple nil :background "#fbf8ef" :foreground "gray20" :inverse-video nil :box (:line-width 3 :color "grey96") :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "PragmataPro"))))
 '(tabbar-selected-highlight ((t (:background "grey96" :foreground "black" :slant normal :box nil))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected :slant normal))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "grey50" :foreground "grey50"))))
 '(tooltip ((t (:background "mediumpurple4" :foreground "wheat"))))
 '(yascroll:thumb-text-area ((t (:background "midnight blue")))))

(set-face-italic-p 'italic nil)

  ;; (dyn-let ((class '((class color) (min-colors 89))) ;;              ~~ Dark ~~                              ~~ Light ~~
  ;;       ;;                                                          GUI       TER                           GUI       TER
  ;;       ;; generic
  ;;       (act1          (if (eq variant 'dark) (if (true-color-p) "#222226" "#121212") (if (true-color-p) "#e7e5eb" "#d7dfff")))
  ;;       (act2          (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#444444") (if (true-color-p) "#d3d3e7" "#afafd7")))
  ;;       (base          (if (eq variant 'dark) (if (true-color-p) "#b2b2b2" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
  ;;       (base-dim      (if (eq variant 'dark) (if (true-color-p) "#686868" "#585858") (if (true-color-p) "#a094a2" "#afafd7")))
  ;;       (bg1           (if (eq variant 'dark) (if (true-color-p) "#292b2e" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
  ;;       (bg2           (if (eq variant 'dark) (if (true-color-p) "#212026" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
  ;;       (bg3           (if (eq variant 'dark) (if (true-color-p) "#100a14" "#121212") (if (true-color-p) "#e3dedd" "#d0d0d0")))
  ;;       (bg4           (if (eq variant 'dark) (if (true-color-p) "#0a0814" "#080808") (if (true-color-p) "#d2ceda" "#bcbcbc")))
  ;;       (border        (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#111111") (if (true-color-p) "#b3b9be" "#b3b9be")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "7b4d9b8a6ada8e24ac9eecd057093b0572d7008dbd912328231d0cada776065a" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" default)))
 '(default-input-method "TeX")
 '(ediff-cmp-program "diff")
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "python3" t)
 '(fci-rule-color "#eee8d5")
 '(git-gutter-fr+-side (quote right-fringe))
 '(global-git-gutter+-mode t)
 '(helm-descbinds-window-style (quote split-window))
 '(helm-display-header-line t)
 '(helm-echo-input-in-header-line t)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-parentheses-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(highlight-parentheses-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-enable-flex-matching t)
 '(ido-max-window-height 20)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ido-vertical-show-count nil)
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(nil nil t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-bullets-bullet-list (quote ("כֶ" "א" "ט" "ש" "צ")))
 '(org-download-timestamp "")
 '(org-protocol-default-template-key "n")
 '(org-startup-truncated nil)
 '(org-support-shift-select (quote always))
 '(outshine-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (pydoc jedi idle-highlight-mode xterm-color better-shell nord-theme lispy helm-addressbook helm-cider-history helm-cscope helm-dictionary helm-firefox helm-git-files helm-git-grep helm-icons helm-org helm-org-ql helm-org-rifle helm-pydoc helm-slime helm-themes doom-themes doom blacken py-autopep8 helm-mode-manager material-theme doom-modeline sly header2 loccur smooth-scroll typopunct navi-mode outline-magic yascroll unicode-fonts emojify add-hooks vdiff benchmark-init org-protocol ahk-mode kotlin-mode cloc company-jedi org-mac-link notmuch helm-unicode helm-swoop ox-reveal deft ical-pull org-babel-eval-in-repl org-beautify-theme org-capture-pop-frame org-download org-gcal dired-k expand-region elnode js-comint nodejs-repl js3-mode bm tabbar jade calfw-gcal howm el-pocket google-translate scala-mode git-timemachine tidy impatient-mode gorepl-mode hungry-delete company-emoji visual-regexp git-gutter-fringe+ delight popwin shackle calfw org-mac-iCal helm-ag go-complete web-mode clojure-snippets java-snippets all-the-icons projectile-speedbar helm-projectile neotree command-log-mode magit-gitflow request restclient elpy clj-refactor parinfer forth-mode ob-applescript volatile-highlights applescript-mode dockerfile-mode changelog-url osx-dictionary mode-icons flyspell-correct-helm helm-chrome helm-cider helm-clojuredocs helm-company helm-git helm-itunes helm-package helm-safari ivy counsel company-flx helm-flx lorem-ipsum org-bullets flatui-theme gist dtrace-script-mode 0blayout inf-clojure latex-preview-pane latex-math-preview latex-pretty-symbols magic-latex-buffer company-go go-mode pp+ rainbow-delimiters rainbow-mode anzu spacemacs-theme ido-vertical-mode golden-ratio highlight which-key helm-descbinds guide-key guide-key-tip flx-ido flx-isearch helm-describe-modes helm yasnippet waher-theme use-package swiper sublime-themes stripe-buffer spaceline solarized-theme soft-charcoal-theme smartparens slime-company popup perspective paredit paradox outshine nlinum nav-flash multiple-cursors move-text monokai-theme mic-paren markdown-mode magit inflections htmlize highlight-symbol highlight-parentheses hideshowvis flycheck flex-autopair eyebrowse edn company-quickhelp color-theme ace-jump-mode)))
 '(perl-indent-level 2)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator (quote utf-8))
 '(python-indent-offset 2)
 '(python-shell-interpreter "python3")
 '(rainbow-delimiters-max-face-count 1)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tabbar-mode t nil (tabbar))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tramp-verbose 6)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(which-key-echo-keystrokes 0.02)
 '(which-key-idle-delay 0.4)
 '(which-key-sort-order (quote which-key-key-order-alpha))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yascroll:scroll-bar (quote (right-fringe left-fringe text-area))))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;(benchmark-init/deactivate)

