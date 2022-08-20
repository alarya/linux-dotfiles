

;;Package settings
;;================
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
	       (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))

(package-initialize)

;;initialize use-package
;;======================
(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;Emacs settings
;;==============
(setq-default fill-column 76)
(setq-default frame-title-format "%b (%f)")
(setq column-number-mode	t)
(setq inhibit-startup-message	t)
(menu-bar-mode -1)
(tool-bar-mode	-1)
(toggle-scroll-bar -1)
;;don't litter the .emacs file with custom-set-variables
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


;;Theme settings
;;===============================
(use-package doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-molokai t)
  ;;(load-theme 'doom-dark+ t)
  ;; (load-theme ' doom-monokai-octagon)
  (load-theme 'doom-snazzy t)
  )

;; (use-package monokai-theme
;;   :ensure t)

;; (use-package material-theme
;;   :ensure t)

;;maximize frame; remove title bar
;; (add-to-list 'default-frame-alist
;; 	     '((fullscreen . maximized)
;; 	       (undecorated . t)))

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(font . "Cascadia Code"))

;; icons in dired mode
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;;sensible window splitting
(setq split-height-threshold 120
      split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))

;;(setq split-window-preferred-function #'my-split-window-sensibly)

;; Use global hl mode - helps with finding the active window/buffer
(global-hl-line-mode)
(set-face-background hl-line-face "gray9")

;;Recent files setting
(use-package recentf
  :defer 5
  :init
  (setq recentf-max-menu-items	20)
  (setq recentf-max-saved-items	20)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  :config
  (recentf-mode 1))

;;dired
(put 'dired-find-alternate-file 'disabled nil)

;;set up dashboard
(use-package dashboard
  :ensure t
  :after (org)
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 10)
			  (projects . 5)
			  (bookmarks . 5)))
  ;; (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold)))))

;;follow sym links
(setq vc-follow-symlinks t)

(setq initial-major-mode (quote org-mode))
(setq major-mode (quote fundamental-mode))

;;set default browser
(setq browse-url-browser-function 'eww-browse-url)

;;Stylize mode line
;;=================
;; For this package to work need to run following command once
;; M-x all-the-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;Elpa keyring update package
;;===========================
(use-package gnu-elpa-keyring-update
  :ensure t
  :defer t)

;;God mode
;;========
;;Useful for easy naviagtion for reading
(use-package god-mode
  :ensure t
  :bind ("C-c m z" . god-mode-all))

;; Expand region for seleting text within delimiters
;; =========
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;;Org mode settings
;;=================
(use-package org

  :init
  ;;Shortcut for org-agenda
  (global-set-key (kbd "C-c a")
		'(lambda (&optional arg)
		   (interactive "P")
		   (org-agenda arg "b")))

  ;;enable auto-fill by default for org mode
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  
  ;;add id for capture templates
  (add-hook 'org-capture-mode-hook 'org-id-get-create)
  
  :mode (("\\.org$" . org-mode))
  
  :bind
  (("C-c c" . org-capture))
  
  :config
  (setq org-return-follows-link t)
  
  (setq org-link-frame-setup
      (quote
       ((vm		.	vm-visit-folder-other-frame)
	(vm-imap	.	vm-visit-imap-folder-other-frame)
	(gnus		.	org-gnus-no-new-news)
	(file		.	find-file-other-window)
	(wl		.	wl-other-frame)
	(pdfview	.	find-file-other-window))))

  ;;Org agenda files
  (setq org-agenda-files
	'("~/github/life/Personal.org"))
  (if (equal (system-name) "AALOK")
	(progn
	  (add-to-list 'org-agenda-files
		       "~/gitlab/aalok-notes/Qualcomm.org")
	  (add-to-list 'org-agenda-files
		       "~/p4/PinMux_Dev/latest/Tools/PinMux/PinMux_Dev/Docs/QPCT.org")
	  (add-to-list 'org-agenda-files
		       "~/gitlab/phy-validation/todo.org")))

  ;;Org todo keywords and faces
  (setq org-todo-keywords
	'((sequence "TODO(t@) IN-PROGRESS(i@) REVIEW(r@) WAIT(w@) HOLD(h@) | DONE(d@) CANCELED(c@)")))

  (setq org-todo-keyword-faces
	'(("TODO" . org-warning)
	  ("IN-PROGRESS" . "orange")
	  ("REVIEW" . org-warning)
	  ("WAITING" . org-warning)
	  ("CANCELED" . "orange")
	  ("DONE" . "green")))
  
  ;;Org capture templates
  (setq org-capture-templates
      '(("w" 
	 "work to do capture"
	 entry
	 (file+headline "~/gitlab/aalok-notes/Qualcomm.org" "Capture")
	 "* TODO %^{Title}\n:PROPERTIES:\n:RECORDED: %U\n:END:\n%^{Description}%?"
	 :empty-lines 1)
	("1" 
	 "qpct capture"
	 entry
	 (file+headline "~/p4/PinMux_Dev/latest/Tools/PinMux/PinMux_Dev/Docs/QPCT.org" "Capture")
	 "* TODO %^{Title}\n:PROPERTIES:\n:RECORDED: %U\n:END:\n%^{Description}%?"
	 :empty-lines 1)
	("p"
	 "personal to do capture"
	 entry
	 (file+headline "~/github/Life/Personal.org" "Capture")
	 "* TODO %^{Title}\n:PROPERTIES:\n:RECORDED: %U\n:END:\n%^{Description}%?"
	 :empty-lines 1)))

  ;;set org refile targets
  (setq org-refile-targets
      '((nil :maxlevel . 1)
	(org-agenda-files :maxlevel . 1)))

  ;;catch invisible edits
  (setq org-catch-invisible-edits 'smart)

  ;;block parent todo from completion if all children are not
  (setq org-enforce-todo-dependencies 1)

  ;;Log closed time for a todo
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-clockreport-parameter-plist
	'(:link t :maxlevel 4 :tags t
		:compact t :narrow 50!))
  (setq org-log-redeadline t)
  (setq org-log-reschedule t)
  
  ;;archive subtree settings
  (setq org-cycle-open-archived-trees t)
  (setq org-export-with-archived-trees t)

  ;; Calculate completion statistics cookie using all todo children
  ;; instead of just direct children
  (setq org-hierarchical-todo-statistics nil)
  
  ;;custom agenda views
  (setq org-agenda-custom-commands
      '(("b" "My org view"
	 ((agenda "")
	  (todo "")
	  (tags "discuss|revisit|bug+TODO|jira+TODO")))))

  (setq org-cycle-separator-lines 1)

  ;;org export settings
  (setq org-html-validation-link nil)

  ;;Enable habit tracker module
  (add-to-list 'org-modules 'org-habit t))

;;load babel languages
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python . t)
			       (emacs-lisp . t)
			       (shell . t)
			       (js . t)
			       (ditaa . t)))

;;set ditaa path for diagrams while export
(setq org-ditaa-jar-path "~/winhome/ditaa0_9/ditaa.jar")

;;X410 shows dark window and it's hard to see the mouse
;;while drawing, so change the background locally in the
;;buffer
;;(defface ditaa-face '(:background "LightBlue4"))
;; (make-face 'ditaa-face)
;; (set-face-background 'ditaa-face "LightBlue4")
;; (add-hook 'artist-mode-hook
;;             (lambda ()
;;               (face-remap-add-relative 'default 'ditaa-face)))

(setq org-confirm-babel-evaluate nil)

;;let css take care of code snippet formatting
(setq org-html-htmlize-output-type 'css)

;;pretty bullets for org mode
(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;;markdown export for org mode
(use-package ox-gfm
  :ensure t
  :after org
  :config
  ;;enable export to markdown
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package org-id
  :after org)

(use-package git-link
  :ensure t
  :init
  :bind ("C-c g l" . git-link))

;;auto complete for org-mode
(use-package org-ac
  :ensure t
  :init
  (org-ac/config-default))

;;presentation for org mode buffer
(use-package org-tree-slide
  :ensure t
  :after org)

;; (use-package org-re-reveal
;;   :ensure t
;;   :after org)

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package htmlize
  :ensure t)

;;Completion frameworks
;;=====================

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

;; Make the minibuffer appear in the center
(use-package ivy-posframe
  :ensure t
  :init
  ;;Show the minibuffer at the center of the screen
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;;Show fringe to distinguish from the background
  (setq ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :custom-face
  (ivy-posframe-border ((t (:background "#ffffff"))))
  :config
  (ivy-posframe-mode 1))

(use-package swiper
  :ensure t
  :after
  (ivy)
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :bind ("C-c m i" . counsel-imenu)
  :after
  (swiper)
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c h") 'counsel-org-agenda-headlines))

;;counsel org clock - access a list of clock entries through counsel
(use-package counsel-org-clock
  :ensure t
  :bind
  ("M-g M-j" . counsel-org-clock-goto)
  ("M-g M-h" . counsel-org-clock-history))

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1))

;; Yasnippet
;; =========
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

;; Highlight symbol
;; ================
(use-package highlight-symbol
  :ensure t
  :bind
  ("C-c m h n" . highlight-symbol-next)
  ("C-c m h p" . highlight-symbol-prev)
  ;; :hook
  ;; (prog-mode . highlight-symbol-mode)
  )

;;Mail settings
;;=============
(use-package notmuch
  :ensure t
  :bind ("C-c m m" . notmuch)
  :config
  (setq notmuch-search-oldest-first	nil)
  (define-key notmuch-show-mode-map "T"
    (lambda ()
      "Trash the message"
      (interactive)
      (notmuch-show-tag (list "+trash" "-inbox")))))

(setq smtpmail-default-smtp-server "smtpserver")

(use-package smtpmail
  :config
  (setq smtpmail-smtp-user	"buntyalok06@gmail.com")
  (setq smtpmail-local-domain	"gmail.com")
  (setq smtpmail-smtp-server	"smtp.gmail.com")
  (setq smtpmail-stream-type	'ssl)
  (setq smtpmail-smtp-service	465)
  (setq smtpmail-debug-info	t)
  (setq smtpmail-debug-verb	t)
  (setq sendmail-program	"/usr/bin/msmtp")
  (setq send-mail-function	'smtpmail-send-it))

(setq message-sendmail-f-is-evil	't)
(setq message-sendmail-extra-arguments	'("--read-envelope-from"))
(setq message-send-mail-function	'message-send-mail-with-sendmail)

(setq message-sendmail-extra-arguments	'("-a" "gmail"))
(setq user-full-name			"Alok Arya")
(setq user-mail-address			"buntyalok06@gmail.com")

(setq message-kill-buffer-on-exit	t)
(setq message-default-mail-headers	"Cc: \nBcc: \n")
(setq message-auto-save-directory	"~/mail/drafts")

;;html support for mails
(use-package org-mime
  :ensure t
  :config
  (setq org-mime-library 'mml)
  (setq org-export-preserve-breaks t)
  (add-hook 'org-mime-html-hook
      (lambda ()
        (org-mime-change-element-style
         "p" "font-family: Consolas, fixed-width;")))

  (add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))

  ;; the following can be used to nicely offset block quotes in email bodies
  (add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))))

;;counsel notmuch
(use-package counsel-notmuch
  :ensure t)

;;Projectile settings
;;===================
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;Treemacs settings
;;=================
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;;XML mode settings
;;=================
(use-package hideshow
  :ensure t
  :hook
  (prog-mode . hs-minor-mode)
;;   :hook
;;   (sgml-mode nxml-mode)
  
;;   :init
;;   (add-hook 'nxml-mode-hook #'hs-minor-mode)

;;   :config
;;   (add-to-list 'hs-special-modes-alist
;; 	     '(nxml-mode
;; 	       "<!--\\|<[^/>]*[^/]>"
;; 	       "-->\\|</[^/>]*[^/]>"

;; 	       "<!--"
;; 	       sgml-skip-tag-forward
;; 	       nil))

;;   ;;(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
)

;;html settings
;;=============
(use-package tagedit
  :ensure t
  :init
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (tagedit-mode 1)))
  :config
  (tagedit-add-paredit-like-keybindings))

;;Paredit settings
;;================
(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook                          #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook                       #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook			#'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook	#'enable-paredit-mode)
  (add-hook 'ielm-mode-hook				#'enable-paredit-mode)
  (add-hook 'lisp-mode-hook				#'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook			#'enable-paredit-mode)
  (add-hook 'scheme-mode-hook				#'enable-paredit-mode))


;;Rainbow delimiters
;;==================
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;;Electric pair mode
;;==================
(use-package electric
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'electric-pair-mode))

;;Auto complete
;;=============
(use-package auto-complete
  :ensure t
  :init
  ;;only enable for these modes
  (setq ac-modes '(emacs-lisp-mode
		   lisp-mode
		   lisp-interaction-mode
		   org-mode))
  (ac-config-default)
  (global-auto-complete-mode t))

;;line numbers
;;============
(use-package linum
  :init
  (add-hook 'prog-mode-hook #'linum-mode))

;;Company mode settings
;;=====================
(use-package company
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook #'company-mode))

;;magit settings
;;==============
(use-package magit
  :ensure t
  :defer t
  :bind ("C-c m g" . magit))

;;Clojure settings
;;===============
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (subword-mode)
  :mode
  (("\\.edn$" . clojure-mode)
   ("\\.boot$" . clojure-mode)
   ("\\.clj.*$" . clojure-mode)
   ("\\.cljs.*$" . clojure-mode)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1)))))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; key bindings
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

;;cider
(use-package cider
  :ensure t
  :defer t
  :config
  (progn
     (define-key clojure-mode-map	(kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map	(kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map	(kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map		(kbd "C-c u") 'cider-user-ns))

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)
  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t))

;;Clojure docs
(use-package ivy-clojuredocs
  :ensure t
  :defer t)

;;ElDoc
;;=====
;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook	'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

;;pdf book library
;;================
(use-package bibliothek
  :ensure t
  :defer t
  :init
  (setq bibliothek-path (list "~/Books")))

;;Pdf settings
;;============
(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  ;; (use-package org-pdfview
  ;;   :ensure t
  ;;   :config
  ;;   (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))
  )

;;C# settings
;;================
(use-package csharp-mode
  :ensure t
  :defer t
  ;; :hook
;;  (csharp-mode . #'customize-csharp-indentation)
  :init
  (electric-pair-local-mode 1))

(use-package omnisharp
  :ensure t
  :hook
  ;; (csharp-mode . omnisharp-mode)
  (csharp-mode . lsp-mode)
  :init
  (add-to-list 'company-backends 'company-omnisharp))

;;Define indentation settings for C# to match Visual studio
(defun customize-csharp-indentation ()
  (setq tab-width 3
	indent-tabs-mode t
	c-basic-offset 3))

;;Fly check settings
;;==================
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  ;; (add-hook 'elpy-mode-hook #'flycheck-mode)
  )

;;Language server protocol (lsp-mode)
;;===================================
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-log-io nil)
  (setq lsp-keymap-prefix "C-c m l")
  ;; :hook
  ;; (python-mode . lsp)
  ;; (csharp-mode . lsp)
)

;; (use-package lsp-ui
;;   :ensure t
;;   :defer t
;;   :commands lsp-ui-mode)

;;Python settings
;;===============
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'python-mode-hook 'linum-mode))

;; python autocompletion
(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode))

;;python docstring editor
(use-package sphinx-doc
  :ensure t
  :defer t
  :hook
  (python-mode . sphinx-doc-mode))

;;Javascript
;;==========
(use-package js2-mode
  :ensure t
  :defer t
  :mode
  (("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
;; (use-package company-tern
;;   :ensure t
;;   :hook
;;   ((js2-mode-hook . tern-mode)
;;    (js2-mode-hook . company-mode))
;;   :init
;;   ;; (add-to-list 'company-backends 'company-tern)
;;   )

;;JSON
;;====
(use-package json-reformat
  :ensure t
  :defer t)

;;React
;;=====
(use-package rjsx-mode
  :ensure t
  :defer t
  :mode
  (("\\.jsx\\'" . rjsx-mode)))

;;Stack exchange client
;;=====================
(use-package sx
  :ensure t
  :defer t)

;;Dictionary
;;==========
(use-package dictionary
  :ensure t
  :defer t)

;;copy as format
;;==============
(use-package copy-as-format
  :ensure t
  :defer t)

;;elfeed setup
;;============
(use-package elfeed
  :ensure t
  :defer t
  :bind ("C-c m f" . elfeed)
  :config
  (setq elfeed-feeds
	'(("http://feeds.bbci.co.uk/news/world/rss.xml" bbc)
	  ("https://hnrss.org/newest" hackernews)
	  ("http://www.sciencemag.org/rss/current.xml" sciencemag)
	  ("https://opensource.com/feed" opensource)
	  ("https://lifehacker.com/rss" lifehacker))))

;;CSV files
;;=========
(use-package csv-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

;;Json files
;;==========
(use-package json-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.[Jj][Ss][Oo][Nn]\\'" . json-mode)))

;;Which key for discoverability
;;=============================
(use-package which-key
  :ensure t
  :defer t
  :bind ("C-c m w" . which-key-mode))

;;Custom shorcuts
;;===============
(defun my-launch-windows-explorer-for-buffer ()
  (interactive)
  (async-shell-command "explorer.exe ." nil nil))
(global-set-key (kbd "C-c m e") 'my-launch-windows-explorer-for-buffer)

(defun my-launch-windows-browser ()
  (interactive)
  (async-shell-command (concat "chrome.exe " (browse-url-url-at-point))))
(global-set-key (kbd "C-c m b") 'my-launch-windows-browser)

;; (defun my-launch-buffer-in-windows-browser ()
;;   (interactive)
;;   (async-shell-command (concat "chrome.exe " (symbol-value 'buffer-file-name))))

(defun my-launch-powershell-for-buffer ()
  (interactive)
  (async-shell-command "powershell.exe ." nil nil))
(global-set-key (kbd "C-c m p") 'my-launch-powershell-for-buffer)

(defun my-launch-pycharm-for-dir ()
  (interactive)
  (async-shell-command "pycharm64.exe ." nil nil))
(global-set-key (kbd "C-c m y") 'my-launch-pycharm-for-dir)

(defun my-org-id-generate ()
  "Create unique ID prop for org entry using org-id-get-create and then 
copy that to CUSTOM_ID prop. Also, generate RECORDED propety with value
as an inactive timestamp for today"
  (interactive)
  (progn
    (org-entry-put (point) "CUSTOM_ID" (org-id-get-create))
    (org-entry-put (point) "RECORDED"
		   (with-temp-buffer (org-mode)  (org-time-stamp '(16) t)))))
(global-set-key (kbd "C-c m c") 'my-org-id-generate)
