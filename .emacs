
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
(set-face-attribute 'default nil
		    :family "Monospace"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 120
		    :width 'normal)
(setq column-number-mode	t)
(setq inhibit-startup-message	t)
(menu-bar-mode -1)
(tool-bar-mode	-1)
(toggle-scroll-bar -1)
;;don't litter the .emacs file with custom-set-variables
(setq custom-file (concat user-emacs-directory "/custom.el"))

(use-package material-theme
  :ensure t
  :init
  (load-theme 'material t))

;;maximize frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(setq split-window-preferred-function #'my-split-window-sensibly)
 
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
;;(dashboard-setup-startup-hook)
;;(setq dashboard-startup-banner 'logo)
;;(setq dashboard-set-footer nil)
;;(setq show-week-agenda-p t)

;;follow sym links
(setq vc-follow-symlinks t)

(setq initial-major-mode (quote org-mode))
(setq major-mode (quote fundamental-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;;Elpa keyring update package
;;===========================
(use-package gnu-elpa-keyring-update
  :ensure t
  :defer t)

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
      '("~/one-drive/Notes-AALOK/Qualcomm.org"
	"~/p4/PinMux_Dev/latest/Tools/PinMux/PinMux_Dev/Docs/QPCT.org"
	"~/github/Life/Personal.org"))

  ;;Org capture templates
  (setq org-capture-templates
      '(("w"
	 "work to do capture"
	 entry
	 (file+headline "~/one-drive/Notes-AALOK/Qualcomm.org" "Capture")
	 "* TODO %^{Title}\n:PROPERTIES:\n:Recorded: %U\n:END:\n%^{Description}%?"
	 :empty-lines 1)
	("p"
	 "personal to do capture"
	 entry
	 (file+headline "~/github/Life/Personal.org" "Capture")
	 "* TODO %^{Title}\n:PROPERTIES:\n:Recorded: %U\n:END:\n%^{Description}%?"
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

  ;;archive subtree settings
  (setq org-cycle-open-archived-trees t)
  (setq org-export-with-archived-trees t)

  ;;custom agenda views
  (setq org-agenda-custom-commands
      '(("b" "My org view"
	 ((agenda "")
	  (todo "")
	  (tags "discuss|revisit|bug+TODO|jira+TODO")))))

  (setq org-cycle-separator-lines 1)

  ;;load python
  (org-babel-do-load-languages 'org-babel-load-languages
  			       '((python . t)))

  ;;let css take care of code snippet formatting
  (setq org-html-htmlize-output-type 'css))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package ox-gfm
  :ensure t
  :after org
  :config
  ;;enable export to markdown
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

;; (use-package org-ref
;;   :ensure t
;;   :after org)

(use-package org-id
  :after org)

;;Completion frameworks
;;=====================

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :after
  (ivy)
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :after
  (swiper)
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c h") 'counsel-org-agenda-headlines))

;;Yasnippet
;;=========
;; (use-package yasnippet
;;   :ensure t
;;   :defer 5
;;   :hook
;;   (prog-mode yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;;Mail settings
;;=============
(use-package notmuch
  :ensure t
  :config
  (setq notmuch-search-oldest-first	nil))

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
  (setq projectile-completion-system 'ivy))

;;XML mode settings
;;=================
;; (use-package hideshow

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
;; )

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
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;Electric pair mode
;;==================
(use-package electric
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'electric-pair-mode))

;;Auto complete
(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (add-hook 'prog-mode-hook #'auto-complete-mode))

;;magit settings
;;==============
(use-package magit
  :ensure t
  :defer t)

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

;;ElDoc
;;=====
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook		'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook	'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook		'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook              'turn-on-eldoc-mode)

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
  (use-package org-pdfview
    :ensure t
    :config
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))))

;;C Sharp settings
;;================
(use-package csharp-mode
  :ensure t
  :defer t
  :init
  (electric-pair-local-mode 1))

(use-package omnisharp
  :ensure t
  :hook
  (csharp-mode . omnisharp-mode))

;;Fly check settings
;;==================
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  (add-hook 'elpy-mode-hook #'flycheck-mode))

;;Python settings
;;===============
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'python-mode-hook 'linum-mode)
  (setq elpy-rpc-python-command "python3"))


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
