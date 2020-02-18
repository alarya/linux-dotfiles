
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

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

(setq package-selected-packages '(gnu-elpa-keyring-update
				  ivy
				  swiper
				  counsel
				  copy-as-format
				  multiple-cursors
				  dictionary
				  outlook
				  magit-p4
				  magit
				  afternoon-theme
				  material-theme
				  sx
				  powerline
				  org-bullets
				  notmuch
				  org-mime
				  counsel-notmuch
				  cider
				  cider-nrepl
				  clojure-mode
				  paredit
				  clojure-mode-extra-font-locking
				  projectile
				  rainbow-delimiters
				  tagedit
				  ivy-clojuredocs
				  pdf-tools
				  org-pdfview
				  bibliothek
				  omnisharp
				  org-ref
				  elpy
				  flycheck
				  ox-gfm))

;;Emacs settings
;;==============
(setq-default fill-column 76)
(setq-default frame-title-format "%b (%f)")
(set-face-attribute 'default nil
		    :family "Consolas"
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
(load-theme	'material t)

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
(recentf-mode 1)
(setq recentf-max-menu-items	20)
(setq recentf-max-saved-items	20)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;dired
(put 'dired-find-alternate-file 'disabled nil)

;;set up dashboard
;;(dashboard-setup-startup-hook)
;;(setq dashboard-startup-banner 'logo)
;;(setq dashboard-set-footer nil)
;;(setq show-week-agenda-p t)

;;prettier status bar
(require 'powerline)
(powerline-default-theme)

(setq initial-major-mode (quote org-mode))
(setq major-mode (quote fundamental-mode))

;;Org mode settings
;;=================
(require 'org-id)
(require 'org-ref)

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

;;Shortcut for org capture
(global-set-key (kbd "C-c c") 'org-capture)

;;Shortcut for org-agenda
(global-set-key (kbd "C-c a")
		'(lambda (&optional arg)
		   (interactive "P")
		   (org-agenda arg "b")))

;;set org refile targets
(setq org-refile-targets
      '((nil :maxlevel . 1)
	(org-agenda-files :maxlevel . 1)))

;;enable auto-fill by default for org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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

;;org-bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;load python
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python . t)))

;;enable export to markdown
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;Ivy settings
;;============
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c h") 'counsel-org-agenda-headlines)

;;Mail settings
;;=============
(require 'notmuch)

(setq smtpmail-default-smtp-server "smtpserver")

(require 'smtpmail)

(setq send-mail-function		'smtpmail-send-it)
(setq message-sendmail-f-is-evil	't)
(setq message-sendmail-extra-arguments	'("--read-envelope-from"))

(setq message-send-mail-function	'message-send-mail-with-sendmail)
(setq sendmail-program			"/usr/bin/msmtp")
(setq message-sendmail-extra-arguments	'("-a" "gmail"))
(setq user-full-name			"Alok Arya")
(setq smtpmail-local-domain		"gmail.com")
(setq user-mail-address			"buntyalok06@gmail.com")
(setq smtpmail-smtp-user		"buntyalok06@gmail	.	com")

(setq smtpmail-smtp-server	"smtp.gmail.com")
(setq smtpmail-stream-type	'ssl)
(setq smtpmail-smtp-service	465)
(setq smtpmail-debug-info	t)
(setq smtpmail-debug-verb	t)

(setq notmuch-search-oldest-first	nil)
(setq message-kill-buffer-on-exit	t)
(setq message-default-mail-headers	"Cc: \nBcc: \n")
(setq message-auto-save-directory	"~/mail/drafts")

;;html support for mails
(require 'org-mime)
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
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

;;Projectile settings
;;===================
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

;;XML mode settings
;;=================
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"

	       "<!--"
	       sgml-skip-tag-forward
	       nil))
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


;;Clojure settings
;;===============
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(require 'clojure-mode-extra-font-locking)
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
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
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

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map	(kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map	(kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map	(kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map		(kbd "C-c u") 'cider-user-ns)))


;;Elisp settings
;;==============
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook				#'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook	#'enable-paredit-mode)
(add-hook 'ielm-mode-hook				#'enable-paredit-mode)
(add-hook 'lisp-mode-hook				#'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook			#'enable-paredit-mode)
(add-hook 'scheme-mode-hook				#'enable-paredit-mode)     
     
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook		'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook	'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook		'turn-on-eldoc-mode)

;;load pdf-tools
(pdf-loader-install)

;;pdf book library
(require 'bibliothek)
(setq bibliothek-path (list "~/Books"))
;;set up org links to pdf
(eval-after-load 'org '(require 'org-pdfview)) 
(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))

;;C Sharp settings
;;================
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)


;;Python settings
;;===============
(elpy-enable)
(add-hook 'python-mode-hook 'linum-mode)
(setq elpy-rpc-python-command "python3")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
