;; TODO(akash)
;; tmux setup
;; Language support (modes for Python, JavaScript, TypeScript, Go, Rust, Racket, LaTex)
;; RSS feeds (blogposts, hnrss, etc.)

;; No startup screen
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;
;;; PACKAGES ;;;
;;;;;;;;;;;;;;;;

;; (setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
 
;; (use-package gnu-elpa-keyring-update
;;   :ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
   (setq use-package-always-ensure t
         use-package-expand-minimally t))

(use-package tmux-pane
  :ensure t
  :bind (("M-i" . tmux-pane-omni-window-up)
         ("M-j" . tmux-pane-omni-window-left)
         ("M-k" . tmux-pane-omni-window-down)
         ("M-l" . tmux-pane-omni-window-right)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PERSONAL PREFERENCES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-display-line-numbers-mode)

(setq make-backup-files nil)

(setq-default tab-width 4)

(unless window-system
  (xterm-mouse-mode 1))
(use-package vterm
  :ensure t)

;;;;;;;;;;;;;;;;;;;
;;; KEYBINDINGS ;;;
;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-;") 'comment-line)

(global-set-key (kbd "C-x j") 'next-buffer)
(global-set-key (kbd "C-x l") 'previous-buffer)

(defun akash/find-init-file ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun akash/find-life-org ()
  "Edit life.org"
  (interactive)
  (find-file "~/Documents/pkm/life.org"))

(defun akash/find-inbox-org ()
  "Edit inbox.org"
  (interactive)
  (find-file "~/Documents/pkm/inbox.org"))

(global-set-key (kbd "C-x C-v") 'akash/find-init-file)
(global-set-key (kbd "C-x ,") 'akash/find-life-org)
(global-set-key (kbd "C-x /") 'akash/find-inbox-org)



;; (global-set-key (kbd "M-i") 'windmove-up)
;; (global-set-key (kbd "M-j") 'windmove-left)
;; (global-set-key (kbd "M-k") 'windmove-down)
;; (global-set-key (kbd "M-l") 'windmove-right)

;;; PROJECT MANAGEMENT (I'm going to use project.el for a while)
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :bind ("C-c p" . 'projectile-command-map))

;; (use-package treemacs
;;   :ensure t
;;   :bind ("C-c t" . 'treemacs))

;; (use-package treemacs-projectile
;;   :ensure t)

;;;;;;;;;;;;;
;;; MAGIT ;;;
;;;;;;;;;;;;;

(use-package compat
  :ensure t)
(use-package magit
  :ensure t
  :bind ("C-c i" . magit-status))

;;; IVY
(use-package counsel
  :ensure t
  :init (counsel-mode))

(use-package ivy
  :ensure t
  :init (ivy-mode))

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Trying this for a while and seeing what breaks

(keymap-global-set "C-s" #'swiper-isearch)
(keymap-global-set "M-x" #'counsel-M-x)
(keymap-global-set "C-x C-f" #'counsel-find-file)
(keymap-global-set "M-y" #'counsel-yank-pop)
(keymap-global-set "<f1> f" #'counsel-describe-function)
(keymap-global-set "<f1> v" #'counsel-describe-variable)
(keymap-global-set "<f1> o" #'counsel-describe-symbol)
(keymap-global-set "<f1> l" #'counsel-find-library)
(keymap-global-set "<f2> i" #'counsel-info-lookup-symbol)
(keymap-global-set "<f2> u" #'counsel-unicode-char)
(keymap-global-set "<f2> j" #'counsel-set-variable)
(keymap-global-set "C-x b" #'ivy-switch-buffer)
(keymap-global-set "C-c v" #'ivy-push-view)
(keymap-global-set "C-c V" #'ivy-pop-view)

;; (keymap-global-set "C-c c" #'counsel-compile)
;; (keymap-global-set "C-c g" #'counsel-git)
;; (keymap-global-set "C-c j" #'counsel-git-grep)
;; (keymap-global-set "C-c L" #'counsel-git-log)
(keymap-global-set "C-c k" #'counsel-rg)
(keymap-global-set "C-c m" #'counsel-linux-app)
(keymap-global-set "C-c n" #'counsel-fzf)
(keymap-global-set "C-x l" #'counsel-locate)
(keymap-global-set "C-c J" #'counsel-file-jump)
(keymap-global-set "C-S-o" #'counsel-rhythmbox)
(keymap-global-set "C-c w" #'counsel-wmctrl)

(keymap-global-set "C-c C-r" #'ivy-resume)
(keymap-global-set "C-c b" #'counsel-bookmark)
(keymap-global-set "C-c d" #'counsel-descbinds)
(keymap-global-set "C-c o" #'counsel-outline)
(keymap-global-set "C-c t" #'counsel-load-theme)
;; (keymap-global-set "C-c F" #'counsel-org-file)


;;; LSP

(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
		 (python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package which-key
  :ensure t
  :config
    (which-key-mode))


(use-package claude-code
  :ensure t)

;;;;;;;;;;;;;;;;
;;; ORG-MODE ;;;
;;;;;;;;;;;;;;;;

;; org-mode configuration
;; Capture
(setq org-directory "~/Documents/pkm")
(setq org-capture-templates
      (quote (("t" "todo" entry (file "inbox.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
			  ("m" "Meeting" entry (file+headline "inbox.org")
			   ,(concat "* %? :meeting:\n"
						"<%<%Y-%m-%d %a %H:00>>"))
			  ("n" "Note" entry  (file "notes.org")
			   ,(concat "* Note (%a)\n"
						"/Entered on/ %U\n" "\n" "%?")))))
(global-set-key (kbd "C-c c") 'org-capture)
;; Refile
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
(setq org-use-fast-todo-selection t)

;; (defun log-todo-next-creation-date (&rest ignore)
;;   "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
;;   (when (and (string= (org-get-todo-state) "NEXT")
;;              (not (org-entry-get nil "ACTIVATED")))
;;     (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
;; ;; (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c b") 'org-switchb)

;; (setq org-agenda-custom-commands
;;       '(("g" "Get Things Done (GTD)"
;;          ((agenda ""
;;                   ((org-agenda-skip-function
;;                     '(org-agenda-skip-entry-if 'deadline))
;;                    (org-deadline-warning-days 0)))
;;           (todo "NEXT"
;;                 ((org-agenda-skip-function
;;                   '(org-agenda-skip-entry-if 'deadline))
;;                  (org-agenda-prefix-format "  %i %-12:c [%e] ")
;;                  (org-agenda-overriding-header "\nTasks\n")))
;;           (agenda nil
;;                   ((org-agenda-entry-types '(:deadline))
;;                    (org-agenda-format-date "")
;;                    (org-deadline-warning-days 7)
;;                    (org-agenda-skip-function
;;                     '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
;;                    (org-agenda-overriding-header "\nDeadlines")))
;;           (tags-todo "inbox"
;;                      ((org-agenda-prefix-format "  %?-12t% s")
;;                       (org-agenda-overriding-header "\nInbox\n")))
;;           (tags "CLOSED>=\"<today>\""
;;                 ((org-agenda-overriding-header "\nCompleted today\n")))))))

;; Clocking
;; (setq org-log-done 'time)


;; code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (C . t)))
(setq org-confirm-babel-evaluate nil)



;; (use-package org-pomodoro
;;   :after org
;;   :bind (("C-c p" . org-pomodoro))
;;   :config
;;   (setq org-pomodoro-length 50               ;; Pomodoro duration in minutes
;;         org-pomodoro-short-break-length 10   ;; Short break duration
;;         org-pomodoro-long-break-length 20    ;; Long break duration

;;         org-pomodoro-manual-break t          ;; Manual break start (optional, see below)
;;         org-pomodoro-clock-break t           ;; Breaks are clocked (shows up in org time tracking)
;;         org-pomodoro-ask-upon-killing t      ;; Prompt when killing a pomodoro early
;;         org-pomodoro-keep-killed-time t      ;; Keep time even if Pomodoro is killed
;; 		))

;; copy-paste
(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

;;; LLVM-MODE
(setq load-path
      (cons (expand-file-name "~/Documents/compilers-research/llvm-project/utils/emacs/") load-path))
(require 'llvm-mode)
;; (require 'llvm-mir-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/pkm/life.org" "/home/akash-deo/Documents/pkm/inbox.org"))
 '(package-selected-packages '(projectile pdf-tools xclip)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


