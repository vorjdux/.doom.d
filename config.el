;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; User Informoation
(setq user-full-name "Matheus (vorjdux) Santos"
      user-mail-address "vorj.dux@gmail.com")

;; Font Configuration
;; Uncomment and adjust the following lines according to your preferences.
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Theme and Display Settings
(setq doom-theme 'doom-dark+
      display-line-numbers-type 'relative
      +format-on-save-enabled-modes '(c++-mode python-mode c-mode latex-mode rust-mode)
      projectile-enable-caching (not (executable-find doom-projectile-fd-binary)))

;; Doom Modeline
(global-auto-revert-mode t)

;; Programming Languages and Frameworks Configuration
;; This section includes settings specific to programming languages like Python, C++, etc.

;; C/C++ and LSP
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--suggest-missing-includes"
                                "--header-insertion=never"))

(add-hook! 'prog-mode-hook
           #'rainbow-delimiters-mode)

;; Ensure clang-format is used for C++ files
(setq +format-with-lsp nil)

;; Hook to use clang-format on save
(defun my-c++-mode-hook ()
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Load clang-format
(use-package! clang-format
  :defer t
  :hook (c-mode-common . (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t))))

;; TeX and LaTeX
(after! tex
  (setq +latex-viewers '(pdf-tools zathura))
  (setq-default TeX-master nil)
  (require 'doc-view))


(after! rainbow-delimiters
  (setq rainbow-delimiters-max-face-count 9))

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; VTerm Configuration
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

;; Additional customizations
(setq-default c-basic-offset 4
              c++-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Hook to format C++ files using clang-format on save
(use-package! clang-format
  :defer t)

(map! :leader
      :desc "Format buffer with clang-format" "c f" #'clang-format-buffer)

(after! lsp-clangd
  (setq lsp-clients-clangd-args '("-j=4" "--clang-tidy"))
  )

(add-hook 'c++-mode-hook
          (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil 'local)))


;; Python (pyenv setup)

(use-package! pyvenv
  :config
  (setq pyvenv-workon-home "~/.virtualenvs")
  (pyvenv-mode 1)

  ;; Update `python-shell-interpreter` dynamically
  (defun update-python-interpreter ()
    (setq python-shell-interpreter (executable-find "python")))

  ;; Auto-activate virtual environment based on project name
  (defun my-auto-activate-venv-by-project-name ()
    "Activate a virtual environment matching the project name in `pyvenv-workon-home`."
    (let* ((project-root (projectile-project-root))
           (project-name (when project-root
                           (file-name-nondirectory (directory-file-name project-root)))))
      (if (and project-name
               (member project-name (directory-files pyvenv-workon-home)))
          (pyvenv-workon project-name)
        (set-pyenv-version))
      (update-python-interpreter)))

  ;; Fallback to pyenv global if no specific venv is found
  (defun set-pyenv-version ()
    (let ((pyenv-path (string-trim (shell-command-to-string "pyenv which python"))))
      (setq python-shell-interpreter pyenv-path)))

  ;; Activate venv by project and update Python interpreter
  (add-hook 'python-mode-hook 'my-auto-activate-venv-by-project-name)
  (add-hook 'pyvenv-post-activate-hooks 'update-python-interpreter))

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd (executable-find "python")))

;; Ensure LSP restarts on environment activation
(add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;(add-to-list 'default-frame-alist
;;             '(ns-transparent-titlebar . t))
;;(add-to-list 'default-frame-alist
;;             '(ns-appearance . light))

(add-to-list 'exec-path "/home/vorjdux/.nvm/versions/node/v20.13.1/bin")

;; (add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)


;; (add-hook! 'org-mode-hook (company-mode -1))
;; (add-hook! 'org-capture-mode-hook (company-mode -1))

(setq baby-blue '("#d2ecff" "#d2ecff" "brightblue"))

(setq
 default-directory "~"
 dart-format-on-save t
 web-mode-markup-indent-offset 4
 web-mode-code-indent-offset 4
 web-mode-css-indent-offset 4
 mac-command-modifier 'meta
 js-indent-level 4
 typescript-indent-level 4
 json-reformat:indent-width 4
 prettier-js-args '("--single-quote")
 projectile-project-search-path '("~/Projects/muzzley" "~/Projects/opensource")
 dired-dwim-target t
 org-ellipsis " ▾ "
 org-bullets-bullet-list '("·")
 org-tags-column -80
 org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
 org-log-done 'time
 css-indent-offset 4
 org-refile-targets (quote ((nil :maxlevel . 1)))
 org-capture-templates '(("x" "Note" entry
                          (file+olp+datetree "journal.org")
                          "**** [ ] %U %?" :prepend t :kill-buffer t)
                         ("t" "Task" entry
                          (file+headline "tasks.org" "Inbox")
                          "* [ ] %?\n%i" :prepend t :kill-buffer t))
 +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir)
 +org-capture-todo-file "tasks.org"
 org-super-agenda-groups '((:name "Today"
                            :time-grid t
                            :scheduled today)
                           (:name "Due today"
                            :deadline today)
                           (:name "Important"
                            :priority "A")
                           (:name "Overdue"
                            :deadline past)
                           (:name "Due soon"
                            :deadline future)
                           (:name "Big Outcomes"
                            :tag "bo")))

(add-hook! reason-mode
  (add-hook 'before-save-hook #'refmt-before-save nil t))

(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC n b" #'org-brain-visualize)

;; Org conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! org
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :foreground "#5B6268"
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :foreground "steelblue2"
                      :background nil
                      :height 1.2
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :foreground "slategray2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :foreground "SkyBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-4 nil
                      :foreground "DodgerBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-5 nil
                      :weight 'normal)
  (set-face-attribute 'org-level-6 nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :foreground "SlateGray1"
                      :background nil
                      :height 1.75
                      :weight 'bold)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(setq +magit-hub-features t)

(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)

(use-package org-mode
  :init
  ;; This allows PlantUML, Graphviz and ditaa diagrams
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (plantuml . t)))

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)

  :custom
  (org-edit-src-content-indentation 0)
  ;; PlantUML was too old on Debian Bookworm, so a recent copy is
  ;; installed in /usr/local/share
  (org-plantuml-jar-path "/usr/local/share/plantuml/plantuml.jar")
  ;; ditaa installed through dpkg on Debian
  (org-ditaa-jar-path "/usr/local/share/ditaa/ditaa.jar")
  ;; Do not ask before evaluating a code block
  (org-confirm-babel-evaluate nil)
  ;; Fix for including SVGs
  (org-latex-pdf-process
   '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )
(plist-put org-format-latex-options :background "White")

;; git conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;; {{ Solution 1: disable all vc backends
;; @see http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; (setq vc-handled-backends ())
;; }}


;; {{ Solution 2: if NO network mounted drive involved
(setq vc-handled-backends '(Git SVN Hg))
;; @see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; open files faster but you can't check if file is version
;; controlled. other VCS functionality still works.
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; }}

;; ;; {{ Solution 3: setup vc-handled-backends per project
;; (setq vc-handled-backends ())
;; (defun my-setup-develop-environment ()
;;   (interactive)
;;   (cond
;;    ((string-match-p (file-truename "~/.emacs.d") (file-name-directory (buffer-file-name))
;;     (setq vc-handled-backends '(Git)))
;;    (t (setq vc-handled-backends nil)))))
;; (add-hook 'java-mode-hook 'my-setup-develop-environment)
;; (add-hook 'emacs-lisp-mode-hook 'my-setup-develop-environment)
;; (add-hook 'org-mode-hook 'my-setup-develop-environment)
;; (add-hook 'js2-mode-hook 'my-setup-develop-environment)
;; (add-hook 'js-mode-hook 'my-setup-develop-environment)
;; (add-hook 'javascript-mode-hook 'my-setup-develop-environment)
;; (add-hook 'web-mode-hook 'my-setup-develop-environment)
;; (add-hook 'c++-mode-hook 'my-setup-develop-environment)
;; (add-hook 'c-mode-hook 'my-setup-develop-environment)
;; ;; }}


(eval-after-load 'magit
  '(progn
     (ivy-mode 1)))

;; Refresh VC State on Buffer Switch
(add-hook 'buffer-list-update-hook #'vc-refresh-state)

;; Move text
(use-package! move-text
  :defer nil  ;; Ensure it's loaded immediately
  :config
  (move-text-default-bindings))

;; hydra conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! hydra
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map))

;; pretty-hydra conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq pretty-hydra-enable-use-package t)

(use-package! pretty-hydra
  :defines (display-line-numbers-mode linum-mode)
  :functions (set-package-archives
              centaur-load-theme
              origami-mode
              counsel-load-theme-action)
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                      :color amaranth :quit-key "q")
    ("Basic"
     (("n" (if (fboundp 'display-line-numbers-mode)
               (display-line-numbers-mode (if display-line-numbers-mode -1 1))
             (global-linum-mode (if global-linum-mode -1 1)))
       "line number" :toggle (if (fboundp 'display-line-numbers-mode)
                                 display-line-numbers-mode
                               global-linum-mode))
      ("a" global-aggressive-indent-mode "aggressive indent" :toggle f)
      ("h" global-hungry-delete-mode "hungry delete" :toggle f)
      ("e" electric-pair-mode "electric pair" :toggle f)
      ("c" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("L" global-page-break-lines-mode "page break lines" :toggle t)
      ("M" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("l" global-hl-line-mode "line" :toggle t)
      ("P" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle f)
      ("w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("d" rainbow-delimiters-mode "delimiter" :toggle t)
      ("i" highlight-indent-guides-mode "indent" :toggle t)
      ("T" global-hl-todo-mode "todo" :toggle t))
     "Coding"
     (("f" global-flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
     "Version Control"
     (("v" global-diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("m" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t))
     "Theme"
     (("t d" (centaur-load-theme 'default) "default"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'default)))
      ("t c" (centaur-load-theme 'classic) "classic"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'classic)))
      ("t r" (centaur-load-theme 'colorful) "colorful"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'colorfult)))
      ("t k" (centaur-load-theme 'dark) "dark"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'dark)))
      ("t l" (centaur-load-theme 'light) "light"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'light)))
      ("t y" (centaur-load-theme 'day) "day"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'day)))
      ("t n" (centaur-load-theme 'night) "night"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'night)))
      ("t o" (ivy-read "Load custom theme: "
                       (mapcar #'symbol-name
                               (custom-available-themes))
                       :predicate (lambda (candidate)
                                    (string-prefix-p "doom-" candidate))
                       :action #'counsel-load-theme-action
                       :caller 'counsel-load-theme)
       "others"))
     "Package Archive"
     (("p m" (progn (setq centaur-package-archives 'melpa)
                    (set-package-archives centaur-package-archives))
       "melpa" :toggle (eq centaur-package-archives 'melpa))
      ("p i" (progn (setq centaur-package-archives 'melpa-mirror)
                    (set-package-archives centaur-package-archives))
       "melpa mirror" :toggle (eq centaur-package-archives 'melpa-mirror))
      ("p c" (progn (setq centaur-package-archives 'emacs-china)
                    (set-package-archives centaur-package-archives))
       "emacs china" :toggle (eq centaur-package-archives 'emacs-china))
      ("p n" (progn (setq centaur-package-archives 'netease)
                    (set-package-archives centaur-package-archives))
       "netease" :toggle (eq centaur-package-archives 'netease))
      ("p t" (progn (setq centaur-package-archives 'tencent)
                    (set-package-archives centaur-package-archives))
       "tencent" :toggle (eq centaur-package-archives 'tencent))
      ("p u" (progn (setq centaur-package-archives 'tuna)
                    (set-package-archives centaur-package-archives))
       "tuna" :toggle (eq centaur-package-archives 'tuna))))))


;; Trigger after rust-mode is loaded
(after! rust-mode
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq scroll-margin 10)

;; Topsi conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! topsy
  :hook (prog-mode . topsy-mode) (magit-section-mode . topsy-mode))

;; Copilot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accept completion from copilot and fallback to company
;; (use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;         ("C-<tab>" . 'copilot-accept-completion-by-word)
;;         :map copilot-completion-map
;;         ("C-y" . 'copilot-accept-completion)
;;         ("C-Y" . 'copilot-accept-completion)))


(setq ispell-hunspell-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))


;; rg config with custom code search in a project
(use-package! rg
  :config
  (rg-enable-default-bindings)
  (setq rg-group-result t
        rg-show-columns t
        rg-custom-type-aliases nil
        rg-default-alias-fallback "all"
        rg-ignore-case 'smart
        rg-global-extra-args '("--glob" "!*.git/*" "--glob" "!*cache*" "--glob" "!node_modules/*" "--glob" "!*.log")))

(add-hook 'prog-mode-hook 'which-function-mode)
