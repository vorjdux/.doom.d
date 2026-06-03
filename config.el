;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;(setq debug-on-error t)

;; User Information
(setq user-full-name "Matheus (vorjdux) Santos"
      user-mail-address "vorj.dux@gmail.com")

(use-package! exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l" "-c"))
  (setq exec-path-from-shell-variables '("PATH" "PYENV_ROOT" "WORKON_HOME" "VIRTUAL_ENV"))
  :config
  (exec-path-from-shell-initialize))

;; Force pyenv path visibility inside Emacs
(let ((pyenv-bin (expand-file-name "~/.pyenv/bin"))
      (pyenv-shims (expand-file-name "~/.pyenv/shims")))
  (setenv "PATH" (concat pyenv-bin ":" pyenv-shims ":" (getenv "PATH")))
  (add-to-list 'exec-path pyenv-bin)
  (add-to-list 'exec-path pyenv-shims))

;; Font Configuration
;; Uncomment and adjust the following lines according to your preferences.
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Theme and Display Settings
(setq doom-theme 'doom-dark+
      display-line-numbers-type 'relative
      ;;+format-on-save-enabled-modes '(python-mode latex-mode rust-mode)
      +format-on-save-enabled-modes '(c++-mode python-mode c-mode latex-mode rust-mode)
      projectile-enable-caching (not (and doom-projectile-fd-binary (executable-find doom-projectile-fd-binary))))

;; Doom Modeline
(global-auto-revert-mode t)

;; Programming Languages and Frameworks Configuration
;; This section includes settings specific to programming languages like Python, C++, etc.

;; C/C++ and LSP
(after! lsp-clangd
  (setq lsp-clients-clangd-args '(
                                  "-j=2"                     ;; Use 4 threads
                                  "--background-index"       ;; Enable background indexing
                                  "--completion-style=detailed" ;; Provide detailed completion info
                                  "--suggest-missing-includes" ;; Suggest missing include headers
                                  "--header-insertion=never"  ;; Prevent automatic header insertion
                                  )
        lsp-clients-clangd-executable "/usr/bin/clangd") ;; Ensure correct clangd path
  (setq +format-with-lsp nil)) ;; Disable formatting through lsp

(defun my-lsp-set-clangd-compile-commands-dir ()
  "Set the correct compile_commands.json directory for the current buffer."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (let ((project-dir (locate-dominating-file default-directory "compile_commands.json")))
      (when project-dir
        (setq-local lsp-clients-clangd-args
                    (list "-j=2"
                          "--background-index"
                          "--completion-style=detailed"
                          "--header-insertion=never"
                          (format "--compile-commands-dir=%s" project-dir)))))))

(add-hook 'c-mode-hook #'my-lsp-set-clangd-compile-commands-dir)
(add-hook 'c++-mode-hook #'my-lsp-set-clangd-compile-commands-dir)

;; Auto-format with clang-format on save
(use-package! clang-format
  :defer t
  :hook ((c-mode-common . (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))))

(setq lsp-log-io t)

(add-hook! 'prog-mode-hook
           #'rainbow-delimiters-mode)

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

;; Python (pyenv setup)

;; (use-package! exec-path-from-shell
;;   :init
;;   (setq exec-path-from-shell-arguments '("-l" "-c"))
;;   (setq exec-path-from-shell-variables '("PATH" "PYENV_VERSION" "WORKON_HOME"))
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package! pyvenv
  :config
  ;; (setq pyvenv-workon-home "~/.pyenv/")
  (setq pyvenv-workon-home (concat (getenv "HOME") "/.pyenv/versions/"))
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
  ;; (add-hook 'pyvenv-post-activate-hooks 'update-python-interpreter)
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (setq lsp-pyright-python-executable-cmd (executable-find "python"))
              (lsp-restart-workspace)))
  )

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd (executable-find "python")))

(use-package! apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "--quiet" "-"))
  (apheleia-global-mode +1))

(after! python
  (add-hook 'python-mode-hook
            (lambda ()
              (when (executable-find "pylint")
                (setq flycheck-python-pylint-executable (executable-find "pylint"))))))

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


;; SQL Formatter

(after! sql
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("--function-case" "1" "--keyword-case" "2")))

(map! :map sql-mode-map
      :localleader
      :desc "Format SQL buffer" "f" #'sqlformat-buffer)


(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))


;; Other things

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
                      :background 'unspecified
                      :height 1.75
                      :weight 'bold)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (plantuml . t)
     (json . t)
     (emacs-lisp . t)
     (python . t)))

  ;; Set paths for PlantUML and ditaa
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml-1.2025.7.jar"
        org-ditaa-jar-path "/usr/share/ditaa/ditaa0_9.jar")

  ;; Automatically redisplay inline images after executing code blocks
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; Disable confirmation for evaluating code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; Configure LaTeX PDF process with shell-escape for SVGs
  ;; (setq org-latex-pdf-process
  ;;       '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "bibtex %b"
  ;;         "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  )

;; Set background color for LaTeX previews
(after! org
  (plist-put org-format-latex-options :background "White"))

;; LaTeX Configuration
(after! ox-latex
  ;; Enable minted for syntax highlighting in LaTeX exports
  (setq org-latex-listings 'minted)

  ;; Add minted package to the LaTeX preamble
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  ;; Use xelatex or lualatex to ensure UTF-8 support and minted compatibility
  ;;(setq org-latex-pdf-process
  ;;      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "rm -f %o/*.aux %o/*.log %o/*.out %o/*.toc %o/*.fls %o/*.fdb_latexmk"))


  ;; Set LaTeX margins and page dimensions
  (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry")) ; Adjust margin as needed

  ;; Set default LaTeX compiler
  (setq org-latex-compiler "xelatex")

  ;; Minted options to handle long lines
  (setq org-latex-minted-options
        '(("breaklines" "true")
          ("breakanywhere" "true")
          ("fontsize" "\\footnotesize")))
  ;; Prevent Infinite Loops
  (setq org-latex-prefer-user-labels t))

(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)

;; git conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;; {{ Solution 1: disable all vc backends
;; @see http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; (setq vc-handled-backends ())
;; }}

;; Magit Hub Features
(setq +magit-hub-features t)

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


;; (eval-after-load 'magit
;;  '(progn
;;     (ivy-mode 1)))

;; Refresh VC State on Buffer Switch
(add-hook 'buffer-list-update-hook #'vc-refresh-state)

;; Move text
(use-package! move-text
  :defer nil  ;; Ensure it's loaded immediately
  :config
  (move-text-default-bindings))

;; ;; hydra conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! hydra
;;   :commands (hydra-default-pre
;;              hydra-keyboard-quit
;;              hydra--call-interactively-remap-maybe
;;              hydra-show-hint
;;              hydra-set-transient-map))

;; ;; pretty-hydra conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq pretty-hydra-enable-use-package t)

;; (use-package! pretty-hydra
;;   :defines (display-line-numbers-mode linum-mode)
;;   :functions (set-package-archives
;;               centaur-load-theme
;;               origami-mode
;;               counsel-load-theme-action)
;;   :bind ("<f6>" . toggles-hydra/body)
;;   :init
;;   (cl-defun pretty-hydra-title (title &optional icon-type icon-name
;;                                       &key face height v-adjust)
;;     "Add an icon in the hydra title."
;;     (let ((face (or face `(:foreground ,(face-background 'highlight))))
;;           (height (or height 1.0))
;;           (v-adjust (or v-adjust 0.0)))
;;       (concat
;;        (when (and (display-graphic-p) icon-type icon-name)
;;          (let ((f (intern (format "all-the-icons-%s" icon-type))))
;;            (when (fboundp f)
;;              (concat
;;               (apply f (list icon-name :face face :height height :v-adjust v-adjust))
;;               " "))))
;;        (propertize title 'face face))))

;;   ;; Global toggles
;;   (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
;;                                       :color amaranth :quit-key "q")
;;     ("Basic"
;;      (("n" (if (fboundp 'display-line-numbers-mode)
;;                (display-line-numbers-mode (if display-line-numbers-mode -1 1))
;;              (global-linum-mode (if global-linum-mode -1 1)))
;;        "line number" :toggle (if (fboundp 'display-line-numbers-mode)
;;                                  display-line-numbers-mode
;;                                global-linum-mode))
;;       ("a" global-aggressive-indent-mode "aggressive indent" :toggle f)
;;       ("h" global-hungry-delete-mode "hungry delete" :toggle f)
;;       ("e" electric-pair-mode "electric pair" :toggle f)
;;       ("c" flyspell-mode "spell check" :toggle t)
;;       ("S" prettify-symbols-mode "pretty symbol" :toggle t)
;;       ("L" global-page-break-lines-mode "page break lines" :toggle t)
;;       ("M" doom-modeline-mode "modern mode-line" :toggle t))
;;      "Highlight"
;;      (("l" global-hl-line-mode "line" :toggle t)
;;       ("P" show-paren-mode "paren" :toggle t)
;;       ("s" symbol-overlay-mode "symbol" :toggle t)
;;       ("r" rainbow-mode "rainbow" :toggle f)
;;       ("w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
;;        "whitespace" :toggle show-trailing-whitespace)
;;       ("d" rainbow-delimiters-mode "delimiter" :toggle t)
;;       ("i" highlight-indent-guides-mode "indent" :toggle t)
;;       ("T" global-hl-todo-mode "todo" :toggle t))
;;      "Coding"
;;      (("f" global-flycheck-mode "flycheck" :toggle t)
;;       ("F" flymake-mode "flymake" :toggle t)
;;       ("o" origami-mode "folding" :toggle t)
;;       ("O" hs-minor-mode "hideshow" :toggle t)
;;       ("u" subword-mode "subword" :toggle t)
;;       ("W" which-function-mode "which function" :toggle t)
;;       ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
;;       ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
;;      "Version Control"
;;      (("v" global-diff-hl-mode "gutter" :toggle t)
;;       ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
;;       ("m" diff-hl-margin-mode "margin gutter" :toggle t)
;;       ("D" diff-hl-dired-mode "dired gutter" :toggle t))
;;      "Theme"
;;      (("t d" (centaur-load-theme 'default) "default"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'default)))
;;       ("t c" (centaur-load-theme 'classic) "classic"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'classic)))
;;       ("t r" (centaur-load-theme 'colorful) "colorful"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'colorfult)))
;;       ("t k" (centaur-load-theme 'dark) "dark"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'dark)))
;;       ("t l" (centaur-load-theme 'light) "light"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'light)))
;;       ("t y" (centaur-load-theme 'day) "day"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'day)))
;;       ("t n" (centaur-load-theme 'night) "night"
;;        :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'night)))
;;       ("t o" (ivy-read "Load custom theme: "
;;                        (mapcar #'symbol-name
;;                                (custom-available-themes))
;;                        :predicate (lambda (candidate)
;;                                     (string-prefix-p "doom-" candidate))
;;                        :action #'counsel-load-theme-action
;;                        :caller 'counsel-load-theme)
;;        "others"))
;;      "Package Archive"
;;      (("p m" (progn (setq centaur-package-archives 'melpa)
;;                     (set-package-archives centaur-package-archives))
;;        "melpa" :toggle (eq centaur-package-archives 'melpa))
;;       ("p i" (progn (setq centaur-package-archives 'melpa-mirror)
;;                     (set-package-archives centaur-package-archives))
;;        "melpa mirror" :toggle (eq centaur-package-archives 'melpa-mirror))
;;       ("p c" (progn (setq centaur-package-archives 'emacs-china)
;;                     (set-package-archives centaur-package-archives))
;;        "emacs china" :toggle (eq centaur-package-archives 'emacs-china))
;;       ("p n" (progn (setq centaur-package-archives 'netease)
;;                     (set-package-archives centaur-package-archives))
;;        "netease" :toggle (eq centaur-package-archives 'netease))
;;       ("p t" (progn (setq centaur-package-archives 'tencent)
;;                     (set-package-archives centaur-package-archives))
;;        "tencent" :toggle (eq centaur-package-archives 'tencent))
;;       ("p u" (progn (setq centaur-package-archives 'tuna)
;;                     (set-package-archives centaur-package-archives))
;;        "tuna" :toggle (eq centaur-package-archives 'tuna))))))


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

(map! :leader
      :desc "Ripgrep search in project"
      "s r" #'rg-project)

(map! :leader
      (:prefix ("s" . "search")
       :desc "Ripgrep in directory" "d" #'rg-dwim
       :desc "Ripgrep current file" "f" #'rg
       :desc "Ripgrep at point"     "t" #'rg-menu))


(add-hook 'prog-mode-hook 'which-function-mode)


;; ORG generate TOC
(setq org-export-with-broken-links 'mark)

(setq org-latex-hyperref-template "
\\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  filecolor=magenta,
  urlcolor=cyan,
  citecolor=blue,
  pdftitle={%t},
  pdfauthor={%a},
  pdfsubject={%d},
  pdfkeywords={%k}
}")

(defun org-generate-toc ()
  "Generate a Table of Contents for the current Org file and insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((toc-lines '()) ;; Collect TOC lines
          (toc-placeholder "#+TOC_PLACEHOLDER:") ;; Placeholder for TOC insertion
          (heading-re "^\\(\\*+\\) \\(.*\\)$")) ;; Regex to match headings
      ;; Ensure the TOC placeholder exists
      (if (re-search-forward (regexp-quote toc-placeholder) nil t)
          (let ((placeholder-pos (point)))
            ;; Collect headings and set CUSTOM_ID
            (goto-char (point-min))
            (while (re-search-forward heading-re nil t)
              (let* ((level (length (match-string 1))) ;; Get heading level
                     (title (match-string 2))         ;; Get heading text
                     (sanitized-title (replace-regexp-in-string
                                       "[^a-zA-Z0-9]+" "-" ;; Replace non-alphanumeric chars with "-"
                                       (downcase title))))
                ;; Set CUSTOM_ID property for the heading
                (save-excursion
                  (org-back-to-heading)
                  (org-entry-put nil "CUSTOM_ID" sanitized-title))
                ;; Create TOC entry
                (push (concat (make-string (* 2 (1- level)) ? ) "- [[#" sanitized-title "][" title "]]") toc-lines)))
            ;; Insert the TOC at the placeholder
            (goto-char placeholder-pos)
            (beginning-of-line)
            (delete-region (point) (line-end-position)) ;; Clear the placeholder line
            (insert "* Navigation (Contents)\n")
            (insert "#+BEGIN_TOC\n")
            (insert (string-join (reverse toc-lines) "\n"))
            (insert "\n#+END_TOC"))
        (message "TOC placeholder not found! Add `#+TOC_PLACEHOLDER:` to your Org file.")))))

(defun org-export-with-toc (&rest _args)
  "Generate TOC before exporting Org file."
  (when (derived-mode-p 'org-mode)
    (condition-case err
        (org-generate-toc)
      (error (message "Error generating TOC: %s" (error-message-string err))))))

;; Add the hook for Org export
(add-hook 'org-export-before-processing-hook #'org-export-with-toc)

;; Crypt gpg files
(after! org
  (defun my/org-copy-secret ()
    "Extracts PASSWORD or SECRET property from the current AST node, yanks to clipboard, and schedules a 15s wipe."
    (interactive)
    ;; org-entry-get fetches from the property drawer of the node at point
    (let ((secret (or (org-entry-get (point) "PASSWORD")
                      (org-entry-get (point) "SECRET"))))
      (if secret
          (progn
            ;; kill-new injects into both Emacs kill-ring and OS clipboard via interprocess communication
            (kill-new secret)
            (message "🔑 Secret loaded to memory. Clipboard wipe scheduled in 15 seconds...")
            ;; Non-blocking thread via timer
            (run-with-timer 15 nil
                            (lambda ()
                              ;; Overwrite the OS clipboard buffer with an empty string
                              (kill-new "")
                              (message "🗑️ Clipboard purged (Auto-Wipe complete)."))))
        (message "Error: No PASSWORD or SECRET property found in current node."))))

  ;; Bind execution to <Local Leader> + y + s (Yank Secret)
  (map! :map org-mode-map :localleader "y s" #'my/org-copy-secret))

;; Claude Code
(use-package! claude-code
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/home/vorjdux/.local/bin"))
  (setq exec-path (append exec-path '("/home/vorjdux/.local/bin")))
  (claude-code-mode)
  :bind-keymap
  ("C-c c" . claude-code-command-map))
