;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matheus (vorjdux) Santos"
      user-mail-address "vorj.dux@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+
      display-line-numbers-type 'relative
      +format-on-save-enabled-modes '(c++-mode python-mode c-mode latex-mode rust-mode)
      projectile-enable-caching (not (executable-find doom-projectile-fd-binary)))

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--suggest-missing-includes"
                                "--header-insertion=never"))

(add-hook! 'prog-mode-hook
           #'rainbow-delimiters-mode)

(after! tex
  (setq +latex-viewers '(pdf-tools zathura))
  (setq-default TeX-master nil)
  (require 'doc-view))

(setq-hook! 'python-mode-hook +format-with-lsp nil)

(after! rainbow-delimiters
  (setq rainbow-delimiters-max-face-count 9))

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

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

(global-auto-revert-mode t)

;; (add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)


;; (add-hook! 'org-mode-hook (company-mode -1))
;; (add-hook! 'org-capture-mode-hook (company-mode -1))

(setq baby-blue '("#d2ecff" "#d2ecff" "brightblue"))

(setq
 default-directory "~"
 dart-format-on-save t
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 mac-command-modifier 'meta
 js-indent-level 2
 typescript-indent-level 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 projectile-project-search-path '("~/Projects/muzzley" "~/Projects/opensource")
 dired-dwim-target t
 org-ellipsis " ▾ "
 org-bullets-bullet-list '("·")
 org-tags-column -80
 org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
 org-log-done 'time
 css-indent-offset 2
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

(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "~/.virtualenvs")

(add-hook! reason-mode
  (add-hook 'before-save-hook #'refmt-before-save nil t))

(add-hook!
 js2-mode 'prettier-js-mode
 (add-hook 'before-save-hook #'refmt-before-save nil t))

(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC n b" #'org-brain-visualize)

;; (def-package! parinfer ; to configure it
;;   :bind (("C-," . parinfer-toggle-mode)
;;          ("<tab>" . parinfer-smart-tab:dwim-right)
;;          ("S-<tab>" . parinfer-smart-tab:dwim-left))
;;   :hook ((clojure-mode emacs-lisp-mode common-lisp-mode lisp-mode) . parinfer-mode)
;;   :config (setq parinfer-extensions '(defaults pretty-parens evil paredit)))

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

(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode)))

(setq +magit-hub-features t)

(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)



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



;; {{ git-gutter
;;(local-require 'git-gutter)

(defun git-gutter-reset-to-head-parent()
  "Reset  gutter to HEAD^.  Support Subversion and Git."
  (interactive)
  (let* (parent (filename (buffer-file-name)))
    (if (eq git-gutter:vcs-type 'svn)
        (setq parent "PREV")
      (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" " filename)) "^") "HEAD^")))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))


(defun my-git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (nonempty-lines (shell-command-to-string git-cmd)))
         (item (ffip-completing-read "git log:" collection)))
    (when item
      (car (split-string item "|" t)))))

(defun my-git-show-commit-internal ()
  "Show git commit"
  (let* ((id (my-git-commit-id)))
    (when id
      (shell-command-to-string (format "git show %s" id)))))

(defun my-git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let* ((ffip-diff-backends '(("Show git commit" . my-git-show-commit-internal))))
    (ffip-show-diff 0)))

(defun git-gutter-toggle ()
  "Toggle git gutter."
  (interactive)
  (git-gutter-mode -1)
  ;; git-gutter-fringe doesn't seem to
  ;; clear the markup right away
  (sit-for 0.1)
  (git-gutter:clear))

(defun git-gutter-reset-to-default ()
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))

(global-git-gutter-mode t)

;; nobody use bzr
;; I could be forced to use subversion or hg which has higher priority
(custom-set-variables '(git-gutter:handled-backends '(svn hg git)))

(unless (fboundp 'global-display-line-numbers-mode)
  ;; git-gutter's workaround for linum-mode bug.
  ;; should not be used in `display-line-number-mode`
  (git-gutter:linum-setup))

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; }}

;; {{ git-timemachine
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                               ;; re-shape list for the ivy-read
                               (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                             (git-timemachine--revisions))))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 8+ and later ivy version
                        (unless (string-match-p "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (my-ensure 'git-timemachine)
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))
;; }}

(defun git-get-current-file-relative-path ()
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (message "DONE! git checkout %s" filename))))

(defvar git-commit-message-history nil)
(defun git-commit-tracked ()
  "Run 'git add -u' and commit."
  (interactive)
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):")
         (msg (read-from-minibuffer hint
                                    nil
                                    nil
                                    nil
                                    'git-commit-message-history)))
    (cond
     ((and msg (> (length msg) 3))
      (shell-command "git add -u")
      (shell-command (format "git commit -m \"%s\"" msg))
      (message "Tracked files is commited."))
     (t
      (message "Do nothing!")))))

(defun git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (when buffer-file-name
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

;; {{ goto next/previous hunk
(defun my-goto-next-hunk (arg)
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-next)
    (forward-line)
    (if (re-search-forward "\\(^<<<<<<<\\|^=======\\|^>>>>>>>\\)" (point-max) t)
        (goto-char (line-beginning-position))
      (forward-line -1)
      (git-gutter:next-hunk arg))))

(defun my-goto-previous-hunk (arg)
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-prev)
    (forward-line -1)
    (if (re-search-backward "\\(^>>>>>>>\\|^=======\\|^<<<<<<<\\)" (point-min) t)
        (goto-char (line-beginning-position))
      (forward-line -1)
      (git-gutter:previous-hunk arg))))
;; }}

;; {{ git-gutter use ivy
(defun my-reshape-git-gutter (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'my-reshape-git-gutter git-gutter:diffinfos)
                :action (lambda (e)
                          (unless (numberp e) (setq e (cdr e)))
                          (goto-line e)))
    (message "NO git-gutters!")))

;; }}

(defun my-git-log-trace-definition ()
  "Similar to `magit-log-trace-definition' but UI is simpler.
If multi-lines are selected, trace the defintion of line range.
If only one line is selected, use current selection as function name to look up.
If nothing is selected, use the word under cursor as function name to look up."
  (interactive)
  (when buffer-file-name
    (let* ((range-or-func (cond
                           ((region-active-p)
                            (cond
                             ((my-is-in-one-line (region-beginning) (region-end))
                              (format ":%s" (my-selected-str)))
                             (t
                              (format "%s,%s"
                                      (line-number-at-pos (region-beginning))
                                      (line-number-at-pos (1- (region-end)))))))
                           (t
                            (format ":%s" (thing-at-point 'symbol)))))
           (cmd (format "git log -L%s:%s" range-or-func (file-truename buffer-file-name)))
           (content (shell-command-to-string cmd)))
      (when (string-match-p "no match" content)
        ;; mark current function and try again
        (mark-defun)
        (setq range-or-func (format "%s,%s"
                                    (line-number-at-pos (region-beginning))
                                    (line-number-at-pos (1- (region-end)))))
        (setq cmd (format "git log -L%s:%s" range-or-func (file-truename buffer-file-name))))
      ;; (message cmd)
      (my-ensure 'find-file-in-project)
      (ffip-show-content-in-diff-mode (shell-command-to-string cmd)))))

(eval-after-load 'magit
  '(progn
     (ivy-mode 1)))

(eval-after-load 'vc-msg-git
  '(progn
     ;; open file of certain revision
     (push '("m" "[m]agit-find-file"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info))
                 (magit-find-file (plist-get info :id )
                                  (concat (vc-msg-sdk-git-rootdir)
                                          (plist-get info :filename))))))
           vc-msg-git-extra)

     ;; copy commit hash
     (push '("h" "[h]ash"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info)
                      (id (plist-get info :id)))
                 (kill-new id)
                 (message "%s => kill-ring" id))))
           vc-msg-git-extra)

     ;; copy commit hash
     (push '("a" "[a]uthor"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info)
                      (author (plist-get info :author)))
                 (kill-new author)
                 (message "%s => kill-ring" author))))
           vc-msg-git-extra)))




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
      ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
      ("h" global-hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("L" global-page-break-lines-mode "page break lines" :toggle t)
      ("M" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("l" global-hl-line-mode "line" :toggle t)
      ("P" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle t)
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


;; Trigger a refresh of vc-modeline  on some magit functions
(with-eval-after-load 'magit
  (defun refresh-vc-state () '(progn (vc-refresh-state)))
  (advice-add 'magit-checkout :after #'refresh-vc-state)
  (advice-add 'magit-branch-create :after #'refresh-vc-state)
  (advice-add 'magit-branch-and-checkout :after #'refresh-vc-state)
  (advice-add 'magit-branch-or-checkout :after #'refresh-vc-state))


;; Trigger after rust-mode is loaded
(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

                                        ; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

                                        ; Magit Blamer
(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode 1))

                                        ; Magit force update
(defun +cwejman-vc-refresh-modelines (a)
  (dolist (buffer (doom-buffer-list))
    (set-buffer buffer)
    (vc-refresh-state)
    (+doom-modeline--update-vcs)))

(advice-add 'magit-checkout :after #'+cwejman-vc-refresh-modelines)

(after! dap-mode
  (setq dap-python-executable "python3"
        dap-python-debugger 'debugpy
        dap-auto-configure-features '(breakpoints expressions controls tooltip repl locals)
        dap-ui-buffer-configurations '(("*dap-ui-locals*" (side . right) (slot . 1) (window-width . 0.3))
                                       ("*dap-ui-expressions*" (side . right) (slot . 2) (window-width . 0.3))
                                       ("*dap-ui-sessions*" (side . right) (slot . 3) (window-width . 0.3))
                                       ("*dap-ui-breakpoints*" (side . left) (slot . 2) (window-width . 35))
                                       ("*debug-window*" (side . bottom) (slot . 3) (window-width . 0.2))
                                       ("*dap-ui-repl*" (side . bottom) (slot . 1) (window-height . 0.45))))
  (map! :map dap-mode-map
        :leader
        :prefix ("d" . "dap")
        ;; basics
        :desc "dap next"          "n" #'dap-next
        :desc "dap step in"       "i" #'dap-step-in
        :desc "dap step out"      "o" #'dap-step-out
        :desc "dap continue"      "c" #'dap-continue
        :desc "dap hydra"         "h" #'dap-hydra
        :desc "dap debug restart" "r" #'dap-debug-restart
        :desc "dap debug"         "s" #'dap-debug

        ;; debug
        :prefix ("dd" . "Debug")
        :desc "dap debug recent"  "r" #'dap-debug-recent
        :desc "dap debug last"    "l" #'dap-debug-last

        ;; eval
        :prefix ("de" . "Eval")
        :desc "eval"                "e" #'dap-eval
        :desc "eval region"         "r" #'dap-eval-region
        :desc "eval thing at point" "s" #'dap-eval-thing-at-point
        :desc "add expression"      "a" #'dap-ui-expressions-add
        :desc "remove expression"   "d" #'dap-ui-expressions-remove

        :prefix ("db" . "Breakpoint")
        :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
        :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
        :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message))

(defun r0fl/python-args-to-docstring (&optional arguments)
  "return docstring format for the python arguments in yas-text or in ARGUMENTS"
  (let* ((indent  "\n" )
         (args (if arguments
                   (python-split-args arguments)
                 (python-split-args yas-text)))
         (formatted-args (mapconcat
                          (lambda (x)
                            (let ((arg (nth 0 x)))
                              (concat arg (if (string-match-p ":" arg) "\n" " :\n")))) args indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Parameter\n---------" formatted-args) indent))))

(defun r0fl/python-return-to-docstring (&optional type)
  "Return docstring format for the python return type in yas-text or in TYPE"
  (let ((type-intern (or type yas-text)))
    (when (not (equal "None" type-intern))
      (concat "Return\n------\n" type-intern " :\n"))))

(defun r0fl/py-args-to-doc (beg end)
  "Convert python arguments between BEG and END to docstring."
  (interactive "r")
  (let ((args (buffer-substring beg end)))
    (setq mark-active nil)
    (end-of-line)
    (yas-expand-snippet
     (concat "\n\"\"\"\n"
             (r0fl/python-args-to-docstring args)
             "\n\"\"\""))))

(defun r0fl/py-def-to-doc (beg end)
  (interactive "r")
  (let ((def (buffer-substring beg end)))
    (setq mark-active nil)
    (end-of-line)
    (yas-expand-snippet (concat
                         "\n\"\"\""
                         (replace-regexp-in-string "_" " " def)
                         ".\"\"\"\n"))))

(defun r0fl/toggle-pyright-type ()
  (interactive)
  (if (string-match-p "basic" lsp-pyright-typechecking-mode)
      (setq lsp-pyright-typechecking-mode "strict")
    (setq lsp-pyright-typechecking-mode "basic"))
  (lsp-restart-workspace)
  (message "typechecking is: " lsp-pyright-typechecking-mode))

;; ejc-sql conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq clomacs-httpd-default-port 8090)
(setq ejc-use-flx t)
(setq ejc-flx-threshold 2)
(setq ejc-completion-system 'ido)

(use-package! ejc-autocomplete)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))

;; Smudge conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! smudge
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-oauth2-client-secret (getenv "SMUDGE_CLIENT_SECRET"))
  (smudge-oauth2-client-id (getenv "SMUDGE_CLIENT_ID"))
  ;; optional: enable transient map for frequent commands
  (smudge-player-use-transient-map t))

(setq ispell-hunspell-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;; Pylint conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/set-pylintrc-path ()
  "Set `flycheck-pylintrc` based on the project's root."
  (let ((project-root (projectile-project-root)))
    (when project-root
      (setq-local flycheck-pylintrc (expand-file-name ".pylintrc" project-root)))))

(add-hook 'python-mode-hook 'my/set-pylintrc-path)

;; Daily Box of names conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar daily-names-list nil
  "List of names to display.")

(defvar daily-used-names nil
  "List of names that have been used.")

(defvar daily-last-picked-name nil
  "The last name that was picked.")

(defun daily-display-boxed-names-slackware-style ()
  "Display a list of NAMES in a centered layout, utilizing the full width of the buffer."
  (let* ((buffer (get-buffer-create "*Team Meeting - Name Draw*"))
         (max-name-length (apply 'max (mapcar 'string-width daily-names-list)))
         (padding 8) ;; Adjust padding around names if needed
         (window-width (window-width (selected-window)))) ;; Use full width of the buffer
    (with-current-buffer buffer
      (erase-buffer)
      ;; Iterate over each name and format it, centralized in the buffer
      (dolist (name daily-names-list)
        (let* ((name-length (string-width name))
               (total-padding (- window-width name-length))
               (padding-left (make-string (/ total-padding 2) ?\s)) ;; Centralize the name
               (is-used (member name daily-used-names))
               (face (cond ((string= name daily-last-picked-name) '(:foreground "yellow" :height 1.5 :weight bold))
                           (is-used '(:strike-through t :height 1.5))
                           (t '(:foreground "light green" :height 1.5))))
               (name-with-face (propertize name 'face face)))
          ;; Insert padded line with name having specific face
          (insert padding-left name-with-face "\n")))
      (goto-char (point-min)))
    ;; Display the buffer
    (pop-to-buffer buffer)))


(defun daily-pick-random-name ()
  "Pick a random name from the list, mark it as used, and highlight it."
  (interactive)
  (unless daily-names-list
    (error "Names list is empty. Please load names first."))
  (let ((unused-names (cl-set-difference daily-names-list daily-used-names :test 'string=)))
    (if (not unused-names)
        (message "All names have been used.")
      (let ((picked-name (nth (random (length unused-names)) unused-names)))
        (push picked-name daily-used-names)
        (setq daily-last-picked-name picked-name)
        (daily-display-boxed-names-slackware-style)
        (message "Picked name: %s" picked-name)
        (play-sound-file "~/.doom.d/sounds/picked.wav")))))

(defun daily-load-names (names)
  "Load a list of NAMES into the names list."
  (interactive "sEnter names (comma-separated): ")
  (setq daily-names-list (split-string names "," t))
  (setq daily-used-names nil)
  (setq daily-last-picked-name nil)
  (message "Loaded names: %s" daily-names-list))

(defun daily-load-names-from-file (file-path)
  "Load names from a file specified by FILE-PATH, with one name per line."
  (interactive "fEnter the path of the file: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (setq daily-names-list (split-string (buffer-string) "\n" t "\\s-*"))
    (setq daily-used-names nil)
    (setq daily-last-picked-name nil)
    (message "Loaded names from file: %s" file-path)))

;; Global keybindings
(global-set-key (kbd "C-c L") 'daily-load-names)
(global-set-key (kbd "C-c F") 'daily-load-names-from-file)
(global-set-key (kbd "C-c R") 'daily-pick-random-name)

(after! popup
  (set-popup-rule! "^\\*Team Meeting - Name Draw\\*$" :size 0.5 :select t :quit t :ttl nil :fullscreen t))
