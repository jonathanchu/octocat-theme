;;; octocat-theme.el --- Octocat color theme
;;
;; Copyright 2016 Jonathan Chu
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/octocat-theme
;; Version: 0.1.0
;;
;;; Commentary:
;;
;; An Octocat theme for Emacs.
;;
;;; Code:

(deftheme octocat
  "An Octocat theme for Emacs.")

;;; Color Palette
(defvar octocat-colors-alist
  '(("octocat-accent"   . "#000000")
    ("octocat-fg"       . "#333333")
    ("octocat-bg"       . "#FFFFFF")
    ("octocat-bg-1"     . "#E5E5E6")  ;; TODO
    ("octocat-bg-hl"    . "#F5F5F5")
    ("octocat-mono-1"   . "#383A42")  ;; TODO
    ("octocat-mono-2"   . "#696C77")  ;; TODO
    ("octocat-mono-3"   . "#A0A1A7")  ;; TODO
    ("octocat-cyan"     . "#0184BC")  ;; TODO
    ("octocat-blue"     . "#B0D6FF")
    ("octocat-blue-2"   . "#6699CC")
    ("octocat-purple"   . "#A626A4")  ;; TODO
    ("octocat-green"    . "#50A14F")  ;; TODO
    ("octocat-red-1"    . "#E45649")  ;; TODO
    ("octocat-red-2"    . "#CA1243")  ;; TODO
    ("octocat-orange-1" . "#986801")  ;; TODO
    ("octocat-orange-2" . "#C18401")  ;; TODO
    ("octocat-gray"     . "#969896")
    ("octocat-silver"   . "#AAAAAA")  ;; TODO
    ("octocat-black"    . "#0F1011")) ;; TODO
  "List of Octocat colors.")

(defmacro octocat-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    octocat-colors-alist))
     ,@body))

(octocat-with-color-variables
 (custom-theme-set-faces
  'octocat

  `(default ((t (:foreground ,octocat-fg :background ,octocat-bg))))
  `(success ((t (:foreground ,octocat-green))))
  `(warning ((t (:foreground ,octocat-orange-2))))
  `(error ((t (:foreground ,octocat-red-1 :weight bold))))
  `(link ((t (:foreground ,octocat-blue :underline t :weight bold))))
  `(link-visited ((t (:foreground ,octocat-blue :underline t :weight normal))))
  `(cursor ((t (:background ,octocat-accent))))
  `(fringe ((t (:background ,octocat-bg))))
  `(region ((t (:background ,octocat-blue))))
  `(highlight ((t (:background ,octocat-gray))))
  `(hl-line ((t (:background ,octocat-bg-hl))))
  `(vertical-border ((t (:foreground ,octocat-mono-3))))
  `(secondary-selection ((t (:background ,octocat-bg-hl))))
  `(query-replace ((t (:inherit (isearch)))))
  `(minibuffer-prompt ((t (:foreground ,octocat-silver))))

  `(font-lock-builtin-face ((t (:foreground ,octocat-cyan))))
  `(font-lock-comment-face ((t (:foreground ,octocat-gray))))
  `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
  `(font-lock-function-name-face ((t (:foreground ,octocat-blue))))
  `(font-lock-keyword-face ((t (:foreground ,octocat-purple))))
  `(font-lock-preprocessor-face ((t (:foreground ,octocat-mono-2))))
  `(font-lock-string-face ((t (:foreground ,octocat-green))))
  `(font-lock-type-face ((t (:foreground ,octocat-orange-2))))
  `(font-lock-constant-face ((t (:foreground ,octocat-orange-1))))
  `(font-lock-variable-name-face ((t (:foreground ,octocat-red-1))))
  `(font-lock-warning-face ((t (:foreground ,octocat-mono-3 :bold t))))

  ;; mode-line
  `(mode-line ((t (:background ,octocat-black :foreground ,octocat-silver))))
  `(mode-line-buffer-id ((t (:weight bold))))
  `(mode-line-emphasis ((t (:weight bold))))
  `(mode-line-inactive ((t (:background ,octocat-gray))))

  ;; ido
  `(ido-first-match ((t (:foreground ,octocat-purple :weight bold))))
  `(ido-only-match ((t (:foreground ,octocat-red-1 :weight bold))))
  `(ido-subdir ((t (:foreground ,octocat-blue))))
  `(ido-virtual ((t (:foreground ,octocat-mono-3))))

  ;; ace-jump
  `(ace-jump-face-background ((t (:foreground ,octocat-mono-3 :background ,octocat-bg-1 :inverse-video nil))))
  `(ace-jump-face-foreground ((t (:foreground ,octocat-red-1 :background ,octocat-bg-1 :inverse-video nil))))

  ;; company-mode
  `(company-tooltip ((t (:foreground ,octocat-fg :background ,octocat-bg-1))))
  `(company-tooltip-annotation ((t (:foreground ,octocat-mono-2 :background ,octocat-bg-1))))
  `(company-tooltip-selection ((t (:foreground ,octocat-fg :background ,octocat-gray))))
  `(company-tooltip-mouse ((t (:background ,octocat-gray))))
  `(company-tooltip-common ((t (:foreground ,octocat-orange-2 :background ,octocat-bg-1))))
  `(company-tooltip-common-selection ((t (:foreground ,octocat-orange-2 :background ,octocat-gray))))
  `(company-preview ((t (:background ,octocat-bg))))
  `(company-preview-common ((t (:foreground ,octocat-orange-2 :background ,octocat-bg))))
  `(company-scrollbar-fg ((t (:background ,octocat-mono-1))))
  `(company-scrollbar-bg ((t (:background ,octocat-bg-1))))

  ;; compilation
  `(compilation-face ((t (:foreground ,octocat-fg))))
  `(compilation-line-number ((t (:foreground ,octocat-mono-2))))
  `(compilation-column-number ((t (:foreground ,octocat-mono-2))))

  ;; isearch
  `(isearch ((t (:foreground ,octocat-bg :background ,octocat-purple))))
  `(isearch-fail ((t (:foreground ,octocat-red-2 :background nil))))
  `(lazy-highlight ((t (:foreground ,octocat-purple :background ,octocat-bg-1 :underline ,octocat-purple))))

  ;; diff-hl (https://github.com/dgutov/diff-hl)
  '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
  '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
  '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

  ;; dired-mode
  '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
  '(dired-flagged ((t (:inherit (diff-hl-delete)))))
  '(dired-symlink ((t (:foreground "#FD5FF1"))))

  ;; helm
  `(helm-header ((t (:foreground ,octocat-mono-2
                                 :background ,octocat-bg
                                 :underline nil
                                 :box (:line-width 6 :color ,octocat-bg)))))
  `(helm-source-header ((t (:foreground ,octocat-orange-2
                                        :background ,octocat-bg
                                        :underline nil
                                        :weight bold
                                        :box (:line-width 6 :color ,octocat-bg)))))
  `(helm-selection ((t (:background ,octocat-gray))))
  `(helm-selection-line ((t (:background ,octocat-gray))))
  `(helm-visible-mark ((t (:foreground ,octocat-bg :foreground ,octocat-orange-2))))
  `(helm-candidate-number ((t (:foreground ,octocat-green :background ,octocat-bg-1))))
  `(helm-separator ((t (:background ,octocat-bg :foreground ,octocat-red-1))))
  `(helm-M-x-key ((t (:foreground ,octocat-orange-1))))
  `(helm-bookmark-addressbook ((t (:foreground ,octocat-orange-1))))
  `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
  `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
  `(helm-bookmark-gnus ((t (:foreground ,octocat-purple))))
  `(helm-bookmark-info ((t (:foreground ,octocat-green))))
  `(helm-bookmark-man ((t (:foreground ,octocat-orange-2))))
  `(helm-bookmark-w3m ((t (:foreground ,octocat-purple))))
  `(helm-match ((t (:foreground ,octocat-orange-2))))
  `(helm-ff-directory ((t (:foreground ,octocat-cyan :background ,octocat-bg :weight bold))))
  `(helm-ff-file ((t (:foreground ,octocat-fg :background ,octocat-bg :weight normal))))
  `(helm-ff-executable ((t (:foreground ,octocat-green :background ,octocat-bg :weight normal))))
  `(helm-ff-invalid-symlink ((t (:foreground ,octocat-red-1 :background ,octocat-bg :weight bold))))
  `(helm-ff-symlink ((t (:foreground ,octocat-orange-2 :background ,octocat-bg :weight bold))))
  `(helm-ff-prefix ((t (:foreground ,octocat-bg :background ,octocat-orange-2 :weight normal))))
  `(helm-buffer-not-saved ((t (:foreground ,octocat-red-1))))
  `(helm-buffer-process ((t (:foreground ,octocat-mono-2))))
  `(helm-buffer-saved-out ((t (:foreground ,octocat-fg))))
  `(helm-buffer-size ((t (:foreground ,octocat-mono-2))))
  `(helm-buffer-directory ((t (:foreground ,octocat-purple))))
  `(helm-grep-cmd-line ((t (:foreground ,octocat-cyan))))
  `(helm-grep-file ((t (:foreground ,octocat-fg))))
  `(helm-grep-finish ((t (:foreground ,octocat-green))))
  `(helm-grep-lineno ((t (:foreground ,octocat-mono-2))))
  `(helm-grep-finish ((t (:foreground ,octocat-red-1))))
  `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))

  ;; git-commit
  `(git-commit-comment-action  ((t (:foreground ,octocat-green :weight bold))))
  `(git-commit-comment-branch  ((t (:foreground ,octocat-blue :weight bold))))
  `(git-commit-comment-heading ((t (:foreground ,octocat-orange-2 :weight bold))))

  ;; magit
  `(magit-section-highlight ((t (:background ,octocat-bg-hl))))
  `(magit-section-heading ((t (:foreground ,octocat-orange-2 :weight bold))))
  `(magit-section-heading-selection ((t (:foreground ,octocat-fg :weight bold))))
  `(magit-diff-file-heading ((t (:weight bold))))
  `(magit-diff-file-heading-highlight ((t (:background ,octocat-gray :weight bold))))
  `(magit-diff-file-heading-selection ((t (:foreground ,octocat-orange-2 :background ,octocat-bg-hl :weight bold))))
  `(magit-diff-hunk-heading ((t (:foreground ,octocat-mono-2 :background ,octocat-gray))))
  `(magit-diff-hunk-heading-highlight ((t (:foreground ,octocat-mono-1 :background ,octocat-mono-3))))
  `(magit-diff-hunk-heading-selection ((t (:foreground ,octocat-purple :background ,octocat-mono-3))))
  `(magit-diff-context ((t (:foreground ,octocat-fg))))
  `(magit-diff-context-highlight ((t (:background ,octocat-bg-1 :foreground ,octocat-fg))))
  `(magit-diffstat-added ((t (:foreground ,octocat-green))))
  `(magit-diffstat-removed ((t (:foreground ,octocat-red-1))))
  `(magit-process-ok ((t (:foreground ,octocat-green))))
  `(magit-process-ng ((t (:foreground ,octocat-red-1))))
  `(magit-log-author ((t (:foreground ,octocat-orange-2))))
  `(magit-log-date ((t (:foreground ,octocat-mono-2))))
  `(magit-log-graph ((t (:foreground ,octocat-silver))))
  `(magit-sequence-pick ((t (:foreground ,octocat-orange-2))))
  `(magit-sequence-stop ((t (:foreground ,octocat-green))))
  `(magit-sequence-part ((t (:foreground ,octocat-orange-1))))
  `(magit-sequence-head ((t (:foreground ,octocat-blue))))
  `(magit-sequence-drop ((t (:foreground ,octocat-red-1))))
  `(magit-sequence-done ((t (:foreground ,octocat-mono-2))))
  `(magit-sequence-onto ((t (:foreground ,octocat-mono-2))))
  `(magit-bisect-good ((t (:foreground ,octocat-green))))
  `(magit-bisect-skip ((t (:foreground ,octocat-orange-1))))
  `(magit-bisect-bad ((t (:foreground ,octocat-red-1))))
  `(magit-blame-heading ((t (:background ,octocat-bg-1 :foreground ,octocat-mono-2))))
  `(magit-blame-hash ((t (:background ,octocat-bg-1 :foreground ,octocat-purple))))
  `(magit-blame-name ((t (:background ,octocat-bg-1 :foreground ,octocat-orange-2))))
  `(magit-blame-date ((t (:background ,octocat-bg-1 :foreground ,octocat-mono-3))))
  `(magit-blame-summary ((t (:background ,octocat-bg-1 :foreground ,octocat-mono-2))))
  `(magit-dimmed ((t (:foreground ,octocat-mono-2))))
  `(magit-hash ((t (:foreground ,octocat-purple))))
  `(magit-tag  ((t (:foreground ,octocat-orange-1 :weight bold))))
  `(magit-branch-remote  ((t (:foreground ,octocat-green :weight bold))))
  `(magit-branch-local   ((t (:foreground ,octocat-blue :weight bold))))
  `(magit-branch-current ((t (:foreground ,octocat-blue :weight bold :box t))))
  `(magit-head           ((t (:foreground ,octocat-blue :weight bold))))
  `(magit-refname        ((t (:background ,octocat-bg :foreground ,octocat-fg :weight bold))))
  `(magit-refname-stash  ((t (:background ,octocat-bg :foreground ,octocat-fg :weight bold))))
  `(magit-refname-wip    ((t (:background ,octocat-bg :foreground ,octocat-fg :weight bold))))
  `(magit-signature-good      ((t (:foreground ,octocat-green))))
  `(magit-signature-bad       ((t (:foreground ,octocat-red-1))))
  `(magit-signature-untrusted ((t (:foreground ,octocat-orange-1))))
  `(magit-cherry-unmatched    ((t (:foreground ,octocat-cyan))))
  `(magit-cherry-equivalent   ((t (:foreground ,octocat-purple))))
  `(magit-reflog-commit       ((t (:foreground ,octocat-green))))
  `(magit-reflog-amend        ((t (:foreground ,octocat-purple))))
  `(magit-reflog-merge        ((t (:foreground ,octocat-green))))
  `(magit-reflog-checkout     ((t (:foreground ,octocat-blue))))
  `(magit-reflog-reset        ((t (:foreground ,octocat-red-1))))
  `(magit-reflog-rebase       ((t (:foreground ,octocat-purple))))
  `(magit-reflog-cherry-pick  ((t (:foreground ,octocat-green))))
  `(magit-reflog-remote       ((t (:foreground ,octocat-cyan))))
  `(magit-reflog-other        ((t (:foreground ,octocat-cyan))))

  ;; rainbow-delimiters
  `(rainbow-delimiters-depth-1-face ((t (:foreground ,octocat-fg))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,octocat-purple))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,octocat-blue))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,octocat-cyan))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,octocat-green))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,octocat-orange-1))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,octocat-orange-2))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,octocat-red-1))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground ,octocat-red-2))))
  `(rainbow-delimiters-depth-10-face ((t (:foreground ,octocat-mono-1))))
  `(rainbow-delimiters-depth-11-face ((t (:foreground ,octocat-mono-2))))
  `(rainbow-delimiters-depth-12-face ((t (:foreground ,octocat-mono-3))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,octocat-black))))

  ;; rbenv
  `(rbenv-active-ruby-face ((t (:foreground ,octocat-green))))

  ;; smartparens
  `(sp-show-pair-mismatch-face ((t (:foreground ,octocat-red-1 :background ,octocat-gray :weight bold))))
  `(sp-show-pair-match-face ((t (:background ,octocat-gray :weight bold))))

  ;; web-mode
  `(web-mode-symbol-face ((t (:foreground ,octocat-orange-1))))

  ;; flx-ido
  '(flx-highlight-face ((t (:inherit (link) :weight bold))))
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'octocat)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; octocat-theme.el ends here
