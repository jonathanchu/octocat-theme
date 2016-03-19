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
  '(("octocat-accent"   . "#526FFF")
    ("octocat-fg"       . "#383A42")
    ("octocat-bg"       . "#FAFAFA")
    ("octocat-bg-1"     . "#E5E5E6")
    ("octocat-bg-hl"    . "#CECECE")
    ("octocat-mono-1"   . "#383A42")
    ("octocat-mono-2"   . "#696C77")
    ("octocat-mono-3"   . "#A0A1A7")
    ("octocat-cyan"     . "#0184BC")
    ("octocat-blue"     . "#4078F2")
    ("octocat-purple"   . "#A626A4")
    ("octocat-green"    . "#50A14F")
    ("octocat-red-1"    . "#E45649")
    ("octocat-red-2"    . "#CA1243")
    ("octocat-orange-1" . "#986801")
    ("octocat-orange-2" . "#C18401")
    ("octocat-gray"     . "#EDEDED")
    ("octocat-silver"   . "#AAAAAA")
    ("octocat-black"    . "#0F1011"))
  "List of Octocat colors.")

(defmacro octocat-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    octocat-colors-alist))
     ,@body))

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; octocat-theme.el ends here
