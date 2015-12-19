;;; fanfic.el ---
;;
;; Filename: fanfic.el
;; Description: "Useful" functionality to help typesetting fanfics
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Tue Sep 15 11:52:17 2015 (+0200)
;; Version: 1.0
;; Package-Requires: ()
;; Last-Updated: Sat Dec 19 14:56:20 2015 (+0100)
;;           By: Lord Yuuma
;;     Update #: 154
;; URL:
;; Doc URL:
;; Keywords: convenience
;; Compatibility: tested with Emacs 24.3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Setup:
;;      Method 1: Adding to load-path and loading
;;        (add-to-list 'load-path "/path/to/fanfic.el")
;;        (require 'fanfic)
;;
;;      Method 2: Autoload
;;        (let ((source "path/to/fanfic.el")
;;              (autoload-file "path/to/fanfic-autoloads.el"))
;;          (update-file-autoloads source t autoload-file)
;;          (load autoload-file))
;;
;;      Method ???: Whatever else you do to load a file.
;;
;;    Usage:
;;      Variables:
;;        `fanfic-antagonists'
;;        `fanfic-antagonist-nick-alist'
;;        `fanfic-cast'
;;        `fanfic-cast-nick-alist'
;;        `fanfic-protagonists'
;;        `fanfic-protagonist-nick-alist'
;;
;;      Faces:
;;        `fanfic-antagonist-face'
;;        `fanfic-antagonist-nick-face'
;;        `fanfic-cast-face'
;;        `fanfic-nick-face'
;;        `fanfic-protagonist-face'
;;        `fanfic-protagonist-nick-face'
;;
;;      Set these to your liking before activating `fanfic-mode'.
;;
;;      Functions & Commands:
;;        `fanfic-mode' (minor-mode)
;;           adds highlights according to the variables and faces
;;           above.
;;        `fanfic-strip-scenes'
;;           filter scenes depending on content.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  1.0:   Generate new buffer in `fanfic-strip-scenes'.
;;         Document EVERYTHING.
;;  0.8.1: Force cookies for safe local variables.
;;  0.8:   Merge global and local cast etc.
;;         Add `fanfic-antagonists' and `fanfic-antagonist-alist'
;;         "Document" customization and `fanfic-mode'.
;;         Use `fanfic--decline' to decline names instead of
;;         hard coding.
;;  0.7:   Add advice for `font-lock-refresh-defaults', so that
;;         highlights are not reset.
;;  0.6:   Support for declinations.
;;  0.5.2: Naming conventions.
;;  0.5.1: Make non-global protagonist and cast variables safe
;;         as file local variables.
;;  0.5:   Support for non-global protagonist and cast.
;;  0.4:   `fanfic-strip-scenes'
;;  0.3.1: Add alist for protagonist-nicknames.
;;  0.3:   Add alists for nicknames
;;         Only one hook running now. Moved if-then-else to internal
;;         method calls
;;  0.2.2: Add cast face
;;         Fix customize
;;  0.2.1: Fontify buffer after modification of font-lock-keywords
;;  0.2:   Add options for highlighting globally defined protagonists
;;         Add faces for cast/protagonists
;;  0.1.3: Make fanfic-mode minor
;;  0.1.2: Fancier name in modeline
;;  0.1.1: Make fanfic-mode autoloaded
;;  0.1:   Derive fanfic-mode from text-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defadvice font-lock-refresh-defaults (after fanfic-font-lock-defaults) (if fanfic-mode (fanfic-mode-activate-or-deactivate)))

(defun fanfic-mode-activate-or-deactivate ()
  "Hook run on the change of `fanfic-mode'. Sets `font-lock-keywords' and refontifies the buffer."
  (let ((cast fanfic-cast)
        (protagonists fanfic-protagonists)
        (antagonists fanfic-antagonists)
        (nicks nil)
        (protag-nicks nil)
        (antag-nicks nil))

    ;; add from cast-nick-alist
    (dolist (association fanfic-cast-nick-alist)
      (add-to-list 'cast (car association))
      (setq nicks (append nicks (cdr association))))

    ;; add from protagonist-nick-alist
    (dolist (association fanfic-protagonist-nick-alist)
      (add-to-list 'protagonists (car association))
      (setq protag-nicks (append protag-nicks (cdr association))))

    (dolist (association fanfic-antagonist-nick-alist)
      (add-to-list 'antagonists (car association))
      (setq antag-nicks (append antag-nicks (cdr association))))

    ;; apply declination formats
    (setq cast (fanfic--decline cast))
    (setq protagonists (fanfic--decline protagonists))
    (setq antagonists (fanfic--decline antagonists))
    (setq nicks (fanfic--decline nicks))
    (setq protag-nicks (fanfic--decline protag-nicks))
    (setq antag-nicks (fanfic--decline antag-nicks))

    ;; nicks have lowest priority in font lock
    ;; so they need to be added first to make their highlights
    ;; overwritten by the full name
    (let ((pattern (regexp-opt nicks 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-nick-face nil))))
    (let ((pattern (regexp-opt protag-nicks 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-protagonist-nick-face nil))))
    (let ((pattern (regexp-opt antag-nicks 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-antagonist-nick-face nil))))

    ;; cast is not that important so he's added in the middle
    ;; in most cases the order of cast and protagonist would not matter
    ;; but better safe than sorry
    (let ((pattern (regexp-opt cast 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-cast-face nil))))

    (let ((pattern (regexp-opt antagonists 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-antagonist-face nil))))

    ;; protagonists have the highest priority so they enter as the
    ;; last ones dramatically and don't leave until the rest does as well
    (let ((pattern (regexp-opt protagonists 'words)))
      (fanfic--add-or-remove-keywords `((,pattern 0 'fanfic-protagonist-face nil))))

    ;; run fontify so that changes are immediately visible
    (font-lock-fontify-buffer)))

;;;###autoload
(defun fanfic-strip-scenes (content)
  "Strip the fanfic to paragraphs including CONTENT. Outputs to a new buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((output (generate-new-buffer "*Stripped*")))
      (while (< (point) (point-max))
        (let ((scene (thing-at-point 'paragraph)))
          (when (string-match-p (regexp-quote content) scene)
            (with-current-buffer output
              (insert scene))))
        (forward-paragraph))
      (display-buffer output))))

;;;###autoload
(defgroup fanfic nil "Utilities for typesetting fanfiction."
  :prefix "fanfic-"
  :group 'convenience)

;;;###autoload
(define-minor-mode fanfic-mode
  "A minor mode for highlighting the name of a fanfic's cast.

`fanfic-mode' reacts to three groups of variables, which can be set through
customization or as file-local variables.
These are `fanfic-protagonists' `fanfic-antagonists' `fanfic-cast' and their
respective alists `fanfic-protagonist-nick-alist', `fanfic-antagonist-nick-alist'
and `fanfic-cast-nick-alist'.
An entry in one of these lists defines a name as belonging to a certain group,
which can be a protagonist, antagonist or just a member of the cast. A key entry
in one of the alists does the same. A value entry corresponds to a nickname
belonging to the same character.
Names and nicknames are highlighted differently to emphasize how these characters
are being referred to. Protagonists are highlighted differently from antagonists
or just the normal cast for obvious reasons.

`fanfic-mode' internally uses `font-lock' for highlighting. While it is active,
an advice, which is run after `font-lock-refresh-defaults' prevents their removal
to some degree. (This is mostly used as a hack for `markdown-mode'.)"
  nil " Fanfiction"
  :after-hook (fanfic-mode-activate-or-deactivate)
  :group 'fanfic
  (if fanfic-mode
      (ad-activate-regexp "fanfic-font-lock-default")
    (ad-deactivate-regexp "fanfic-font-lock-default")))

;;;###autoload
(defcustom fanfic-cast '("Carol" "Dave")
  "The cast of the fic. Not necessarily important people, but they still are a part."
  :type '(repeat string)
  :safe 'fanfic--safe-list-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-protagonists '("Alice" "Bob")
  "Names of characters, which are always considered to be very important."
  :type '(repeat string)
  :safe 'fanfic--safe-list-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-antagonists '("Eve")
  "Who you're up against. The villains in most cases."
  :type '(repeat string)
  :safe 'fanfic--safe-list-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-antagonist-nick-alist ()
  "Like `fanfic-antagonists', but maps names to nicknames."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-alist-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-protagonist-nick-alist ()
  "Like `fanfic-protagonists', but maps names to nicknames."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-alist-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-cast-nick-alist ()
  "Like `fanfic-cast', but maps names to nicknames."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-alist-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-declinations '("%s" "%s's")
  "Ways in which a name may appear in the language the fic is written in.

Each value is a string in which `%s' will get replaced by the name of your character
when constructing a list of highlights."
  :type '(repeat string)
  :safe 'fanfic--safe-declination-p)

;;;###autoload
(defface fanfic-cast-face
  '((t (:inherit font-lock-keyword-face :foreground "deep sky blue")))
  "Face to highlight the names of the cast in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-protagonist-face
  '((t (:inherit fanfic-cast-face :foreground "lime green")))
  "Face to highlight the names of the protagonists in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-antagonist-face
  '((t (:inherit fanfic-cast-face :foreground "red")))
  "Face to highlight the names of the antagonists in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-nick-face
  '((t (:inherit fanfic-cast-face :foreground "powder blue")))
  "Face to highlight nicknames in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-protagonist-nick-face
  '((t (:inherit fanfic-nick-face :foreground "light green")))
  "Face to highlight nicknames of protagonists in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-antagonist-nick-face
  '((t (:inherit fanfic-nick-face :foreground "dark orange")))
  "Face to highlight nicknames of antagonists in."
  :group 'fanfic)

;;; Private area.

(defun fanfic--add-or-remove-keywords (kwds)
  "Used internally by `fanfic-mode' to ease highlighting. NOT for external use."
  (if fanfic-mode
      (font-lock-add-keywords nil kwds)
    (font-lock-remove-keywords nil kwds)))

(defun fanfic--decline (xs)
  "Used internally by `fanfic-mode' to produce correct format strings. NOT for external use."
  (let ((ds))
    (dolist (decl fanfic-declinations ds)
      (dolist (x xs)
        (add-to-list 'ds (format decl x))))))

;;;###autoload
(defun fanfic--safe-declination-p (str)
  "Used by `fanfic.el' to set safety parameters for `fanfic-declarations'. NOT for external use."
  (let ((a 0)
        (b 0)
        (last 0))
    (while (string-match "%" str last)
      (setq last (match-end 0))
      (setq a (+ a 1)))
    (setq last 0)
    (while (string-match "%s" str last)
      (setq last (match-end 0))
      (setq b (+ b 1)))
    (and (< a 2) (eq a b))))

;;;###autoload
(defun fanfic--safe-alist-p (xs)
  "Used by `fanfic.el' to define safety parameters for customization options. NOT for external use.'"
  (let ((v t))
    (and (listp xs) (dolist (x xs v) (setq v (and v (fanfic--safe-list-p x)))))))

;;;###autoload
(defun fanfic--safe-list-p (xs)
  "Used by `fanfic.el' to define safety parameters for customization options. NOT for external use.'"
  (let ((v t))
    (and (listp xs) (dolist (x xs v) (setq v (and v (stringp x)))))))


;;; Hack Area

;; `update-fileautoloads' does not take `:safe' in `defcustom' well
;; because users might want to set these variables WITHOUT
;; explicitly loading the whole file and without changing anything
;; in the customization, we force these to appear in the autoloads
;; by generating a cookie for them.

;;;###autoload
(put 'fanfic-cast 'safe-local-variable 'fanfic--safe-list-p)
;;;###autoload
(put 'fanfic-protagonists 'safe-local-variable 'fanfic--safe-list-p)
;;;###autoload
(put 'fanfic-antagonists 'safe-local-variable 'fanfic--safe-list-p)
;;;###autoload
(put 'fanfic-cast-nick-alist 'safe-local-variable 'fanfic--safe-alist-p)
;;;###autoload
(put 'fanfic-protagonist-nick-alist 'safe-local-variable 'fanfic--safe-alist-p)
;;;###autoload
(put 'fanfic-antagonist-nick-alist 'safe-local-variable 'fanfic--safe-alist-p)


(provide 'fanfic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fanfic.el ends here
