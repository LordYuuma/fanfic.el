;;; fanfic.el ---
;;
;; Filename: fanfic.el
;; Description: "Useful" functionality to help typesetting fanfics
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Tue Sep 15 11:52:17 2015 (+0200)
;; Version: 1.5
;; Package-Requires: ((dash "2.12.1"))
;; Last-Updated: Mon Jan  4 12:15:33 2016 (+0100)
;;           By: Lord Yuuma
;;     Update #: 209
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
;;        `fanfic-keywords'
;;        `fanfic-protagonists'
;;        `fanfic-antagonists'
;;        `fanfic-cast'
;;
;;      Faces:
;;        `fanfic-keyword-face'
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
;;        `fanfic-mode-recast'
;;           re-evaluates the variables above to adjust highlights.
;;        `fanfic-strip-scenes'
;;           filter scenes depending on content.
;;        `fanfic-dramatis-personae'
;;           inserts a dramatis personae into the current buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  1.5:   Refactor name and nickname handling.
;;  1.4:   Add `fanfic-keywords'.
;;  1.3:   Implement `fanfic-dramatis-personae'.
;;  1.2:   Reimplement `fanfic-mode-recast', `fanfic--safe-list-p'
;;         and `fanfic--safe-alist-p' using `dash'.
;;         Remove `fanfic--decline'.
;;  1.1.1: Fix bug in which `fanfic-mode' would break existing
;;         highlights. Make it so that highlights are applied upon
;;         them.
;;  1.1:   Rename `fanfic-mode-activate-or-deactivate' to
;;         `fanfic-mode-recast' and make it a command.
;;         Use newly added `fanfic--font-lock' and
;;         `fanfic--font-unlock' to allow for changes to happen
;;         to cast-related variables.
;;         Remove `fanfic--add-or-remove-keywords'.
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
(require 'dash)

;;;###autoload
(defadvice font-lock-refresh-defaults (after fanfic-font-lock-defaults) (if fanfic-mode (fanfic--font-lock)))

(defun fanfic-mode-recast ()
  "Refreshes `font-lock-keywords' according to the `fanfic-' variables.

At the first step, highlights already set by `fanfic-mode' are reset.
Afterwards, when `fanfic-mode' is truthy, keywords are set to what they
should be according to `fanfic-cast', `fanfic-protagonists', `fanfic-antagonists',
possible `fanfic-declinations' thereof and `fanfic-keywords'.
As a last step, `font-lock-fontify-buffer' will be called to make these changes
visible.

This command is automatically run as a hook after `fanfic-mode'.
You may feel the need to run it yourself after editing cast-related variables."
  (interactive)
  (fanfic--font-unlock)
  (setq fanfic--highlights nil)

  (when fanfic-mode
    (cl-flet ((add-highlights (list face)
                              (add-to-list 'fanfic--highlights `((,(regexp-opt list 'words) 0 ,face t))))
              (decline (personae) (-flatten (--map (-map (lambda (fmt) (format fmt it)) fanfic-declinations) personae))))
      (--each '(fanfic-protagonists fanfic-antagonists fanfic-cast)
        (let ((personae (decline (--map (if (listp it) (car it) it) (symbol-value it))))
              (nicks (decline (--mapcat (if (listp it) (cdr it) nil) (symbol-value it))))
              (personae-face (nth 1 (assoc it '((fanfic-protagonists 'fanfic-protagonist-face)
                                                (fanfic-antagonists 'fanfic-antagonists-face)
                                                (fanfic-cast 'fanfic-cast-face)))))
              (nick-face (nth 1 (assoc it '((fanfic-protagonists 'fanfic-protagonist-nick-face)
                                            (fanfic-antagonists 'fanfic-antagonist-nick-face)
                                            (fanfic-cast 'fanfic-nick-face))))))
          (add-highlights personae personae-face)
          (add-highlights nicks nick-face)))

      (add-highlights (-flatten fanfic-keywords) ''fanfic-keyword-face)
      (fanfic--font-lock)))
  (font-lock-fontify-buffer))

;;;###autoload
(defun fanfic-strip-scenes (content &optional exclude)
  "Strip the fanfic to paragraphs including CONTENT. Outputs to a new buffer."
  (interactive (list (read-string (format "%s: " this-command)) current-prefix-arg))
  (save-excursion
    (goto-char (point-min))
    (let ((output (generate-new-buffer "*Stripped*")))
      (while (< (point) (point-max))
        (let ((scene (thing-at-point 'paragraph)))
          (when (eq (not (string-match-p (regexp-quote content) scene))
                    (not (not exclude)))
            (with-current-buffer output
              (insert scene))))
        (forward-paragraph))
      (display-buffer output))))

;;;###autoload
(defun fanfic-dramatis-personae (&optional prefix)
  "Inserts a dramatis personae at the current point.
If PREFIX is given, insert at the start of the file."
  (interactive "P")
  (save-excursion
    (if prefix
        (goto-char (point-min))
      ;; I have no idea, why `delete-selection-mode' does not do anything
      ;; without the following, but having it is better than nothing
      (when (and (boundp 'delete-selection-mode) delete-selection-mode
                 (region-active-p))
        (delete-selection-helper (get this-command 'delete-selection))))
    (insert (fanfic--dramatis-personae))))
(put 'fanfic-dramatis-personae 'delete-selection t)

;;;###autoload
(defgroup fanfic nil "Utilities for typesetting fanfiction."
  :prefix "fanfic-"
  :group 'convenience)

;;;###autoload
(defgroup fanfic-dramatis-personae nil "Dramatis Personae"
  :prefix "fanfic-dramatis-personae"
  :group 'fanfic)

(define-widget 'fanfic-cast-type 'lazy
  "A cast of persons with names (and possibly nicknames)."
  :tag ""
  :type '(repeat :tag "Cast"
                 (choice :tag "Person"
                         (string :tag "Name")
                         (list :tag "Name and Nicknames"
                               (string :tag "Name")
                               (repeat :tag "Nicknames" :inline t (string :tag "Nick"))))))

;;;###autoload
(define-minor-mode fanfic-mode
  "A minor mode for highlighting the name of a fanfic's cast.

`fanfic-mode' reacts to `fanfic-keywords', `fanfic-protagonists',
`fanfic-cast' and `fanfic-antagonists'. Of these, only `fanfic-keywords'
are highlighted as they are.

In the other three, strings or the cars of string lists are names,
whereas the cdrs of string lists are nicknames. Names and nicknames are
highlighted differently, but otherwise treated the same. Both of them are
not added as-is, but first formatted as in the format strings given
in `fanfic-declinations'.

`fanfic-mode' internally uses `font-lock' for highlighting. While it is active,
an advice, which is run after `font-lock-refresh-defaults' prevents their removal
to some degree. (This is mostly used as a hack for `markdown-mode'.)"
  nil " Fanfiction"
  :after-hook (fanfic-mode-recast)
  :group 'fanfic
  (if fanfic-mode
      (ad-activate-regexp "fanfic-font-lock-default")
    (ad-deactivate-regexp "fanfic-font-lock-default")))

;;;###autoload
(defcustom fanfic-keywords '(("MacGuffin" "Phlebotinum" "Plot Device")
                             ("orb" "orbs" "crystal" "crystals" "whatever"))
  "Important objects/places/whatever your plot needs."
  :type '(repeat (choice (string :tag "Keyword") (repeat :tag "Keywords" string)))
  :safe 'fanfic--safe-when-flattened
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-cast '("Carol" "Dave")
  "The cast of the fic. Not necessarily important people, but they still are a part."
  :type 'fanfic-cast-type
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-protagonists '("Alice" "Bob")
  "Names of characters, which are always considered to be very important."
  :type 'fanfic-cast-type
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-antagonists '("Eve")
  "Who you're up against. The villains in most cases."
  :type 'fanfic-cast-type
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-antagonist-nick-alist ()
  "DEPRECATED: Use `fanfic-antagonists' instead."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-protagonist-nick-alist ()
  "DEPRECATED: Use `fanfic-protagonists' instead."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-cast-nick-alist ()
  "DEPRECATED: Use `fanfic-cast' instead."
  :type '(alist :key-type string :value-type (repeat string))
  :safe 'fanfic--safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-declinations '("%s" "%s's")
  "Ways in which a name may appear in the language the fic is written in.

Each value is a string in which `%s' will get replaced by the name of your character
when constructing a list of highlights."
  :type '(repeat string)
  :safe 'fanfic--safe-declination-p)

;;;###autoload
(defface fanfic-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight keywords in."
  :group 'fanfic)

;;;###autoload
(defface fanfic-cast-face
  '((t (:inherit fanfic-keyword-face :foreground "deep sky blue")))
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

;;;###autoload
(defcustom fanfic-dramatis-personae-annotate-group nil
  "When activated, annotate each group with their group name, otherwise leave a blank line."
  :type 'boolean
  :safe 'booleanp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-group-prefix ""
  "String to be inserted before each group in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-group-suffix ""
  "String to be inserted after each group in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-header "~ Dramatis Personae ~\n"
  "Header to insert before the actual dramatis personae."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-item-prefix ""
  "String to be inserted before each name in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-item-suffix ""
  "String to be inserted after each name in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-nick-prefix "(\""
  "String to be inserted before each nick in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-nick-suffix "\")"
  "String to be inserted after each nick in `fanfic-dramatis-personae'."
  :type 'string
  :safe 'stringp
  :group 'fanfic-dramatis-personae)

;;; Private area.

(defvar fanfic--highlights nil "All `font-lock-keywords' for the current buffer which come from `fanfic-mode'.
DO NOT MODIFY THIS VARIABLE! It is needed to properly undo any changes made.")
(make-variable-buffer-local 'fanfic--highlights)

(defun fanfic--dramatis-personae ()
  (--reduce-from  (format "%s%s%s%s\n" acc fanfic-dramatis-personae-group-prefix
                          (--reduce-from (format "%s\n%s%s%s" acc fanfic-dramatis-personae-item-prefix
                                                 (if (stringp it) it (format "%s %s%s%s" (car it)
                                                                             fanfic-dramatis-personae-nick-prefix
                                                                             (car (cdr it))
                                                                             fanfic-dramatis-personae-nick-suffix))
                                                 fanfic-dramatis-personae-item-suffix)
                                         (if fanfic-dramatis-personae-annotate-group
                                             (nth 1 (assoc it
                                                           '((protags "Protagonists:")
                                                             (antags "Antagonists:")
                                                             (cast "Minor Characters:"))))
                                           "")
                                         (symbol-value it))
                          fanfic-dramatis-personae-group-suffix) fanfic-dramatis-personae-header '(protags antags cast)))

(defun fanfic--font-lock ()
  "Adds all highlights in `fanfic--highlights' to `font-lock-keywords'. Not very meaningful when used externally."
  (dolist (highlight fanfic--highlights)
    ;; any non-nil value would do as the fourth argument, but I figured 'append would make the most sense
    ;; from a semantic perspective.
    (font-lock-add-keywords nil highlight 'append)))

(defun fanfic--font-unlock ()
  "Removes all changes to `font-lock-keywords' done by `fanfic-mode'. Not intended for external use."
  (dolist (highlight fanfic--highlights)
    (font-lock-remove-keywords nil highlight)))

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
(defun fanfic--safe-cast-p (xs)
  "Used by `fanfic.el' to define safety parameters for customization options. NOT for external use.'"
  (and (listp xs) (--all-p (or (stringp it) (--all-p 'stringp it)) xs)))

;;;###autoload
(defun fanfic--safe-when-flattened (xs)
  "Used by `fanfic.el' to define safety parameters for customization options. NOT for external use.'"
  (and (listp xs) (-all-p 'stringp (-flatten xs))))

;;; Hack Area

;; `update-fileautoloads' does not take `:safe' in `defcustom' well
;; because users might want to set these variables WITHOUT
;; explicitly loading the whole file and without changing anything
;; in the customization, we force these to appear in the autoloads
;; by generating a cookie for them.

;;;###autoload
(put 'fanfic-cast 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-protagonists 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-antagonists 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-cast-nick-alist 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-protagonist-nick-alist 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-antagonist-nick-alist 'safe-local-variable 'fanfic--safe-cast-p)
;;;###autoload
(put 'fanfic-keywords 'safe-local-variable 'fanfic--safe-when-flattened)

;;;###autoload
(put 'fanfic-dramatis-personae-annotate-group 'safe-local-variable 'booleanp)
;;;###autoload
(put 'fanfic-dramatis-personae-item-prefix 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-item-suffix 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-header 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-group-prefix 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-group-suffix 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-nick-prefix 'safe-local-variable 'stringp)
;;;###autoload
(put 'fanfic-dramatis-personae-nick-suffix 'safe-local-variable 'stringp)

(provide 'fanfic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fanfic.el ends here
