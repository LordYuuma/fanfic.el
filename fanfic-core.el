;;; fanfic-core.el ---
;;
;; Filename: fanfic-core.el
;; Description: Core functionality of fanfic.el
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 09:49:03 2016 (+0200)
;; Version: 3.1
;; Package-Requires: ((dash "2.12.1") (cl-lib "0.5"))
;; Last-Updated: Sat Feb 11 15:22:36 2017 (+0100)
;;           By: Lord Yuuma
;;     Update #: 56
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    The following functions are defined in this module:
;;
;;    `fanfic-make-universe' and other low-level functions to
;;    interact with the `fanfic-universe' struct.
;;    `fanfic-decline'
;;    `fanfic-add-highlights' and `fanfic-remove-highlights'.
;;    `fanfic-safe-cast-p' and `fanfic-safe-keywords-p'.
;;    `fanfic-mode-recast'
;;    `fanfic-make-snippets'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.1:   Add snippet support.
;;         Import `fanfic-mode-recast'.
;;  3.0:   Split from other modules.
;;
;;  Previously:
;;    2.0: Removed alists.
;;         Added `fanfic-add-highlights', `fanfic-remove-highlights'
;;         and `fanfic-decline'.
;;         Changed `fanfic-declination' to use "{name}" instead
;;         of "%s"
;;    1.5: Refactor name and nickname handling.
;;    1.4: Add `fanfic-keywords'.
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


(require 'cl-lib)
(require 'dash)



(cl-defstruct (fanfic-object (:type list) :named
                             (:constructor nil)
                             (:constructor fanfic-make-object
                                           (base &key face transform)))
  face base transform)

(cl-defstruct (fanfic-cast-like
               (:include fanfic-object)
               (:constructor nil)
               (:constructor fanfic-make-cast
                             (base &key
                                   (face '(fanfic-cast-face
                                           fanfic-cast-nick-face))
                                   (transform 'fanfic-declinations)))
               (:constructor fanfic-make-protagonist
                             (base &key
                                   (transform 'fanfic-declinations)
                                   &aux
                                   (face '(fanfic-protagonist-face
                                           fanfic-protagonist-nick-face))))
               (:constructor fanfic-make-antagonist
                             (base &key
                                   (transform 'fanfic-declinations)
                                   &aux
                                   (face '(fanfic-antagonist-face
                                           fanfic-antagonist-nick-face))))))

(cl-defstruct (fanfic-metadata (:type list) :named
                               (:constructor fanfic-make-metadata (key value)))
  key value)

(cl-defstruct (fanfic-setting (:type list)
                              (:constructor fanfic-make-setting
                                            (objects &optional metadata)))
  objects metadata)



;;;###autoload
(defgroup fanfic nil "Utilities for typesetting fanfiction."
  :prefix "fanfic-"
  :group 'convenience)

;;;###autoload
(defgroup fanfic-universes nil
  "(Third Party) settings for fanfics set in specific universes."
  :group 'fanfic)

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
                               (repeat :tag "Nicknames"
                                       :inline t
                                       (string :tag "Nick"))))))


;;;###autoload
(defcustom fanfic-keywords '(("MacGuffin" "Phlebotinum" "Plot Device")
                             ("orb" "orbs" "crystal" "crystals" "whatever"))
  "Important objects/places/whatever your plot needs."
  :type '(repeat (choice (string :tag "Keyword")
                         (repeat :tag "Keywords" string)))
  :safe #'fanfic-safe-keywords-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-cast '("Carol" "Dave")
  "The cast of the fic."
  :type 'fanfic-cast-type
  :safe #'fanfic-safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-protagonists '("Alice" "Bob")
  "Names of characters, which are always considered to be very important."
  :type 'fanfic-cast-type
  :safe #'fanfic-safe-cast-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-antagonists '("Eve")
  "Who you're up against. The villains in most cases."
  :type 'fanfic-cast-type
  :safe #'fanfic-safe-cast-p
  :group 'fanfic)



;;;###autoload
(defcustom fanfic-declinations '("{base}" "{base}'s")
  "Ways in which a name may appear in the language the fic is written in.
Each value is a string in which `{base}' will get replaced by the name of
your character when constructing a list of highlights."
  :type '(repeat string)
  :safe (lambda (xs) (-all-p 'stringp xs))
  :group 'fanfic)



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



(defvar fanfic-mode nil "Whether fanfic-mode is activated")
(defvar fanfic--highlights nil
  "`font-lock-keywords' for the current buffer which come from `fanfic-mode'.")
(defvar fanfic--setting nil "The default setting for the current buffer.")
(make-variable-buffer-local 'fanfic--highlights)
(make-variable-buffer-local 'fanfic--setting)



(defun fanfic-add-highlights (names face &optional skip-font-lock)
  "Add NAMES highlighted under FACE to the list of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, do not run fontification afterwards."
  (unless fanfic-mode
    (error "Attempt to modify fanfic highlights outside of fanfic-mode"))
  (let ((highlight `((,(regexp-opt names 'words) 0 ',face t))))
    (add-to-list 'fanfic--highlights highlight t)
    (unless skip-font-lock
      (font-lock-add-keywords nil highlight 'append)
      (font-lock-fontify-buffer))))

(defun fanfic-remove-highlights (names face &optional skip-font-lock)
  "Remove NAMES highlighted under FACE from the list of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, do not run fontification afterwards."
  (unless fanfic-mode
    (error "Attempt to modify fanfic highlights outside of fanfic-mode"))
  (let ((highlight `((,(regexp-opt names 'words) 0 ',face t))))
    (unless (-contains-p fanfic--highlights highlight)
      (error "Attempt to remove non-present fanfic highlights"))
    (set 'fanfic--highlights (delq highlight fanfic--highlights))
    (unless skip-font-lock
      (font-lock-remove-keywords nil highlight)
      (font-lock-fontify-buffer))))



(defun fanfic--object-do-transform (obj-or-objs &optional transform)
  "Transform OBJ-OR-OBJS according to TRANSFORM.
If OBJ-OR-OBJS is a string, return a list with each occurence of {base} replaced by OBJ-OR-OBJS.
If OBJ-OR-OBJS is a list, apply the transformation recursively."
  (unless transform (setq transform '("{base}")))
  (if (stringp obj-or-objs)
      (--map (replace-regexp-in-string "{base}" obj-or-objs it t t)
             transform)
    (--map (fanfic--object-do-transform it transform) obj-or-objs)))


(defun fanfic-object-do-transform (object)
  (let ((transform (fanfic-object-transform object)))
    (fanfic--object-do-transform
     (fanfic-object-base object)
     (if (symbolp transform)
         (symbol-value transform)
       transform))))

(defun fanfic--object-primary-and-secondary (object)
  (let ((transformed (fanfic-object-do-transform object)))
    (cons
     (-flatten (--map (if (listp (car it)) (caar it) (car it)) transformed))
     (-flatten (--mapcat (when (listp (car it)) (cadr it)) transformed)))))

(defun fanfic-setting-highlight (setting)
  (--each (fanfic-setting-objects setting)
    (let* ((face (fanfic-object-face it))
           (primary-face (if (listp face) (car face) face))
           (secondary-face (if (listp face) (cdr face) face))
           (primary-and-secondary (fanfic--object-primary-and-secondary it))
           (primary (car primary-and-secondary))
           (secondary (cdr primary-and-secondary)))
      (fanfic-add-highlights secondary secondary-face t)
      (fanfic-add-highlights primary primary-face t)))
  (fanfic--font-lock)
  (font-lock-fontify-buffer))

(defun fanfic-merge-settings (&rest settings)
  (fanfic-make-setting
   (apply #'append (-map #'fanfic-setting-objects settings))
   (apply #'append (-map #'fanfic-setting-metadata settings))))



;;;###autoload
(defun fanfic-safe-cast-p (cast)
  "Return t if CAST is a cast safe for usage within fanfic functions.

A cast is considered safe, if its elements are either strings or list of strings.
In the case of a single string element, this is interpreted as a cast member with only one name.
In the case of a list of strings, this is interpreted as a cast member with one full name (or long name)
and a list of nicknames in the format
  (FULL-NAME NICK1 NICK2 ... NICKN)"
  (and (listp cast) (--all-p (or (stringp it) (-all-p #'stringp it)) cast)))

;;;###autoload
(defun fanfic-safe-keywords-p (keywords)
  "Return t if KEYWORDS is safe to be used as keywords within fanfic."
  (and (listp keywords) (-all-p #'stringp (-flatten keywords))))



(defun fanfic-make-snippets (cast)
  "Make snippet definitions for CAST.

CAST is a list that satisfies `fanfic-safe-cast-p'.
For each cast member for each of his nicknames (assuming he has nicknames),
a snippet is defined, so that the nickname is expanded to the member's full name.

The returned list of snippets can be used with `yas-define-snippets'.
`yas-define-snippets' is not called directly.
Due to the yasnippet internals it may become necessary to reverse the list before
passing it onto yasnippet, if one insists on a given order."
  (-non-nil
   (--mapcat
    (when (listp it)
      (-map (lambda (abbrev)
              (list abbrev ; KEY
                    (car it) ; TEMPLATE
                    (car it) ; NAME
                    'fanfic-mode ; CONDITION
                    (list "Fanfiction") ; GROUP
                    nil ; EXPAND-ENV
                    nil ; LOAD-FILE
                    nil ; KEYBINDING
                    ;; UUID
                    (concat
                     "[Fanfic-Core] "
                     abbrev
                     " => "
                     (car it))))
            (cdr it)))
    cast)))



(defun fanfic-default-face-p (face)
  "Return t if FACE is shipped with `fanfic-core'."
  (-contains-p
   '(fanfic-protagonist-face
     fanfic-protagonist-nick-face
     fanfic-antagonist-face
     fanfic-antagonist-nick-face
     fanfic-cast-face
     fanfic-nick-face
     fanfic-keyword-face)
   face))



;;; Private area.

(defun fanfic--font-lock ()
  "Add all highlights in `fanfic--highlights' to `font-lock-keywords'.
Not very meaningful when used externally."
  (dolist (highlight fanfic--highlights)
    ;; any non-nil value would do as the fourth argument
    ;; but I really want the keywords to be appended
    (font-lock-add-keywords nil highlight 'append)))

(defun fanfic--font-unlock ()
  "Remove all changes to `font-lock-keywords' done by `fanfic-mode'.
Not intended for external use."
  (dolist (highlight fanfic--highlights)
    (font-lock-remove-keywords nil highlight)))



(provide 'fanfic-core)
