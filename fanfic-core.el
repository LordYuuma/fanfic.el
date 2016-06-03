;;; fanfic-core.el ---
;;
;; Filename: fanfic-core.el
;; Description:
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 09:49:03 2016 (+0200)
;; Version:
;; Package-Requires: ((dash "2.12.1") (cl-lib "0.5"))
;; Last-Updated: Fri Jun  3 15:46:23 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 11
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.0: Split from other modules.
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

(cl-defstruct (fanfic-universe (:constructor fanfic-make-universe)
                               (:copier fanfic-copy-universe))
  name cast keywords)



;;;###autoload
(defgroup fanfic nil "Utilities for typesetting fanfiction."
  :prefix "fanfic-"
  :group 'convenience)

;;;###autoload
(defgroup fanfic-universes nil "(Third Party) settings for fanfics set in specific universes."
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
                               (repeat :tag "Nicknames" :inline t (string :tag "Nick"))))))


;;;###autoload
(defcustom fanfic-keywords '(("MacGuffin" "Phlebotinum" "Plot Device")
                             ("orb" "orbs" "crystal" "crystals" "whatever"))
  "Important objects/places/whatever your plot needs."
  :type '(repeat (choice (string :tag "Keyword") (repeat :tag "Keywords" string)))
  :safe #'fanfic-safe-keywords-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-cast '("Carol" "Dave")
  "The cast of the fic. Not necessarily important people, but they still are a part."
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
(defcustom fanfic-declinations '("{name}" "{name}'s")
  "Ways in which a name may appear in the language the fic is written in.

Each value is a string in which `{name}' will get replaced by the name of your character
when constructing a list of highlights."
  :type '(repeat string)
  :safe (lambda (xs) (-all-p 'stringp xs)))



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
(defvar fanfic--highlights nil "All `font-lock-keywords' for the current buffer which come from `fanfic-mode'.")
(make-variable-buffer-local 'fanfic--highlights)



(defun fanfic-decline (name-or-names)
  "Decline NAME-OR-NAMES according to `fanfic-declinations'.
If NAME-OR-NAMES is a string, a list is returned, in which each element is the corresponding element of
`fanfic-declination' with {name} replaced by NAME-OR-NAMES.
If NAME-OR-NAMES is a list, `fanfic-decline' is called recursively for each element in that list."
  (if (stringp name-or-names)
      (--map (replace-regexp-in-string "{name}" name-or-names it t t) fanfic-declinations)
    (-map #'fanfic-decline name-or-names)))



(defun fanfic-add-highlights (names face &optional skip-font-lock)
  "Adds NAMES highlighted under FACE to the list of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, keywords generated this way are
not yet added to font-lock and fontification is not run afterwards."
  (unless fanfic-mode
    (error "Attempt to modify fanfic highlights outside of fanfic-mode"))
  (let ((highlight `((,(regexp-opt names 'words) 0 ',face t))))
    (add-to-list 'fanfic--highlights highlight t)
    (unless skip-font-lock
      (font-lock-add-keywords nil highlight 'append)
      (font-lock-fontify-buffer))))

(defun fanfic-remove-highlights (names face &optional skip-font-lock)
  "Removes NAMES highlighted under FACE from the list of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, keywords keywords generated this way
are not yet removed from font-lock and fontification is not run afterwards."
  (unless fanfic-mode
    (error "Attempt to modify fanfic highlights outside of fanfic-mode"))
  (let ((highlight `((,(regexp-opt names 'words) 0 ',face t))))
    (unless (-contains-p fanfic--highlights highlight)
      (error "Attempt to remove non-present fanfic highlights"))
    (set 'fanfic--highlights (delq highlight fanfic--highlights))
    (unless skip-font-lock
      (font-lock-remove-keywords nil highlight)
      (font-lock-fontify-buffer))))



;;;###autoload
(defun fanfic-safe-cast-p (object)
  "Returns t if OBJECT is a cast safe for usage within fanfic functions."
  (and (listp object) (--all-p (or (stringp it) (-all-p #'stringp it)) object)))

;;;###autoload
(defun fanfic-safe-keywords-p (object)
  "Returns t if OBJECT is safe to be used as keywords within fanfic."
  (and (listp object) (-all-p #'stringp (-flatten object))))

(provide 'fanfic-core)
