;;; fanfic-universe.el ---
;;
;; Filename: fanfic-universe.el
;; Description: Universe related functions for fanfic.el
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 09:47:57 2016 (+0200)
;; Version: 3.0
;; Package-Requires: ((dash "2.12.1"))
;; Last-Updated: Sat Jun  4 16:11:15 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 83
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Defines `fanfic-universe-from' methods as well as
;;    `fanfic-load-universe'.
;;    Defines `fanfic-safe-universe-p'.
;;    Defines how cast and keywords are handled for universes
;;    (highlighting).
;;    Defines `fanfic-add-universe' and `fanfic-available-universes'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.0: Split from other fanfic modules.
;;       Add functions to read universes from files.
;;
;;  Previously:
;;    2.1: Universe support first added.
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

(require 'dash)

(require 'fanfic-core)



;;;###autoload
(defcustom fanfic-universes nil
  "Each entry is a name of a universe the current fic is set in.
This is mostly useful for `fanfic-add-keywords-from-universes', `fanfic-add-cast-from-universes'
and their hook versions `fanfic-universes-special-keywords' and `fanfic-universes-special-cast'.

Use M-x `fanfic-available-universes' to get a list of meaningful values.
Use `fanfic-add-universe' to make a universe \"available\"."
  :type '(repeat string)
  :safe #'fanfic-safe-universes-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-universe-dirs (list (expand-file-name "fanfic-universes" user-emacs-directory))
  "Directories in which to search for fanfic universes."
  :type '(repeat directory)
  :group 'fanfic)



(defun fanfic-universe-from-string (str)
  "Read a universe from STR.

Universes are parsed according to their own LISP-like language. The following functions are defined:
(name . NAME) set the universe name to NAME.
(protagonists &rest PROTAGONISTS...) adds PROTAGONISTS under `fanfic-protagonist-face' to the universe's cast.
(antagonists &rest ANTAGONISTS) adds ANTAGONIST under `fanfic-antagonist-face' to the universe's cast.
(cast &rest CAST) adds CAST under `fanfic-cast-face' to the universe's cast.
(keywords &rest KEYWORDS) adds KEYWORDS under `fanfic-keyword-face' to the universe's cast.

So far, these have been functions to provide the standard functionality of `fanfic-mode' and conformity to
`fanfic-add-universe' by giving the universe a name. The following functions add extended behavior.

(make-face FACE &rest ATTS) defines FACE as a new face with ATTS as its face attributes for all display types.
(character FACE &rest CHARACTER) adds CHARACTER under the face FACE to the universe's cast.
(keywords* FACE &rest KEYWORDS) adds KEYWORDS under the face FACE to the universe's cast."
  (let ((universe (fanfic-universe-from-string-1 str)))
    (setf (fanfic-universe-cast universe) (nreverse (fanfic-universe-cast universe)))
    (setf (fanfic-universe-keywords universe) (nreverse (fanfic-universe-keywords universe)))
    universe))

(defun fanfic-universe-from-string-1 (str)
  (let ((start 0)
        (end (length str))
        obj-and-idx obj
        (universe (fanfic-make-universe))
        identifier)
    (condition-case error
        (while t
          (setq obj-and-idx (read-from-string str start end))
          (setq obj (car obj-and-idx) start (cdr obj-and-idx))
          (pcase obj
            (`(name . ,name)
             (setf (fanfic-universe-name universe) name)
             (setq identifier (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase name)))
             (eval `(defgroup ,(intern (concat "fanfic-universe-" identifier)) nil
                      ,(format "Options generated by `fanfic-universe' for \"%s\"" name)
                      :group 'fanfic-universes)))
            (`(protagonists . ,protagonists) (push (cons protagonists 'fanfic-protagonist-face) (fanfic-universe-cast universe)))
            (`(antagonists . ,antagonists) (push (cons antagonists 'fanfic-antagonist-face) (fanfic-universe-cast universe)))
            (`(cast . ,cast) (push (cons cast 'fanfic-cast-face) (fanfic-universe-cast universe)))
            (`(keywords . ,keywords) (push (cons keywords 'fanfic-keyword-face) (fanfic-universe-keywords universe)))
            (`(make-face ,name . ,facespec)
             (let ((face (intern (concat "fanfic-" identifier "-" (symbol-name name) "-face"))))
               (eval `(defface ,face '((t . ,facespec))
                        "Face generated by `fanfic-universe'."
                        :group (quote ,(intern (concat "fanfic-universe-" identifier)))))))
            (`(character ,name . ,character)
             (let ((face (intern (concat "fanfic-" identifier "-" (symbol-name name) "-face"))))
               (push (cons character face) (fanfic-universe-cast universe))))
            (`(keywords* ,name . ,keywords)
             (let ((face (intern (concat "fanfic-" identifier "-" (symbol-name name) "-face"))))
               (push (cons keywords face) (fanfic-universe-keywords universe))))
            (_ (error "Unkown command encountered: %S" obj))))
      (end-of-file universe))))

(defun fanfic-universe-from-file (file)
  "Read a universe from FILE.

See also: `fanfic-univere-from-string'"
  (fanfic-universe-from-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

;;;###autoload
(defun fanfic-load-universe (file &optional overwrite noerror)
  "Read a universe from FILE and add it to the list of available universes.

This function acts as both a shortcut and user interface to `fanfic-add-universe'
and `fanfic-universe-from-file' which make the most sense when combined.
Naturally, it has an `interactive' form.

OVERWRITE and NOERROR are passed to `fanfic-add-universe' while FILE is passed
to `fanfic-universe-from-file'. When called interactively, nil is assumed for both."
  (interactive "f")
  (fanfic-add-universe (fanfic-universe-from-file file) overwrite noerror))

(defun fanfic-load-universes (directory &optional demote-errors)
  "Load all universes defined in DIRECTORY.

If DEMOTE-ERRORS is truthy, run each load with demoted errors, so that one malformed file does
not affect the rest of the directory. "
  (dolist (file (directory-files directory t))
    (unless (file-directory-p file)
      (if demote-errors
          (with-demoted-errors "Error: %S" (fanfic-load-universe file))
        (fanfic-load-universe file)))))

(defvar fanfic--universes-initialized-p nil)

(defun fanfic-universes-init ()
  (let ((overwrite t))
    (--each fanfic-universe-dirs (fanfic-load-universes it)))
  (setq fanfic--universes-initialized-p t))



(defvar fanfic--universes (make-hash-table :test #'equal))
(defvar fanfic--active-universes nil)
(make-variable-buffer-local 'fanfic--active-universes)



(defun fanfic-add-keywords-from-universes (&optional skip-font-lock)
  "Add the keywords of all universes in `fanfic-universes' to the lists of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, do not run fontification afterwards."
  (--each fanfic-universes
    (let ((universe (gethash it fanfic--universes)))
      (when (fanfic-universe-p universe)
        (--each (fanfic-universe-keywords universe)
          (fanfic-add-highlights (-flatten (car it)) (cdr it) skip-font-lock))))))

(defun fanfic-add-cast-from-universes (&optional skip-font-lock)
  "Add the casts of all universes in `fanfic-universes' to the lists of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, do not run fontification afterwards."
  (--each fanfic-universes
    (let ((universe (gethash it fanfic--universes)))
      (when (fanfic-universe-p universe)
        (--each (fanfic-universe-cast universe)
          (fanfic-add-highlights (-flatten (fanfic-decline (car it))) (cdr it) skip-font-lock))))))



(defun fanfic-active-universe-p (name)
  "Return t if NAME is an entry in `fanfic-universes', that points to a safe universe."
  (-contains-p fanfic--active-universes name))

(defun fanfic-update-active-universes ()
  "Refresh the list of active universes to contain all entries in `fanfic-universes' that point towards safe universes.
This function is meant for internal use. Calling it from the outside may mess with the behavior of `fanfic-active-universe-p' as it refers to the aforementioned list of safe entries.
It could thus also be used for debugging purposes, but I doubt that it makes much sense to do so."
  (setq fanfic--active-universes (--filter (fanfic-safe-universe-p (gethash it fanfic--universes)) fanfic-universes)))

(defun fanfic-add-universe (universe &optional overwrite noerror)
  "Make UNIVERSE available for use within `fanfic-mode', most notably for the use in `fanfic-universes'.
UNIVERSE must have a name, otherwise an error is signaled.

When OVERWRITE is t, replace already existing universes when they exist. Otherwise signal an error in that case.

When NOERROR is t and an error occurs, return nil instead of signaling the error."
  (let ((name (fanfic-universe-name universe)))
    (cond ((not name) (unless noerror (error "Name of universe must not be empty")))
          ((and (not overwrite) (gethash name fanfic--universes nil)) (unless noerror (error "%s already exists" universe)))
          (t (puthash name universe fanfic--universes)))))

(defun fanfic-available-universes ()
  "Return names of all available fanfic universes."
  (interactive)
  (-sort 'string< (let ((acc nil))
                    (maphash (lambda (k v) (add-to-list 'acc k))
                             fanfic--universes) acc)))



;;;###autoload
(defun fanfic-safe-universe-p (object)
  "Return t if OBJECT is a universe safe for usage within fanfic.

In order to create such an universe it is best to use `fanfic-make-universe'.
The following have to be satisfied in order to make a universe \"safe\":
  * The universe name must be a string.
  * The universe cast must be a list of cons cells, in which each car
    satisfies `fanfic-safe-cast-p' and each cdr is a face.
  * The universe keywords must be a list of cons cells, in which each car
    satisfies `fanfic-safe-keywords-p' and each cdr is a face."
  (and (fanfic-universe-p object)
       (stringp (fanfic-universe-name object))
       (--all-p (and (fanfic-safe-cast-p (car it)) (facep (cdr it))) (fanfic-universe-cast object))
       (--all-p (and (fanfic-safe-keywords-p (car it)) (facep (cdr it))) (fanfic-universe-keywords object))))

;;;###autoload
(defun fanfic-safe-universes-p (universes)
  "Test whether it is safe to use UNIVERSES as value for `fanfic-universes'."
  (and (-all-p #'stringp universes)
       (or (not fanfic--universes-initialized-p)
           (--all-p (gethash it fanfic--universes) universes))))

(provide 'fanfic-universe)
