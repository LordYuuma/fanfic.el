;;; fanfic-universe.el ---
;;
;; Filename: fanfic-universe.el
;; Description:
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 09:47:57 2016 (+0200)
;; Version: 3.0
;; Package-Requires: ()
;; Last-Updated: Fri Jun  3 19:12:37 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 46
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
;;  3.0: Split from other fanfic modules.
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

(require 'fanfic-core)



(defun fanfic-universe-from-string (str)
  (let ((universe (fanfic-universe-from-string-1 str)))
    (setf (fanfic-universe-cast universe) (nreverse (fanfic-universe-cast universe)))
    (setf (fanfic-universe-keywords universe) (nreverse (fanfic-universe-keywords universe)))
    universe))

(defun fanfic-universe-from-string-1 (str)
  (let ((start 0)
        (end (length str))
        obj-and-idx obj
        (universe (fanfic-make-universe)))
    (condition-case error
        (while t
          (setq obj-and-idx (read-from-string str start end))
          (setq obj (car obj-and-idx) start (cdr obj-and-idx))
          (pcase obj
            (`(name . ,name) (setf (fanfic-universe-name universe) name))
            (`(protagonists . ,protagonists) (push (cons protagonists 'fanfic-protagonist-face) (fanfic-universe-cast universe)))
            (`(antagonists . ,antagonists) (push (cons antagonists 'fanfic-antagonist-face) (fanfic-universe-cast universe)))
            (`(cast . ,cast) (push (cons cast 'fanfic-cast-face) (fanfic-universe-cast universe)))
            (`(keywords . ,keywords) (push (cons keywords 'fanfic-keyword-face) (fanfic-universe-keywords universe)))
            (_ (print obj))))
      (end-of-file universe))))

(defun fanfic-universe-from-file (file)
  (fanfic-universe-from-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))



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



(defvar fanfic--universes (make-hash-table :test #'equal))
(defvar fanfic--active-universes nil)
(make-variable-buffer-local 'fanfic--active-universes)



(defun fanfic-add-keywords-from-universes (&optional skip-font-lock)
  "Adds the keywords of all universes in `fanfic-universes' to the lists of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, keywords generated this way are
not yet added to font-lock and fontification is not run afterwards."
  (--each fanfic-universes
    (let ((universe (gethash it fanfic--universes)))
      (when (fanfic-universe-p universe)
        (--each (fanfic-universe-keywords universe)
          (fanfic-add-highlights (-flatten (car it)) (cdr it) skip-font-lock))))))

(defun fanfic-add-cast-from-universes (&optional skip-font-lock)
  "Adds the casts of all universes in `fanfic-universes' to the lists of fanfic generated highlights.
If optional argument SKIP-FONT-LOCK is non-nil, keywords generated this way are
not yet added to font-lock and fontification is not run afterwards."
  (--each fanfic-universes
    (let ((universe (gethash it fanfic--universes)))
      (when (fanfic-universe-p universe)
        (--each (fanfic-universe-cast universe)
          (fanfic-add-highlights (-flatten (fanfic-decline (car it))) (cdr it) skip-font-lock))))))



(defun fanfic-active-universe-p (name)
  "Returns t if NAME is an entry in `fanfic-universes', that points to a safe universe."
  (-contains-p fanfic--active-universes name))

(defun fanfic-update-active-universes ()
  "Refreshes the list of active universes to contain all entries in `fanfic-universes' that point towards safe universes.
This function is meant for internal use. Calling it from the outside may mess with the behavior of `fanfic-active-universe-p' as it refers to the aforementioned list of safe entries.
It could thus also be used for debugging purposes, but I doubt that it makes much sense to do so."
  (setq fanfic--active-universes (--filter (fanfic-safe-universe-p (gethash it fanfic--universes)) fanfic-universes)))

(defun fanfic-add-universe (universe &optional overwrite noerror)
  "Makes UNIVERSE available for use within `fanfic-mode', most notably for the use in `fanfic-universes'.
This function performs type checks on UNIVERSE which may be stronger than `fanfic-universe-p'. An error
is signaled when either check fails.
An error is also signaled, when UNIVERSE appears to have already been added and OVERWRITE is nil.
When NOERROR is t, nil is returned instead, when an error would be signaled."
  (let ((name (fanfic-universe-name universe)))
    (cond ((not name) (unless noerror (error "Name of universe must not be empty")))
          ((and (not overwrite) (gethash name fanfic--universes nil)) (unless noerror (error "%s already exists" universe)))
          (t (puthash name universe fanfic--universes)))))

(defun fanfic-available-universes ()
  "Returns names of all available fanfic universes."
  (interactive)
  (-sort 'string< (let ((acc nil))
                    (maphash (lambda (k v) (add-to-list 'acc k))
                             fanfic--universes) acc)))



;;;###autoload
(defun fanfic-safe-universe-p (object)
  "Returns t if OBJECT is a universe safe for usage within fanfic.

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
  "Tests whether it is safe to use UNIVERSES as value for `fanfic-universes'."
  (and (-all-p #'stringp universes)
       (--all-p (gethash it fanfic--universes) universes)))

(provide 'fanfic-universe)
