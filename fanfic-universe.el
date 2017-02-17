;;; fanfic-universe.el ---
;;
;; Filename: fanfic-universe.el
;; Description: Universe related functions for fanfic.el
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 09:47:57 2016 (+0200)
;; Version: 3.1
;; Package-Requires: ((dash "2.12.1"))
;; Last-Updated: Fri Feb 17 10:38:25 2017 (+0100)
;;           By: Lord Yuuma
;;     Update #: 144
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
;;    Defines `fanfic-universe-mode', a major mode for writing
;;    fanfic-universes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.1: Add `fanfic-universe-to-string' and `fanfic-get-universe'.
;;       Have `fanfic-universes-init' demote errors.
;;       Add snippet support.
;;       Add `fanfic-universe-mode'.
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

(declare-function s-chop-prefix "s" (prefix str))
(declare-function s-chop-suffix "s" (suffix str))
(declare-function s-join "s" (sep strings))
(declare-function s-prefix-p "s" (prefix s &optional ignore-case))
(declare-function fanfic-reset "fanfic" (&optional setting))

(require 'fanfic-core)

;; s is only needed for some features, so we don't force the user to have it.
(require 's nil t)

(cl-defstruct (fanfic-universe (:constructor fanfic-make-universe)
                               (:copier fanfic-copy-universe))
  name cast keywords)



;;;###autoload
(defcustom fanfic-universes nil
  "Each entry is a name of a universe the current fic is set in.
This is mostly useful for `fanfic-add-keywords-from-universes',
`fanfic-add-cast-from-universes' and their hook versions
`fanfic-universes-special-keywords' and `fanfic-universes-special-cast'.

Use M-x `fanfic-available-universes' to get a list of meaningful values.
Use `fanfic-add-universe' to make a universe \"available\"."
  :type '(repeat string)
  :safe #'fanfic-safe-universes-p
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-universe-dirs (list (expand-file-name "fanfic-universes"
                                                        user-emacs-directory))
  "Directories in which to search for fanfic universes."
  :type '(repeat directory)
  :group 'fanfic)



(defun fanfic-universe-identifier (universe)
  "Return the universe identifier of UNIVERSE.

The identifier of a universe is a downcase of its name with non-alphanumerics
replaced by dashes."
  (replace-regexp-in-string "[^a-z0-9]+" "-"
                            (downcase (fanfic-universe-name universe))))



(defun fanfic-universe-make-snippets (universe &optional reverse-cast)
  "Make snippet definitions for the cast of UNIVERSE for the use with yasnippet.

UNIVERSE maybe a universe or the name of a universe in
`fanfic-available-universes'.

See also `fanfic-make-snippets'. Some universe specific changes are applied.
If optional argument REVERSE-CAST is truthy, use a reversed copy of the
universe's cast."
  (setq universe (fanfic-get-universe universe))
  (let ((name (fanfic-universe-name universe))
        (cast (fanfic-universe-cast universe))
        snippets)
    (setq
     cast (if reverse-cast (reverse cast) cast)
     snippets (-non-nil (-mapcat #'fanfic-make-snippets (-map #'car cast))))
    (dolist (snippet snippets snippets)
      (setf
       ;; CONDITION
       (nth 3 snippet) (list #'fanfic-active-universe-p name)
       ;; GROUP
       (nth 4 snippet) (list "Fanfiction" name)
       ;; UUID
       (nth 8 snippet)
       (concat "[Fanfic] " name ": "
               ;; KEY " => " TEMPLATE
               (nth 0 snippet) " => " (nth 1 snippet))))))



(defun fanfic-universe-from-string (str)
  "Read a universe from STR.

Universes are parsed according to their own LISP-like language.
The following functions are defined:

  (name . NAME) set the universe name to NAME.
  (protagonists &rest PROTAGONISTS...)  adds PROTAGONISTS under
                                        `fanfic-protagonist-face' to the
                                        universe's cast.
  (antagonists &rest ANTAGONISTS)       adds ANTAGONIST under
                                        `fanfic-antagonist-face' to the
                                        universe's cast.
  (cast &rest CAST)                     adds CAST under `fanfic-cast-face'
                                        to the universe's cast.
  (keywords &rest KEYWORDS)             adds KEYWORDS under
                                        `fanfic-keyword-face' to the
                                        universe's list of keywords.

So far, these have been functions to provide the standard functionality of
`fanfic-mode' and conformity to `fanfic-add-universe' by giving the universe a
name. The following functions add extended behavior.

  (make-face FACE &rest ATTS)           defines FACE as a new face with ATTS as
                                        its face attributes for all displays.
  (character FACE &rest CHARACTER)      adds CHARACTER under the face FACE
                                        to the universe's cast.
  (keywords* FACE &rest KEYWORDS)       adds KEYWORDS under the face FACE
                                        to the universe's list of keywords.
  (make-snippets &rest MODES)           defines snippets for the universe using
                                        `fanfic-universe-make-snippets' and
                                        `yas-define-snippets'.
                                        Does not explicitly require yasnippet.
                                        Does nothing when yasnippet is not
                                        loaded."
  (let ((universe (fanfic-universe-from-string-1 str)))
    (setf (fanfic-universe-cast universe) (nreverse (fanfic-universe-cast universe))
          (fanfic-universe-keywords universe) (nreverse (fanfic-universe-keywords universe)))
    universe))

(defun fanfic-universe-from-string-1 (str)
  (let ((start 0)
        (end (length str))
        obj-and-idx obj
        (universe (fanfic-make-universe))
        identifier)
    (condition-case error
        (while t
          (setq obj-and-idx (read-from-string str start end)
                obj (car obj-and-idx)
                start (cdr obj-and-idx))
          (pcase obj
            (`(name . ,name)
             (setf (fanfic-universe-name universe) name)
             (setq identifier (fanfic-universe-identifier universe))
             (custom-declare-group
              (intern (concat "fanfic-universe-" identifier)) nil
              (format "Options generated by `fanfic-universe' for \"%s\"" name)
              :group 'fanfic-universes))
            (`(protagonists . ,protagonists)
             (push (cons protagonists 'fanfic-protagonist-face)
                   (fanfic-universe-cast universe)))
            (`(antagonists . ,antagonists)
             (push (cons antagonists 'fanfic-antagonist-face)
                   (fanfic-universe-cast universe)))
            (`(cast . ,cast)
             (push (cons cast 'fanfic-cast-face)
                   (fanfic-universe-cast universe)))
            (`(keywords . ,keywords)
             (push (cons keywords 'fanfic-keyword-face)
                   (fanfic-universe-keywords universe)))
            (`(make-face ,name . ,facespec)
             (let ((face (intern (concat "fanfic-" identifier "-"
                                         (symbol-name name) "-face"))))
               (custom-declare-face face `((t . ,facespec))
                                    "Face generated by `fanfic-universe'."
                                    :group (intern (concat "fanfic-universe-"
                                                           identifier)))))
            (`(character ,name . ,character)
             (let ((face (intern (concat "fanfic-" identifier "-"
                                         (symbol-name name) "-face"))))
               (push (cons character face)
                     (fanfic-universe-cast universe))))
            (`(keywords* ,name . ,keywords)
             (let ((face (intern (concat "fanfic-" identifier "-"
                                         (symbol-name name) "-face"))))
               (push (cons keywords face)
                     (fanfic-universe-keywords universe))))
            (`(make-snippets . ,modes)
             (when (fboundp 'yas-define-snippets)
               (setq modes (if modes modes '(text-mode)))
               (-each modes
                 (lambda (mode)
                   (yas-define-snippets
                    mode
                    (reverse (fanfic-universe-make-snippets universe t)))))))
            (_ (error "Unkown command encountered: %S" obj))))
      (end-of-file universe))))

(defun fanfic-universe-from-file (file)
  "Read a universe from FILE.

See also: `fanfic-univere-from-string'"
  (fanfic-universe-from-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))



(defun fanfic-universe-to-string (universe)
  "Return UNIVERSE as a string readable with `fanfic-universe-from-string'.

UNIVERSE can either be a universe that satisfies `fanfic-universe-p'
or the name of an available universe.
In the case of the latter, let universe be that universe.

First, write the name of UNIVERSE.
Then, write the standard cast and keywords, that are highlighted with faces
from `fanfic-core'.
Then, write the face declarations for faces that have been defined by the
universe itself.
Then, write special cast and keywords, that are highlighted with such faces.

Face names, that have been defined by the universe are named either
fanfic-IDENTIFIER-FACE-face or just IDENTIFIER-FACE-face,
where IDENTIFIER is the `fanfic-universe-identifier' of UNIVERSE.
A universe, that has been read by `fanfic-universe-from-string' will
automatically follow the pattern fanfic-IDENTIFIER-FACE-face.
A universe that has been defined in an Emacs Lisp module is encouraged to
ensure, that IDENTIFIER is the module name, in which it has been defined.

This feature requires `s.el'."
  (unless (featurep 's) (error "s is required to make use of this function"))
  (setq universe
        (or (fanfic-get-universe universe)
            (error "Universe %s could not be found." universe)))
  (s-join "\n"
          (-flatten
           (list
            (format "(name . \"%s\")" (fanfic-universe-name universe))
            (let* ((short-prefix (concat
                                  (fanfic-universe-identifier universe) "-"))
                   (prefix (concat "fanfic-" short-prefix)))
              (-map
               (lambda (prefix)
                 (list
                  (when prefix
                    (fanfic--universe-to-string-make-face
                     prefix
                     (-map #'cdr (append (fanfic-universe-cast universe)
                                         (fanfic-universe-keywords universe)))))
                  (--map (fanfic--universe-to-string-format-cast
                          prefix (car it) (cdr it))
                         (fanfic-universe-cast universe))
                  (--map (fanfic--universe-to-string-format-keywords
                          prefix (car it) (cdr it))
                         (fanfic-universe-keywords universe))))
               (list nil short-prefix prefix)))))))

(defun fanfic--universe-to-string-format-cast (prefix cast face)
  "Format CAST with FACE to be readable by `fanfic-universe-from-string'.

If PREFIX is non-nil, check whether FACE starts with PREFIX and if it does,
format it according to the (character FACE characters) syntax.
If PREFIX is nil, match FACE against faces from `fanfic-core' and use the
corresponding function."
  (let ((cast (s-join " " (--map (format "%S" it) cast))))
    (if prefix
        (when (s-prefix-p prefix (symbol-name face))
          (format "(character %s %s)"
                  (s-chop-prefix prefix
                                 (s-chop-suffix "-face"
                                                (symbol-name face)))
                  cast))
      (pcase face
        (`fanfic-protagonist-face (format "(protagonist %s)" cast))
        (`fanfic-antagonist-face (format "(antagonist %s)" cast))
        (`fanfic-cast-face (format "(cast %s)" cast))))))

(defun fanfic--universe-to-string-format-keywords (prefix kwds face)
  "Format KWDS with FACE to be readable by `fanfic-universe-from-string'.

If PREFIX is non-nil, check whether FACE starts with PREFIX and if it does,
format it according to the (keywords* FACE characters) syntax.
If PREFIX is nil, match FACE against faces from `fanfic-core' and use the
corresponding function."
  (let ((kwds (s-join " " (--map (format "%S" it) kwds))))
    (if prefix
        (when (s-prefix-p prefix (symbol-name face))
          (format "(keywords* %s %s)"
                  (s-chop-prefix prefix
                                 (s-chop-suffix "-face"
                                                (symbol-name face)))
                  kwds))
      (when (eq face 'fanfic-keyword-face)
        (format "(keywords %s)" kwds)))))

(defun fanfic--universe-to-string-make-face (prefix face-names)
  "Write (make-face FACE ATTS) for all faces in FACE-NAMES with prefix PREFIX."
  (let ((l (length prefix)))
    (setq face-names
          (--filter
           (and
            (> (length (symbol-name it)) l)
            (string= prefix (substring (symbol-name it) 0 l)))
           face-names))
    (--map
     (format "(make-face %s %s)"
             (s-chop-prefix prefix
                            (s-chop-suffix "-face"
                                           (symbol-name (car it))))
             (s-join " " (--map (format "%s" it) (cdr it))))
     (-zip-pair face-names (--map (cdr (assoc t (face-default-spec it)))
                                  face-names)))))



;;;###autoload
(defun fanfic-load-universe (file &optional overwrite noerror)
  "Read a universe from FILE and add it to the list of available universes.

This function acts as both a shortcut and user interface to
`fanfic-add-universe'mand `fanfic-universe-from-file' which make the most sense
when combined. Naturally, it has an `interactive' form.

OVERWRITE and NOERROR are passed to `fanfic-add-universe' while FILE is passed
to `fanfic-universe-from-file'. When called interactively, both assume nil."
  (interactive "f")
  (fanfic-add-universe (fanfic-universe-from-file file) overwrite noerror))

(defun fanfic-load-universes (directory &optional demote-errors)
  "Load all universes defined in DIRECTORY.

If DEMOTE-ERRORS is truthy, run each load with demoted errors, so that one
malformed file does not affect the rest of the directory. "
  (dolist (file (directory-files directory t))
    (unless (file-directory-p file)
      (if demote-errors
          (with-demoted-errors "Error: %S" (fanfic-load-universe file))
        (fanfic-load-universe file)))))

(defvar fanfic--universes-initialized-p nil)

(defun fanfic-universes-init (&optional force)
  (unless (and fanfic--universes-initialized-p (not force))
    (--each fanfic-universe-dirs
      (when (file-directory-p it)
        (fanfic-load-universes it t))))
  (setq fanfic--universes-initialized-p t))



(defun fanfic-universe-to-setting (universe)
  (let ((metadata (list (fanfic-make-metadata 'universe
                                              (fanfic-universe-name universe))))
        objects)
    (--each (fanfic-universe-keywords universe)
      (push (fanfic-make-object (car it) :face (cdr it)) objects))
    (--each (fanfic-universe-cast universe)
      (push (fanfic-make-cast (car it) :face (cdr it)) objects))
    (fanfic-make-setting (nreverse objects) metadata)))

(defun fanfic-active-universe-p (name)
  "Return t if NAME is an entry in `fanfic-universes', that points to a safe universe."
  (-contains-p (fanfic-setting-metadata fanfic--setting)
               (fanfic-make-metadata 'universe name)))



(defvar fanfic--universes (make-hash-table :test #'equal))

(defun fanfic-add-universe (universe &optional overwrite noerror)
  "Make UNIVERSE available for use within `fanfic-mode'.

UNIVERSE must have a name, otherwise an error is signaled.
When OVERWRITE is t, replace already existing universes when they exist.
Otherwise signal an error in that case.
When NOERROR is t and an error occurs, return nil instead."
  (let ((name (fanfic-universe-name universe)))
    (cond ((not name)
           (unless noerror (error "Name of universe must not be empty")))
          ((and (not overwrite)
                (gethash name fanfic--universes nil))
           (unless noerror (error "%s already exists" universe)))
          (t (puthash name universe fanfic--universes)))))

(defun fanfic-available-universes ()
  "Return names of all available fanfic universes."
  (interactive)
  (-sort 'string< (let ((acc nil))
                    (maphash (lambda (k v) (add-to-list 'acc k))
                             fanfic--universes) acc)))

(defun fanfic-get-universe (universe)
  "If UNIVERSE satisfies `fanfic-universe-p', return UNIVERSE.
Otherwise look up UNIVERSE in the list of available universes and return the
result of that lookup."
  (if (fanfic-universe-p universe)
      universe
    (gethash universe fanfic--universes)))



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
       (--all-p (and (consp it)
                     (fanfic-safe-cast-p (car it))
                     (facep (cdr it)))
                (fanfic-universe-cast object))
       (--all-p (and (consp it)
                     (fanfic-safe-keywords-p (car it))
                     (facep (cdr it)))
                (fanfic-universe-keywords object))))

;;;###autoload
(defun fanfic-safe-universes-p (universes)
  "Test whether it is safe to use UNIVERSES as value for `fanfic-universes'."
  (and (-all-p #'stringp universes)
       (or (not fanfic--universes-initialized-p)
           (--all-p (gethash it fanfic--universes) universes))))



(defvar fanfic-universe-keywords
  '("name"
    "protagonists" "antagonists" "cast" "keywords"
    "make-face" "character" "keywords*"
    "make-snippets"))

(defvar fanfic-universe-syntax-table
  (let ((syntax-table (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?* "_" syntax-table)
    syntax-table))

(defvar fanfic-universe-font-lock
  `((,(regexp-opt fanfic-universe-keywords 'symbols) . font-lock-keyword-face)
    ;; mostly useful for the properties in make-face
    ("\\_<:\\sw+\\_>" . font-lock-builtin-face)
    ("\\(?:make-face\\|character\\|keywords\\*\\)\\(?:\\s-\\|\n\\)+\\(\\sw+\\)"
     (1 font-lock-variable-name-face))))

(defvar fanfic-universe-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'fanfic-universe-mode-preview)
    keymap))

;;;###autoload
(define-derived-mode fanfic-universe-mode lisp-mode "Fanfic-Universe"
  ;; TODO: insert docstring
  (setq font-lock-defaults '((fanfic-universe-font-lock)))
  (fanfic-universe-mode-preview)
  (add-hook 'after-save-hook #'fanfic-universe-mode-preview nil t))

(defun fanfic-universe-mode-preview ()
  "Preview the currently edited universe."
  (interactive)
  (when (fboundp #'fanfic-reset)
    (let ((fanfic-mode t))
      (fanfic-reset (fanfic-universe-to-setting
                     (fanfic-universe-from-string (buffer-string)))))))

(provide 'fanfic-universe)
