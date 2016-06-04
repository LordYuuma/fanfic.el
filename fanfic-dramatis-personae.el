;;; fanfic-dramatis-personae.el ---
;;
;; Filename: fanfic-dramatis-personae.el
;; Description: Dramatis personae for fanfic.el
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 12:09:38 2016 (+0200)
;; Version: 3.0
;; Package-Requires: ((dash "2.12.1"))
;; Last-Updated: Sat Jun  4 16:02:17 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 8
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    The functions in this module list the cast of the current
;;    buffer.
;;    `fanfic-dramatis-personae' inserts this list at POINT.
;;    Alternatively for debugging `fanfic--dramatis-personae'
;;    returns the list itself. It is considered private, but not
;;    much harm is done if it is used for debugging.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.0: Split from other modules.
;;
;;  Previously:
;;    2.1: Adaption to universes.
;;    1.5: Adapted according to core.
;;    1.3: First implementation
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
(require 'fanfic-universe)

(require 'dash)



;;;###autoload
(defcustom fanfic-dramatis-personae-group-prefix ""
  "String to be inserted before each group in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-group-suffix ""
  "String to be inserted after each group in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-item-prefix ""
  "String to be inserted before each name in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-item-suffix ""
  "String to be inserted after each name in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-nick-prefix "(\""
  "String to be inserted before each nick in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)

;;;###autoload
(defcustom fanfic-dramatis-personae-nick-suffix "\")"
  "String to be inserted after each nick in `fanfic-dramatis-personae'."
  :type 'string
  :safe #'stringp
  :group 'fanfic-dramatis-personae)



;;;###autoload
(defun fanfic-dramatis-personae (&optional prefix)
  "Insert a dramatis personae at the current point.
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

(defun fanfic--dramatis-personae ()
  (--reduce (concat acc "\n" it)
            (--map (concat fanfic-dramatis-personae-group-prefix
                           it
                           fanfic-dramatis-personae-group-suffix)
                   (-flatten
                    (list
                     (--map (--reduce (if it (concat acc "\n" it) acc) (fanfic--dramatis-personae-function (symbol-value it)))
                            '(fanfic-protagonists fanfic-antagonists fanfic-cast))
                     (--map (let ((universe (gethash it fanfic--universes)))
                              (when (fanfic-safe-universe-p universe)
                                (--reduce (if it (concat acc "\n" it) acc) (fanfic--dramatis-personae-function (-mapcat 'car (fanfic-universe-cast universe))))))
                            fanfic-universes))))))

(defun fanfic--dramatis-personae-function (cast)
  (--map (if (stringp it)
             it
           (concat (car it)
                   " "
                   fanfic-dramatis-personae-nick-prefix
                   (car (cdr it))
                   fanfic-dramatis-personae-nick-suffix))
         cast))

(provide 'fanfic-dramatis-personae)
