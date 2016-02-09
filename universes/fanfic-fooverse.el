;;; fanfic-fooverse.el ---
;;
;; Filename: fanfic-fooverse.el
;; Description:
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jan 15 20:55:03 2016 (+0100)
;; Version:
;; Package-Requires: (fanfic "2.1")
;; Last-Updated: Tue Feb  9 23:11:29 2016 (+0100)
;;           By: Lord Yuuma
;;     Update #: 9
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Example for fanfic-universe features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(require 'fanfic)

;;;###autoload
(eval-after-load "fanfic"
  '(progn
     ;; register Fooverse in fanfic without loading the whole file.
     ;; in this example this doesn't really do much, especially when it is not autoloaded,
     ;; when these files get longer, there'd be a bigger difference
     (fanfic-add-universe (fanfic-make-universe :name "Fooverse" :requires 'fanfic-fooverse) nil t)))

(let ((fooverse (fanfic-make-universe :name "Fooverse")))
  (setf (fanfic-universe-cast fooverse)
        '((("Alice" "Bob") . fanfic-protagonist-face)
          (("Eve") . fanfic-antagonist-face)))
  (setf (fanfic-universe-keywords fooverse)
        '(((("foo" "bar" "baz")
            ("wibble" "wobble" "wubble")
            ("spam" "ham" "eggs")) . fanfic-keyword-face)))
  (fanfic-add-universe fooverse t))

(provide 'fanfic-fooverse)
