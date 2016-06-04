;;; fanfic-extras.el ---
;;
;; Filename: fanfic-extras.el
;; Description: Extra functionality for fanfic.el
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Fri Jun  3 15:22:51 2016 (+0200)
;; Version: 3.0
;; Package-Requires: ()
;; Last-Updated: Sat Jun  4 15:56:30 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 7
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    As of now, this only holds `fanfic-strip-scenes'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  3.0: Split from other fanfic modules.
;;
;;  Previously:
;;    1.0: `fanfic-strip-scenes' generates new buffer
;;    0.4: `fanfic-strip-scenes'
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
(defun fanfic-strip-scenes (content &optional exclude)
  "Strip the fanfic to paragraphs including CONTENT. Output to a new buffer."
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

(provide 'fanfic-extras)
