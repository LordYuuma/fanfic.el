;;; fanfic.el ---
;;
;; Filename: fanfic.el
;; Description: "Useful" functionality to help typesetting fanfics
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Created: Tue Sep 15 11:52:17 2015 (+0200)
;; Version: 2.1
;; Package-Requires: ((dash "2.12.1") (cl-lib "0.5"))
;; Last-Updated: Sun Jul  3 15:25:50 2016 (+0200)
;;           By: Lord Yuuma
;;     Update #: 321
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
;;        `fanfic-add-highlights'
;;        `fanfic-remove-highlights'
;;           add or remove highlights
;;        `fanfic-strip-scenes'
;;           filter scenes depending on content.
;;        `fanfic-dramatis-personae'
;;           inserts a dramatis personae into the current buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;  For module-specific Change Log, look into the module's files.
;;
;;  3.0:   Split into modules.
;;
;;  Previously:
;;    2.0: Removing of alists and added universe support.
;;    1.0: First really usable release.
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
(require 'fanfic-dramatis-personae)



;;;###autoload
(define-minor-mode fanfic-mode
  "Toggle `fanfic-mode'.

When `fanfic-mode' is t, `fanfic-keywords', `fanfic-protagonists',
`fanfic-cast' and `fanfic-antagonists' are highlighted according to
their faces.

When `fanfic-universes' is non-nil also highlight the casts and
keywords of all universes that satisfy `fanfic-safe-universe-p'.

`fanfic-protagonists', `fanfic-cast' and `fanfic-antagonists' as well
as the casts added by `fanfic-universes' are all declined using
`fanfic-decline' before being highlighted.

While `fanfic-mode' is t, font-locks are kept even along `font-lock-refresh-defaults'."
  nil " Fanfiction"
  :after-hook (fanfic-mode-recast)
  :group 'fanfic
  (if fanfic-mode
      (ad-activate-regexp "fanfic-font-lock-default")
    (ad-deactivate-regexp "fanfic-font-lock-default")))



;;;###autoload
(defcustom fanfic-special-keyword-hook '(fanfic-universes-special-keywords)
  "Hook to run after adding `fanfic-keywords' to the list of fanfic highlights.
This hook is run before any cast related keywords are added and should be used to define
special keywords, which are to be highlighted differently than `fanfic-keywords'."
  :type 'hook
  :group 'fanfic)

;;;###autoload
(defcustom fanfic-special-cast-hook '(fanfic-universes-special-cast)
  "Hook to run after adding cast related keywords to the list of fanfic highlights.
This hook should be used to add names of characters, who don't fit any of the
categories provided by fanfic.el or need to be colored differently because of an
already color coded cast."
  :type 'hook
  :group 'fanfic)

;;;###autoload
(defadvice font-lock-refresh-defaults (after fanfic-font-lock-defaults) (when fanfic-mode (fanfic--font-lock)))



(defun fanfic-universes-special-keywords ()
  "A version of `fanfic-add-keywords-from-universes', that can be used as hook for `fanfic-special-keyword-hook'."
  (fanfic-add-keywords-from-universes t))

(defun fanfic-universes-special-cast ()
  "A version of `fanfic-add-cast-from-universes', that can be used as hook for `fanfic-special-cast-hook'."
  (fanfic-add-cast-from-universes t))



;;; Hack Area

;; `update-fileautoloads' does not take `:safe' in `defcustom' well
;; because users might want to set these variables WITHOUT
;; explicitly loading the whole file and without changing anything
;; in the customization, we force these to appear in the autoloads
;; by generating a cookie for them.

;;;###autoload(put 'fanfic-cast 'safe-local-variable #'fanfic-safe-cast-p)
;;;###autoload(put 'fanfic-protagonists 'safe-local-variable #'fanfic-safe-cast-p)
;;;###autoload(put 'fanfic-antagonists 'safe-local-variable #'fanfic-safe-cast-p)
;;;###autoload(put 'fanfic-keywords 'safe-local-variable #'fanfic-safe-keywords-p)
;;;###autoload(put 'fanfic-universes 'safe-local-variable #'fanfic-safe-universes-p)

;;;###autoload(put 'fanfic-dramatis-personae-item-prefix 'safe-local-variable #'stringp)
;;;###autoload(put 'fanfic-dramatis-personae-item-suffix 'safe-local-variable #'stringp)
;;;###autoload(put 'fanfic-dramatis-personae-group-prefix 'safe-local-variable #'stringp)
;;;###autoload(put 'fanfic-dramatis-personae-group-suffix 'safe-local-variable #'stringp)
;;;###autoload(put 'fanfic-dramatis-personae-nick-prefix 'safe-local-variable #'stringp)
;;;###autoload(put 'fanfic-dramatis-personae-nick-suffix 'safe-local-variable #'stringp)

(provide 'fanfic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fanfic.el ends here
