fanfic.el
=========

Setup
-----
Method 1: Adding to load-path and loading

    (add-to-list 'load-path "/path/to/fanfic.el")
    (require 'fanfic)

Method 2: Autoload

    (let ((source "path/to/fanfic.el")
          (autoload-file "path/to/fanfic-autoloads.el"))
      (update-file-autoloads source t autoload-file)
      (load autoload-file))
    
Method ???: Whatever else you do to load a file.

Usage
-----
### Variables

* `fanfic-keywords`
* `fanfic-protagonists`
* `fanfic-antagonists`
* `fanfic-cast`

### Faces
* `fanfic-keyword-face`
* `fanfic-antagonist-face`
* `fanfic-antagonist-nick-face`
* `fanfic-cast-face`
* `fanfic-nick-face`
* `fanfic-protagonist-face`
* `fanfic-protagonist-nick-face`

Set these variables and faces to your liking before activating `fanfic-mode`.

### Functions & Commands
* `fanfic-mode` (minor-mode)
  adds highlights according to the variables and faces
  above.

* `fanfic-mode-recast` re-evaluates the variables above to adjust highlights.

* `fanfic-strip-scenes` filter scenes depending on content.

* `fanfic-add-highlights` can be used while `fanfic-mode` is active to add highlights.

* `fanfic-remove-highlights` can be used while `fanfic-mode` is active to remove highlights.

* `fanfic-dramatis-personae` inserts a dramatis personae into the current buffer.

Universes
---------
Universes are a feature of version 2.1 and above in order to handle different settings across different fics.
Each universe can have a cast in the form of `((PERSONAE . FACE) ...)` 
and keywords in the form of `((KEYWORDS . FACE) ...)`. When this universe is "active" (i.e. its name is
contained in `fanfic-universes`), personae and keywords are highlighted according to their faces, when
`fanfic-mode` is activated.

To make a new universe, use `fanfic-make-universe`. Assuming that *universe* is a universe created this way,
use `(fanfic-universe-cast universe)` or `(fanfic-universe-keywords universe)` to get these values and
`(setf (fanfic-universe-cast universe) ...)` or
`(setf (fanfic-universe-keywords universe) ...)` to set them. To make your universe available to `fanfic`
use `fanfic-add-universe`.
