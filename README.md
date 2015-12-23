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

* `fanfic-antagonists`
* `fanfic-antagonist-nick-alist`
* `fanfic-cast`
* `fanfic-cast-nick-alist`
* `fanfic-protagonists`
* `fanfic-protagonist-nick-alist`

### Faces
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
