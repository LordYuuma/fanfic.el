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

If you use this method, make sure to autoload all needed fanfic modules.
    
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

As of version 3, functionality is split across modules (or features as Emacs calls them).
For the time being `(require 'fanfic)` will also load `fanfic-universe` and `fanfic-dramatis-personae`, but not `fanfic-extras`.
Future versions will rely on autoloading for these modules as well and only provide `fanfic-core` unless otherwise specified.

#### Fanfic Core and `fanfic.el`

* `fanfic-mode` (minor-mode)
  adds highlights according to the variables and faces
  above.

* `fanfic-mode-recast` re-evaluates the variables above to adjust highlights.

* `fanfic-add-highlights` can be used while `fanfic-mode` is active to add highlights.

* `fanfic-remove-highlights` can be used while `fanfic-mode` is active to remove highlights.

#### Universes (module `fanfic-universe`)
Universes are a feature of version 2.1 and above in order to handle different settings across different files or directories more easily.
Instead of setting the fanfic variables per buffer or directory to include the whole cast, they can be specified as universes and then used with the `fanfic-universes` variable in a similar manner as before.

Each universe can have a cast in the form of `((PERSONAE . FACE) ...)`
and keywords in the form of `((KEYWORDS . FACE) ...)`. They can either be defined directly by using the `fanfic-make-universe` function or by writing a universe file.
For the specifications of the universe file, have a look at `fanfic-universe-from-string`.

The function `fanfic-safe-universe-p` checks, whether a universe adheres to the specification. If this predicate returns t, it can be added to the list of available universes using `fanfic-add-universe`, after which it can be used with `fanfic-universes` as it was mentioned before. In order to load a universe from a file, `fanfic-load-universe` can be used instead.

**Important note when upgrading from version 2:** In version 2, universes had an additional field, that allowed the loading of an external Emacs Lisp module to define one's universe. This field is no longer in use and has been removed. Instead, add universes that are to be loaded on the first use to the `fanfic-universe-dirs`. Universes can under some circumstances be converted to the new format with `fanfic-universe-to-string`. Of course, modules written in Emacs Lisp can still be used to define universes.

#### Other modules

* `fanfic-dramatis-personae` (from the `fanfic-dramatis-personae` module) inserts a dramatis personae into the current buffer.

* `fanfic-strip-scenes` (from the `fanfic-extras` module) filter scenes depending on content.

