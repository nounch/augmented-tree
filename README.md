# Augmented Tree

Augmented Tree augments the output of the `tree` shell command with
clickable text buttons which can be used to directly open or preview
files and directories. This allows faster navigation (e.g. `show
subtree` or `show parent`) and code browsing in larger codebases.

The Augmented Tree packages includes a handy sidebar feature which puts
the augmented tree in a unobtrusive sidebar which allows fast
resizing and previewing. The augmeted output, however, does not have to
be used with the sidebar functionality. Instead, it can reside just fine
in any other window.

The sidebar works best with in a conventional horizontally split setup
(sidebar on the far left, one or two code windows on the right). Since
Augmented Tree displays the full subtree, it is highly encouraged to use
(RegEx/isearch) search in its buffer to find files and navigate the
tree.

The provided keybindings for the Augmented Tree buffer can be
overrriden, if desired.

The tree buffer provides various key bindings for navigating the tree.


# Interactive commands

- `augmented-tree`: Show the Augmented Tree in the currently selected window.
- `augmented-tree-other-window`: Show the Augmented Tree in different window.
- `augmented-tree-sidebar`: Show the Augmented Tree sidebar window.
- `augmented-tree-focus-window`: Jump to the Augmented Tree window.


# Keybindings

```
Cursor movement:

  "n" / "j" - Next line
  "p" / "k" - Previous line

File preview:

  "N" - Preview next line
  "P" - Preview previous line
  "M-k" / "i" - Preview current line

Buffer management:

  "q" - Kill Augmented Tree buffer (even if it is not the current one)
  "t" / "SPC" / "M-<right>" / "l" - Show subtree for current
                                    file/directory
  "^" / "M-<up>" / "h" - Go-to-parent
  "M-h" - Go-to-parent with cursor on previous file/directory name


File/directory opening:

  "v" - Open current file/directory read-only
  "o" - Open current file/directory in other window
  "r" - Open current file/directory read-only in other window

Resizing:

  "M-r" - Resize to specified width ("aug-sidebar-width", customizable)
  "M-l" - Toggle preview
```


# Notes

- Vim-like tree navigation is possible with "h", "j", "k" and "l".
- There is no folding functionality. `tree` always shows the full
  subtree. However, there is the option to preview a target directory
  in another window which gives a flat list of all files/directories in
  this directory.
- Moving the cursor to the next file/dir in the same level by skipping
  all lines in between is not possible.


# Warnings

- Augmented Tree parses the output of the `tree` command. This is and
  will always be irresponsible because
  + the output of `tree` is not meant to be read by machines and
  + there are various versions of `tree` with varying functionality
    across systems.

  So why is it still like that? Well, it is reaonable to assume that a
  decent developing system has access to `tree` with all required
  features and non-cryptic output that can be parsed by Augmented Tree.

  The required features are:
  + `-f`: Print full path prefix for each file.
  + `-n`: Turn colorization off always.
  + `--charset='ASCII`: Use ASCII characters only (This may
    lead to problems with non-ASCII file/dir names, but then
    again, it may be considered a good habit to not use them
    for file/dir names in software projects anyway). You can try to
    omit it, but you will have to live with the consequences.
- Calling Augmented Tree on large and deeply nested directory structures
  (e.g. `/`) takes as long as `tree` is finished and augmented and Emacs
  has inserted it in a buffer (if configured and capable to do so);
  i.e.: Think twice before you jump to the parent of the current
  directory. Augmented Tree is meant to navigate individual projects,
  not file system trees.
- Augmented Tree does work with vertically split windows. `aug-resize`,
  however may show unexpected behavior,


# Usage

``` lisp
(require 'augmented-tree)
(global-set-key (kbd "C-c M-p") 'augmented-tree)
(global-set-key (kbd "C-c M-o") 'augmented-tree-other-window)
(global-set-key (kbd "C-c M-s") 'augmented-tree-sidebar)
(global-set-key (kbd "C-c M-f") 'augmented-tree-focus-window)
```
