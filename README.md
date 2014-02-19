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


# ★Features★

- Preview files/directories
- Toggle indentation markers
- Toggle showing hidden files/directories
- Toggle showing the subtree of the current directory
- Always exclude certain files/directories even when hidden files/directories are not set to be visible
- Customizable variables (buffer name, files/directories exclusion list, sidebar width, file prefix, directory prefix, ...) via `customize-mode`
- Colorization (differentiate files from directories)
- Sidebar or normal buffer or buffer in other window or a combination of either of those
- Temporarily maximize the sidebar
- Resize all buffers so they still are balanced even with the sidebar enabled (if possible)
- Close the sidebar/tree buffer from any other buffer
- Sticky sidebar: will not close when closing all windows except the current one, will not be used for `*Help*` buffers etc.
- "Sticky" settings: when navigating to the parent/child tree, keep the current settings (indentation prefix, sort order, sort order reversal)
- Manually update for file/directory changes
- Cycle between sorting types (at the moment: lexicographic and by code-point)
- Reverse the sort order
- Jump to next/previous button in the tree buffer
- Navigation: go to parent/go to child/go to parent, but keep point on the button to the previous directory/file
- Refresh the tree on every interface change (when toggling sort order/indentation marker etc.)
- Open file/directory in view mode and jump back to the Augmented Tree buffer when hitting `q`
- Open file/directory read-only or writable
- Intuitive keybindings (the keymap can be changed at will, though)

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
  "^" / "M-<up>" / "h" - Go to the parent directory
  "g" - Update the current tree
  "M-h" - Go to the parent directory with the cursor on the previous
        file/directory name

Sorting:

  "R" - Reverse the current sort order
  "C" - Cycle between available sorting types
  "|" - Toggle the indentation prefix on/off
  "." - Toggle displaying dotfiles/dirs.
  "m" - Toggle the visibility of the subtree of the current directory.

File/directory opening:

  "v" - Open current file/directory in view mode. Hitting "q" then will
        move the cursor back to the Augmented Tree buffer.
  "V" - Open current file/directory read-only
  "o" - Open current file/directory in other window
  "r" - Open current file/directory read-only in other window

Resizing:

  "M-r" - Resize to specified width ("aug-sidebar-width", customizable)
  "M-l" - Toggle preview
```


# Notes


NOTE:  THE FOLLOWING STATEMENT DOES NOT APPLY TO THE LATEST VERSION SINCE
       Augmented Tree NOW COMES WITH ITS OWN BUILT-IN DIRECTORY
       TRAVERSER!!! - This notice remains here until it is clear whether
       future versions will OPTIONALLY offer to use an external `tree`
       command for speed improvements (pure C is faster after all).


 - <del>Augmented Tree parses the output of the `tree` command. This is and
  will always be irresponsible because</del>
  + <del>the output of `tree` is not meant to be read by machines and</del>
  + <del>there are various versions of `tree` with varying functionality
    across systems.</del>

  <del>So why is it still like that? Well, it is reaonable to assume that a
  decent developing system has access to `tree` with all required
  features and non-cryptic output that can be parsed by Augmented Tree.</del>

  <del>The required features are:</del>
  + <del>`-f`: Print full path prefix for each file.</del>
  + <del>`-n`: Turn colorization off always.</del>
  + <del>`--charset='ASCII`: Use ASCII characters only (This may
    lead to problems with non-ASCII file/dir names, but then
    again, it may be considered a good habit to not use them
    for file/dir names in software projects anyway). You can try to
    omit it, but you will have to live with the consequences.</del>

- Augmented Tree tries to be as intuitive as possible when in doubt. As a result, most commands will rescan the directory tree and update it for moved, new or removed files and directories. So simple interface changes (toggling the indentation marker, changing/reversing the sort order, toggling hidden files etc.) will result in a rescan.
- Vim-like tree navigation is possible with "h", "j", "k" and "l".
- There is no folding functionality. The full subtree is always shown.
  However, there is the option to preview a target directory
  in another window which gives a flat list of all files/directories in
  this directory.
- Moving the cursor to the next file/dir in the same level by skipping
  all lines in between is not possible.
- Calling Augmented Tree on large and deeply nested directory structures
  (e.g. `/`) takes as long as `tree` is finished and augmented and Emacs
  has inserted it in a buffer (if configured and capable to do so);
  i.e.: Think twice before you jump to the parent of the current
  directory. Augmented Tree is meant to navigate individual projects,
  not file system trees.
- Augmented Tree does work with vertically split windows. `aug-resize`,
  however may show unexpected behavior,
- Always use `aug-kill-buffer` to quit Augmented Tree (e.g. hit `q` while the cursor is in the Augmented Tree buffer). This is necessary for deinitialization.

# Usage

``` lisp
(require 'augmented-tree)
(global-set-key (kbd "C-c M-p") 'augmented-tree)
(global-set-key (kbd "C-c M-o") 'augmented-tree-other-window)
(global-set-key (kbd "C-c M-s") 'augmented-tree-sidebar)
(global-set-key (kbd "C-c M-f") 'augmented-tree-focus-window)
(global-set-key (kbd "C-c M-q") 'augmented-tree-kill-buffer)
```
