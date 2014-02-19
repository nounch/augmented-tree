;;; augmented-tree.el --- Augments the `tree' command with button text

;; Copyright (C) 2014  -

;; Author: - <gronpy@gronpy.gronpy>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Augmented Tree
;; ==============
;;
;; Augmented Tree augments the output of the `tree' shell command with
;; clickable text buttons which can be used to directly open or preview
;; files and directories. This allows faster navigation (e.g. `show
;; subtree' or `show parent') and code browsing in larger codebases.
;;
;; Augmented Tree does not depend on the external `tree' command, but
;; handles the directory traversal with built-in Emacs facilities.
;;
;; The Augmented Tree packages includes a handy sidebar feature which puts
;; the augmented tree in a unobtrusive sidebar which allows fast resizing
;; and previewing. The augmeted output, however, does not have to be used
;; with the sidebar functionality. Instead, it can reside just fine in any
;; other window.
;;
;; The sidebar works best with in a conventional horizontally split setup
;; (sidebar on the far left, one or two code windows on the right). Since
;; Augmented Tree displays the full subtree, it is highly encouraged to use
;; (RegEx/isearch) search in its buffer to find files and navigate the
;; tree.
;;
;; The provided keybindings for the Augmented Tree buffer can be
;; overrriden, if desired.
;;
;; The tree buffer provides various key bindings for navigating the tree.
;;
;;
;; Interactive commands
;; ====================
;;
;; - `augmented-tree': Show the Augmented Tree in the currently selected
                                        ;                      window.
;; - `augmented-tree-other-window': Show the Augmented Tree in different
;;                                  window.
;; - `augmented-tree-sidebar': Show the Augmented Tree sidebar window.
;; - `augmented-tree-focus-window': Jump to the Augmented Tree window.
;;
;;
;; Keybindings
;; ===========
;;
;; Cursor movement:
;;
;;   "n" / "j" - Next line
;;   "p" / "k" - Previous line
;;  "M-N" - Next directory
;;  "M-P" - Previous directory
;;
;; File preview:
;;
;;   "N" - Preview next line
;;   "P" - Preview previous line
;;   "M-k" / "i" - Preview current line
;;   "C-c o a" - Open all files and directories in the current region
;;   "C-c o f" - Open all files in the current region
;;   "C-c o d" - Open all directories in the current region
;;
;; Buffer management:
;;
;;   "q" - Kill Augmented Tree buffer (even if it is not the current one)
;;   "t" / "SPC" / "M-<right>" / "l" - Show subtree for current
;;                                     file/directory
;;   "^" / "M-<up>" / "h" - Go to the parent directory
;;   "g" - Update the current tree
;;   "M-h" - Go to the parent directory with the cursor on the previous
;;           file/directory name
;;
;; Sorting:
;;
;;   "R" - Reverse the current sort order
;;   "C" - Cycle between available sorting types
;;   "|" - Toggle the indentation prefix on/off
;;   "." - Toggle displaying dotfiles/dirs.
;;   "m" - Toggle the visibility of the subtree of the current directory.
;;
;; File/directory opening:
;;
;;   "v" - Open current file/directory in view mode. Hitting "q" then will
;;         move the cursor back to the Augmented Tree buffer.
;;   "V" - Open current file/directory read-only
;;   "o" - Open current file/directory in other window
;;   "r" - Open current file/directory read-only in other window
;;
;; Resizing:
;;
;;   "M-r" - Resize to specified width (`aug-sidebar-width', customizable)
;;   "M-l" - Toggle preview
;;
;;
;; Notes
;; =====
;;
;; - Vim-like tree navigation is possible with "h", "j", "k" and "l".
;; - There is no folding functionality. `tree' always shows the full
;;   subtree. However, there is the option to preview a target directory
;;   in another window which gives a flat list of all files/directories in
;;   this directory.
;; - Moving the cursor to the next file/dir in the same level by skipping
;;   all lines in between is not possible.
;;
;;
;; Warnings
;; ========
;;
;; NOTE: THE FOLLOWING STATEMENT DOES NOT APPLY TO THE LATEST VERSION SINCE
;;   Augmented Tree NOW COMES WITH ITS OWN BUILT-IN DIRECTORY
;;   TRAVERSER!!! - This notice remains here until it is clear whether
;;   future versions will OPTIONALLY offer to use an external `tree'.
;;   command for speed improvements (pure C is faster after all).
;;
;; (- Augmented Tree parses the output of the `tree' command. This is and
;;   will always be irresponsible because
;;   + the output of `tree' is not meant to be read by machines and
;;   + there are various versions of `tree' with varying functionality
;;     across systems.
;;
;;   So why is it still like that? Well, it is reaonable to assume that a
;;   decent developing system has access to `tree' with all required
;;   features and non-cryptic output that can be parsed by Augmented Tree.
;;
;;   The required features are:
;;   + `-f': Print full path prefix for each file.
;;   + `-n': Turn colorization off always.
;;   + `--charset='ASCII': Use ASCII characters only (This may
;;     lead to problems with non-ASCII file/dir names, but then
;;     again, it may be considered a good habit to not use them
;;     for file/dir names in software projects anyway). You can try to
;;     omit it, but you will have to live with the consequences.)
;;
;; - Calling Augmented Tree on large and deeply nested directory structures
;;   (e.g. `/') takes as long as `tree' is finished and augmented and Emacs
;;   has inserted it in a buffer (if configured and capable to do so);
;;   i.e.: Think twice before you jump to the parent of the current
;;   directory. Augmented Tree is meant to navigate individual projects,
;;   not file system trees.
;; - Augmented Tree does work with vertically split windows. `aug-resize',
;;   however may show unexpected behavior,
;;
;;
;; Usage
;; =====
;;
;; (require 'augmented-tree)
;; (global-set-key (kbd "C-c M-p") 'augmented-tree)
;; (global-set-key (kbd "C-c M-o") 'augmented-tree-other-window)
;; (global-set-key (kbd "C-c M-s") 'augmented-tree-sidebar)
;; (global-set-key (kbd "C-c M-f") 'augmented-tree-focus-window)


;;; Code:


;;=========================================================================
;; Variables
;;=========================================================================

(defvar aug-sidebar-enlarged-p nil
  "Indicates if the sidebar is currently enlarged.")

(defvar aug-last-pointer-position-in-sidebar nil
  "Stores the pointer position so that it can be restored after
previewing.")

(defvar aug-last-window-start-in-sidebar nil
  "Stores the position of the first line in the window so that it can be
restored after previewing.")

(defvar aug-current-sorting-type "lexicographically"
  "A string idicating the current sorting type. For available options see:
`aug-generate-tree-string'.")

(defvar aug-currently-reversed nil
  "Indicates if the sorting order is currently reversed. `t' means the
sorting order is currently reversed, anything else means it is currently
not reversed.")

(defvar aug-sorting-types (list "code-point" "lexicographically")
  "List of strings indicating available sorting types")

(defvar aug-previous-indentation-prefix nil
  "Previous indentation prefix string. Used for indentation prefix
toggling.")

(defvar aug-hide-dotfiles t
  "Indicates if dotfiles/dirs should be included in the output tree. `t'
means: Do not include directories. Default value: `t'")

(defvar aug-in-other-window nil
  "Indicates whether Augmented Tree has been started using
`aug-tree-other-window'. `t' if this is the case.")

(defvar aug-invisibility-marker :invisible
  "Text property used to make text invisible. This is used in combination
with `buffer-invisibility-spec'.")

(defvar aug-thing-type-directory :directory
  "Indicates that the referenced thing is a direcotry.")

(defvar aug-thing-type-file :file
  "Indicates that the referenced thing is a file.")


;;=========================================================================
;; Customizable variables
;;=========================================================================

;; DEBUG
(defcustom aug-tree-command "tree -fn --charset='ASCII'" ;; "tree -fnX"
  "Tree command which is executed (has to be installed on the system).

Note: By default hidden files (prefixed with a dot `.') are not printed.")

(defcustom aug-buffer "*Augmented Tree*"
  "Name of the output buffer.")

(defcustom aug-parent-link-text "Go up"
  "Text for the link to the parent directory in the output buffer.")

(defcustom aug-sidebar-width 30
  "Size of the sidebar window.")

(defcustom aug-sidebar-enlarged-width nil
  "Size of the sidebar window.")

(defcustom aug-balance-windows-before-resizing t
  "Instructs to balance windows before resizing when using the sidebar
feature.

Note: The window balancing is done *before* the sidebar resizing. This
leads to the windows not appearing really balanced after resizing the
sidebar. With reasonable screen size, however, the windows will look a lot
more decent with balancing before resizing.")

(defcustom aug-window-configuration-register ?1
  "Register to store the window layout for sidebar resizing in.")

(defcustom aug-smart-resizing t
  "Indicates if, when using the sidebar, other windows in the same frame
should be balanced by compensating (``ignoring'') the sidebar while doing
so.")

(defcustom aug-indentation-prefix "  "
  "Prefix to use when indenting lines in the tree representation.")

(defcustom aug-indentation-delimiter-prefix "| "
  "Prefix to use when toggling delimiter for lines in the tree
representation.")

(defcustom aug-file-prefix ""
  "Prefix for file names in the tree representation.")

(defcustom aug-dir-prefix ""
  "Prefix for dir names in the tree representation.")

(defcustom aug-ignore-list (list ".git" ".svn")
  "List of file and directory names to ignore even when dotfiles are
shown.")

(defcustom aug-hidden-marker " [+]"
  "Marker which is attached to a directory name which has its current
subtree hidden.")


;;=========================================================================
;; Functions
;;=========================================================================

(defun aug-traverse-dir (dir &optional no-dotfiles sort-predicate reverse)
  "Traverse directory DIR and return a nested hash table with directory and
file names as keys and either another hash table as value for directories
or the string \"FILE\" as value for files

DIR - String path of the directory to create a hash table for.
NO-DOTFILES - Indicates if dotfiles should be included or left out. If `t',
              dotfiles will be excluded. For any other value dotfiles will
              be included.
SORT-PREDICATE - Sor predicate which is called for the directory DIR and
                 all its subdirectories. It is called with the full path
                 names for the current file or directory and the full path
                 name for the next file or directory. Thus, it may be
                 necessary to call `file-name-nondirectory' or similar
                 methods on each file/directory name.
REVERSE - If `t', reverse the sort order after sorting using
         `SORT-PREDICATE'or the default.

Returns a nested hash tables with file or directory names as keys and with
either nested hash tables (for directories) or the string \"FILE\" (for
files) as values."
  (interactive "D")
  (let ((dir-tree (make-hash-table))
        (current-dir (make-hash-table))
        (thing-to-put nil)
        (sort-predicate (or sort-predicate nil))
        (reverse (or reverse nil)))
    (if (file-directory-p dir)
        (mapc (lambda (dir-or-file)
                (if (and (file-directory-p dir-or-file))
                    (setq thing-to-put (aug-traverse-dir dir-or-file
                                                         (or no-dotfiles
                                                             nil)
                                                         sort-predicate
                                                         reverse))
                    (setq thing-to-put "FILE"))
                ;; For RegEx matching one could also use the MATCH
                ;; argument for `directory-files', but this solution
                ;; gives flexibility for alternative file/dir
                ;; ex-/inclusions/sorting etc.
                (if (not (and (eq no-dotfiles nil)
                              (not (eq (string-match-p
                                        "^\\.\\w.*$"
                                        (file-name-nondirectory
                                         dir-or-file)) nil))))
                    ;; `member' returns non-nil, not `t'.
                    (if (equal nil (member
                                    (file-name-nondirectory
                                     dir-or-file)
                                    aug-ignore-list))
                        (puthash dir-or-file thing-to-put dir-tree))))
              (progn
                (defvar dir-files nil)
                (if sort-predicate
                    (setq dir-files (sort (directory-files dir t "[^.]+" t)
                                          sort-predicate))
                    (setq dir-files (directory-files dir t "[^.]+")))
                (if reverse
                    (reverse dir-files)
                    dir-files))))
    dir-tree))

(defun aug-pretty-print-hash-table (table &optional output-string
                                          indentation-level
                                          file-or-dir-name-only)
  "Pretty print the nested hash table TABLE created by `aug-traverse-dir'.

OUTPUT-STRING - The string to append the pretty printed version to. It is
                handed to every recursive call of this function.
INDENTATION-LEVEL - Integer representing the indentation level for the
                    current element. It is handed to every recursive call
                    of this function.
FILE-OR-DIR-NAME-ONLY - If `t', the file or directory name for each file or
                        directory is printed without its according
                        directory. Any other value will cause the full path
                        to be printed.

Returns a pretty-printed string representation of the hash-table TABLE."
  (maphash (lambda (key value)
             ;; Creating a new variable each time should not be a problem
             ;; in reality since the tree cannot be used for massive file
             ;; system hierarchies anyway.
             (defvar blanks "")
             (setq blanks "")
             (dotimes (i indentation-level)
               (setq blanks (format "%s%s" blanks
                                    aug-indentation-prefix)))
             (if (hash-table-p value)
                 (progn

                   (setq output-string
                         (aug-pretty-print-hash-table
                          value (format "%s\n%s%s%s" output-string blanks
                                        aug-dir-prefix
                                        (if file-or-dir-name-only
                                            (file-name-nondirectory key)
                                            key))
                          (1+ indentation-level)
                          (or file-or-dir-name-only nil))))
                 (setq output-string
                       (format "%s\n%s%s%s" output-string blanks
                               aug-file-prefix
                               (if file-or-dir-name-only
                                   (file-name-nondirectory key)
                                   key)))))
           table)
  output-string)

(defun aug-generate-tree-string (&optional sorting-type reverse
                                           no-dotfiles)
  "Generate the string representation of the directory tree which roots in
the current directory.

SORTING-TYPE - The string `code-point' or `lexicographically'. `code-point'
               will sort by code point (upper anc lower case strings are
               sorted alphabetically, but separately, with upper case
               strings first). `lexicographically' will sort
               lexicographically. Default: `lexicographically'. Any other
               value will force lexiographical sorting.
REVERSE - If `t' reverse the sorting order after applying the sorting order
          specified by `sorting-order' or by default.
NO-DOTFILES - If `t', dotfiles will not be included in the output tre.
              Default vaue: `t'.

Generate a string reprensentation of the file hierarchy (without text
properties)."
  (let ((current-dir (replace-regexp-in-string
                      "\/+$" "" (file-truename default-directory)))
        (dir-table (make-hash-table)))
    ;; Include the current directory as the single root element.
    (puthash current-dir
             (aug-traverse-dir
              current-dir (or no-dotfiles nil)
              ;; Use a custom sort predicate since `string<'/`string-lessp'
              ;; and `compare-strings' use code point sorting which will
              ;; cause upper case files/dirs to be sorted individually in a
              ;; lexicographically so that they will be displayed before
              ;; lower case ones in the final output. It is more intuitive
              ;; for lexicographically sorted files/dirs to be
              ;; case-insensitive.
              (if (equal sorting-type "code-point")
                  'string<
                  (lambda (a b)
                    (let ((a (downcase (file-name-nondirectory a)))
                          (b (downcase (file-name-nondirectory b))))
                      (if (< (compare-strings a 0 (length a) b 0 (length
                                                                  b))
                             0)
                          t
                          nil))))
              (or reverse nil))
             dir-table)
    ;; Pretty-print the tree.
    (aug-pretty-print-hash-table dir-table "" 0)))


;; Path: "\\(\\.*/\\)\\(.*\\)$"
;; Everything before the path: "^[^\\(\\.*/\\)\\(.*\\)$]*"
(defun aug-augment-line (line)
  "Make a proper file URL (`file://'-prefixed) from the string LINE and
return it as a string.

LINE - A string representing a Unix file path.

Returns a string represnting a file URL."
  (let ((augmented-line ""))
    (replace-regexp-in-string "\\(\\.*/\\)\\(.*\\)$"
                              (lambda (match)
                                (concat "file://" match)))))

(defun aug-insert-tree (tree-string-lines)
  "Insert an augmented tree created from TREE-STRING-LINES in the currently
selected buffer.

TREE-STRING-LINES - A list of strings produced from splitting the output of
                    calling the `tree' shell command on newlines (`\n').

Returns nothing, inserts a string in the current buffer."
  (augmented-tree-mode)
  ;; Set up invisibility conditions. Important: This has to be run after
  ;; starting `augmented-tree-mode' since the mode initialization would
  ;; override it again.
  (with-current-buffer (current-buffer)
    (add-to-invisibility-spec aug-invisibility-marker)
    (remove-from-invisibility-spec t)
    (setq buffer-invisibility-spec (list aug-invisibility-marker)))
  ;; Make buffer writable.
  (toggle-read-only -1)
  (erase-buffer)
  ;; Insert parent directory link.
  ;; (let* ((s "./this/is/a/path/")
  ;;        (match-index (string-match "\/[^\/]*.?$" s)))
  ;;   (substring s 0 match-index))
  (let* ((current-path default-directory)
         (match-index (string-match "\/[^\/]*.?$" current-path))
         (parent-path (substring current-path 0 match-index)))
    (insert-text-button
     (format "%s: %s"  aug-parent-link-text parent-path)
     'action (lambda (x)
               ;; Set `default-directory' since the `Go up' (parent link)
               ;; link depends on it.
               (setq default-directory (button-get x 'parent-path))
               ;; Display the subtree for the current element.
               (aug-tree nil (format "%s %s" aug-tree-command
                                     (button-get x 'parent-path))))
     'face 'font-lock-string-face
     'parent-path parent-path))
  (insert "\n\n")
  ;; Insert tree element links.
  (mapc (lambda (line)
          ;; `cruft': everything before the path
          ;; `path': the path itself
          (if (string-match "^\s*.\s*$" line)
              ;; Insert initial newline.
              (insert (format "%s\n" line))
              (progn
                ;; Insert the cruft.
                (let ((cruft-match-index
                       (string-match "^[^\\(\\.*/\\)\\(.*\\)$]*"
                                     line)))
                  (unless (eq cruft-match-index nil)
                    (insert  (substring line cruft-match-index
                                        (match-end 0)))))
                ;; Insert the path.
                (let ((path-match-index (string-match
                                         "\\(\\.*/\\)\\(.*\\)$"
                                         line)))
                  (unless (eq path-match-index nil)
                    (let* ((file-path (substring line path-match-index
                                                 nil))
                           ;; Distinguish between files, directories and
                           ;; symlinks.
                           (thing-face (if (file-directory-p file-path)
                                           'font-lock-function-name-face
                                           (if (file-symlink-p file-path)
                                               'font-lock-constant-face
                                               'font-lock-keyword-face))))
                      (insert-text-button
                       (file-name-nondirectory file-path)
                       'action (lambda (x)
                                 (progn
                                   ;; Shrink the sidebar, if it is
                                   ;; currently enlarged.
                                   (if aug-sidebar-enlarged-p
                                       (call-interactively
                                        'aug-toggle-preview))
                                   ;; Actually open the file/dir
                                   (find-file (button-get x 'file-path))))
                       ;; Add some color.
                       'face thing-face
                       'file-path file-path
                       ;; Initialize `invisible' property with empty list.
                       ;; If a line is set to be invisible,
                       ;; `aug-invisibility-marker' is added to the list.
                       ;; If there is no `aug-invisibility-marker' lef in
                       ;; the list, the line is visible. Initializing the
                       ;; whole is not necessary since it is ensured that
                       ;; the cursor is always on the button when examining
                       ;; a lines visibility.
                       'invisible '(:init)
                       ;; Initialize the visibility marker for a directory
                       ;; (or file). This is `t' when the subtree of a dir
                       ;; (or file) is currently hidden and `nil' if it is
                       ;; currently visible.
                       'subtree-hidden nil
                       'aug-thing-type (if (file-directory-p file-path)
                                           aug-thing-type-directory
                                           aug-thing-type-file)))
                    (insert "\n"))))))
        tree-string-lines)
  ;; If the `tree' system command was used, it would be sensible to include
  ;; a newline at the end so that the cursor navigation will move to the
  ;; top as soon as it has reached the bottom of the buffer and vice versa.
  ;;
  ;; (insert "\n")
  ;; Use a buffer-local keymap.
  (use-local-map augmented-tree-mode-map)
  ;; Make buffer read-only.
  (toggle-read-only 1)
  ;; Move the cursor to the begiing of the buffer.
  (beginning-of-buffer)
  (linum-mode 1)
  (setq truncate-lines t))

;; Alternative way to prevent multiple window splits (Warning: Does
;; require manual initial split).
;; (defun aug-no-split-window()
;;   (interactive)
;;   nil)

(defun aug-initialize ()
  "Initialize Augmented Tree."
  ;; Alternative way to prevent multiple window splits (Warning: Does
  ;; require manual initial split).
  ;; (setq split-window-preferred-function 'aug-no-split-window)
  ;;
  ;; Use an unreasonable width/height as split treshold so that the windows
  ;; will never be resplit for previews.
  (setq split-width-threshold 9999999)
  (setq split-height-threshold 9999999))

(defun aug-tree (&optional current-window tree-command sorting-type
                           reverse no-dotfiles)
  "Create augmented output for the `tree' shell command after calling it.
The tree output is augmented by `clickable' buttons for every directory or
file in the tree.

CURRENT-WINDOW - Window where the augmented tree output should be
                 inserted.
TREE-COMMAND - `tree' shell command to be called before augmenting its
               output. Default: `aug-tree-command'. The command should at
               least include the following options:
               + `-f': Print full path prefix for each file.
               + `-n': Turn colorization off always.
               + `--charset='ASCII': Use ASCII characters only (This may
                 lead to problems with non-ASCII file/dir names, but then
                 again, it may be considered a good habit to not use them
                 for file/dir names anyway). You can try to omit it, but
                 you will have to live with the consequences.
SORTING-TYPE - String specifying the sorting type. For available strings
               see: `aug-generate-tree-string'.
REVERSE - If `t', reverse the sort order after sorting using.
         `SORT-PREDICATE'or the default. Default: `nil'.
NO-DOTFILES - If `t', dotfiles will not be included in the output tree.
              Default: `t'.

Returns nothing, creates augmeted tree output and displays it in a buffer."
  (interactive)
  (let* ((tree-command (or tree-command (format "%s %s" aug-tree-command
                                                default-directory)))
         (tree-string-lines (split-string (aug-generate-tree-string
                                           (or sorting-type nil)
                                           (or reverse nil)
                                           (or no-dotfiles nil))
                                          ;; This is a clever point to
                                          ;; dispatch to a shell command
                                          ;; instead
                                          ;;
                                          ;; (shell-command-to-string
                                          ;;  tree-command)
                                          "\n")))
    (aug-initialize)
    (if current-window
        (progn
          (display-buffer (get-buffer-create aug-buffer) t)
          (with-selected-window  (get-buffer-window aug-buffer)
            (aug-insert-tree tree-string-lines))
          (switch-to-buffer-other-window aug-buffer))
        (progn
          (switch-to-buffer aug-buffer)
          (aug-insert-tree tree-string-lines)))
    (aug-remove-overlays)))

(defun aug-tree-other-window (input)
  "Like `aug-tree', but display the output in a window other than the
currently selected one.

Returns nothing, creates augmeted tree output and displays it in a buffer
in another window."
  (interactive "P")
  (setq aug-in-other-window t)
  (aug-tree t))

;; It is also possible to use `point-at-bol' and `point-at-eol', but then a
;; manual check is still necessary to determin if the point is really at
;; the beginning/end of the line using `beginning-of-line'/`end-of-line'.
(defun aug-get-current-line-length ()
  "Calculate the current length of the line where point is currently
located.

Returns the length of the current line as an integer."
  (let ((line-start)
        (line-end))
    (save-excursion
      (beginning-of-line)
      (setq line-start (point))
      (end-of-line)
      (setq line-end (point)))
    (length (buffer-substring line-start line-end))))

(defun aug-line-move (n)
  "Moves point N lines down if N is positive and up N lines if it is
negative even for invisible lines. This compensates for
`next-line'/`line-move' not working correctly even if `line-move-visual'
and `line-move-ingnore-invisible' are set to be `nil'.

N - Integer number indicating how many lines to move point. May have a
    negative prefix.

Returns nothing."
  (let ((n (or n 1)))
    (if (wholenump n)
        (dotimes (i n)
          (end-of-line)
          (forward-char))
        (dotimes (i (abs n))
          (beginning-of-line)
          (backward-char)))))

(defun aug-next-line (input)
  "Move the cursor to the next line in `aug-buffer'. When at the end of the
buffer, jump to the first line of the buffer. Skip empty lines.

Returns nothing."
  (interactive "P")
  (line-move 1)
  ;; If the cursor is at the end of the buffer, jump to the beginning of
  ;; the buffer.
  (if (= (point) (point-max))
      (beginning-of-buffer))
  (move-end-of-line nil)
  ;; Skip single empty lines.
  (if (not (= (aug-get-current-line-length) 0))
      (backward-char)
      (line-move 1)))

(defun aug-previous-line (input)
  "Move the cursor to the previous line in `aug-buffer'. When at the
beginning of the buffer, jump to the last line of the buffer. Skip empty
lines.

Returns nothing."
  (interactive "P")
  ;; If the cursor is at the beginning of the buffer, jump to the end of
  ;; the buffer.
  (move-beginning-of-line nil)
  (if (= (point) (point-min))
      (end-of-buffer))
  (line-move -1)
  (move-end-of-line nil)
  ;; Skip single empty lines.
  (if (not (= (aug-get-current-line-length) 0))
      (backward-char)
      (line-move -1)))

(defun aug-next-directory (input)
  "Move the cursor to the next directory in `aug-buffer'. When at the end
of the buffer, jump to the first line of the buffer. Skip empty lines.

Returns nothing."
  (interactive "P")
  (let ((current-aug-thing-type nil))
    (while (not (equal current-aug-thing-type aug-thing-type-directory))
      (call-interactively 'aug-next-line)
      (setq current-aug-thing-type (get-text-property (point)
                                                      'aug-thing-type)))))

(defun aug-previous-directory (input)
  "Move the cursor to the previous directory in `aug-buffer'. When at the
beginning of the buffer, jump to the last line of the buffer. Skip empty
lines.

Returns nothing."
  (interactive "P")
  (let ((current-aug-thing-type nil))
    (while (not (equal current-aug-thing-type aug-thing-type-directory))
      (call-interactively 'aug-previous-line)
      (setq current-aug-thing-type (get-text-property (point)
                                                      'aug-thing-type)))))

(defun aug-kill-buffer (input)
  "Kill `aug-buffer', even when the cursor is currently not in that buffer.
This is a convenient way to remove the sidebar.

Returns nothing."
  (interactive "P")
  (setq aug-in-other-window nil)
  (kill-buffer aug-buffer))

(defun aug-path-of-current-thing ()
  "Get the file path that the current button is pointing to.

Returns nothing."
  (get-text-property (point) 'file-path))

(defun aug-open-current-thing-read-only (input)
  "Open the file or directory that the current button is pointing to in the
current window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (find-file-read-only (aug-path-of-current-thing)))

(defun aug-open-current-thing-in-view-mode (input)
  "Open the file or directory that the current button is pointing to in the
current window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  ;; If there is only one window, use the current one for the preview,
  ;; otherwise use another one. This is more intuitive.
  (if (equal aug-in-other-window t) ;; (equal (length (window-list)) 1)
      (find-file-other-window (aug-path-of-current-thing))
      (find-file (aug-path-of-current-thing)))
  (view-mode 1)
  ;; Override the `q' key to kill the current buffer and to go back to the
  ;; Augmented Tree buffer.
  (define-key view-mode-map (kbd "q")
    (lambda ()
      (interactive)
      (kill-buffer)
      (unless (equal (select-window (get-buffer-window aug-buffer)) nil)
        (switch-to-buffer aug-buffer)))))

(defun aug-preview-current-line (input)
  "Preview the current file or directory in a read-only buffer.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (aug-preview-thing))

(defun aug-preview-next-line (input)
  "Preview the file or directory in the next line in a read-only buffer and
move point to the next line.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (call-interactively 'aug-next-line)
  (aug-preview-thing))

(defun aug-preview-previous-line (input)
  "Preview the file or directory in the previous line in a read-only buffer
and move point to the previous line.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (call-interactively 'aug-previous-line)
  (aug-preview-thing))

(defun aug-preview-thing ()
  "Open the current file or directory in a read-only buffer in the current
window.

Returns nothing."
  (with-selected-window
      (get-buffer-window (find-file-read-only-other-window
                          (aug-path-of-current-thing)))
    ;; Do nothing since the only desired effect of using
    ;; `with-selected-window' is having the cursor jump back to the
    ;; original buffer.
    (progn)))

(defun aug-open-thing-other-window (input)
  "Open the current file or directory buffer in another window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (get-buffer-window (find-file-other-window
                      (aug-path-of-current-thing))))

(defun aug-open-thing-read-only-other-window (input)
  "Open the current file or directory in a read-only buffer in another
window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (get-buffer-window (find-file-read-only-other-window
                      (aug-path-of-current-thing))))

(defun aug-show-subtree (input)
  "Call `aug-tree' for the current file or directory and display it in
`aug-buffer'.

Returns nothing."
  (interactive "P")
  ;; Manually set the local variable `default-directory' since the `Go up'
  ;; (parent directory) link depends on it.
  ;;
  ;; If, in the future, this should evolve, here is a starting point:
  ;; (let* ((current-path (aug-path-of-current-thing))
  ;;        (match-index (string-match "\/[^\/]*.?$" current-path))
  ;;        (parent-path (substring current-path 0 match-index)))
  ;;   (setq default-directory current-path))
  (setq default-directory (aug-path-of-current-thing))
  (aug-tree nil (format "%s %s" aug-tree-command
                        (aug-path-of-current-thing)))
  (aug-remove-overlays))

(defun aug-go-to-parent (input)
  "Call `aug-tree' for the parent directory and display it in
`aug-buffer'.

Returns nothing."
  (interactive "P")
  ;; Manually set the local variable `default-directory' since the `Go up'
  ;; (parent directory) link depends on it.
  (let* ((current-path default-directory)
         (match-index (string-match "\/[^\/]*.?$" current-path))
         (parent-path (substring current-path 0 match-index)))
    (setq default-directory parent-path)
    ;; Display the subtree for the current element.
    (aug-tree nil (format "%s %s" aug-tree-command parent-path))
    (aug-remove-overlays)))

(defun aug-go-to-parent-with-cursor-on-previous (input)
  "Call `aug-tree' for the parent directory and display it in
`aug-buffer'. Put the cursor on the line of the previously shown file or
directory..

Returns nothing."
  (interactive "P")
  (let ((previous default-directory)
        (stop nil))
    (call-interactively 'aug-go-to-parent)
    (beginning-of-buffer)
    ;; Inspect the text properties of each character in the current buffer.
    ;; If it has the `file-path' text property and its value matches the
    ;; value of the `default-directory' variable in the previous tree, stop
    ;; searching and move the cursor there. Fail silently (do nothing).
    (while (and (not stop) (not (equal (point) (point-max))))
      (forward-char)
      (if (equal (get-text-property (point) 'file-path) previous)
          (progn
            (setq stop t)
            (end-of-line)
            (backward-char)
            (recenter))))
    (unless stop
      (beginning-of-buffer))
    (aug-remove-overlays)))

(defun aug-focus-window (input)
  "Make the window which currently displays `aug-buffer' the currently
selected window.

Returns nothing."
  (interactive "P")
  (switch-to-buffer-other-window aug-buffer))

;; Sidebar

(defun aug-tree-sidebar (input)
  "Produce an augmented tree and display it in a dedicated sidebar window.
The sidebar window has special properties:
- It cannot be deleted as long as it displays `aug-buffer'.
- It cannot be used to display anything else than the `aug-buffer'.
- It provides a preview toggle command which will temporarily shrink other
windows to increase its own screen estate.
- It provides a resize command to be resized to its dedicated width if it
changed in the meanwhile (Side effect: Other windows get balanced in a smart
way, if specified.).

Returns nothing."
  (interactive "P")
  ;; Prevent the Augmented Tree window from being closed.
  (defadvice delete-other-windows (around aug-delete-other-window-advice
                                          activate)
    "Prevents window from being deleted."
    ;; (get-buffer-window aug-buffer)
    (let* ((aug-window (get-buffer-window aug-buffer))
           (aug-active-p (and aug-window (window-live-p aug-window))))
      (if aug-active-p
          (let ((current-window (selected-window)))
            (dolist (win (window-list))
              (when (and (window-live-p win)
                         (not (eq current-window win))
                         (not (window-dedicated-p win))
                         (not (eq win aug-window)))
                (delete-window win))))
          ad-do-it)))
  (split-window-horizontally)
  (aug-tree nil)
  ;; Resize the sidebar window.
  (aug-resize-sidebar)
  ;; Prevent any other buffer from showing up in the sidebar window.
  (set-window-dedicated-p (selected-window) (not current-prefix-arg)))

(defun aug-resize-sidebar (&optional new-width)
  "Resize the sidebar window displaying `aug-buffer' to
`aug-sidebar-width' or NEW-WIDTH instead, if specified.

NEW-WIDTH - Integer width to resize the sidebar window to (must be in a
reasonable range regarding the size of the frame/screen/other windows, of
course.

Returns nothing."
  (let ((current-width (window-width))
        (new-width (or new-width aug-sidebar-width)))
    (if (< current-width new-width)
        (shrink-window-horizontally (- current-width new-width)))
    (if (> current-width aug-sidebar-width)
        (enlarge-window-horizontally (- aug-sidebar-width
                                        current-width)))))

(defun aug-resize (input &optional smart-resizing)
  "Resize the sidebar window displaying `aug-buffer'. Be smart about
resizing, if SMART-RESIZING has the value `t'.

SMART-RESIZING - Boolean specifying if the resizing should balance other
windows in a smart manner so that they have relatively equal size.

Returns nothing."
  (interactive "P")
  (let ((smart-resizing (or smart-resizing t)))
    (if aug-balance-windows-before-resizing
        (balance-windows))
    (aug-resize-sidebar)
    ;; Compensate for the tree buffer being in the way while resizing so
    ;; that adjacent horizontal windows are evenly resized by adding
    ;; the width of the sidebar (which was in the way during window
    ;; balancing) proportionally as horizontal space to the last window in
    ;; the window list. This is not accurate in heavily-nested/vertically
    ;; nested window setups, but it covers popular editing setups (one or
    ;; more code buffers next to each other with the sidebar on the far
    ;; left side).
    (if (and aug-smart-resizing smart-resizing)
        (let* ((aug-window (get-buffer-window aug-buffer))
               (win-list (window-list nil 1 aug-window)))
          (select-window (nth (- (length win-list) 1) win-list))
          (enlarge-window-horizontally
           (/ (/ (frame-width) (length win-list)) (+ (length win-list) 1)))
          (select-window aug-window)))))

(defun aug-toggle-preview (input)
  "Toggle the preview for the sidebar window displaying `aug-buffer'. If
the preview is toggled on, the sidebar will save the current window
configuration and shrink all other windows to their minimally required
size. If it is turned off again, the previous window configuration will be
with two convenient differences:
- The scroll position of `aug-buffer' will be kept.
- The cursor will stay on the current line.
Keeping the scroll and cursor positions is more intuitive than having the
cursor jump to the pre-preview position.

Returns nothing."
  (interactive "P")
  ;; Heuristic: take the number of all windows and reserve
  ;; (`window-min-width' + 1) * 2 for each so that other windows get pushed
  ;;to the side, but will not automatically closed (which is Emacs default
  ;; behavior).
  (if aug-sidebar-enlarged-p
      ;; End preview
      (progn
        ;; Save the position of the first line in the sidebar window.
        (setq aug-last-window-start-in-sidebar (window-start))
        ;; Save the pointer position in the sidebar window.
        (setq aug-last-pointer-position-in-sidebar (point))
        ;; Restore the window layout.
        (jump-to-register aug-window-configuration-register)
        (setq aug-sidebar-enlarged-p nil)
        ;; Recenter the window.
        (goto-char aug-last-window-start-in-sidebar)
        (recenter 0)
        ;; Put the cursor back to its original position.
        (goto-char aug-last-pointer-position-in-sidebar))
      ;; Start preview
      (progn
        ;; Save the window configuration.
        (window-configuration-to-register
         aug-window-configuration-register)
        ;; Actually resize the sidebar.
        (aug-resize-sidebar
         (or aug-sidebar-enlarged-width
             (- (frame-width) (* (+ 1 window-min-width)
                                 (- (length (window-list)) 1)))))
        (setq aug-sidebar-enlarged-p t))))

(defun aug-remove-overlays ()
  "Remove all overlays used by Augmented Tree.

Returns nothing."
  (remove-overlays (point-min) (point-max) 'after-string
                   aug-hidden-marker))

(defun aug-update (input)
  "Update the current tree in case files/directories have been moved, added
or removed, The cursor jumps to the beginning of the buffer.

Returns nothing."
  (interactive "P")
  (aug-tree nil (format "%s %s" aug-tree-command default-directory))
  ;; Remove any potential overlay residues.
  (aug-remove-overlays))

(defun aug-reverse (input)
  "Reverse the sort order for the current sorting type. For more info on
sorting types see: `aug-generate-tree-string'.

Returns nothing."
  (interactive "P")
  (let ((reverse (if (equal aug-currently-reversed t) nil
                     t)))
    (aug-tree nil (format "%s %s" aug-tree-command default-directory)
              aug-current-sorting-type reverse)
    (if (equal reverse t)
        (progn
          (setq aug-currently-reversed t)
          (message "Reversed sort order"))
        (progn
          (setq aug-currently-reversed nil)
          (message "Normal sort order")))))

(defun aug-cycle-sorting (input)
  "Cycle through the available sorting orders. For more info on sorting
types see: `aug-generate-tree-string'. This setting will persist when
moving to parent or child directories.

Returns nothing."
  (interactive "P")
  (let ((new-sorting-type))
    (setq new-sorting-type (pop aug-sorting-types))
    (setq aug-sorting-types (append aug-sorting-types (list
                                                       new-sorting-type)))
    (setq aug-current-sorting-type new-sorting-type)
    (aug-tree nil (format "%s %s" aug-tree-command default-directory)
              aug-current-sorting-type aug-currently-reversed))
  (message (format "New sort order: %s"
                   aug-current-sorting-type)))

(defun aug-toggle-indentation-prefix (input)
  "Toggle the indentation prefix on/off. The customizable prefix will be
used. With a prefix, indentation levels are easier to distinguish.

Returns nothing."
  (interactive "P")
  (let ((blanks ""))
    (if (not (equal aug-indentation-delimiter-prefix
                    aug-indentation-prefix))
        (progn
          (setq aug-previous-indentation-prefix aug-indentation-prefix)
          (setq aug-indentation-prefix aug-indentation-delimiter-prefix))
        (setq aug-indentation-prefix aug-previous-indentation-prefix))
    (aug-tree nil (format "%s %s" aug-tree-command default-directory)
              aug-current-sorting-type aug-currently-reversed)))

(defun aug-toggle-dotfiles (input)
  "Toggle displaying dotfiles/dirs in the tree.

Returns nothing."
  (interactive "P")
  (if (equal aug-hide-dotfiles t)
      (progn
        (aug-tree nil (format "%s %s" aug-tree-command default-directory)
                  aug-current-sorting-type aug-currently-reversed
                  aug-hide-dotfiles)
        (setq aug-hide-dotfiles nil)
        (message "Dotfiles now shown"))
      (progn
        (aug-tree nil (format "%s %s" aug-tree-command default-directory)
                  aug-current-sorting-type aug-currently-reversed
                  aug-hide-dotfiles)
        (setq aug-hide-dotfiles t)
        (message "Dotfiles now hidden"))))

(defun aug-toggle-current-line-visibility (make-invisible start end)
  "Toggle the visibility of the string between START and END accoring to
MAKE-VISIBLE by appending or removing `aug-invisibility-marker' to the
list comprisign the 'invisible text property of the string.

Check the setting of `buffer-invisibility-spec' for more details.

Note: Any line movement in this function should be done using
`aug-line-move' instead of `line-move' or `next-line'. See `aug-line-move'
for more details.

START - Point in the buffer.
END - Point in the buffer which has to be located *after* START. There are
      no guarantees for any violations of this constraint.
MAKE-VISIBLE - Indicates if the string should be made visible (`t') or if
               it should be made invisible (any non-`t' value).

Returns nothing."
  ;; Allow moving to invisible lines.
  (setq line-move-ignore-invisible nil)
  (setq line-move-visual nil)
  (let ((previous-invisibility-list (get-text-property (point)
                                                       'invisible)))
    ;; Add another invisibility marker to the list.
    (progn
      (if (equal make-invisible t)
          ;; Make text invisible.
          (progn
            (setq previous-invisibility-list
                  (append (list aug-invisibility-marker)
                          previous-invisibility-list))
            (put-text-property start end 'invisible
                               previous-invisibility-list))
          ;; Make text visible.
          (progn
            (if (> (length previous-invisibility-list) 1)
                (progn
                  (pop previous-invisibility-list)))
            (put-text-property start end 'invisible
                               previous-invisibility-list)))))
  ;; Disallow moving to invisible lines.
  (setq line-move-ignore-invisible t)
  (setq line-move-visual t))

(defun aug-next-line-is-nested ()
  "Returns `t' if the next line in the treee has a nested file or
directory and `nil' otherwise. This uses `save-excursion' and does put
point back to its previous position.

Returns `t' if there is a subtree, `nil' otherwise."
  (let ((current-file-path (save-excursion
                             (end-of-line)
                             (backward-char 1)
                             (file-name-directory
                              (directory-file-name (get-text-property
                                                    (point)
                                                    'file-path))))))
    (save-excursion
      (aug-line-move 1)
      (end-of-line)
      (backward-char 1)
      (> (length (file-name-directory
                  (directory-file-name (get-text-property (point)
                                                          'file-path))))
         (length current-file-path)))))

(defun aug-toggle-subtree-visibility (input)
  "Toggle the visibility of the directory point is currently on. If the
point is not currently on the name of a directory, this function will fail
graciously. A hidden directory is suffixed with an `afters-string' overlay
consisting of `aug-hidden-marker'. Hidden directories within other hidden
directories will stay hidden even if the outer directories are visible
again.

Note: Any line movement in this function should be done using
`aug-line-move' instead of `line-move' or `next-line'. See `aug-line-move'
for more details.

Returns nothing."
  ;; Important note: Do not use `aug-next-line'/`aug-previous-line'/
  ;; `line-move'/`next-line'/`previous-line'/etc. since they do not work
  ;; properly (read: `not at all') with lines which have the `invisible'
  ;; property set (including the newline). Instead, rely on
  ;; `aug-line-move'/`beginning-of-line'/`end-of-line'/`forward-char'/
  ;; `backward-char'.
  (interactive "P")
  ;; Allow moving to invisible lines.
  (setq line-move-ignore-invisible nil)
  (setq line-move-visual nil)
  (let ((start (save-excursion
                 (progn
                   (beginning-of-line)
                   (point))))
        (end (save-excursion
               (progn
                 (aug-line-move 1)
                 (point))))
        (current-file-path (get-text-property (point) 'file-path))
        (current-subtree-hidden (get-text-property (point)
                                                   'subtree-hidden))
        (current-overlay nil))
    (save-excursion  ; Keep the cursor on the current directory name
      (toggle-read-only -1)
      ;; Exclude files since only directories can have subtrees. This also
      ;; excludes the `Go up' link and other potential future user
      ;; interface elements. It also prevents glitches in case the user
      ;; runs `aug-toggle-subtree-visibility' while the cursor is not
      ;; exactly on a clickable text.
      (if (and (equal (get-text-property (point) 'aug-thing-type)
                      aug-thing-type-directory)
               (aug-next-line-is-nested))
          ;; Check if the subtree is already hidden.
          (if (equal current-subtree-hidden t)
              ;; Make the subtree visible.
              (progn
                (aug-line-move 1)
                (end-of-line)
                (backward-char)
                (while (and (not
                             (equal (file-name-directory
                                     (directory-file-name
                                      (concat "" (get-text-property
                                                  (point) 'file-path))))
                                    current-file-path))
                            (> (length (file-name-directory
                                        (directory-file-name
                                         (concat "" (get-text-property
                                                     (point)
                                                     'file-path)))))
                               (length current-file-path)))
                  (let ((line-start (save-excursion
                                      (beginning-of-line)
                                      (point)))
                        (line-end (save-excursion
                                    (aug-line-move 1)
                                    (beginning-of-line)
                                    (point))))
                    (remove-overlays start end 'after-string
                                     aug-hidden-marker)
                    (save-excursion
                      (aug-toggle-current-line-visibility
                       (not current-subtree-hidden) line-start line-end))
                    (aug-line-move 1)
                    (end-of-line)
                    (backward-char 1))))
              ;; Make the subtree invisible.
              (progn
                (aug-line-move 1)
                (end-of-line)
                (backward-char 1)
                (setq current-overlay (make-overlay start (1- end)))
                (overlay-put current-overlay 'after-string
                             aug-hidden-marker)
                (while (and (not
                             (equal (file-name-directory
                                     (directory-file-name
                                      (concat "" (get-text-property
                                                  (point) 'file-path))))
                                    current-file-path))
                            (> (length (file-name-directory
                                        (directory-file-name
                                         (concat "" (get-text-property
                                                     (point)
                                                     'file-path)))))
                               (length current-file-path)))
                  (let  ((line-start (save-excursion
                                       (beginning-of-line)
                                       (point)))
                         (line-end (save-excursion
                                     (aug-line-move 1)
                                     (beginning-of-line)
                                     (point))))
                    (unless (equal (get-text-property (point)
                                                      'subtree-hidden) t))
                    (aug-toggle-current-line-visibility
                     (not current-subtree-hidden) line-start line-end)
                    (aug-line-move 1)
                    (end-of-line)
                    (backward-char 1))))))
      (put-text-property start end 'subtree-hidden
                         (not current-subtree-hidden))
      (toggle-read-only 1)))
  ;; Disallow moving to invisible lines.
  (setq line-move-ignore-invisible t)
  (setq line-move-visual t))

(defun aug-open-files-and-dirs-in-region (input)
  "Open all files and directories in the current region.

Returns nothing."
  (interactive "P")
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beginning)
      (call-interactively 'aug-previous-line)
      (call-interactively 'aug-next-line)
      (while (< (point) end)
        (message (format "OPEN: %s" (get-text-property (point)
                                                       'file-path)))
        (find-file-noselect (get-text-property (point) 'file-path))
        (call-interactively 'aug-next-line)))))

(defun aug-open-files-in-region (input)
  "Open all files in the current region.

Returns nothing."
  (interactive "P")
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beginning)
      (call-interactively 'aug-previous-line)
      (call-interactively 'aug-next-line)
      (while (< (point) end)
        (if (equal (get-text-property (point) 'aug-thing-type)
                   aug-thing-type-file)
            (find-file-noselect (get-text-property (point) 'file-path)))
        (call-interactively 'aug-next-line)))))

(defun aug-open-dirs-in-region (input)
  "Open all directories in the current region.

Returns nothing."
  (interactive "P")
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beginning)
      (call-interactively 'aug-previous-line)
      (call-interactively 'aug-next-line)
      (while (< (point) end)
        (if (equal (get-text-property (point) 'aug-thing-type)
                   aug-thing-type-directory)
            (find-file-noselect (get-text-property (point) 'file-path)))
        (call-interactively 'aug-next-line)))))


;;=========================================================================
;; Local keymap
;;=========================================================================

(defvar augmented-tree-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Cursor movement
    (define-key map (kbd "n") 'aug-next-line)
    (define-key map (kbd "j") 'aug-next-line)  ; Same as `n'
    (define-key map (kbd "p") 'aug-previous-line)
    (define-key map (kbd "k") 'aug-previous-line)  ; Same as `p'
    (define-key map (kbd "M-N") 'aug-next-directory)
    (define-key map (kbd "M-P") 'aug-previous-directory)
    ;; File Preview
    (define-key map (kbd "N") 'aug-preview-next-line)
    (define-key map (kbd "P") 'aug-preview-previous-line)
    (define-key map (kbd "M-k") 'aug-preview-current-line)
    (define-key map (kbd "i") 'aug-preview-current-line)  ; Same as `M-k'
    (define-key map (kbd "C-c o a") 'aug-open-files-and-dirs-in-region)
    (define-key map (kbd "C-c o f") 'aug-open-files-in-region)
    (define-key map (kbd "C-c o d") 'aug-open-dirs-in-region)
    ;; Buffer management
    (define-key map (kbd "q") 'aug-kill-buffer)
    (define-key map (kbd "t") 'aug-show-subtree)
    (define-key map (kbd "SPC") 'aug-show-subtree)  ; Same as `t'
    (define-key map (kbd "M-<right>") 'aug-show-subtree)  ; Same as `t'
    (define-key map (kbd "l") 'aug-show-subtree)  ; Same as `t'
    (define-key map (kbd "^") 'aug-go-to-parent)
    (define-key map (kbd "M-<up>") 'aug-go-to-parent)  ; Same as `^'
    (define-key map (kbd "h") 'aug-go-to-parent)  ; Same as `^'
    (define-key map (kbd "M-h") 'aug-go-to-parent-with-cursor-on-previous)
    (define-key map (kbd "g") 'aug-update)
    ;; Sorting
    (define-key map (kbd "R") 'aug-reverse)
    (define-key map (kbd "C") 'aug-cycle-sorting)
    (define-key map (kbd "|") 'aug-toggle-indentation-prefix)
    (define-key map (kbd ".") 'aug-toggle-dotfiles)
    (define-key map (kbd "m") 'aug-toggle-subtree-visibility)
    ;; File/directory opening
    (define-key map (kbd "V") 'aug-open-current-thing-read-only)
    (define-key map (kbd "v") 'aug-open-current-thing-in-view-mode)
    (define-key map (kbd "o") 'aug-open-thing-other-window)
    (define-key map (kbd "r") 'aug-open-thing-read-only-other-window)
    ;; Resizing
    (define-key map (kbd "M-r") 'aug-resize)
    (define-key map (kbd "M-l") 'aug-toggle-preview)
    map)
  "Keymap for which is active in `aug-buffer'. Individual key bindings can
be reconfigured without side effects.")

;;=========================================================================
;; External interface
;;=========================================================================

(defalias 'augmented-tree 'aug-tree
  "Show the Augmented Tree in the currently selected window.")
(defalias 'augmented-tree-other-window 'aug-tree-other-window
  "Show the Augmented Tree in different window.")
(defalias 'augmented-tree-sidebar 'aug-tree-sidebar
  "Show the Augmented Tree sidebar window.")
(defalias 'augmented-tree-focus-window 'aug-focus-window
  "Make the window displaying `aug-buffer' the currently selected window.")
(defalias 'augmented-tree-kill-buffer 'aug-kill-buffer
  "Kill the Augmented Tree buffer from wherever the cursor is right now.")


;;=========================================================================
;; Major moden
;;=========================================================================

(define-derived-mode augmented-tree-mode fundamental-mode "Augmented Tree"
  "Mode for augmented `tree' command.")


;;=========================================================================
;; Provide
;;=========================================================================

(provide 'augmented-tree)
;;; augmented-tree.el ends here
