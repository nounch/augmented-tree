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
;;
;; File preview:
;;
;;   "N" - Preview next line
;;   "P" - Preview previous line
;;   "M-k" / "i" - Preview current line
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
;; File/directory opening:
;;
;;   "v" - Open current file/directory read-only
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
;; - Augmented Tree parses the output of the `tree' command. This is and
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
;;     omit it, but you will have to live with the consequences.
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


;;=========================================================================
;; Functions
;;=========================================================================


;; Path: "\\(\\.*/\\)\\(.*\\)$"
;; Everything before the path: "^[^\\(\\.*/\\)\\(.*\\)$]*"
(defun aug-augment-line(line)
  "Make a proper file URL (`file://'-prefixed) from the string LINE and
return it as a string.

LINE - A string representing a Unix file path.

Returns a string represnting a file URL."
  (let ((augmented-line ""))
    (replace-regexp-in-string "\\(\\.*/\\)\\(.*\\)$"
                              (lambda (match)
                                (concat "file://" match)))))

(defun aug-insert-tree(tree-string-lines)
  "Insert an augmented tree created from TREE-STRING-LINES in the currently
selected buffer.

TREE-STRING-LINES - A list of strings produced from splitting the output of
                    calling the `tree' shell command on newlines (`\n').

Returns nothing, inserts a string in the current buffer."
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
                    (let ((file-path (substring line path-match-index
                                                nil)))
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
                       'file-path file-path))
                    (insert "\n"))))))
        tree-string-lines)
  (insert "\n")
  ;; Use a buffer-local keymap.
  (use-local-map aug-keymap)
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

(defun aug-initialize()
  "Initialize Augmented Tree."
  ;; Alternative way to prevent multiple window splits (Warning: Does
  ;; require manual initial split).
  ;; (setq split-window-preferred-function 'aug-no-split-window)
  ;;
  ;; Use an unreasonable width/height as split treshold so that the windows
  ;; will never be resplit for previews.
  (setq split-width-threshold 9999999)
  (setq split-height-threshold 9999999))

(defun aug-tree(&optional current-window tree-command)
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

Returns nothing, creates augmeted tree output and displays it in a buffer."
  (interactive)
  (let* ((tree-command (or tree-command (format "%s %s" aug-tree-command
                                                default-directory)))
         (tree-string-lines (split-string (shell-command-to-string
                                           tree-command) "\n")))
    (aug-initialize)
    (if current-window
        (progn
          (display-buffer (get-buffer-create aug-buffer) t)
          (with-selected-window  (get-buffer-window aug-buffer)
            (aug-insert-tree tree-string-lines))
          (switch-to-buffer-other-window aug-buffer))
        (progn
          (switch-to-buffer aug-buffer)
          (aug-insert-tree tree-string-lines)))))

(defun aug-tree-other-window(input)
  "Like `aug-tree', but display the output in a window other than the
currently selected one.

Returns nothing, creates augmeted tree output and displays it in a buffer
in another window."
  (interactive "P")
  (aug-tree t))

;; It is also possible to use `point-at-bol' and `point-at-eol', but then a
;; manual check is still necessary to determin if the point is really at
;; the beginning/end of the line using `beginning-of-line'/`end-of-line'.
(defun aug-get-current-line-length()
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

(defun aug-next-line(input)
  "Move the cursor to the next line in `aug-buffer'. When at the end of the
buffer, jump to the first line of the buffer. Skip empty lines.

Returns nothing."
  (interactive "P")
  (next-line)
  ;; If the cursor is at the end of the buffer, jump to the beginning of
  ;; the buffer.
  (if (= (point) (point-max))
      (beginning-of-buffer))
  (move-end-of-line nil)
  ;; Skip single empty lines.
  (if (not (= (aug-get-current-line-length) 0))
      (backward-char)
      (next-line)))

(defun aug-previous-line(input)
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
  (previous-line)
  (move-end-of-line nil)
  ;; Skip single empty lines.
  (if (not (= (aug-get-current-line-length) 0))
      (backward-char)
      (previous-line)))

(defun aug-kill-buffer(input)
  "Kill `aug-buffer', even when the cursor is currently not in that buffer.
This is a convenient way to remove the sidebar.

Returns nothing."
  (interactive "P")
  (kill-buffer aug-buffer))

(defun aug-path-of-current-thing()
  "Get the file path that the current button is pointing to.

Returns nothing."
  (get-text-property (point) 'file-path))

(defun aug-open-current-thing-read-only(input)
  "Open the file or directory that the current button is pointing to in the
current window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (find-file-read-only (aug-path-of-current-thing)))

(defun aug-preview-current-line(input)
  "Preview the current file or directory in a read-only buffer.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (aug-preview-thing))

(defun aug-preview-next-line(input)
  "Preview the file or directory in the next line in a read-only buffer and
move point to the next line.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (call-interactively 'aug-next-line)
  (aug-preview-thing))

(defun aug-preview-previous-line(input)
  "Preview the file or directory in the previous line in a read-only buffer
and move point to the previous line.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (call-interactively 'aug-previous-line)
  (aug-preview-thing))

(defun aug-preview-thing()
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

(defun aug-open-thing-other-window(input)
  "Open the current file or directory buffer in another window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (get-buffer-window (find-file-other-window
                      (aug-path-of-current-thing))))

(defun aug-open-thing-read-only-other-window(input)
  "Open the current file or directory in a read-only buffer in another
window.

Returns nothing."
  (interactive "P")
  (if aug-sidebar-enlarged-p (call-interactively 'aug-toggle-preview))
  (get-buffer-window (find-file-read-only-other-window
                      (aug-path-of-current-thing))))

(defun aug-show-subtree(input)
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
                        (aug-path-of-current-thing))))

(defun aug-go-to-parent(input)
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
    (aug-tree nil (format "%s %s" aug-tree-command parent-path))))

(defun aug-go-to-parent-with-cursor-on-previous(input)
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
      (beginning-of-buffer))))

(defun aug-focus-window(input)
  "Make the window which currently displays `aug-buffer' the currently
selected window.

Returns nothing."
  (interactive "P")
  (switch-to-buffer-other-window aug-buffer))

;; Sidebar

(defun aug-tree-sidebar(input)
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

(defun aug-resize-sidebar(&optional new-width)
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

(defun aug-resize(input &optional smart-resizing)
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

(defun aug-toggle-preview(input)
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

(defun aug-update(input)
  (interactive "P")
  (aug-tree nil (format "%s %s" aug-tree-command default-directory)))

;;=========================================================================
;; Local keymap
;;=========================================================================

(defvar aug-keymap
  (let ((i 0)
        (map (make-keymap)))
    ;; Cursor movement
    (define-key map (kbd "n") 'aug-next-line)
    (define-key map (kbd "j") 'aug-next-line)  ; Same as `n'
    (define-key map (kbd "p") 'aug-previous-line)
    (define-key map (kbd "k") 'aug-previous-line)  ; Same as `p'
    ;; File Preview
    (define-key map (kbd "N") 'aug-preview-next-line)
    (define-key map (kbd "P") 'aug-preview-previous-line)
    (define-key map (kbd "M-k") 'aug-preview-current-line)
    (define-key map (kbd "i") 'aug-preview-current-line)  ; Same as `M-k'
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
    ;; File/directory opening
    (define-key map (kbd "v") 'aug-open-current-thing-read-only)
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

;;=========================================================================
;; Provide
;;=========================================================================

(provide 'augmented-tree)
;;; augmented-tree.el ends here
