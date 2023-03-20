;;; oh-hi-mark --- notmuch-emacs's "mark now, decide later"                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hugo Nobrega

;; Author: Hugo Nobrega <hugonobrega@gmail.com>
;; Keywords: lisp
;; Package-Requires: ((emacs "27.1") (notmuch "0.32")
;; Version: 0.0.1

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

;; Defining the sorely-missed "mark now, decide later" functionality for notmuch-emacs

;;; Code:
(require 'notmuch)
(require 'notmuch-tree)
(require 'evil-collection-notmuch)

(defvar ohm--mark-tag "ohm_bogus"
  "What tag should be used as a `marker'.")

(defvar ohm--mark-tag-face '(:underline (:style wave :color "darkgray"))
  "The face to apply to marked messages.

This should be the same as the user's definition in
`notmuch-search-line-faces' for consistent styling across notmuch
buffers.")

(defun ohm--mark-tag-search ()
  "Create the search string for the marker tag."
  (concat "tag:" ohm--mark-tag))

(defun ohm--mark-add-tag-changes ()
  (notmuch-tag-change-list (list ohm--mark-tag)))

(defun ohm--mark-remove-tag-changes ()
  (notmuch-tag-change-list (list ohm--mark-tag) t))

;;; Marking

;;;###autoload
(defun ohm-tree-toggle-mark-message (&optional force)
  (interactive)
  (unless force
    (evil-collection-notmuch-toggle-tag "ohm_bogus" "tree" #'notmuch-tree-next-message)))

;;;###autoload
(defun ohm-tree-toggle-mark-thread (&optional force)
  (interactive)
  (let* ((tags (plist-get (notmuch-tree-get-message-properties) :tags))
         (must-add (or force (not (member ohm--mark-tag tags)))))
    (notmuch-tree-tag-thread
     (if must-add
         (ohm--mark-add-tag-changes)
       (ohm--mark-remove-tag-changes)))))

;;;###autoload
(defun ohm-search-toggle-mark-thread (&optional force)
  "Toggle marks in the notmuch search buffer."
  (interactive)
  (unless force
    (evil-collection-notmuch-toggle-tag "ohm_bogus" "search" #'notmuch-search-next-thread)))

;;; (un)Tagging

;;;###autoload
(defun ohm-tag-marked-messages ()
  (interactive)
  (let* ((mark-count (string-to-number (notmuch-saved-search-count (ohm--mark-tag-search))))
         (existing-tags (notmuch-tag-completions (ohm--mark-tag-search)))
         (tagchange (if (> mark-count 0)
                        (notmuch-read-tag-changes existing-tags "Tag changes: ")
                      (error "No marked messages to tag"))))
    (if (y-or-n-p (format "Really apply tag changes %s to %s marked messages?"
                          tagchange mark-count))
        (progn (notmuch-tag (ohm--mark-tag-search) tagchange)
               (notmuch-refresh-this-buffer))
      (error "Canceled"))))

(defun ohm-remove-all-marks ()
  (interactive)
  (notmuch-tag (ohm--mark-tag-search) (ohm--mark-remove-tag-changes))
  (notmuch-refresh-this-buffer))

;;; Styling

(defun ohm--propertize-tree-message (msg)
  "After notmuch-emacs inserts a message in the tree, re-style it to
include the mark face."
  (let ((tags (plist-get msg :tags)))
    (when (and ohm--mark-tag-face
               (member ohm--mark-tag tags))
      (save-excursion
        (forward-line -1)
        (notmuch-apply-face (current-buffer)
                            ohm--mark-tag-face
                            t
                            (line-beginning-position)
                            (line-end-position))))))

(advice-add 'notmuch-tree-insert-msg :after #'ohm--propertize-tree-message)

(provide 'oh-hi-mark)

;;; oh-hi-mark.el ends here
