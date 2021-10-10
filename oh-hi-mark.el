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
(require 'notmuch-tree)

(defvar ohm-marked nil)

(defun ohm-toggle--mark-message (&optional force)
  (let* ((message-id (notmuch-tree-get-message-id))
         (already-marked (member message-id ohm-marked)))
    (cond
     ((or
       (and (eq force 'mark) (not already-marked))
       (and (not (eq force 'unmark)) (not already-marked)))
      (add-to-list 'ohm-marked (notmuch-tree-get-message-id)))
     ((or
       (and (eq force 'unmark) already-marked)
       (and (not (eq force 'mark)) already-marked))
      (setq ohm-marked (delete message-id ohm-marked))))))

;;;###autoload
(defun ohm-toggle-mark-message (&optional force)
  (interactive)
  (ohm-toggle--mark-message force)
  (notmuch-refresh-this-buffer))

;;;###autoload
(defun ohm-toggle-mark-thread (&optional force)
  (interactive)
  (let* ((message-id (notmuch-tree-get-message-id))
         (force
          (or force 
              (cond
               ((member message-id ohm-marked)
                'unmark)
               (t 'mark)))))
    (notmuch-tree-thread-mapcar (lambda () (ohm-toggle--mark-message force))))
  (notmuch-refresh-this-buffer))

(defun ohm--insert-mark (orig-fun &rest msg)
  (let* ((message-id (concat "id:" (plist-get (car msg) :id)))
         (marked (member message-id ohm-marked)))
    (if marked
        (let* ((actualmsg (car msg))
               (reldate (plist-get actualmsg :date_relative))
               (msg (cons (plist-put actualmsg :mark t)
                          (cdr msg))))
          (apply orig-fun msg))
      (apply orig-fun msg))))

(advice-add 'notmuch-tree-insert-msg :around #'ohm--insert-mark)

;;;###autoload
(defun ohm-tag-marked-messages ()
  (interactive)
  (if ohm-marked
      (let ((search (string-join ohm-marked " or "))
            (tagchange (notmuch-read-tag-changes nil "Tag changes: ")))
        (if (y-or-n-p (format "Really apply tag changes %s to %s marked messages?" tagchange (length ohm-marked)))
            (progn (notmuch-tag search tagchange)
                   (setq ohm-marked nil)
                   (notmuch-refresh-this-buffer))
          (error "Canceled"))
        (error "No marked messages to tag"))))

(defun ohm-remove-all-marks ()
  (interactive)
  (setq ohm-marked nil)
  (notmuch-refresh-this-buffer))

;;; Give marked messages a different face
(after! notmuch
  (defun notmuch-tree-format-field-list (field-list msg)
    "Format fields of MSG according to FIELD-LIST and return string."
    (let ((face (cond
                 ((plist-get msg :mark)
                  '(underline :color "orange" :style wave))
                 ((plist-get msg :match)
		  'notmuch-tree-match-face)
	         (t 'notmuch-tree-no-match-face)))
	  (result-string))
      (dolist (spec field-list result-string)
        (let ((field-string (notmuch-tree-format-field (car spec) (cdr spec) msg)))
	  (setq result-string (concat result-string field-string))))
      (notmuch-apply-face result-string face t))))

(provide 'oh-hi-mark)

;;; oh-hi-mark.el ends here
