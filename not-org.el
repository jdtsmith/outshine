;;; not-org.el --- Treat non-Org buffers as Org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: Org, outlines

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A library to handle commented Org outlines in non-Org buffers.

;; NOTE: This is designed only to work with comments that start at the
;; beginning of a line, optionally after whitespace.

;;; Code:

;;;; Requirements


;;;; Variables


;;;; Customization

;;;; Macros

(defmacro not-org-with-uncommented (beg end &rest body)
  ;; FIXME: Improve docstring.
  "Eval BODY in a temporary buffer with the content between BEG and END uncommented, replacing region with result."
  (declare (indent defun))
  ;; TODO: Handle `comment-end'.
  `(let* ((source-comment-start comment-start)
          (source-comment-add comment-add)
          (source-comment-style comment-style)
          (source-comment-padding comment-padding)
          (source-comment-region-function comment-region-function)
          (source-uncomment-region-function uncomment-region-function)
          (source-buffer (current-buffer))
          result)
     (with-current-buffer (not-org--buffer)
       (erase-buffer)
       (insert (with-current-buffer source-buffer
                 (buffer-substring beg end)))
       (let ((comment-start source-comment-start)
             (comment-add source-comment-add)
             (comment-style source-comment-style)
             (comment-padding source-comment-padding)
             (uncomment-region-function source-uncomment-region-function))
         (uncomment-region (point-min) (point-max)))
       ,@body
       (let ((comment-start source-comment-start)
             (comment-add source-comment-add)
             (comment-style source-comment-style)
             (comment-padding source-comment-padding)
             (comment-region-function source-comment-region-function))
         (comment-region (point-min) (point-max)))
       (setf result (buffer-substring (point-min) (point-max))))
     (setf (buffer-substring beg end) result)))

;;;; Commands


;;;; Functions

(defun not-org--buffer ()
  "Return dedicated `not-org' temp buffer."
  (or (get-buffer " *Not Org Temp Buffer*")
      (with-current-buffer (get-buffer-create " *Not Org Temp Buffer*")
        (org-mode)
        (current-buffer))))

(defun not-org-key-sequence (key)
  ;; FIXME: Improve docstring.
  "Process KEY in `org-mode-map' as if the current buffer were an Org buffer."
  (-let* ((definition (lookup-key org-mode-map key 'accept-defaults))
          ((beg . end) (not-org--comment-region-bounds)))
    (not-org-with-uncommented beg end
      (call-interactively definition))))

(defun not-org--comment-region-bounds ()
  "Return cons (BEG . END) for bounds of commented region around point."
  (cons (save-excursion
          (or (re-search-backward (rx bol (optional (1+ blank))
                                      (not (syntax comment-start)))
                                  nil 'noerror)
              (point-min)))
        (save-excursion
          (or (re-search-forward (rx bol (optional (1+ blank))
                                     (not (syntax comment-start)))
                                 nil 'noerror)
              (point-max)))))

;;;; Footer

(provide 'not-org)

;;; not-org.el ends here
