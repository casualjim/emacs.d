;;; ensime-semantic-highlight.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defvar ensime-sem-high-faces
  '(
   (var . font-lock-variable-name-face)
   (val . (:foreground "#acecf8"))
   (varField . (:foreground "#b4eb6d"))
   (valField . (:foreground "#acecf8"))
   (functionCall . (:foreground "#f8ebac"))
   (operator . (:foreground "#bbbbbb"))
   (param . (:foreground "#EEE8AA"))
   (class . font-lock-type-face)
   (trait . (:foreground "#4fd6b8"))
   (object . (:foreground "#e3bb02"))
   (package . font-lock-preprocessor-face)
   (method . (:foreground "#ffffff"))
   )
  "Faces for semantic highlighting. Symbol types not mentioned here
 will not be requested from server.")

(defun ensime-sem-high-apply-properties (info)
  "Use provided info to modify font-lock properties of identifiers
 in the program text."
  (let ((file (plist-get info :file))
	(syms (plist-get info :syms)))
    (when-let (buf (find-buffer-visiting file))
      (with-current-buffer buf
	(dolist (sym syms)
	  (let* ((type (nth 0 sym))
		 (start (+ ensime-ch-fix (nth 1 sym)))
		 (end (+ ensime-ch-fix (nth 2 sym)))
		 (face (cdr (assoc type ensime-sem-high-faces))))
	    (let ((ov (make-overlay start end buf)))
	      (overlay-put ov 'face face)
	      (overlay-put ov 'ensime-sem-high-overlay t)
	      (overlay-put ov 'ensime-sym-type type))
	    ))
	))))

(defun ensime-sem-high-clear-buffer ()
  (ensime-sem-high-clear-region 0 (point-max)))

(defun ensime-sem-high-clear-region (beg end)
  (let ((ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (overlay-get ov 'ensime-sem-high-overlay)
	(delete-overlay ov)))))

(defun ensime-sem-high-refresh-hook ()
  "Update semantic highlighting for the current buffer.
 For big buffers, update visible region first."
  (interactive)
  (let ((visible-size (- (window-end) (window-start)))
	(total-size (point-max)))
    (when (> total-size (* 5 visible-size))
      (ensime-sem-high-refresh-region (window-start) (window-end)))
    (ensime-sem-high-refresh-region 0 (point-max))))
    

(defun ensime-sem-high-refresh-buffer (&optional buffer)
  "Refresh semantic highlighting for the entire buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (ensime-sem-high-refresh-region 0 (point-max))))

(defun ensime-sem-high-refresh-region (beg end)
  "Refresh semantic highlighting for the given region."
  (ensime-rpc-symbol-designations
   buffer-file-name beg end
   (mapcar 'car ensime-sem-high-faces)
   `(lambda (info)
      (ensime-sem-high-clear-region ,beg ,end)
      (ensime-sem-high-apply-properties info)
      (ensime-event-sig :region-sem-highlighted nil)
      )))

(defun ensime-sem-high-inspect-highlight ()
  (interactive)
  (let ((ovs (overlays-at (point))))
    (message "%S" (mapcar 
		   (lambda (ov)
		     (format "%S %S %S" 
			     (overlay-get ov 'face)
			     (overlay-get ov 'ensime-sem-high-overlay)
			     (overlay-get ov 'ensime-sym-type)))
		   ovs))))

(defun ensime-sem-high-sym-types-at-point ()
  (let ((ovs (overlays-at (point))))
    (mapcar 
     (lambda (ov)
       (overlay-get ov 'ensime-sym-type))
    ovs)))


(provide 'ensime-semantic-highlight)