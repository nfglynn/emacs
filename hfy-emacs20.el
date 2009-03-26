;; hfy-emacs20.el  - emacs20 compatibility duct tape for `htmlfontify'

;; Emacs Lisp Archive Entry
;; Package: htmlfontify
;; Filename: hfy-emacs20.el
;; Version: 0.20
;; Keywords: emacs20, compatibility
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Created: 2002-01-05
;; Description: emacs20 compatibility code
;; URL: http://rtfm.etla.org/emacs/htmlfontify/
;; Compatibility: Emacs20, Emacs21
;; Incompatibility: Emacs19
;; Last-Updated: Sat 2003-02-15 03:49:15 +0000

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;  Copyright (C) 2002,2003 Vivek Dasmohapatra <vivek@etla.org>

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.

;; Weasels. emacs20 doesn't have built in hashes. Kludge it:
;; minimal hash table implementation:
(defun hfy-asst (key alist &optional test default)
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) elt default))))
      (setq tail (cdr tail))) value))

(if (not (fboundp 'make-hash-table))
    (defun make-hash-table (&rest KEYWORD-ARGS)
      (cons (or (cadr (memq :test KEYWORD-ARGS)) 'eql) (list))) )

(if (not (fboundp 'puthash))
    (defun puthash (KEY VALUE TABLE)
      (let ((entry         nil)
	    (test  (car TABLE))
	    (alist (cdr TABLE)))
	(if (setq entry (hfy-asst KEY alist test))
	    (setq alist (delq entry alist)))
	(setcdr TABLE (cons (cons KEY VALUE) alist)))) )

(if (not (fboundp 'clrhash))
    (defun clrhash (TABLE) (setcdr TABLE nil)) )

(if (not (fboundp 'gethash))
    (defun gethash (KEY TABLE &optional DFLT)
      (let ((test  (car TABLE))
	    (alist (cdr TABLE)))
	(or (cdr (hfy-asst KEY alist test)) DFLT))) )

(if (not (fboundp 'remhash))
    (defun remhash (KEY TABLE)
      (let ((entry         nil)
	    (test  (car TABLE))
	    (alist (cdr TABLE)))
	(if (setq entry (hfy-asst KEY alist test))
	    (setcdr TABLE (delq entry alist))))) )

(if (not (fboundp 'maphash))
    (defun maphash (FUN TABLE)
      (mapcar (lambda (CONS) (funcall FUN (car CONS) (cdr CONS)))
	      (cdr TABLE)) nil) )

;; misc emacs20 duct tape:
(if (not (fboundp 'color-values))
    (defalias 'color-values 'x-color-values))

(provide 'hfy-emacs20)
