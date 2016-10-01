;;; windows-path.el -- Teach cygwin/WSL EMACS about Windows file system

;; Copyright (C) 2009 Victor Ren
;;

;; Author: Victor Ren <victorhge@gmail.com>
;; Keywords: Windows, mount, cygwin, path
;; Version:0.1
;; X-URL: https://www.emacswiki.org/emacs/windows-path.el
;;        https://github.com/victorhge/windows-path

;; This file is *NOT* (yet) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package lets you use windows-style filenames like "c:/path/file" or
;; "c:\path\file" in cygwin Emacs, or Emacs runing on WSL (Windows Subsystem for
;; Linux).  There is a cygwin-mount.el let you use Unix style path in native
;; Windows Emacs.  This package is a opposite of it.

;;; Installation:

;; Put in your .emacs or site-start.el file the following lines:
;;   (require 'windows-path)
;;   (windows-path-activate)

;;; Compatibility

;; The package is only tested with Cygwin Emacs 24.3.

;; How it works:
;; Basically some hook functions are put onto file-name-handler-alist.
;; They detect filenames expressed in Windows style, and translate
;; those names into Unix style.

;;; Code:
(if (eq system-type 'cygwin)
    (require 'cygwin-mount))

(defconst windows-path-version "0.1")

(defgroup windows-path nil
  "Proper handling of windows filenames."
  :prefix "windows-path-"
  :group 'files)

(defvar windows-path-mount-prefix ""
  "Prefix for the mount point of Windows file system.
A cygwin-user can change the \"/mount\" to whatever he wants to access
files at MS-DOS drives. For example many people seem to like to have the
drives accessible as a directory so that c: == /c, which means the
mount-prefix is \"/\" instead of \"/mount\". This prefix must end
with a '/'!  Do not set this variable because the value of this variable is
determined at activation-time of windows-path \(see
`windows-path-activate')")

(defun windows-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(windows-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defconst windows-path-style0-regexp "\\`\\(.*/\\)?\"\\([a-zA-Z]:\\)\\\\.*\"")
(defconst windows-path-style1-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)\\\\")
(defconst windows-path-style2-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)/")

;; We cannot assume that NAME matched windows-path-style1-regexp nor
;; windows-path-cygwin-style2-regexp because this function could be called with
;; either argument to `expand-file-name', but only one argument to
;; `expand-file-name' may have matched a regexp.  For example,
;; `(expand-file-name ".." "c:/")' will trigger `(windows-path-convert-file-name
;; "..")' and `(windows-path-convert-file-name "c:/")' to be called.
(defun windows-path-convert-file-name (name)
  "Convert file NAME, to Unix style.
`x:/' to `/mount/x/'.
NOTE: \"/mount/\" is only an example for the mount-prefix \(see
`windows-path-mount-prefix')."
  (cond ((string-match windows-path-style0-regexp name)
         (while (string-match "\"" name) ; remove quote
           (setq name
                 (replace-match "" t nil name)))
         (windows-path-convert-file-name name))

        ((string-match windows-path-style1-regexp name)
         (setq name
               (replace-match (concat windows-path-mount-prefix
                                      (downcase (substring (match-string 2 name) 0 1)))
                              t nil name 2))
         (while (string-match "\\\\" name)
           (setq name
                 (replace-match "/" t nil name)))
         name)
        ((string-match windows-path-style2-regexp name)
         (replace-match (concat windows-path-mount-prefix
                                (downcase (substring (match-string 2 name) 0 1)))
                        t nil name 2))

        (t name)))

;; (string-match windows-path-style2-regexp "/sd/c:/xpd/file.txt")
;; (windows-path-convert-file-name "sd/c:/xpd/file.txt")
;; (windows-path-convert-file-name "c:/xpd/file.txt")
;; (windows-path-convert-file-name "~/path/c:/sds/")
;; (windows-path-convert-file-name "/c:/sds/")
;; (windows-path-convert-file-name "~/emacs/C:\\Users\\CommonProps.properties")
;; (windows-path-convert-file-name "~/emacs/\"C:\\Users\\test.t\"")

(defun windows-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on cygwin NAME with ARGS.
Map Windows sytle name to the cygwin-style \"/[A-Za-z]/\" and call
OPERATION with the mapped filename\(s). NAME must have the format looks like
\"^/[A-Za-z]:/\" or \"^[A-Za-z]:\\\"  here. Note that at least the first
element of ARGS could be a filename too \(then it must have the same syntax
like NAME!) which must be converted \(e.g. `expand-file-name' can be called
with two filenames).
NOTE: \"/mount/\" is only an example for the mount-prefix \(see
`windows-path-mount-prefix')."
  (windows-path-run-real-handler
   operation
   (cons (windows-path-convert-file-name name)
		 (if (stringp (car args))
			 (cons (windows-path-convert-file-name (car args))
				   (cdr args))
		   args))))

(defvar windows-path-activated nil
  "This is a global varialbe indicating if windows-path is active.")

(defun windows-path-activate ()
  "Activate windows-path-style-handling."
  (interactive)
  (unless windows-path-activated
    (setq windows-path-mount-prefix
          (if (eq system-type 'cygwin)
              (cygwin-mount-get-cygdrive-prefix)
            "/mnt"))
    (add-to-list 'file-name-handler-alist
                 (cons windows-path-style0-regexp
                       'windows-path-map-drive-hook-function))
    (add-to-list 'file-name-handler-alist
                 (cons windows-path-style1-regexp
                       'windows-path-map-drive-hook-function))
    (add-to-list 'file-name-handler-alist
                 (cons windows-path-style2-regexp
                       'windows-path-map-drive-hook-function))
    (setq windows-path-activated t)))

(defun windows-path-deactivate ()
  "Deactivate windows-style-path handling."
  (interactive)
  (unless (not windows-path-activated)
      (setq windows-path-mount-prefix "")
      (setq file-name-handler-alist
            (delete (assoc windows-path-style0-regexp file-name-handler-alist)
                    file-name-handler-alist))
      (setq file-name-handler-alist
            (delete (assoc windows-path-style1-regexp file-name-handler-alist)
                    file-name-handler-alist))
      (setq file-name-handler-alist
            (delete (assoc windows-path-style2-regexp file-name-handler-alist)
                    file-name-handler-alist))
      (setq windows-path-activated nil)))

(provide 'windows-path)

;;; windows-path.el ends here
