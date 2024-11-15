;;; ddf.el --- Declare Dotfiles -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/ddf
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Declare Dotfiles - A simple and declarative dotfiles manager.

;;; Code:

(require 'f)

(defgroup ddf nil
  "Group for `ddf' customizations."
  :group 'ddf)

(defcustom ddf-directory "~/dotfiles/"
  "Dotfiles directory."
  :type 'string)

(defcustom ddf-packages
  (file-name-concat ddf-directory "packages.eld")
  "Dotfiles declarations."
  :type 'string)

(defcustom ddf-symlinks-relative nil
  "Use relative instead of absolute symlinks."
  :type 'boolean)

(defcustom ddf-symlinks-overwrite 1
  "Overwrite existing dotfiles if t. If an integer, request confirmation."
  :type 'symbol)

(defcustom ddf-uninstall-first t
  "Always uninstall declared host packages before installing them."
  :type 'boolean)

(defcustom ddf-log-events t
  "Log events."
  :type 'boolean)

(defcustom ddf-log-buffer "*ddf-log*"
  "Log buffer."
  :type 'string)

(defvar ddf-host-list nil
  "List of hosts declared in `ddf-packages' file.")

(defvar ddf-pkg-list nil
  "List of packages declared in `ddf-packages' file.")


;;;; Structs

;; TODO 2024-11-14: add structure description to docstring

(cl-defstruct (ddf-host
               (:constructor ddf-host-make)
               (:copier nil))
  "A host."
  (name nil
        :type string
        :documentation "Host name.")
  (dir  "~/"
        :type string
        :documentation "Target directory.")
  (env  "bin"
        :type string
        :documentation "Environment path, relative to target directory.")
  (pkgs nil
        :type list
        :documentation "List of packages."))


(cl-defstruct (ddf-pkg
               (:constructor ddf-pkg-make)
               (:copier nil))
  "A package."
  (name   nil
          :type string
          :documentation "A directory name in `ddf-directory'.")
  (target nil
          :type string
          :documentation "Folder structure.")
  (files  nil
          :type list
          :documentation "List of relevant package files.")
  (globs  nil
          :type list
          :documentation "Alist of files wildcards and destinations."))


;;; Load package declarations

(defun ddf-log (log-msg &optional echo)
  "Write LOG-MSG to `ddf-log-buffer' when `ddf-log-events' is non-nil.
When ECHO is non-nil, also display LOG-MSG in the echo area."
  (when ddf-log-events
    (with-current-buffer (get-buffer-create ddf-log-buffer)
      (special-mode)
      (setq-local buffer-read-only nil)
      (goto-char (point-max))
      (insert (concat (format-time-string "[%F %R] ")
                      log-msg "\n"))
      (setq-local buffer-read-only t)))
  (when echo (message log-msg)))

;; NOTE 2024-11-11: adapted from `tempel--file-read',
;; see also `org-id-locations-load'
(defun ddf--file-read (file)
  "Load declarations from FILE."
  (with-temp-buffer
    (insert "(\n")
    (insert-file-contents file)
    (goto-char (point-max))
    (insert "\n)")
    (goto-char (point-min))
    (let ((data (read (current-buffer)))
          (declarations))
      (while (consp (car data))
        (push (pop data) declarations))
      (nreverse declarations))))

(defun ddf--load-host (plist)
  "Load host from a declaration PLIST."
  (ddf-host-make :name (plist-get plist :name)
                 :dir  (plist-get plist :dir)
                 :env  (plist-get plist :env)
                 :pkgs (plist-get plist :pkgs)))

(defun ddf--load-pkg (plist)
  "Load package from a declaration PLIST."
  (ddf-pkg-make :name   (plist-get plist :name)
                :target (plist-get plist :target)
                :files  (plist-get plist :files)
                :globs  (plist-get plist :globs)))

(defun ddf-load ()
  "Load package declarations from `ddf-packages' into `ddf-pkg-list'."
  (setq ddf-host-list nil
        ddf-pkg-list  nil)
  (mapc (lambda (p)
          (let ((type (car p))
                (form (cdr p)))
            (pcase type
              ('host (push (ddf--load-host form) ddf-host-list))
              ('pkg  (push (ddf--load-pkg form)  ddf-pkg-list)))))
        (reverse (ddf--file-read ddf-packages)))
  (ddf-log (format "Loaded %s host(s) and %s packages."
                   (length ddf-host-list) (length ddf-pkg-list))))

(defun ddf-get (type name)
  "Return struct of TYPE by NAME."
  (let* ((host-p (eq type 'host))
         (lst (if host-p ddf-host-list ddf-pkg-list))
         (fn (if host-p #'ddf-host-name #'ddf-pkg-name)))
    (car (seq-filter (lambda (s)
                       (equal (format "%s" name) (funcall fn s)))
                     lst))))

(defun ddf-read (type)
  "Interactively read struct of TYPE."
  (let* ((host-p (eq type 'host))
         (lst (if host-p ddf-host-list ddf-pkg-list))
         (fn (if host-p #'ddf-host-name #'ddf-pkg-name))
         (prompt (if host-p "Known hosts: " "Packages: ")))
    (ddf-get type (completing-read prompt (mapcar fn lst) nil t))))


;;;; Parsing

;;;;; Declaration checks

(defun ddf--check-value (value)
  "Check if VALUE is a string and return it."
  (if (stringp value)
      value
    (user-error "Slot value must be a string: %s" value)))

(defun ddf--check-list (lst)
  "Check if LST is a non-empty list and return it."
  (if (and (listp lst) (not (null lst)))
      lst
    (user-error "Slot value must be a non-empty list: %s" lst)))

(defun ddf--check-file (file)
  "Check if FILE exists, if it's not a symlink, and return it."
  (if (f-exists? file)
      (if (f-symlink? file)
          (user-error "Source file cannot be a symlink: %s" file)
        file)
    (user-error "Source file not found: %s" file)))

(defun ddf--check-symlink (symlink)
  "Check if SYMLINK can be created and return it."
  (if (and (not (f-exists? symlink))
           (f-directory? (f-dirname symlink)))
      symlink
    (user-error "Symlink cannot be created at %s" symlink)))

;; REVIEW 2024-11-14: unnused
(defun ddf--check-pkg-name (package)
  "Check if PACKAGE is valid and return it."
  (thread-last package
               (ddf--check-name)
               (ddf--check-file)))

;; REVIEW 2024-11-14: unnused
(defun ddf--check-links (dotfiles)
  "Return DOTFILES if source files are valid and symlinks can be created."
  (mapcar (lambda (d)
            (cons (ddf--check-file (car d))
                  (ddf--check-symlink (cdr d))))
          dotfiles))


;;;;; Parse package

(cl-defmethod ddf-pkg-package-path ((pkg ddf-pkg))
  "Return the path for PKG if it is a package in `ddf-directory'."
  (condition-case nil
      (ddf--check-file
       (file-name-concat ddf-directory (ddf-pkg-name pkg)))
    (error (user-error "Invalid or not found package \"%s\""
                       (ddf-pkg-name pkg)))))

(cl-defmethod ddf-pkg-target-path ((pkg ddf-pkg))
  "Return target path for PKG."
  (condition-case nil
      (ddf-pkg-target pkg)
    (error (user-error "Invalid or undefined target for \"%s\": %s"
                       (ddf-pkg-name pkg) (ddf-pkg-target pkg)))))

(defun ddf-pkg--files (files pkg-path target-path)
  "Return a list of (file . symlink) for FILES from PKG-PATH to TARGET-PATH."
  (mapcar
   (lambda (f)
     (let ((src (if (stringp f) f (car f)))
           (sym (if (stringp f) (f-filename f) (f-filename (cdr f))))
           (sym-dir (if (stringp f) "" (f-dirname (cdr f)))))
       (cons (ddf--check-file (file-name-concat pkg-path src))
             (file-name-concat (if (string-match-p "^[~/]" sym-dir)
                                   sym-dir
                                 target-path)
                               sym))))
   files))

(defun ddf-pkg--globs (globs pkg-path target-path)
  "Return a list of files that match GLOBS from PKG-PATH to TARGET-PATH."
  (let ((default-directory pkg-path))
    (mapcan
     (lambda (g)
       (let ((glob (if (stringp g) g (car g)))
             (dest (if (stringp g) target-path (cdr g))))
         (mapcar (lambda (f)
                   (cons (ddf--check-file f)
                         (file-name-concat dest (f-filename f))))
                 (file-expand-wildcards glob :full :regexp))))
     globs)))

(cl-defmethod ddf-pkg-dotfiles ((pkg ddf-pkg))
  "Return a list of (source . symlink) for PKG."
  (let ((pkg-path (ddf-pkg-package-path pkg))
        (target-path (ddf-pkg-target-path pkg))
        (files (ddf-pkg-files pkg))
        (globs (ddf-pkg-globs pkg)))
    (if (and (not files) (not globs))
        (list (cons pkg-path target-path))
      (delete-dups
       (append
        (and files (ddf-pkg--files files pkg-path target-path))
        (and globs (ddf-pkg--globs globs pkg-path target-path)))))))


;;;;; Parse host

;; NOTE 2024-11-14: unnused
(cl-defmethod ddf-host-path ((host ddf-host))
  "Return environment path for HOST."
  (file-name-concat (ddf--check-value (ddf-host-dir host))
                    (ddf--check-value (ddf-host-env host))))

(cl-defmethod ddf-host-pkg-list ((host ddf-host))
  "Return the package declaration for HOST.
If host :pkgs is nil. return all known packages."
  (or (ddf-host-pkgs host)
      (mapcar #'ddf-pkg-name ddf-pkg-list)))

(cl-defmethod ddf-host-dotfiles ((host ddf-host))
  "Return an alist of all source files and links for HOST packages.
Replace target path if host defines something different from \"~/\"."
  (mapcar (lambda (s)
            (cons (car s)
                  (replace-regexp-in-string "^~"
                                            (ddf-host-dir host)
                                            (cdr s))))
          (mapcan #'ddf-pkg-dotfiles
                  (mapcar (lambda (p) (ddf-get 'pkg p))
                          (ddf--check-list (ddf-host-pkg-list host))))))


;;;; Dotfiles installation

(defun ddf--make-directory (dir)
  "Create the directory DIR and its parents."
  (unless (f-directory? dir)
    (make-directory dir :parents)
    (ddf-log (format "Created directory: %s" dir))))

(defun ddf--make-symlink (dotfile)
  "Make symlink for DOTFILE.
Make a relative symlink if `ddf-symlinks-relative' is non-nil.
Overwrite symlinks if `ddf-symlinks-overwrite' is non-nil, which see."
  (let ((src (car dotfile))
        (sym (cdr dotfile)))
    (when (not (f-symlink? sym))
      (make-symbolic-link (if ddf-symlinks-relative
                              (file-relative-name src (f-dirname sym))
                            src)
                          sym
                          ddf-symlinks-overwrite)
      (ddf-log (format "Created symlink: %s" (cdr dotfile))))))

(defun ddf--delete-directory (dir)
  "Delete the directory DIR (it must be empty)."
  (when (and (f-directory? dir) (directory-empty-p dir))
    (delete-directory dir)
    (ddf-log (format "Deleted directory: %s" dir))))

(defun ddf--delete-symlink (symlink)
  "Delete SYMLINK."
  (when (f-symlink? symlink)
    (delete-file symlink)
    (ddf-log (format "Deleted symlink: %s" symlink))))

(defun ddf--dotfiles-dirs (dotfiles)
  "Return a list of unique directories from DOTFILES alist."
  (delete-dups
   (mapcar (lambda (d) (f-dirname (cdr d)))
           dotfiles)))

(defun ddf--dotfiles-symlinks (dotfiles)
  "Return a list of symlinks from DOTFILES alist."
  (mapcar #'cdr dotfiles))

(cl-defmethod ddf-uninstall-dotfiles ((host ddf-host))
  "Uninstall packages at HOST directory."
  (when-let* ((dotfiles (ddf-host-dotfiles host))
              (directories (ddf--dotfiles-dirs dotfiles))
              (symlinks (ddf--dotfiles-symlinks dotfiles))
              (host-name (ddf-host-name host)))
    (ddf-log (format "Uninstalling dotfiles for '%s'..." host-name))
    (mapc #'ddf--delete-symlink symlinks)
    (mapc #'ddf--delete-directory directories)
    (ddf-log (format "Dotfiles uninstalled for '%s'." host-name) :echo)))

(cl-defmethod ddf-install-dotfiles ((host ddf-host))
  "Install packages at HOST directory."
  (when ddf-uninstall-first (ddf-uninstall-dotfiles host))
  (ddf--make-directory (ddf-host-dir host))
  (when-let* ((dotfiles (ddf-host-dotfiles host))
              (directories (ddf--dotfiles-dirs dotfiles))
              (host-name (ddf-host-name host)))
    (ddf-log (format "Installing dotfiles for '%s'..." host-name))
    (mapc #'ddf--make-directory directories)
    (mapc #'ddf--make-symlink dotfiles)
    (ddf-log (format "Dotfiles installed for '%s'." host-name) :echo)))

(defun ddf-host-with-pkg (&optional host-name pkg-name)
  "Overwrite HOST-NAME :pkgs list with PKG-NAME."
  (let ((host (if host-name (ddf-get 'host host-name) (ddf-read 'host))))
    (setf (ddf-host-pkgs host)
          (list (or pkg-name (ddf-pkg-name (ddf-read 'pkg)))))
    (ddf-load)
    host))


;;;; Commands

;;;###autoload
(defun ddf-dired (&optional arg)
  "Dired for dotfiles. With ARG, reload packages."
  (interactive "P")
  (when (or arg (not ddf-pkg-list))
    (ddf-load))
  (dired (expand-file-name (ddf-pkg-name (ddf-read 'pkg))
                           ddf-directory)))

;;;###autoload
(defun ddf-install (&optional arg)
  "Install dotfiles for a host.
With ARG, also prompt for a specific package."
  (interactive "P")
  (ddf-install-dotfiles (if arg (ddf-host-with-pkg) (ddf-read 'host))))

;;;###autoload
(defun ddf-uninstall (&optional arg)
  "Uninstall dotfiles for a host.
With ARG, also prompt for a specific package."
  (interactive)
  (ddf-uninstall-dotfiles (if arg (ddf-host-with-pkg) (ddf-read 'host))))


;;; Provide

(provide 'ddf)

;;; ddf.el ends here
