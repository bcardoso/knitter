;;; knitter.el --- A Declarative Dotfiles Manager -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/knitter
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

;; A simple declarative dotfiles manager.

;;; Code:

(require 'f)

(defgroup knitter nil
  "Group for `knitter' customizations."
  :group 'knitter)

(defcustom knitter-directory "~/dotfiles/"
  "Dotfiles directory."
  :type 'string)

(defcustom knitter-packages
  (file-name-concat knitter-directory "packages.eld")
  "Dotfiles declarations."
  :type 'string)

(defcustom knitter-symlinks-relative nil
  "Use relative instead of absolute symlinks."
  :type 'boolean)

(defcustom knitter-symlinks-overwrite 1
  "Overwrite existing dotfiles if t. If an integer, request confirmation."
  :type 'symbol)

(defcustom knitter-uninstall-first t
  "Always uninstall declared host packages before installing them."
  :type 'boolean)

(defcustom knitter-log-events t
  "Log events."
  :type 'boolean)

(defcustom knitter-log-buffer "*knitter-log*"
  "Log buffer."
  :type 'string)

(defvar knitter-host-list nil
  "List of hosts declared in `knitter-packages' file.")

(defvar knitter-pkg-list nil
  "List of packages declared in `knitter-packages' file.")


;;;; Structs

;; TODO 2024-11-14: add structure description to docstring

(cl-defstruct (knitter-host
               (:constructor knitter-host-make)
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


(cl-defstruct (knitter-pkg
               (:constructor knitter-pkg-make)
               (:copier nil))
  "A package."
  (name   nil
          :type string
          :documentation "A directory name in `knitter-directory'.")
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

(defun knitter-log (log-msg &optional echo)
  "Write LOG-MSG to `knitter-log-buffer' when `knitter-log-events' is non-nil.
When ECHO is non-nil, also display LOG-MSG in the echo area."
  (when knitter-log-events
    (with-current-buffer (get-buffer-create knitter-log-buffer)
      (special-mode)
      (setq-local buffer-read-only nil)
      (goto-char (point-max))
      (insert (concat (format-time-string "[%F %R] ")
                      log-msg "\n"))
      (setq-local buffer-read-only t)))
  (when echo (message log-msg)))

(defun knitter--file-read (file)
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

(defun knitter--load-host (plist)
  "Load host from a declaration PLIST."
  (knitter-host-make :name (plist-get plist :name)
                     :dir  (plist-get plist :dir)
                     :env  (plist-get plist :env)
                     :pkgs (plist-get plist :pkgs)))

(defun knitter--load-pkg (plist)
  "Load package from a declaration PLIST."
  (knitter-pkg-make :name   (plist-get plist :name)
                    :target (plist-get plist :target)
                    :files  (plist-get plist :files)
                    :globs  (plist-get plist :globs)))

(defun knitter-load ()
  "Load package declarations from `knitter-packages' into `knitter-pkg-list'."
  (setq knitter-host-list nil
        knitter-pkg-list  nil)
  (mapc (lambda (p)
          (let ((type (car p))
                (form (cdr p)))
            (pcase type
              ('host (push (knitter--load-host form) knitter-host-list))
              ('pkg  (push (knitter--load-pkg form)  knitter-pkg-list)))))
        (reverse (knitter--file-read knitter-packages)))
  (put 'knitter-pkg-list
       'last-update (f-modification-time knitter-packages))
  (knitter-log (format "Loaded %s host(s) and %s packages."
                       (length knitter-host-list) (length knitter-pkg-list))))

(defun knitter-reload ()
  "Reload `knitter-packages' if needed."
  (when (or (not knitter-pkg-list)
            (time-less-p (get 'knitter-pkg-list 'last-update)
                         (f-modification-time knitter-packages)))
    (knitter-load)))

(defun knitter-get (type name)
  "Return struct of TYPE by NAME."
  (let* ((host-p (eq type 'host))
         (lst (if host-p knitter-host-list knitter-pkg-list))
         (fn (if host-p #'knitter-host-name #'knitter-pkg-name)))
    (car (seq-filter (lambda (s)
                       (equal (format "%s" name) (funcall fn s)))
                     lst))))

(defun knitter-read (type)
  "Interactively read struct of TYPE."
  (let* ((host-p (eq type 'host))
         (lst (if host-p knitter-host-list knitter-pkg-list))
         (fn (if host-p #'knitter-host-name #'knitter-pkg-name))
         (prompt (if host-p "Known hosts: " "Packages: ")))
    (knitter-get type (completing-read prompt (mapcar fn lst) nil t))))


;;;; Parsing

;;;;; Declaration checks

(defun knitter--check-value (value)
  "Check if VALUE is a string and return it."
  (if (stringp value)
      value
    (user-error "Slot value must be a string: %s" value)))

(defun knitter--check-list (lst)
  "Check if LST is a non-empty list and return it."
  (if (and (listp lst) (not (null lst)))
      lst
    (user-error "Slot value must be a non-empty list: %s" lst)))

(defun knitter--check-file (file)
  "Check if FILE exists, if it's not a symlink, and return it."
  (if (f-exists? file)
      (if (f-symlink? file)
          (user-error "Source file cannot be a symlink: %s" file)
        file)
    (user-error "Source file not found: %s" file)))

(defun knitter--check-symlink (symlink)
  "Check if SYMLINK can be created and return it."
  (if (and (not (f-exists? symlink))
           (f-directory? (f-dirname symlink)))
      symlink
    (user-error "Symlink cannot be created at %s" symlink)))

;; NOTE 2024-11-14: unused
(defun knitter--check-pkg-name (package)
  "Check if PACKAGE is valid and return it."
  (thread-last package
               (knitter--check-name)
               (knitter--check-file)))

;; NOTE 2024-11-14: unused
(defun knitter--check-links (dotfiles)
  "Return DOTFILES if source files are valid and symlinks can be created."
  (mapcar (lambda (d)
            (cons (knitter--check-file (car d))
                  (knitter--check-symlink (cdr d))))
          dotfiles))


;;;;; Parse package

(cl-defmethod knitter-pkg-package-path ((pkg knitter-pkg))
  "Return the path for PKG if it is a package in `knitter-directory'."
  (condition-case nil
      (knitter--check-file
       (file-name-concat knitter-directory (knitter-pkg-name pkg)))
    (error (user-error "Invalid or not found package \"%s\""
                       (knitter-pkg-name pkg)))))

(cl-defmethod knitter-pkg-target-path ((pkg knitter-pkg))
  "Return target path for PKG."
  (condition-case nil
      (knitter-pkg-target pkg)
    (error (user-error "Invalid or undefined target for \"%s\": %s"
                       (knitter-pkg-name pkg) (knitter-pkg-target pkg)))))

(defun knitter-pkg--files (files pkg-path target-path)
  "Return an alist (file . symlink) for FILES from PKG-PATH to TARGET-PATH."
  (mapcar
   (lambda (f)
     (let ((src (if (stringp f) f (car f)))
           (sym (if (stringp f) (f-filename f) (f-filename (cdr f))))
           (sym-dir (if (stringp f) "" (f-dirname (cdr f)))))
       (cons (knitter--check-file (file-name-concat pkg-path src))
             (file-name-concat (if (string-match-p "^[~/]" sym-dir)
                                   sym-dir
                                 target-path)
                               sym))))
   files))

(defun knitter-pkg--globs (globs pkg-path target-path)
  "Return a list of files that match GLOBS from PKG-PATH to TARGET-PATH."
  (let ((default-directory pkg-path))
    (mapcan
     (lambda (g)
       (let ((glob (if (stringp g) g (car g)))
             (dest (if (stringp g) target-path (cdr g))))
         (mapcar (lambda (f)
                   (cons (knitter--check-file f)
                         (file-name-concat dest (f-filename f))))
                 (file-expand-wildcards glob :full :regexp))))
     globs)))

(cl-defmethod knitter-pkg-dotfiles ((pkg knitter-pkg))
  "Return a list of (source . symlink) for PKG."
  (let ((pkg-path (knitter-pkg-package-path pkg))
        (target-path (knitter-pkg-target-path pkg))
        (files (knitter-pkg-files pkg))
        (globs (knitter-pkg-globs pkg)))
    (if (and (not files) (not globs))
        (list (cons pkg-path target-path))
      (delete-dups
       (append
        (and files (knitter-pkg--files files pkg-path target-path))
        (and globs (knitter-pkg--globs globs pkg-path target-path)))))))


;;;;; Parse host

;; NOTE 2024-11-14: unused
(cl-defmethod knitter-host-path ((host knitter-host))
  "Return environment path for HOST."
  (file-name-concat (knitter--check-value (knitter-host-dir host))
                    (knitter--check-value (knitter-host-env host))))

(cl-defmethod knitter-host-pkg-list ((host knitter-host))
  "Return the package declaration for HOST.
If host :pkgs is nil. return all known packages."
  (or (knitter-host-pkgs host)
      (mapcar #'knitter-pkg-name knitter-pkg-list)))

(cl-defmethod knitter-host-dotfiles ((host knitter-host))
  "Return an alist of all source files and links for HOST packages.
Replace target path if host defines something different from \"~/\"."
  (mapcar (lambda (s)
            (cons (car s)
                  (replace-regexp-in-string
                   "^~/"
                   (file-name-as-directory (knitter-host-dir host))
                   (cdr s))))
          (mapcan #'knitter-pkg-dotfiles
                  (mapcar (lambda (p)
                            (knitter-get 'pkg p))
                          (knitter--check-list
                           (knitter-host-pkg-list host))))))

(defun knitter-host--with-pkg (&optional host-name pkg-name)
  "Overwrite HOST-NAME :pkgs list with PKG-NAME."
  (let ((host (if host-name
                  (knitter-get 'host host-name)
                (knitter-read 'host))))
    (setf (knitter-host-pkgs host)
          (list (or pkg-name (knitter-pkg-name (knitter-read 'pkg)))))
    (knitter-load)
    host))


;;;; Dotfiles installation

(defun knitter--make-directory (dir)
  "Create the directory DIR and its parents."
  (unless (f-directory? dir)
    (make-directory dir :parents)
    (knitter-log (format "Created directory: %s" dir))))

(defun knitter--make-symlink (dotfile)
  "Make symlink for DOTFILE.
Make a relative symlink if `knitter-symlinks-relative' is non-nil.
Overwrite symlinks if `knitter-symlinks-overwrite' is non-nil, which see."
  (let ((src (car dotfile))
        (sym (cdr dotfile)))
    (when (not (f-symlink? sym))
      (make-symbolic-link (if knitter-symlinks-relative
                              (file-relative-name src (f-dirname sym))
                            src)
                          sym
                          knitter-symlinks-overwrite)
      (knitter-log (format "Created symlink: %s" (cdr dotfile))))))

(defun knitter--delete-directory (dir)
  "Delete the directory DIR (it must be empty)."
  (when (and (f-directory? dir) (directory-empty-p dir))
    (delete-directory dir)
    (knitter-log (format "Deleted directory: %s" dir))))

(defun knitter--delete-symlink (symlink)
  "Delete SYMLINK."
  (when (f-symlink? symlink)
    (delete-file symlink)
    (knitter-log (format "Deleted symlink: %s" symlink))))

(defun knitter--dotfiles-dirs (dotfiles)
  "Return a list of unique directories from DOTFILES alist."
  (delete-dups
   (mapcar (lambda (d) (f-dirname (cdr d)))
           dotfiles)))

(defun knitter--dotfiles-symlinks (dotfiles)
  "Return a list of symlinks from DOTFILES alist."
  (mapcar #'cdr dotfiles))

(cl-defmethod knitter-uninstall-dotfiles ((host knitter-host))
  "Uninstall packages at HOST directory."
  (when-let* ((dotfiles (knitter-host-dotfiles host))
              (directories (knitter--dotfiles-dirs dotfiles))
              (symlinks (knitter--dotfiles-symlinks dotfiles))
              (host-name (knitter-host-name host)))
    (knitter-log (format "Uninstalling dotfiles for '%s'..." host-name))
    (mapc #'knitter--delete-symlink symlinks)
    (mapc #'knitter--delete-directory directories)
    (knitter-log (format "Dotfiles uninstalled for '%s'." host-name) :echo)))

(cl-defmethod knitter-install-dotfiles ((host knitter-host))
  "Install packages at HOST directory."
  (when knitter-uninstall-first (knitter-uninstall-dotfiles host))
  (knitter--make-directory (knitter-host-dir host))
  (when-let* ((dotfiles (knitter-host-dotfiles host))
              (directories (knitter--dotfiles-dirs dotfiles))
              (host-name (knitter-host-name host)))
    (knitter-log (format "Installing dotfiles for '%s'..." host-name))
    (mapc #'knitter--make-directory directories)
    (mapc #'knitter--make-symlink dotfiles)
    (knitter-log (format "Dotfiles installed for '%s'." host-name) :echo)))


;;;; Commands

;;;###autoload
(defun knitter-dired ()
  "Dired for dotfiles."
  (interactive)
  (knitter-reload)
  (dired (file-name-concat knitter-directory
                           (knitter-pkg-name (knitter-read 'pkg)))))

;;;###autoload
(defun knitter-install (&optional host-name pkg-name)
  "Install dotfiles for a host."
  (interactive)
  (knitter-reload)
  (knitter-install-dotfiles (knitter-host--with-pkg host-name pkg-name)))

;;;###autoload
(defun knitter-uninstall (&optional host-name pkg-name)
  "Uninstall dotfiles for a host."
  (interactive)
  (knitter-reload)
  (knitter-uninstall-dotfiles (knitter-host--with-pkg host-name pkg-name)))


;;; Provide

(provide 'knitter)

;;; knitter.el ends here
