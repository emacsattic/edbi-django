;;; edbi-django.el --- Run edbi with django settings

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-django
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (edbi "0.1.3") (f "0.16.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To connect to databases defined in django settings module you must
;; prepare your Emacs environment.  Two environment variables must have
;; same values used to run django-admin.py command
;;
;;     M-x setenv RET DJANGO_SETTINGS_MODULE RET project.settings
;;     M-x setenv RET PYTHONPATH RET /home/user/path/to/project/
;;
;; Also if you use virtual environment you need to provide
;; `python-shell-virtualenv-path' variable
;;
;;     (setq python-shell-virtualenv-path "/home/user/path/to/env/")
;;
;; After Django project databases settings are available for you.  Run
;; command below and select available databases with your preferred
;; completing system
;;
;;     M-x edbi-django

;;; Code:

(require 'python)
(require 'json)
(require 'edbi)
(require 'f)

(defvar edbi-django-directory (f-dirname load-file-name)
  "Directory contain `django-edbi' package.")

(defvar edbi-django-script (f-join edbi-django-directory "edbi_django.py")
  "Script path to read django settings.")

(defvar edbi-django-engines
  '(("django.db.backends.postgresql_psycopg2" . "Pg")
    ("django.db.backends.sqlite3" . "SQLite")
    ("django.db.backends.oracle" . "Oracle")
    ("django.db.backends.mysql" . "mysql"))
  "Django to DBI engines mapping.")

(defvar edbi-django-options
  '(("NAME" . "dbname")
    ("HOST" . "host")
    ("PORT" . "port"))
  "Django to BDI connect options mapping.")

(defvar edbi-django-connection nil
  "Django edbi connection.")

(defun edbi-django-completing-read (prompt collection)
  "Ask with PROMPT for COLLECTION element."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (completing-read prompt collection))))

(defun edbi-django-settings ()
  "Read django settings."
  (let ((python-shell-interpreter "python")
        (python-shell-interpreter-args edbi-django-script)
        (json-object-type 'hash-table))
    (condition-case nil
        (json-read-from-string
         (shell-command-to-string
          (python-shell-parse-command)))
      (error (error "Unable to read database django settings")))))

(defun edbi-django-filter (item mapping)
  "Get Django ITEM by DBI MAPPING."
  (cdr (--first (s-equals? (car it) item) mapping)))

(defun edbi-django-engine (engine)
  "Get DBI engine by Django ENGINE."
  (edbi-django-filter engine edbi-django-engines))

(defun edbi-django-option (option)
  "Get DBI option by Django OPTION."
  (edbi-django-filter option edbi-django-options))

(defun edbi-django-format-engine (options)
  "Generate DBI engine info from Django OPTIONS."
  (format "dbi:%s" (edbi-django-engine (gethash "ENGINE" options))))

(defun edbi-django-format-options (options)
  "Generate DBI options from Django OPTIONS."
  (let ((ignore-options '("ENGINE" "USER" "PASSWORD"))
        params)
    (maphash
     (lambda (key value)
       (unless (member key ignore-options)
         (push
          (format "%s=%s" (edbi-django-option key) value)
          params)))
     options)
    (mapconcat 'identity params ";")))

(defun edbi-django-databases (settings)
  "Databases list defined in SETTINGS."
  (let (databases)
    (maphash
     (lambda (key value)
       (push key databases))
     settings)
    (sort databases 'string<)))

(defun edbi-django-uri (options)
  "Generate DBI connection uri from Django OPTIONS."
  (format "%s:%s"
          (edbi-django-format-engine options)
          (edbi-django-format-options options)))

(defun edbi-django-user (options)
  "Get USER from Django OPTIONS."
  (gethash "USER" options))

(defun edbi-django-password (options)
  "Get PASSWORD from Django OPTIONS."
  (gethash "PASSWORD" options))

(defun edbi-django-connect ()
  "Connect to database used in Django."
  (let* ((settings (edbi-django-settings))
         (databases (edbi-django-databases settings))
         (database (edbi-django-completing-read "Database: " databases))
         (options (gethash database settings)))
    (setq edbi-django-connection (edbi:start))
    (edbi:connect edbi-django-connection
                  (edbi:data-source (edbi-django-uri options)
                                    (edbi-django-user options)
                                    (edbi-django-password options)))))

;;;###autoload
(defun edbi-django ()
  "Connect to Django databases."
  (interactive)
  (edbi-django-connect)
  (edbi:dbview-open edbi-django-connection))

(provide 'edbi-django)

;;; edbi-django.el ends here
