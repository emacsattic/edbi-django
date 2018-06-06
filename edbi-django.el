;;; edbi-django.el --- Run edbi with django settings

;; Copyright (C) 2014-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-django
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (pythonic "0.1.0") (edbi "0.1.3"))

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

;; See the README for more details.

;;; Code:

(require 'pythonic)
(require 'json)
(require 'edbi)
(require 'f)

(defvar edbi-django-command "
from __future__ import print_function
from django.conf import settings
from json import dumps

print(dumps(settings.DATABASES))
" "Python code to read django settings.")

(defvar edbi-django-engines
  '(("django.db.backends.postgresql_psycopg2" . "Pg")
    ("django.db.backends.sqlite3" . "SQLite")
    ("django.db.backends.oracle" . "Oracle")
    ("django.db.backends.mysql" . "mysql"))
  "Django to DBI engines mapping.")

(defun edbi-django-completing-read (prompt collection)
  "Ask with PROMPT for COLLECTION element."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (completing-read prompt collection))))

(defun edbi-django-settings ()
  "Read django settings."
  (let ((json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string
     (with-output-to-string
       (with-current-buffer
           standard-output
         (pythonic-call-process :buffer standard-output
                                :args (list "-c" edbi-django-command)))))))

(defun edbi-django-databases (settings)
  "Databases list defined in SETTINGS."
  (mapcar 'car settings))

(defun edbi-django-uri (options)
  "Generate DBI connection uri from Django OPTIONS."
  (format "dbi:%s:%s"
          (edbi-django-engine options)
          (s-join "" (list (edbi-django-dbname options)
                           (edbi-django-host options)
                           (edbi-django-port options)))))

(defun edbi-django-engine (options)
  "Get ENGINE from Django OPTIONS."
  (cdr (assoc (cdr (assoc "ENGINE" options)) edbi-django-engines)))

(defun edbi-django-dbname (options)
  "Get NAME from Django OPTIONS."
  (--if-let (cdr (assoc "NAME" options))
      (format "dbname=%s;" it)
    ""))

(defun edbi-django-host (options)
  "Get HOST from Django OPTIONS."
  (--if-let (cdr (assoc "HOST" options))
      (format "host=%s;" it)
    ""))

(defun edbi-django-port (options)
  "Get PORT from Django OPTIONS."
  (--if-let (cdr (assoc "PORT" options))
      (format "port=%s;" it)
    ""))

(defun edbi-django-user (options)
  "Get USER from Django OPTIONS."
  (cdr (assoc "USER" options)))

(defun edbi-django-password (options)
  "Get PASSWORD from Django OPTIONS."
  (cdr (assoc "PASSWORD" options)))

(defun edbi-django-data-source (options)
  "Make `edbi' data source from Django OPTIONS."
  (edbi:data-source (edbi-django-uri options)
                    (edbi-django-user options)
                    (edbi-django-password options)))

;;;###autoload
(defun edbi-django ()
  "Connect to Django databases."
  (interactive)
  (let* ((settings (edbi-django-settings))
         (databases (edbi-django-databases settings))
         (database (edbi-django-completing-read "Database: " databases))
         (options (cdr (assoc database settings)))
         (connection (edbi:start))
         (source (edbi-django-data-source options)))
    (edbi:connect connection source)
    (edbi:dbview-open connection)))

(provide 'edbi-django)

;;; edbi-django.el ends here
