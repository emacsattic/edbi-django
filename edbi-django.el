;;; edbi-django.el --- Run edbi with django settings

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-django
;; Version: 0.0.1
;; Package-Require: ((emacs "24") (f "0.16.2"))

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

;;; Code:

(require 'python)
(require 'json)
(require 'f)

(defvar edbi-django-directory (f-dirname load-file-name)
  "Directory contain `django-edbi' package.")

(defvar edbi-django-script (f-join edbi-django-directory "edbi_django.py")
  "Script path to read django settings.")

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
        (json-object-type 'plist))
    (condition-case nil
        (json-read-from-string
         (shell-command-to-string
          (python-shell-parse-command)))
      (error (error "Unable to read database django settings")))))

(provide 'edbi-django)

;;; edbi-django.el ends here
