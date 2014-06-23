(require 'ert)

(ert-deftest test-read-django-settings ()
  (let ((settings (gethash "default" (edbi-django-settings))))
    (should (s-equals? (gethash "ENGINE" settings) "django.db.backends.sqlite3"))
    (should (s-equals? (gethash "NAME" settings) (f-join project-directory "db.sqlite3")))))

(ert-deftest test-read-settings-error ()
  (let* ((envvar "DJANGO_SETTINGS_MODULE=project.settings")
         (process-environment (--remove (s-equals-p it envvar) process-environment)))
    (should-error (edbi-django-settings))))

(ert-deftest test-read-settings-error-message ()
  (let* ((envvar "DJANGO_SETTINGS_MODULE=project.settings")
         (process-environment (--remove (s-equals-p it envvar) process-environment)))
    (should (s-equals? "Unable to read database django settings"
                       (condition-case err
                           (edbi-django-settings)
                         (error (error-message-string err)))))))

(ert-deftest test-completing-read-nil ()
  (should (null (edbi-django-completing-read "" nil))))

(ert-deftest test-completing-read-single ()
  (should (s-equals? "single" (edbi-django-completing-read "" '("single")))))

(ert-deftest test-completing-read-multiple ()
  (let ((completing-read-function (lambda (p c &rest i) (car c))))
    (should (s-equals? "first" (edbi-django-completing-read "" '("first" "second"))))))

(ert-deftest test-get-dbi-engine ()
  (should (s-equals? "Pg" (edbi-django-engine "django.db.backends.postgresql_psycopg2"))))

(ert-deftest test-get-dbi-option ()
  (should (s-equals? "dbname" (edbi-django-option "NAME"))))

(ert-deftest test-build-dbi-format-engine ()
  (should (s-equals? (edbi-django-format-engine (gethash "default" (edbi-django-settings)))
                     "dbi:SQLite")))

(ert-deftest test-build-dbi-format-options ()
  (should (s-equals? (edbi-django-format-options (gethash "default" (edbi-django-settings)))
                     (concat "dbname=" (f-join project-directory "db.sqlite3")))))

(ert-deftest test-read-table-list ()
  (edbi-django-connect)
  (should (equal '("auth_group"
                   "auth_group_permissions"
                   "auth_permission"
                   "auth_user"
                   "auth_user_groups"
                   "auth_user_user_permissions"
                   "django_admin_log"
                   "django_content_type"
                   "django_session")
                 (sort (--map
                        (cadr it)
                        (edbi:sync edbi:select-all-d
                                   edbi-django-connection
                                   "SELECT * FROM sqlite_master WHERE type='table';"))
                       'string<))))
