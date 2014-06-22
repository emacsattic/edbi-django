(require 'ert)

(ert-deftest test-read-django-settings ()
  (let ((settings (plist-get (edbi-django-settings) :default)))
    (should (s-equals? (plist-get settings :ENGINE) "django.db.backends.sqlite3"))
    (should (s-equals? (plist-get settings :NAME) (f-join project-directory "db.sqlite3")))))
