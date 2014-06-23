(require 'ert)

(ert-deftest test-read-django-settings ()
  (let ((settings (plist-get (edbi-django-settings) :default)))
    (should (s-equals? (plist-get settings :ENGINE) "django.db.backends.sqlite3"))
    (should (s-equals? (plist-get settings :NAME) (f-join project-directory "db.sqlite3")))))

(ert-deftest test-completing-read-nil ()
  (should (null (edbi-django-completing-read "" nil))))

(ert-deftest test-completing-read-single ()
  (should (s-equals? "single" (edbi-django-completing-read "" '("single")))))

(ert-deftest test-completing-read-multiple ()
  (let ((completing-read-function (lambda (p c &rest i) (car c))))
    (should (s-equals? "first" (edbi-django-completing-read "" '("first" "second"))))))
