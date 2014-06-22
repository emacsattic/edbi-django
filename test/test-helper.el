(require 'cask)

(defvar root-directory (locate-dominating-file load-file-name "Cask"))

(defvar project-directory (expand-file-name ".project" root-directory))

(cask-initialize root-directory)

(add-to-list 'load-path root-directory)

(require 'edbi-django)

(unless (file-exists-p project-directory)
  (let ((default-directory root-directory))
    (make-directory project-directory)
    (shell-command "django-admin.py startproject project .project")))

(setenv "DJANGO_SETTINGS_MODULE" "project.settings")

(setenv "PYTHONPATH" project-directory)
