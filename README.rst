.. |melpa| image:: http://melpa.org/packages/edbi-django-badge.svg
    :target: http://melpa.org/#/edbi-django
    :alt: Melpa

===========
Edbi Django
===========

|melpa|

Edbi_ helper for Django_ projects.

Installation
------------

You can install this package from Melpa_::

    M-x package-install RET edbi-django RET

Usage
-----

To connect to databases defined in django settings module you must
prepare your Emacs environment.  Two environment variables must have
same values used to run ``django-admin.py`` command::

    M-x setenv RET DJANGO_SETTINGS_MODULE RET project.settings
    M-x setenv RET PYTHONPATH RET /home/user/path/to/project/

Also if you use virtual environment you need to provide
``python-shell-virtualenv-path`` variable.

.. code:: lisp

    (setq python-shell-virtualenv-path "/home/user/path/to/env/")

After Django project databases settings are available for you.  Run
command below and select available databases with your preferred
completing system::

    M-x edbi-django

.. _Edbi: https://github.com/kiwanami/emacs-edbi
.. _Django: https://docs.djangoproject.com/
.. _Melpa: http://melpa.milkbox.net/
