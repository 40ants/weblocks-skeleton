========================================
 Weblocks Site Skeleton For Quick Start
========================================

What is this?
=============

This is a skeleton repository for a quickstart with
an application which uses Weblocks and Postgresql.

It is highly opionated and includes other choices like:

* Using Zurb Foundation for the frontend;
* Using both Google Analytics and Yandex Metriks;
* Using Mito for ORM.
* Using qlot, roswell and lake.
* Using SLY instead of SLIME.

Quickstart
==========

1. Install roswell, qlot and `lake`_.
2. Install and start Docker.
3. Edit some variables in ``src/variables.lisp``.
4. Run ``lake migrate``.
5. Run ``lake devserver``.
6. Connect to the SLYNK server on ``localhost:14005``.
7. Hack your application.
8. Build an image for production with ``lake push``.

.. _lake: https://github.com/takagi/lake
