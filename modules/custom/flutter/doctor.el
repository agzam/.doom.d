;;; custom/flutter/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "dart")
  (warn! "dart executable isn't on $PATH."))

(unless (executable-find "flutter")
  (warn! "flutter executable isn't on $PATH."))
