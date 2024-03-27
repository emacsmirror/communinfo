;;; communinfo.el --- Community maintained Info-url-alist  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Mekeor Melire <mekeor@posteo.de>
;; Created: 2024
;; Homepage: https://codeberg.org/mekeor/emacs-communinfo
;; Keywords: docs
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "30.0.50"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Version 30 of GNU Emacs introduces a user option variable
;; `Info-url-alist' that allows to associate Info manuals to URLs. Its
;; default value only associates those Info manuals that are part of
;; Emacs. This package provides the variable `communinfo' which can be
;; used as value for `Info-url-alist' that provides URL-associations
;; for many more free, libre and open source software projects.

;; Here's how to use `communinfo' after installation:
;;
;;   (require 'info)
;;   (require 'communinfo)
;;
;;   (setopt Info-url-alist communinfo)

;;; Roadmap:

;; - Test non-Top node names.
;; - Option: Use short or long URL?
;; - Option: Link to which version? (Number, latest release or
;;   development?)
;; - Support manuals that are given by single HTMLs only.
;; - Option: Use single-HTML or HTML-per-node?
;; - Make `Info-copy-current-node-name' copy URL for certain prefix
;;   argument.

;;; Code:

(defun communinfo-url-cgdb (_ node encoded-node)
  "Associate `cgdb' manual nodes with URL."
  (concat
    "https://cgdb.github.io/docs/"
    (if (string-equal node "Top")
      "cgdb-split.html"
      (concat "cgdb-split/" encoded-node))))

(defconst communinfo
  '( (("lepton-manual" "lepton-scheme") . "https://lepton-eda.github.io/%m.html/%e")
     (("8sync") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("annotate" "bfd" "ctf-spec" "gdb" "stabs") . "https://sourceware.org/gdb/current/onlinedocs/gdb/%e")
     (("as" "bfd" "binutils" "ld" "gprof") . "https://sourceware.org/binutils/docs/%m/%e")
     (("auth" "autotype" "bovine" "calc" "ccmode" "cl" "dbus" "dired-x" "ebrowse" "ede" "ediff" "edt" "efaq" "efaq-w32" "eglot" "eieio" "eintr" "elisp" "emacs" "emacs-gnutls" "emacs-mime" "epa" "erc" "ert" "eshell" "eudc" "eww" "flymake" "forms" "gnus" "htmlfontify" "idlwave" "ido" "info" "mairix-el" "message" "mh-e" "modus-themes" "newsticker" "nxml-mode" "octave-mode" "org" "pcl-cvs" "pgg" "rcirc" "reftex" "remember" "sasl" "sc" "semantic" "ses" "sieve" "smtpmail" "speedbar" "srecode" "todo-mode" "tramp" "transient" "url" "use-package" "vhdl-mode" "vip" "viper" "vtable" "widget" "wisent" "woman") . "https://www.gnu.org/software/emacs/manual/html_node/%m/%e")
     (("cc-mode") . "https://cc-mode.sourceforge.net/html-manual/%e")
     (("cgdb") . communinfo-url-cgdb)
     (("chickadee") . "https://files.dthompson.us/docs/chickadee/latest/%e")
     (("complexity") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("cuirass") . "https://guix.gnu.org/%m/manual/html_node/%e")
     (("emacs-shroud") . "https://www.nongnu.org/emacs-shroud/manual/html_node/%e")
     (("emacsy") . "https://www.nongnu.org/emacsy/manual/html_node/%e")
     (("epkg" "borg") . "https://emacsmirror.net/manual/%m/%e")
     (("forge" "magit" "ghub" "transient" "magit-section" "with-editor") . "https://magit.vc/manual/%m/%e")
     (("freetalk") . "https://www.gnu.org/software/freetalk/manual/html_node/%e")
     (("g-golf") . "https://www.gnu.org/software/g-golf/manual/html_node/%e")
     (("geiser") . "https://www.nongnu.org/geiser/%e")
     (("gnumach-doc") . "https://www.gnu.org/software/hurd/gnumach-doc/%e")
     (("gnutls" "gnutls-client-server-use-case" "gnutls-crypto-layers" "gnutls-handshake-sequence" "gnutls-handshake-state" "gnutls-internals" "gnutls-layers" "gnutls-logo" "gnutls-modauth" "gnutls-x509" "pkcs11-vision") . "https://gnutls.org/manual/html_node/%e")
     (("gnutls-guile") . "https://gnutls.gitlab.io/guile/manual/html_node/%e")
     (("groff") . "https://www.gnu.org/software/%m/manual/%m.html.node/%e")
     (("grub" "grub-dev" "multiboot" "multiboot2") . "https://www.gnu.org/software/grub/manual/%m/html_node/%e")
     (("guile") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("guile-algorithms") . "https://guile-algorithms.lajszczak.dev/%e")
     (("guile-email") . "https://guile-email.systemreboot.net/manual/dev/en/%e")
     (("guile-proba") . "https://luis-felipe.gitlab.io/%m/manual/%e")
     (("guile-shapefile") . "https://hugonikanor.github.io/guile-shapefile/%e")
     (("guix") . "https://guix.gnu.org/%m/manual/en/html_node/%e")
     (("gwl") . "https://guixwl.org/manual/html_node/%e")
     (("haskell-mode") . "https://haskell.github.io/%m/manual/latest/%e")
     (("haunt") . "https://files.dthompson.us/docs/%m/latest/%e")
     (("libc") . "https://sourceware.org/glibc/manual/latest/html_node/%i")
     (("mailutils") . "https://mailutils.org/manual/html_node/%e")
     (("maintain" "standards") . "https://www.gnu.org/prep/%m/html_node/%e")
     (("mcron") . "https://www.gnu.org/software/mcron/manual/html_node/%e")
     (("mercury_faq" "mercury_library" "mercury_ref" "mercury_trans_guide" "mercury_user_guide") . "https://mercurylang.org/information/doc-release/%m/%e")
     (("mes") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("mit-scheme-ffi" "mit-scheme-ref" "mit-scheme-sos" "mit-scheme-user") . "https://www.gnu.org/software/mit-scheme/documentation/stable/%m/%e")
     (("octave") . "https://docs.octave.org/latest/%e")
     (("optionmatrix") . "https://anthonybradford.github.io/optionmatrix/%e")
     (("org") . "https://orgmode.org/manual/%e")
     (("orgguide") . "https://orgmode.org/guide/%e")
     (("polipo") . "https://www.irif.fr/~jch/software/%m/manual/%e")
     (("pspp") . "https://www.gnu.org/software/pspp/manual/html_node/%e")
     (("pspp-dev") . "https://www.gnu.org/software/pspp/pspp-dev/html_node/%e")
     (("recutils") . "https://www.gnu.org/software/%m/manual/%e")
     (("rottlog") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("sawfish") . "https://sawfish.tuxfamily.org/%m.html/%e")
     (("scm" "Xlibscm" "hobbit" "slib" "r5rs") . "https://people.csail.mit.edu/jaffer/%m/%e")
     (("screen") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("slime") . "https://slime.common-lisp.dev/doc/html/%e")
     (("wdiff") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("xboard") . "https://www.gnu.org/software/%m/manual/html_node/%e")
     (("zsh") . "https://zsh.sourceforge.io/Doc/Release/%e")))

(provide 'communinfo)

;;; communinfo.el ends here
