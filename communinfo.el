;;; communinfo.el --- Community maintained Info-url-alist  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Mekeor Melire <mekeor@posteo.de>
;; Created: 2024
;; Homepage: https://codeberg.org/mekeor/emacs-communinfo
;; Keywords: docs
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "30"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Version 30 of GNU Emacs introduces a user option variable
;; `Info-url-alist' that allows to associate Info manuals to URLs.
;; Its default value only associates those Info manuals that are part
;; of Emacs.  This package provides the variable
;; `communinfo-url-alist' which can be used as value for
;; `Info-url-alist' that provides URL-associations for many more free,
;; libre and open source software projects.

;; Here's how to use `communinfo' after installation:
;;
;;   (require 'info)
;;   (require 'communinfo)
;;
;;   (setopt Info-url-alist communinfo-url-alist)

;;; Code:

(defun communinfo-url-cgdb (_ node encoded-node)
  "Associate `cgdb' manual nodes with URL.

NODE is the name of the `cgbd' info manual node.
ENCODED-NODE is the URL-encoding of NODE.

See `communinfo-url-alist' for more information."
  (concat "https://cgdb.github.io/docs/"
          (if (string-equal node "Top")
              "cgdb-split.html"
            (concat "cgdb-split/" encoded-node))))

(defconst communinfo-url-alist
  '((("3dldf") . "https://gnu.org/s/%m/manual/user_ref/%e")
    (("8sync" "alive" "anubis" "artanis" "autoconf" "autogen" "automake" "bash" "bison" "ccd2cue" "cflow" "chess" "combine" "complexity" "coreutils" "cpio" "dico" "diffutils" "emacs-muse" "g-golf" "gama" "gawk" "gcal" "gettext" "gnubg" "gnubik" "gnulib" "gperf" "grep" "groff" "gsasl" "gsl" "gsrc" "gss" "guile" "guile-rpc" "gv" "gzip" "hello" "idutils" "inetutils" "libextractor" "libidn" "libmicrohttpd" "librejs" "libtasn1" "libtool" "lightning" "liquidwar6" "m4" "mailutils" "make" "mdk" "mes" "parted" "proxyknife" "pspp" "pyconfigure" "rcs" "remotecontrol" "rottlog" "rush" "screen" "sed" "sharutils" "shepherd" "sourceinstall" "sqltutor" "tar" "teseq" "units" "vc-dwim" "wdiff" "websocket4j" "wget" "xboard") . "https://gnu.org/s/%m/manual/html_node/%e")
    (("Xlibscm" "hobbit" "r5rs" "scm" "slib") . "https://people.csail.mit.edu/jaffer/%m/%e")
    (("annotate" "gdb" "stabs") . "https://sourceware.org/gdb/current/onlinedocs/%m.html/%e")
    (("as" "bfd" "binutils" "gprof" "ld") . "https://sourceware.org/binutils/docs/%m/%e")
    (("assuan" "dirmngr" "gcrypt" "gnupg" "gpgme" "ksba") . "https://www.gnupg.org/documentation/manuals/%m/%e")
    (("atk" "clutter" "clutter-glx" "corba" "gconf" "gdk" "glib" "gnome-vfs" "gobject" "gtk" "libglade" "libgnome" "libgnomecanvas" "libgnomeui" "pango" "pangocairo") . "https://gnu.org/s/guile-gnome/docs/%m/html/%e")
    (("auctex" "preview-latex") . "https://gnu.org/s/auctex/manual/%m/%e")
    (("auth" "autotype" "bovine" "calc" "ccmode" "cl" "dbus" "dired-x" "ebrowse" "ede" "ediff" "edt" "efaq" "efaq-w32" "eglot" "eieio" "eintr" "elisp" "emacs" "emacs-gnutls" "emacs-mime" "epa" "erc" "ert" "eshell" "eudc" "eww" "flymake" "forms" "gnus" "htmlfontify" "idlwave" "ido" "info" "mairix-el" "message" "mh-e" "modus-themes" "newsticker" "nxml-mode" "octave-mode" "org" "pcl-cvs" "pgg" "rcirc" "reftex" "remember" "sasl" "sc" "semantic" "ses" "sieve" "smtpmail" "speedbar" "srecode" "todo-mode" "tramp" "url" "use-package" "vhdl-mode" "vip" "viper" "vtable" "widget" "wisent" "woman") . "https://gnu.org/s/emacs/manual/html_node/%m/%e")
    (("cgdb") . communinfo-url-cgdb)
    (("chickadee") . "https://files.dthompson.us/docs/chickadee/latest/%e")
    (("cpp" "cppinternals" "gcc" "gccint" "gfc-internals" "gfortran" "gnat-style" "gnat_rm" "gnat_ugn" "libgomp" "libiberty" "libstdc++") . "https://gcc.gnu.org/onlinedocs/%m/%e")
    (("cuirass") . "https://guix.gnu.org/%m/manual/html_node/%e")
    (("cvs" "gnun" "web-trans") . "https://gnu.org/s/trans-coord/manual/%m/html_node/%e")
    (("easejs" "emms" "gdbm" "gmediaserver" "gnu-crypto" "gnuprologjava" "motti" "recutils" "thales") . "https://gnu.org/s/%m/manual/%e")
    (("emacs-shroud") . "https://nongnu.org/emacs-shroud/manual/html_node/%e")
    (("emacsy") . "https://nongnu.org/emacsy/manual/html_node/%e")
    (("epkg" "borg") . "https://emacsmirror.net/manual/%m/%e")
    (("flex") . "https://westes.github.io/flex/manual/%e")
    (("forge" "magit" "ghub" "transient" "magit-section" "with-editor") . "https://magit.vc/manual/%m/%e")
    (("freetalk") . "https://gnu.org/s/freetalk/manual/html_node/%e")
    (("gawkinet" "gawkworkflow" "pm-gawk") . "https://gnu.org/s/gawk/manual/%m/html_node/%e")
    (("geiser") . "https://nongnu.org/geiser/%e")
    (("gforth") . "https://www.complang.tuwien.ac.at/forth/%m/Docs-html/%e")
    (("gmp") . "https://www.gmplib.org/manual/%e")
    (("gnu-arch") . "https://gnu.org/s/gnu-arch/tutorial/%e")
    (("gnumach-doc") . "https://gnu.org/s/hurd/gnumach-doc/%e")
    (("gnutls") . "https://gnutls.org/manual/html_node/%e")
    (("gnutls-guile") . "https://gnutls.gitlab.io/guile/manual/html_node/%e")
    (("grub" "grub-dev" "multiboot" "multiboot2") . "https://gnu.org/s/grub/manual/%m/html_node/%e")
    (("gst") . "https://gnu.org/s/smalltalk/manual/html_node/%e")
    (("gst-base") . "https://gnu.org/s/smalltalk/manual-base/html_node/%e")
    (("gst-libs") . "https://gnu.org/s/smalltalk/manual-libs/html_node/%e")
    (("guile-algorithms") . "https://guile-algorithms.lajszczak.dev/%e")
    (("guile-email") . "https://guile-email.systemreboot.net/manual/dev/en/%e")
    (("guile-gtk") . "https://gnu.org/s/guile-gtk/docs/%m/%e")
    (("guile-proba") . "https://luis-felipe.gitlab.io/%m/manual/%e")
    (("guile-shapefile") . "https://hugonikanor.github.io/guile-shapefile/%e")
    (("guix") . "https://guix.gnu.org/%m/manual/en/html_node/%e")
    (("gwl") . "https://guixwl.org/manual/html_node/%e")
    (("haskell-mode") . "https://haskell.github.io/%m/manual/latest/%e")
    (("haunt") . "https://files.dthompson.us/docs/%m/latest/%e")
    (("info-stnd" "texi2any_api" "texinfo") . "https://gnu.org/s/texinfo/manual/%m/html_node/%e")
    (("lepton-manual" "lepton-scheme") . "https://lepton-eda.github.io/%m.html/%e")
    (("libc") . "https://sourceware.org/glibc/manual/latest/html_node/%e")
    (("libidn2") . "https://gnu.org/s/libidn/libidn2/manual/html_node/%e")
    (("maintain" "standards") . "https://gnu.org/prep/%m/html_node/%e")
    (("mcron") . "https://gnu.org/s/mcron/manual/html_node/%e")
    (("mercury_faq" "mercury_library" "mercury_ref" "mercury_trans_guide" "mercury_user_guide") . "https://mercurylang.org/information/doc-release/%m/%e")
    (("mit-scheme-ffi" "mit-scheme-ref" "mit-scheme-sos" "mit-scheme-user") . "https://gnu.org/s/mit-scheme/documentation/stable/%m/%e")
    (("octave") . "https://docs.octave.org/latest/%e")
    (("optionmatrix") . "https://anthonybradford.github.io/optionmatrix/%e")
    (("orgguide") . "https://orgmode.org/guide/%e")
    (("pies") . "https://gnu.org.ua/software/%m/manual/%e")
    (("plotutils") . "https://gnu.org/s/%m/manual/en/html_node/%e")
    (("polipo") . "https://www.irif.fr/~jch/software/%m/manual/%e")
    (("pspp-dev") . "https://gnu.org/s/pspp/pspp-dev/html_node/%e")
    (("sawfish") . "https://sawfish.tuxfamily.org/%m.html/%e")
    (("slime") . "https://slime.common-lisp.dev/doc/html/%e")
    (("zsh") . "https://zsh.sourceforge.io/Doc/Release/%e")))

(provide 'communinfo)

;;; communinfo.el ends here
