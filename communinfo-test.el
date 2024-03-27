;;; communinfo-test.el -*- lexical-binding: t; -*-

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

;;; Code:

(require 'communinfo)
(require 'ert)

(ert-deftest communinfo-test-manuals-unique ()
  "Each associated manual should be unique."
  (=
    (length (apply #'append (map-keys communinfo)))
    (length (apply #'append (seq-uniq (map-keys communinfo))))))

(ert-deftest communinfo-test-online ()
  "Associated URLs should be online."
  (map-keys-apply
    (lambda (manuals)
      (dolist (manual manuals)
        (if-let*
          ( (node (format "(%s)Top" manual))
            (url
              (condition-case nil
                (let ((Info-url-alist communinfo))
                  (Info-url-for-node node))
                (error nil))))
          (progn
            (message "communinfo-test: test: `%s' -> `%s'" node url)
            ;; test for http status code "ok".
            (should
              (equal 200
                (url-http-symbol-value-in-buffer
                  'url-http-response-status
                  ;; catch errors like "443 name or service not
                  ;; known".
                  (condition-case err
                    (url-retrieve-synchronously url t t 30)
                    (error (plist-get err 'error)))))))
          (message "communinfo-test: skip: `%s'" node))))
    communinfo))

(ert-deftest communinfo-test-urls-use-manual ()
  "If multiple manuals are associated at once, the URL-specification
should contain `%m'."
  (seq-map
    (pcase-lambda (`(,manuals . ,url-spec))
      (if (cdr manuals)
        (should (string-match-p "%m" url-spec))))
    communinfo))

(ert-deftest communinfo-test-urls-use-node ()
  "If the URL-specification is a string, it should contain `%e'."
  (map-values-apply
    (lambda (url-spec)
      (if (stringp url-spec)
        (should (string-match-p "%e" url-spec))))
    communinfo))

(provide 'communinfo-test)

;;; communinfo.el ends here
