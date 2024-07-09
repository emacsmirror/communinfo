;;; communinfo-test.el --- Tests for communinfo  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Homepage:                https://codeberg.org/mekeor/communinfo
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:

(require 'communinfo)
(require 'ert)
(require 'url)

(defun communinfo-test-duplicates (elements)
  "Calculate list of duplicated elements in given list."
  (let ((seen nil) (duplicates nil))
    (while elements
      (let ((current (car elements)))
        (if (member current seen)
            (push current duplicates)
          (push current seen))
        (setq elements (cdr elements))))
    duplicates))

(ert-deftest communinfo-test-unique-manuals ()
  "Each associated manual should be unique."
  (should-not
   (communinfo-test-duplicates
    (flatten-list (map-keys communinfo-url-alist)))))

(ert-deftest communinfo-test-unique-url-specs ()
  "Each associated URL-specifications should be unique."
  (should-not
   (communinfo-test-duplicates
    (flatten-list (map-values communinfo-url-alist)))))

(ert-deftest communinfo-test-online ()
  "HTTP GET requests to URLs of `Top'-nodes should respond with OK."
  (map-keys-apply
   (lambda (manuals)
     (dolist (manual manuals)
       (when-let*
           ((url (let ((Info-url-alist communinfo-url-alist))
                   (ignore-errors
                     (Info-url-for-node (format "(%s)Top" manual))))))
         (should
          (equal
           (list manual url 200)
           (list manual url
                 (ignore-errors
                   (url-http-symbol-value-in-buffer
                    'url-http-response-status
                    (url-retrieve-synchronously url t t 30)))))))))
   communinfo-url-alist))

(ert-deftest communinfo-test-urls-use-manual ()
  "URL-specification of multi-manual association should contain `%m'.

The URL-specification of an association that maps multiple manuals at
once, should contain `%m' if it's a string."
  (seq-map (pcase-lambda (`(,manuals . ,url-spec))
             (if (and (cdr manuals) (stringp url-spec))
                 (should (string-match-p "%m" url-spec))))
           communinfo-url-alist))

(ert-deftest communinfo-test-urls-use-node ()
  "If URL-specification is string, it should contain `%e'."
  (map-values-apply (lambda (url-spec)
                      (if (stringp url-spec)
                          (should (string-match-p "%e" url-spec))))
                    communinfo-url-alist))

(provide 'communinfo-test)

;;; communinfo.el ends here
