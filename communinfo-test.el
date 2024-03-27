;; -*- lexical-binding: t; -*-

(require 'communinfo)
(require 'ert)

(ert-deftest communinfo-test ()
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
