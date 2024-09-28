;;; run-server.scm

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (ice-9 popen))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format))
(use-modules (ice-9 string-fun))
(use-modules (ice-9 iconv))
(use-modules (web server))


(define source-files
  ;; byggsteg source code ordered list of modules to be loaded.
  ;; please ensure that load order is respected.
  '("preferences.scm"
    "process.scm"
    "base16.scm"
    "url.scm"
    "job.scm"
    "log.scm"
    "tailwind.scm"
    "html.scm"
    "server.scm"
    "main.scm"))
(for-each (lambda(file) (load (format #f "byggsteg/~a" file))) source-files)

(use-modules (byggsteg-main))
(run-server byggsteg-http-server)


