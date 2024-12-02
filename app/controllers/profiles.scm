;;; profiles.scm

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

(define-artanis-controller profiles)

(import (byggsteg html))

(get "/profiles"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc (profile-list-page)))))

(get "/profiles/:path"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc (profile-page (params rc "path"))))))

(get "/profiles/manage/create"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc (add-profile-form-page)))))

(post "/profiles/manage/submit" profile-submit-endpoint)

(post "/profiles/manage/delete" profile-delete-endpoint)
