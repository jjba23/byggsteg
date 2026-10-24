;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A job that names an image is built by a container runtime instead of by the
;; daemon, and the whole of that decision is a command line and a handful of
;; variables.  So that is what these readings are: the command a step would run,
;; read as the words it is made of, and the environment its Docker clients read.
;;
;; Three things are worth checking and each of them is a way the build could be
;; wrong rather than merely different:
;;
;;   - the clone is mounted at the path it already has, and the container's
;;     user is the account that owns it, or the build writes files the daemon
;;     cannot read afterwards (or files owned by a subuid);
;;
;;   - a job that names no image is unchanged, byte for byte, because a
;;     deployment that never asked for containers must not start handing its
;;     commands to a runtime that happens to be installed;
;;
;;   - the socket a testcontainers suite connects to is the socket that exists.
;;     DOCKER_HOST pointing at nothing is worse than no DOCKER_HOST: a client
;;     waits for a connection instead of saying there is no runtime.

(define-module (veritas unit container-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg container)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg job steps)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (spec))

;; A socket that exists, so that readings about the environment can be taken
;; without a container runtime running on the machine that runs the suite.
(define fake-socket
  (format #f "/tmp/byggsteg-container-spec-~a.sock"
          (getpid)))

(unless (file-exists? fake-socket)
  (call-with-output-file fake-socket
    (lambda (port) (display "" port))))

(define (word-list-in command)
  "The words COMMAND is made of, as far as a reading needs them.

The command is a shell string whose words are single quoted by
`shell-quote', so this answers with the words as they were quoted and
not with the quotes."
  (map (lambda (word)
         (if (and (>= (string-length word) 2)
                  (char=? (string-ref word 0) #\')
                  (char=? (string-ref word
                                      (- (string-length word)
                                         1)) #\'))
             (substring word 1 (- (string-length word) 1))
             word))
       (string-split command #\space)))

(define (has-word? command word)
  (and (member word (word-list-in command)) #t))

(define (word-before command word)
  "The word immediately before WORD in COMMAND, or #f."
  (let loop ((words (word-list-in command)))
    (cond
      ((null? words) #f)
      ((null? (cdr words)) #f)
      ((string=? (cadr words) word) (car words))
      (else (loop (cdr words))))))

(define* (command-image #:key (runtime 'podman) (image "example/build:1"))
  (container-command "maak test"
                     #:image image
                     #:workdir "/tmp/byggsteg-clone"
                     #:runtime runtime))

(define podman-command (command-image))
(define docker-command (command-image #:runtime 'docker))

(define container-suite
  (suite "Container Suite"

         (test "A job that names no image runs on the machine"
               (assert-equal #:expect "maak test"
                             #:got (container-command "maak test")
                             #:name "Nothing is wrapped when nothing asked for it")
               (assert-equal #:expect "maak test"
                             #:got (container-command "maak test" #:image #f)
                             #:name "No image, no runtime")
               (assert-equal #:expect "maak test"
                             #:got (parameterize ((container-runtime-override
                                                   'none))
                                     (container-command "maak test"
                                                        #:image "alpine"))
                             #:name "A runtime of none leaves the command alone"))

         (test "The deployment says which runtime, and none is a real answer"
               (assert-equal #:expect 'podman
                           #:got (parameterize ((job-container-runtime "podman")
                                                (container-runtime-override #f))
                                  (container-runtime))
                           #:name "podman is taken at its word")
               (assert-equal #:expect 'docker
                           #:got (parameterize ((job-container-runtime "docker")
                                                (container-runtime-override #f))
                                  (container-runtime))
                           #:name "so is docker")
               (assert-equal #:expect 'none
                           #:got (parameterize ((job-container-runtime "none")
                                                (container-runtime-override #f))
                                  (container-runtime))
                           #:name "none is a runtime a deployment may name"))

         (test "An executable is found in PATH by its full path"
               (assert-true (and (find-executable "sh") #t)
                            #:name "a program that is installed is found")
               (assert-false (find-executable "byggsteg-no-such-program")
                             #:name "a program that is not is not")
               (assert-false (find-executable "/bin/sh")
                             #:name "an absolute name is not looked up in PATH"))

         (test "The clone is mounted where it already is"
               (assert-true (has-word? podman-command
                                       "/tmp/byggsteg-clone")
                            #:name "The workdir is named")
               (assert-true (has-word? podman-command
                                       "/tmp/byggsteg-clone:/tmp/byggsteg-clone")
                            #:name "and mounted at the same path")
               (assert-true (has-word? podman-command "/bin/sh")
                            #:name "the shell is the entrypoint")
               (assert-true (has-word? podman-command "-lc")
                            #:name "the command is a login shell's -c"))

         (test "A podman build is the caller's, rootless"
               (assert-true (has-word? podman-command "--userns=keep-id")
                            #:name "keep-id maps the container's user to the caller")
               (assert-false (has-word? podman-command "--user=0:0")
                             #:name "the build is not root"))

         (test "A docker build names the user to write as"
               (assert-true (any (lambda (word)
                                   (string-prefix? "--user=" word))
                                 (word-list-in docker-command))
                            #:name "docker has no keep-id, so the identity is named")
               (assert-false (has-word? docker-command "--userns=keep-id")
                             #:name "and the podman-only flag is not passed"))

         (test "The job's environment reaches the container"
               (let ((command (container-command "maak test"
                                                 #:image "alpine"
                                                 #:workdir "/tmp/c"
                                                 #:env (list (cons "FOO" "bar")))))
                 (assert-true (has-word? command "--env")
                              #:name "variables are passed with --env")
                 (assert-true (has-word? command "FOO=bar")
                              #:name "the job's own variables are passed as written")
                 (assert-true (has-word? command "LC_ALL=C.UTF-8")
                              #:name "a locale is named when the job named none")))

         (test "A locale the job names is left as it is"
               (let ((command (container-command "maak test"
                                                 #:image "alpine"
                                                 #:env (list (cons "LANG"
                                                                   "nl_NL.UTF-8")))))
                 (assert-true (has-word? command "LANG=nl_NL.UTF-8")
                              #:name "the job's locale is passed")
                 (assert-false (has-word? command "LC_ALL=C.UTF-8")
                               #:name "and not overruled by the default")))

         (test "The image is the word before the shell's arguments"
               (assert-equal #:expect "example/build:1"
                             #:got (word-before podman-command "-lc")
                             #:name "the image is what -lc runs in")
               (assert-equal #:expect "--entrypoint"
                             #:got (word-before podman-command "/bin/sh")
                             #:name "the shell is named as the entrypoint"))

         (test "The runtime an image is built with can be named per deployment"
               (assert-equal #:expect "alpine"
                             #:got (let ((image-name "alpine"))
                                     (parameterize ((job-container-image image-name))
                                       (container-image)))
                             #:name "BYGGSTEG_CONTAINER_IMAGE is the default image")
               (assert-false (container-image)
                             #:name "and empty means no default image"))

         (test "A socket that exists is the one a client is told about"
               (assert-equal #:expect fake-socket
                             #:got (parameterize ((container-runtime-override
                                                   'podman)
                                                  (container-docker-host
                                                   (string-append "unix://"
                                                                  fake-socket))
                                                  (job-container-user "")
                                                  (job-container-network ""))
                                     (container-socket-path))
                             #:name "DOCKER_HOST wins when the deployment set it")
               (let ((environment
                      (parameterize ((container-runtime-override 'podman)
                                     (container-docker-host
                                      (string-append "unix://" fake-socket))
                                     (job-container-user "")
                                     (job-container-network ""))
                        (container-environment))))
                 (assert-true (and (member (format #f "DOCKER_HOST=unix://~a"
                                                   fake-socket)
                                           environment) #t)
                              #:name "the job is handed one DOCKER_HOST")
                 (assert-true (and (member "TESTCONTAINERS_RYUK_DISABLED=true"
                                           environment) #t)
                              #:name "the reaper is off unless a deployment wants it")
                 (assert-true (any (lambda (entry)
                                     (string-prefix? "TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE="
                                                     entry))
                                   environment)
                              #:name "the reaper is told which socket is mounted")))

         (test "A socket that is not there is not handed out"
               (let ((environment
                      (parameterize ((container-runtime-override 'podman)
                                     (container-docker-host
                                      "unix:///tmp/byggsteg-no-socket")
                                     (job-container-user "")
                                     (job-container-network ""))
                        (container-environment))))
                 (assert-equal #:expect '()
                               #:got environment
                               #:name "A DOCKER_HOST that points at nothing is worse than none")))

         (test "A job that names its own runtime keeps it"
               (let ((environment
                      (parameterize ((container-runtime-override 'podman)
                                     (container-docker-host
                                      (string-append "unix://" fake-socket))
                                     (job-container-user "")
                                     (job-container-network ""))
                        (job-command-environment
                         (list (cons "DOCKER_HOST" "unix:///run/docker.sock"))))))
                 (assert-true (and (member "DOCKER_HOST=unix:///run/docker.sock"
                                           environment) #t)
                              #:name "the job's own DOCKER_HOST is what the command sees")
                 (assert-false (member (format #f "DOCKER_HOST=unix://~a"
                                               fake-socket)
                                       environment)
                               #:name "and byggsteg does not add a second one")))

         (test "A deployment that names a runner runs the runtime as it"
               (let ((command (parameterize ((job-container-user "nobody")
                                             (container-runtime-override
                                              'podman))
                               (container-command "true"
                                                  #:image "alpine"
                                                  #:workdir "/tmp"))))
                 (assert-true (has-word? command "sudo")
                              #:name "the runtime is started as the other account")
                 (assert-true (has-word? command "nobody")
                              #:name "and the account is named")
                 (assert-true (any (lambda (word)
                                     (string-prefix? "HOME=" word))
                                   (word-list-in command))
                              #:name "HOME is passed through sudo")
                 (assert-true (any (lambda (word)
                                     (string-prefix? "XDG_RUNTIME_DIR=/run/user/"
                                                     word))
                                   (word-list-in command))
                              #:name "and so is the runtime directory of that account")))))

(define (spec)
  (veritas-run container-suite))
