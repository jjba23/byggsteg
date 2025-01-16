# byggsteg - the CI/CD orchestrator written in Guile Scheme

# Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
# Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

# byggsteg is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# byggsteg is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.


.PHONY: prepare-dirs clean-dirs create server

prepare-dirs:
	mkdir -p /var/log/byggsteg || true
	mkdir -p /var/log/byggsteg/job-clone || true
clean-dirs:
	rm -rfv /var/log/byggsteg/* || true
create:
	sudo mkdir /var/log/byggsteg || true
	sudo chown $(shell whoami) /var/log/byggsteg
dev:
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"
server:
	art work
production-server:
	art work -c conf/production.conf
fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;
rebuild-db:
	-rm byggsteg.db
	-rm byggsteg.db-shm
	-rm byggsteg.db-wal
	touch byggsteg.db
	sqlite3 byggsteg.db 'PRAGMA journal_mode=WAL;'
compile-gettext:
	msgfmt -v -o sys/i18n/locale/en_US/LC_MESSAGES/dict.mo sys/i18n/locale/en_US/LC_MESSAGES/dict.po
	msgfmt -v -o sys/i18n/locale/nl_NL/LC_MESSAGES/dict.mo sys/i18n/locale/nl_NL/LC_MESSAGES/dict.po 
extract-gettext:
	xgettext --default-domain=dict \
		--output=sys/i18n/locale/dict.pot \
		--language=Scheme \
		--keyword=G_ \
		lib/byggsteg/job/*.scm \
		lib/byggsteg/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		--from-code UTF-8
nl-gettext:
	msginit -l nl_NL.UTF8 -o sys/i18n/locale/nl_NL/LC_MESSAGES/dict.po -i sys/i18n/locale/dict.pot --no-translator
en-gettext:
	msginit -l en_US.UTF8 -o sys/i18n/locale/en_US/LC_MESSAGES/dict.po -i sys/i18n/locale/dict.pot --no-translator
