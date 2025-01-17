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

BYGGSTEG_LOCATION=/etc/byggsteg
BYGGSTEG_DB=byggsteg.db

.PHONY: dir-install dir-uninstall dev server production-server fmt rebuild-db compile-gettext new-gettext merge-gettext init-po-nl init-po-en merge-po-nl merge-po-en

define byggsteg-log-info
	@printf "\n\033[1m>>= $(shell date) - info - %s\033[0m\n" $(1)
endef

define byggsteg-notify
	@-notify-send $(1)
endef

dir-install:
	$(call sss-log-info,"preparing directories for byggsteg")
	sudo mkdir -pv $(BYGGSTEG_LOCATION)/job-clone || true
	sudo chown -R $(shell whoami) $(BYGGSTEG_LOCATION)
dir-uninstall:
	$(call sss-log-info,"cleaning byggsteg installation")
	rm -rfv $(BYGGSTEG_LOCATION)/* || true
dev:
	$(call sss-log-info,"starting development server for byggsteg")
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"
server:
	$(call sss-log-info,"starting byggsteg server")
	art work
production-server:
	art work -c conf/production.conf
fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;

# Initialize a new database and enable WAL mode for higher concurrent writes to SQLite 3
rebuild-db:
	-rm -v $(BYGGSTEG_DB) $(BYGGSTEG_DB)-shm $(BYGGSTEG_DB)-wal
	touch 
	sqlite3 byggsteg.db 'PRAGMA journal_mode=WAL;'

define byggsteg-msgfmt
	msgfmt -v -o sys/i18n/locale/$(1)/LC_MESSAGES/translate.mo \
		sys/i18n/locale/$(1)/LC_MESSAGES/translate.po
endef

compile-gettext:
	$(call byggsteg-msgfmt "en_US")
	$(call byggsteg-msgfmt "nl_NL")

new-gettext:
	xgettext --default-domain=translate \
		--output=sys/i18n/locale/translate.pot \
		--language=Scheme \
		--keyword=G_ \
		--sort-by-file \
		--copyright-holder="Josep Bigorra & Mu Lei" \
		--package-name="byggsteg" \
		lib/byggsteg/job/*.scm \
		lib/byggsteg/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		--from-code UTF-8

merge-gettext:
	xgettext --default-domain=translate \
		--output=sys/i18n/locale/translate.pot \
		--language=Scheme \
		--keyword=G_ \
		--sort-by-file \
		--join-existing \
		--copyright-holder="Josep Bigorra & Mu Lei" \
		--package-name="byggsteg" \
		lib/byggsteg/job/*.scm \
		lib/byggsteg/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		--from-code UTF-8

init-po-nl:
	msginit -l nl_NL.UTF8 -o sys/i18n/locale/nl_NL/LC_MESSAGES/translate.po -i sys/i18n/locale/translate.pot --no-translator
init-po-en:
	msginit -l en_US.UTF8 -o sys/i18n/locale/en_US/LC_MESSAGES/translate.po -i sys/i18n/locale/translate.pot --no-translator
merge-po-nl:
	msgmerge -U sys/i18n/locale/nl_NL/LC_MESSAGES/translate.po sys/i18n/locale/translate.pot
merge-po-en:
	msgmerge -U sys/i18n/locale/en_US/LC_MESSAGES/translate.po sys/i18n/locale/translate.pot
