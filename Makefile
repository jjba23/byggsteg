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

# Code:

# Constants

BYGGSTEG_LOCATION="/etc/byggsteg"
BYGGSTEG_DB="byggsteg.db"
COPYRIGHT_HOLDERS="Josep Bigorra & Mu Lei"
I18N_LOCATION="sys/i18n/locale"

.PHONY: dir-install dir-uninstall dev server production-server fmt rebuild-db compile-i18n new-i18n join-i18n init-i18n-nl init-i18n-en merge-i18n-nl merge-i18n-en

# Logging helpers

define byggsteg-log-info
	@printf "\n\033[1m>>= $(shell date) - info - %s\033[0m\n" $(1)
endef

define byggsteg-notify
	@-notify-send $(1)
endef

# i18n helpers

define byggsteg-msgfmt
	msgfmt -v -o $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.mo \
		$(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po
endef

define byggsteg-msginit
	msginit -l $(1).UTF8 -o $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po \
		-i $(I18N_LOCATION)/translate.pot --no-translator
endef

define byggsteg-merge
	msgmerge -U $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po \
		$(I18N_LOCATION)/translate.pot
endef

# Target to prepare directories for the byggsteg project
# Create the necessary directory for job cloning, if it doesn't exist
# Change ownership of the byggsteg location to the current user
dir-install:
	$(call sss-log-info,"preparing directories for byggsteg")
	sudo mkdir -pv $(BYGGSTEG_LOCATION)/job-clone || true
	sudo chown -R $(shell whoami) $(BYGGSTEG_LOCATION)

# Target to clean up the byggsteg installation (remove job files)
# Remove all files in the byggsteg location, ignoring errors
dir-uninstall:
	$(call sss-log-info,"cleaning byggsteg installation")
	rm -rfv $(BYGGSTEG_LOCATION)/* || true

# Target to start a development server for byggsteg
# Use watchexec to watch for file changes and restart the server on changes
dev:
	$(call sss-log-info,"starting development server for byggsteg")
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"

# Target to start the byggsteg server
# Start the server with the default configuration
server:
	$(call sss-log-info,"starting byggsteg server")
	art work

# Target to start the byggsteg server with production configuration
production-server:
	art work -c conf/production.conf

# Target to format source code files according to the style guide
# Find all .scm files and format them using 'guix style'
fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;


# Initialize a new database and enable WAL mode for higher concurrent writes to SQLite 3
rebuild-db:
	-rm -v $(BYGGSTEG_DB) $(BYGGSTEG_DB)-shm $(BYGGSTEG_DB)-wal
	touch 
	sqlite3 byggsteg.db 'PRAGMA journal_mode=WAL;'

compile-i18n:
	$(call byggsteg-msgfmt "en_US")
	$(call byggsteg-msgfmt "nl_NL")

new-i18n:
	xgettext --default-domain=translate \
		--output=$(I18N_LOCATION)/translate.pot \
		--language=Scheme \
		--keyword=G_ \
		--sort-by-file \
		--copyright-holder=$(COPYRIGHT_HOLDERS) \
		--package-name="byggsteg" \
		lib/byggsteg/job/*.scm \
		lib/byggsteg/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		--from-code UTF-8

join-i18n:
	xgettext --default-domain=translate \
		--output=$(I18N_LOCATION)/translate.pot \
		--language=Scheme \
		--keyword=G_ \
		--sort-by-file \
		--join-existing \
		--copyright-holder=$(COPYRIGHT_HOLDERS) \
		--package-name="byggsteg" \
		lib/byggsteg/job/*.scm \
		lib/byggsteg/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		--from-code UTF-8

init-i18n-nl:
	$(call byggsteg-msginit,"nl_NL")
init-i18n-en:
	$(call byggsteg-msginit,"en_US")
merge-i18n-nl:
	msgmerge -U $(I18N_LOCATION)/nl_NL/LC_MESSAGES/translate.po $(I18N_LOCATION)/translate.pot
merge-i18n-en:
	$(call byggsteg-merge,"en_US")
