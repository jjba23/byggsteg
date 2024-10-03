
prepare-dirs:
	mkdir -p /var/log/byggsteg || true
	mkdir -p /var/log/byggsteg/job-log || true
	mkdir -p /var/log/byggsteg/job-failure || true
	mkdir -p /var/log/byggsteg/job-clone || true
	mkdir -p /var/log/byggsteg/job-success || true
	mkdir -p /var/log/byggsteg/job-detail || true
	mkdir -p /var/log/byggsteg/profile || true
clean-dirs:
	sudo rm -rfv /var/log/byggsteg/* || true
server:
	GUILE_AUTO_COMPILE=0 guile run-server.scm

xgettext:
	xgettext --keyword=gettext \
		--language=scheme \
		--add-comments \
		--sort-output \
		-o po/byggsteg.pot \
		./src/html.scm \
		./src/server.scm \
		./src/job.scm

msginit:
	msginit --input=po/byggsteg.pot --locale=${BYGGSTEG_LOCALE} \
		--output=po/byggsteg.${BYGGSTEG_LOCALE}.po

msgmerge:
	msgmerge --update po/byggsteg.${BYGGSTEG_LOCALE}.po po/byggsteg.pot
msgfmt:
	msgfmt --output-file=locale/${BYGGSTEG_LOCALE}/LC_MESSAGES/byggsteg.mo po/byggsteg.nl.po

i18n: xgettext msginit msgmerge msgfmt

