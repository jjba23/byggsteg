
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
	xgettext --keyword=gettext --language=scheme --add-comments --sort-output -o po/html.pot byggsteg/html.scm
msginit:
	msginit --input=po/html.pot --locale=nl --output=po/nl/html.po
msginit-en:
	msginit --input=po/html.pot --locale=en --output=po/en/html.po
msgmerge:
	msgmerge --update po/nl/html.po po/html.pot
msgmerge-en:
	msgmerge --update po/en/html.po po/html.pot
msgfmt:
	msgfmt --output-file=po/nl/html.mo po/nl/html.po
msgfmt-en:
	msgfmt --output-file=po/en/html.mo po/en/html.po
