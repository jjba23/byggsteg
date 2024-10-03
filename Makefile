
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
	xgettext --keyword=_ --language=scheme --add-comments --sort-output -o po/html.pot byggsteg/html.scm 
