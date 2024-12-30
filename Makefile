.PHONY: prepare-dirs clean-dirs create server

prepare-dirs:
	mkdir -p /var/log/byggsteg || true
	mkdir -p /var/log/byggsteg/job-log || true
	mkdir -p /var/log/byggsteg/job-failure || true
	mkdir -p /var/log/byggsteg/job-clone || true
	mkdir -p /var/log/byggsteg/job-success || true
	mkdir -p /var/log/byggsteg/job-detail || true
	mkdir -p /var/log/byggsteg/profile || true
clean-dirs:
	rm -rfv /var/log/byggsteg/* || true
create:
	sudo mkdir /var/log/byggsteg || true
	sudo chown $(shell whoami) /var/log/byggsteg
dev:
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"
server:
	art work
fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;
rebuild-db:
	-rm byggsteg.db
	-rm byggsteg.db-shm
	-rm byggsteg.db-wal
	touch byggsteg.db
	sqlite3 byggsteg.db 'PRAGMA journal_mode=WAL;'
