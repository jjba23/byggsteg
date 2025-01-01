.PHONY: create server

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
