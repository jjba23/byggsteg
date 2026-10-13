CREATE TABLE IF NOT EXISTS "artifact" (
	"id"	varchar(36) NOT NULL,
	"job_id"	varchar(36) NOT NULL,
	"title"	TEXT,
	"artifact_path"	TEXT NOT NULL,
	"artifact_size"	integer NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	PRIMARY KEY("id")
);

CREATE INDEX IF NOT EXISTS "artifact_job_id_index" ON "artifact" ("job_id");
