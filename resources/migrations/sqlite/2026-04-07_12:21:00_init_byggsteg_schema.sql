CREATE TABLE IF NOT EXISTS "job" (
	"id"	varchar(36) NOT NULL,
	"human_id"	TEXT NOT NULL,
	"job_data"	TEXT NOT NULL,
	"job_success"	tinyint NOT NULL,
	"job_in_progress"	tinyint NOT NULL,
	"job_failed"	tinyint NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "log" (
	"id"	varchar(36) NOT NULL,
	"human_id"	TEXT NOT NULL,
	"job_id"	varchar(36) NOT NULL,
	"log_data"	TEXT NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "password_reset" (
	"user_id"	varchar(36) NOT NULL,
	"reset_token"	TEXT NOT NULL,
	"created_at"	date NOT NULL,
  PRIMARY KEY("user_id")
);

CREATE TABLE IF NOT EXISTS "profile" (
	"id"	varchar(36) NOT NULL,
	"human_id"	TEXT NOT NULL UNIQUE,
	"profile_data"	TEXT NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "user" (
	"id"	varchar(36) NOT NULL,
	"username"	varchar(256) NOT NULL,
	"email"	TEXT NOT NULL UNIQUE,
	"password"	TEXT NOT NULL,
	"salt"	varchar(36) NOT NULL,
	"permissions"	varchar(2048),
	PRIMARY KEY("id")
);
