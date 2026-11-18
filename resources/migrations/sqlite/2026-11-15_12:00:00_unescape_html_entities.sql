-- byggsteg - the CI/CD orchestrator written in Guile Scheme

-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
-- Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

-- Text columns held HTML-escaped text until now: every writer escaped it on its
-- way in and every reader unescaped it on its way out.  They hold the text as it
-- was written from this release on -- a page escapes it on its way in -- so this
-- migration unescapes what is already stored.
--
-- The replacements are every entity kracht's `html-escape' has written for this
-- project: kracht 0.3.0's `html-escape-entities' and the older `html-escape-alist'
-- beside it, whose symbols (`&sol;', `&percnt;', `&ast;' and `&dollar;') 0.3.0
-- leaves alone.  `&amp;' is applied last, which is the order a single
-- left-to-right reading decodes in.
--
-- Job and log titles were escaped twice -- `job-title-prefix' was escaped and
-- then the title built from it -- so they are decoded twice.  A tarball's title
-- is a copy of its job's and is matched against the job row it was built from,
-- which leaves the name a collection asked for as it is.

-- The pairs are written once, in a temporary table that lives as long as the run,
-- and every decode below reads them one at a time.  That is the same order and the
-- same single left-to-right reading as two hundred and fifty-seven nested
-- `replace' calls, which is what these statements used to be -- and what SQLite
-- will not parse: its parser stack holds thirty nested calls (3.39, the version
-- the pinned manifest builds), deeper ones overflow it, and where the limit sits
-- differs between versions, so a nested chain is not portable.
--
-- One step is one pair: replace every occurrence of "entity"."pattern" numbered
-- step + 1, and stop once the last pair has been applied.  `&amp;' stays the last
-- pair, so a `&lt;' that `&amp;lt;' leaves behind is not read a second time.
CREATE TEMP TABLE "entity" (
  "n" INTEGER PRIMARY KEY,
  "pattern" TEXT NOT NULL,
  "replacement" TEXT NOT NULL);

INSERT INTO "entity" ("n", "pattern", "replacement") VALUES
  (1, '&quot;', '"'),
  (2, '&apos;', ''''),
  (3, '&lt;', '<'),
  (4, '&gt;', '>'),
  (5, '&nbsp;', ' '),
  (6, '&iexcl;', '¡'),
  (7, '&cent;', '¢'),
  (8, '&pound;', '£'),
  (9, '&curren;', '¤'),
  (10, '&yen;', '¥'),
  (11, '&brvbar;', '¦'),
  (12, '&sect;', '§'),
  (13, '&uml;', '¨'),
  (14, '&copy;', '©'),
  (15, '&ordf;', 'ª'),
  (16, '&laquo;', '«'),
  (17, '&not;', '¬'),
  (18, '&shy;', '­'),
  (19, '&reg;', '®'),
  (20, '&macr;', '¯'),
  (21, '&deg;', '°'),
  (22, '&plusmn;', '±'),
  (23, '&sup2;', '²'),
  (24, '&sup3;', '³'),
  (25, '&acute;', '´'),
  (26, '&micro;', 'µ'),
  (27, '&para;', '¶'),
  (28, '&middot;', '·'),
  (29, '&cedil;', '¸'),
  (30, '&sup1;', '¹'),
  (31, '&ordm;', 'º'),
  (32, '&raquo;', '»'),
  (33, '&frac14;', '¼'),
  (34, '&frac12;', '½'),
  (35, '&frac34;', '¾'),
  (36, '&iquest;', '¿'),
  (37, '&Agrave;', 'À'),
  (38, '&Aacute;', 'Á'),
  (39, '&Acirc;', 'Â'),
  (40, '&Atilde;', 'Ã'),
  (41, '&Auml;', 'Ä'),
  (42, '&Aring;', 'Å'),
  (43, '&AElig;', 'Æ'),
  (44, '&Ccedil;', 'Ç'),
  (45, '&Egrave;', 'È'),
  (46, '&Eacute;', 'É'),
  (47, '&Ecirc;', 'Ê'),
  (48, '&Euml;', 'Ë'),
  (49, '&Igrave;', 'Ì'),
  (50, '&Iacute;', 'Í'),
  (51, '&Icirc;', 'Î'),
  (52, '&Iuml;', 'Ï'),
  (53, '&ETH;', 'Ð'),
  (54, '&Ntilde;', 'Ñ'),
  (55, '&Ograve;', 'Ò'),
  (56, '&Oacute;', 'Ó'),
  (57, '&Ocirc;', 'Ô'),
  (58, '&Otilde;', 'Õ'),
  (59, '&Ouml;', 'Ö'),
  (60, '&times;', '×'),
  (61, '&Oslash;', 'Ø'),
  (62, '&Ugrave;', 'Ù'),
  (63, '&Uacute;', 'Ú'),
  (64, '&Ucirc;', 'Û'),
  (65, '&Uuml;', 'Ü'),
  (66, '&Yacute;', 'Ý'),
  (67, '&THORN;', 'Þ'),
  (68, '&szlig;', 'ß'),
  (69, '&agrave;', 'à'),
  (70, '&aacute;', 'á'),
  (71, '&acirc;', 'â'),
  (72, '&atilde;', 'ã'),
  (73, '&auml;', 'ä'),
  (74, '&aring;', 'å'),
  (75, '&aelig;', 'æ'),
  (76, '&ccedil;', 'ç'),
  (77, '&egrave;', 'è'),
  (78, '&eacute;', 'é'),
  (79, '&ecirc;', 'ê'),
  (80, '&euml;', 'ë'),
  (81, '&igrave;', 'ì'),
  (82, '&iacute;', 'í'),
  (83, '&icirc;', 'î'),
  (84, '&iuml;', 'ï'),
  (85, '&eth;', 'ð'),
  (86, '&ntilde;', 'ñ'),
  (87, '&ograve;', 'ò'),
  (88, '&oacute;', 'ó'),
  (89, '&ocirc;', 'ô'),
  (90, '&otilde;', 'õ'),
  (91, '&ouml;', 'ö'),
  (92, '&divide;', '÷'),
  (93, '&oslash;', 'ø'),
  (94, '&ugrave;', 'ù'),
  (95, '&uacute;', 'ú'),
  (96, '&ucirc;', 'û'),
  (97, '&uuml;', 'ü'),
  (98, '&yacute;', 'ý'),
  (99, '&thorn;', 'þ'),
  (100, '&yuml;', 'ÿ'),
  (101, '&OElig;', 'Œ'),
  (102, '&oelig;', 'œ'),
  (103, '&Scaron;', 'Š'),
  (104, '&scaron;', 'š'),
  (105, '&Yuml;', 'Ÿ'),
  (106, '&fnof;', 'ƒ'),
  (107, '&circ;', 'ˆ'),
  (108, '&tilde;', '˜'),
  (109, '&Alpha;', 'Α'),
  (110, '&Beta;', 'Β'),
  (111, '&Gamma;', 'Γ'),
  (112, '&Delta;', 'Δ'),
  (113, '&Epsilon;', 'Ε'),
  (114, '&Zeta;', 'Ζ'),
  (115, '&Eta;', 'Η'),
  (116, '&Theta;', 'Θ'),
  (117, '&Iota;', 'Ι'),
  (118, '&Kappa;', 'Κ'),
  (119, '&Lambda;', 'Λ'),
  (120, '&Mu;', 'Μ'),
  (121, '&Nu;', 'Ν'),
  (122, '&Xi;', 'Ξ'),
  (123, '&Omicron;', 'Ο'),
  (124, '&Pi;', 'Π'),
  (125, '&Rho;', 'Ρ'),
  (126, '&Sigma;', 'Σ'),
  (127, '&Tau;', 'Τ'),
  (128, '&Upsilon;', 'Υ'),
  (129, '&Phi;', 'Φ'),
  (130, '&Chi;', 'Χ'),
  (131, '&Psi;', 'Ψ'),
  (132, '&Omega;', 'Ω'),
  (133, '&alpha;', 'α'),
  (134, '&beta;', 'β'),
  (135, '&gamma;', 'γ'),
  (136, '&delta;', 'δ'),
  (137, '&epsilon;', 'ε'),
  (138, '&zeta;', 'ζ'),
  (139, '&eta;', 'η'),
  (140, '&theta;', 'θ'),
  (141, '&iota;', 'ι'),
  (142, '&kappa;', 'κ'),
  (143, '&lambda;', 'λ'),
  (144, '&mu;', 'μ'),
  (145, '&nu;', 'ν'),
  (146, '&xi;', 'ξ'),
  (147, '&omicron;', 'ο'),
  (148, '&pi;', 'π'),
  (149, '&rho;', 'ρ'),
  (150, '&sigmaf;', 'ς'),
  (151, '&sigma;', 'σ'),
  (152, '&tau;', 'τ'),
  (153, '&upsilon;', 'υ'),
  (154, '&phi;', 'φ'),
  (155, '&chi;', 'χ'),
  (156, '&psi;', 'ψ'),
  (157, '&omega;', 'ω'),
  (158, '&thetasym;', 'ϑ'),
  (159, '&upsih;', 'ϒ'),
  (160, '&piv;', 'ϖ'),
  (161, '&ensp;', ' '),
  (162, '&emsp;', ' '),
  (163, '&thinsp;', ' '),
  (164, '&zwnj;', '‌'),
  (165, '&zwj;', '‍'),
  (166, '&lrm;', '‎'),
  (167, '&rlm;', '‏'),
  (168, '&ndash;', '–'),
  (169, '&mdash;', '—'),
  (170, '&lsquo;', '‘'),
  (171, '&rsquo;', '’'),
  (172, '&sbquo;', '‚'),
  (173, '&ldquo;', '“'),
  (174, '&rdquo;', '”'),
  (175, '&bdquo;', '„'),
  (176, '&dagger;', '†'),
  (177, '&Dagger;', '‡'),
  (178, '&bull;', '•'),
  (179, '&hellip;', '…'),
  (180, '&permil;', '‰'),
  (181, '&prime;', '′'),
  (182, '&Prime;', '″'),
  (183, '&lsaquo;', '‹'),
  (184, '&rsaquo;', '›'),
  (185, '&oline;', '‾'),
  (186, '&frasl;', '⁄'),
  (187, '&euro;', '€'),
  (188, '&image;', 'ℑ'),
  (189, '&weierp;', '℘'),
  (190, '&real;', 'ℜ'),
  (191, '&trade;', '™'),
  (192, '&alefsym;', 'ℵ'),
  (193, '&larr;', '←'),
  (194, '&uarr;', '↑'),
  (195, '&rarr;', '→'),
  (196, '&darr;', '↓'),
  (197, '&harr;', '↔'),
  (198, '&crarr;', '↵'),
  (199, '&lArr;', '⇐'),
  (200, '&uArr;', '⇑'),
  (201, '&rArr;', '⇒'),
  (202, '&dArr;', '⇓'),
  (203, '&hArr;', '⇔'),
  (204, '&forall;', '∀'),
  (205, '&part;', '∂'),
  (206, '&exist;', '∃'),
  (207, '&empty;', '∅'),
  (208, '&nabla;', '∇'),
  (209, '&isin;', '∈'),
  (210, '&notin;', '∉'),
  (211, '&ni;', '∋'),
  (212, '&prod;', '∏'),
  (213, '&sum;', '∑'),
  (214, '&minus;', '−'),
  (215, '&lowast;', '∗'),
  (216, '&radic;', '√'),
  (217, '&prop;', '∝'),
  (218, '&infin;', '∞'),
  (219, '&ang;', '∠'),
  (220, '&and;', '∧'),
  (221, '&or;', '∨'),
  (222, '&cap;', '∩'),
  (223, '&cup;', '∪'),
  (224, '&int;', '∫'),
  (225, '&there4;', '∴'),
  (226, '&sim;', '∼'),
  (227, '&cong;', '≅'),
  (228, '&asymp;', '≈'),
  (229, '&ne;', '≠'),
  (230, '&equiv;', '≡'),
  (231, '&le;', '≤'),
  (232, '&ge;', '≥'),
  (233, '&sub;', '⊂'),
  (234, '&sup;', '⊃'),
  (235, '&nsub;', '⊄'),
  (236, '&sube;', '⊆'),
  (237, '&supe;', '⊇'),
  (238, '&oplus;', '⊕'),
  (239, '&otimes;', '⊗'),
  (240, '&perp;', '⊥'),
  (241, '&sdot;', '⋅'),
  (242, '&lceil;', '⌈'),
  (243, '&rceil;', '⌉'),
  (244, '&lfloor;', '⌊'),
  (245, '&rfloor;', '⌋'),
  (246, '&loz;', '◊'),
  (247, '&spades;', '♠'),
  (248, '&clubs;', '♣'),
  (249, '&hearts;', '♥'),
  (250, '&diams;', '♦'),
  (251, '&lang;', '⟨'),
  (252, '&rang;', '⟩'),
  (253, '&sol;', '/'),
  (254, '&ast;', '*'),
  (255, '&dollar;', '$'),
  (256, '&percnt;', '%'),
  (257, '&amp;', '&');

-- A tarball's title, decoded alongside the job title it was copied from: the
-- job's own decode below would leave this copy one escape behind.
UPDATE "artifact"
SET "title" = (SELECT (WITH RECURSIVE "decoded" ("step", "text") AS (
                        SELECT 0, "job"."title"
                        UNION ALL
                        SELECT "decoded"."step" + 1,
                               replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                        FROM "decoded", "entity"
                        WHERE "entity"."n" = "decoded"."step" + 1
                          AND "decoded"."step" < 257)
                       SELECT "text" FROM "decoded" WHERE "step" = 257)
               FROM "job"
               WHERE "job"."id" = "artifact"."job_id")
WHERE "title" = (SELECT "job"."title"
                 FROM "job"
                 WHERE "job"."id" = "artifact"."job_id");

UPDATE "artifact"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257)
WHERE "title" = (SELECT (WITH RECURSIVE "decoded" ("step", "text") AS (
                          SELECT 0, "job"."title"
                          UNION ALL
                          SELECT "decoded"."step" + 1,
                                 replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                          FROM "decoded", "entity"
                          WHERE "entity"."n" = "decoded"."step" + 1
                            AND "decoded"."step" < 257)
                         SELECT "text" FROM "decoded" WHERE "step" = 257)
                 FROM "job"
                 WHERE "job"."id" = "artifact"."job_id");

UPDATE "job"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257),
    "job_data" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                   SELECT 0, "job_data"
                   UNION ALL
                   SELECT "decoded"."step" + 1,
                          replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                   FROM "decoded", "entity"
                   WHERE "entity"."n" = "decoded"."step" + 1
                     AND "decoded"."step" < 257)
                  SELECT "text" FROM "decoded" WHERE "step" = 257);

UPDATE "log"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257),
    "log_data" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                   SELECT 0, "log_data"
                   UNION ALL
                   SELECT "decoded"."step" + 1,
                          replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                   FROM "decoded", "entity"
                   WHERE "entity"."n" = "decoded"."step" + 1
                     AND "decoded"."step" < 257)
                  SELECT "text" FROM "decoded" WHERE "step" = 257);

UPDATE "profile"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257),
    "profile_data" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                       SELECT 0, "profile_data"
                       UNION ALL
                       SELECT "decoded"."step" + 1,
                              replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                       FROM "decoded", "entity"
                       WHERE "entity"."n" = "decoded"."step" + 1
                         AND "decoded"."step" < 257)
                      SELECT "text" FROM "decoded" WHERE "step" = 257);

UPDATE "user"
SET "username" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                   SELECT 0, "username"
                   UNION ALL
                   SELECT "decoded"."step" + 1,
                          replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                   FROM "decoded", "entity"
                   WHERE "entity"."n" = "decoded"."step" + 1
                     AND "decoded"."step" < 257)
                  SELECT "text" FROM "decoded" WHERE "step" = 257),
    "email" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "email"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257);

-- The second escape of a job's or a log's title, in statements of their own: it
-- reads what the statements above wrote, and one statement cannot read a column
-- it is filling.
UPDATE "job"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257);

UPDATE "log"
SET "title" = (WITH RECURSIVE "decoded" ("step", "text") AS (
                SELECT 0, "title"
                UNION ALL
                SELECT "decoded"."step" + 1,
                       replace("decoded"."text", "entity"."pattern", "entity"."replacement")
                FROM "decoded", "entity"
                WHERE "entity"."n" = "decoded"."step" + 1
                  AND "decoded"."step" < 257)
               SELECT "text" FROM "decoded" WHERE "step" = 257);

-- The run is over, and so is the table it read: a temporary table outlives the
-- transaction, so a second run of this file would find it already there.
DROP TABLE "entity";
