--- TABLES ---------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS Boards (
    Name            TEXT        NOT NULL    UNIQUE              CHECK(LENGTH(Name) BETWEEN 1 AND 8),
    Title           TEXT        NOT NULL    UNIQUE              CHECK(LENGTH(Title) BETWEEN 1 AND 32),
    Subtitle        TEXT        NOT NULL                        CHECK(LENGTH(Subtitle) <= 64),
    MaxPostNumber   INTEGER     NOT NULL    DEFAULT 0           CHECK(MaxPostNumber >= 0),
    Lock            BOOLEAN     NOT NULL    DEFAULT FALSE,
    PostMinLength   INTEGER     NOT NULL    DEFAULT 1,
    PostMaxLength   INTEGER     NOT NULL    DEFAULT 8192        CHECK(PostMaxLength <= 32768),
    PostMaxNewlines INTEGER     NOT NULL    DEFAULT 64          CHECK(PostMaxNewlines <= 1024),
    PostLimit       INTEGER     NOT NULL    DEFAULT 250,
    ThreadLimit     INTEGER     NOT NULL    DEFAULT 100         CHECK(ThreadLimit BETWEEN 1 AND 1000),

    PRIMARY KEY (Name)
);

CREATE TABLE IF NOT EXISTS Posts (
    Board           TEXT        NOT NULL,
    Number          INTEGER                 DEFAULT NULL,
    Parent          INTEGER,
    Date            INTEGER     NOT NULL    DEFAULT 0,
    Name            TEXT        NOT NULL    DEFAULT 'Nameless'  CHECK(length(Name)    <= 64),
    Email           TEXT        NOT NULL    DEFAULT ''          CHECK(length(Email)   <= 320),
    Subject         TEXT        NOT NULL    DEFAULT ''          CHECK(length(Subject) <= 128),
    Text            TEXT        NOT NULL    DEFAULT ''          CHECK(length(Text)    <= 32768),
    FileId          INTEGER,

    PRIMARY KEY (Board,Number),
    FOREIGN KEY (Board) REFERENCES Boards (Name) ON DELETE CASCADE ON UPDATE CASCADE,
    FOREIGN KEY (Board,Parent) REFERENCES Posts (Board,Number) ON DELETE CASCADE,
    FOREIGN KEY (FileId) REFERENCES Files (Id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS ThreadInfo (
    Board           TEXT        NOT NULL,
    Number          INTEGER     NOT NULL,
    LastBump        INTEGER     NOT NULL    DEFAULT 0,
    Sticky          BOOLEAN     NOT NULL    DEFAULT FALSE,
    Lock            BOOLEAN     NOT NULL    DEFAULT FALSE,
    Autosage        BOOLEAN     NOT NULL    DEFAULT FALSE,
    Cycle           BOOLEAN     NOT NULL    DEFAULT FALSE,
    ReplyCount      INTEGER     NOT NULL    DEFAULT 0           CHECK(ReplyCount >= 0),

    PRIMARY KEY (Board,Number),
    FOREIGN KEY (Board) REFERENCES Boards (Name) ON DELETE CASCADE ON UPDATE CASCADE,
    FOREIGN KEY (Board,Number) REFERENCES Posts (Board,Number) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Files (
    Id              INTEGER     NOT NULL,
    Name            TEXT        NOT NULL    UNIQUE              CHECK(length(Name) between 130 and 133),
    Extension       INTEGER     NOT NULL,
    Size            INTEGER     NOT NULL                        CHECK(Size between 1 and 16777216),
    Width           INTEGER                 DEFAULT NULL,
    Height          INTEGER                 DEFAULT NULL,
    
    PRIMARY KEY (Id),
    CHECK((Width IS NULL) = (Height IS NULL))
);

CREATE TABLE IF NOT EXISTS Accounts (
    Username        TEXT        NOT NULL,
    Password        TEXT        NOT NULL,
    Role            TEXT        NOT NULL                        CHECK(Role IN ('admin','moderator')),
    CreationDate    INTEGER     NOT NULL    DEFAULT 0,
    PRIMARY KEY (Username)
);

CREATE TABLE IF NOT EXISTS Sessions (
    Key             TEXT        NOT NULL,
    Username        TEXT        NOT NULL,
    ExpireDate      INTEGER     NOT NULL    DEFAULT 0,

    PRIMARY KEY (Username),
    FOREIGN KEY (Username) REFERENCES Accounts (Username) ON DELETE CASCADE
);

--- INDEXES --------------------------------------------------------------------

CREATE UNIQUE INDEX IF NOT EXISTS filename_index ON Files (Name);

--- TRIGGERS -------------------------------------------------------------------


CREATE TRIGGER IF NOT EXISTS set_post_date AFTER INSERT ON Posts
BEGIN
    UPDATE Posts SET Date = strftime('%s','now') WHERE ROWID = NEW.ROWID;
END;

CREATE TRIGGER IF NOT EXISTS set_lastbump AFTER INSERT ON ThreadInfo
BEGIN
    UPDATE ThreadInfo SET LastBump = strftime('%s','now') WHERE ROWID = NEW.ROWID;
END;

CREATE TRIGGER IF NOT EXISTS bump_thread AFTER INSERT ON Posts
  WHEN NEW.Parent IS NOT NULL AND NEW.Email NOT LIKE '%sage%'
BEGIN
  UPDATE ThreadInfo SET LastBump = STRFTIME('%s', 'now') 
    WHERE Number = NEW.Parent AND Board = NEW.Board AND Autosage = FALSE;
END;

CREATE TRIGGER IF NOT EXISTS increment_post_number AFTER INSERT ON Posts
  WHEN NEW.PARENT IS NOT NULL
BEGIN
  UPDATE Posts SET Number = (SELECT MaxPostNumber + 1 FROM Boards WHERE Name = NEW.Board) WHERE ROWID = NEW.ROWID;
  UPDATE Boards SET MaxPostNumber = MaxPostNumber + 1 WHERE Name = NEW.Board;
  UPDATE ThreadInfo SET ReplyCount = ReplyCount + 1 WHERE NEW.Parent = ThreadInfo.Number AND NEW.Board = ThreadInfo.Board;
END;

CREATE TRIGGER IF NOT EXISTS add_threadinfo AFTER INSERT ON Posts
  WHEN NEW.Parent IS NULL
BEGIN
  UPDATE Posts SET Number = (SELECT MaxPostNumber + 1 FROM Boards WHERE Name = NEW.Board) WHERE ROWID = NEW.ROWID;
  UPDATE Boards SET MaxPostNumber = MaxPostNumber + 1 WHERE Name = NEW.Board;
  INSERT INTO ThreadInfo (Number, Board) VALUES ((SELECT MaxPostNumber FROM Boards WHERE Name = NEW.Board), NEW.Board);
END;

CREATE TRIGGER IF NOT EXISTS delete_old_sessions BEFORE INSERT ON Sessions
BEGIN
  DELETE FROM Sessions WHERE Username = NEW.Username;
END;

CREATE TRIGGER IF NOT EXISTS set_session_expiry AFTER INSERT ON Sessions
BEGIN
  UPDATE Sessions SET ExpireDate = strftime('%s','now','+1 hours') WHERE Key = NEW.Key;
END;

CREATE TRIGGER IF NOT EXISTS set_account_date AFTER INSERT ON Accounts
BEGIN
  UPDATE Accounts SET CreationDate = strftime('%s','now') WHERE ROWID = NEW.ROWID;
END;

CREATE TRIGGER IF NOT EXISTS slide_thread BEFORE INSERT ON ThreadInfo
  WHEN (SELECT COUNT(*) FROM ThreadInfo WHERE Board = NEW.Board)
       >= (SELECT ThreadLimit FROM Boards WHERE Name = NEW.Board)
BEGIN
  DELETE FROM ThreadInfo
  WHERE Board = NEW.Board AND Sticky = FALSE
  AND LastBump = (SELECT MIN(LastBump) FROM ThreadInfo WHERE Board = NEW.Board);
END;


-- CREATE TRIGGER IF NOT EXISTS remove_old_refs BEFORE DELETE ON Posts
-- BEGIN
--   DELETE FROM FileRefs WHERE Number = OLD.Number;
-- END;

-- CREATE TRIGGER IF NOT EXISTS remove_file_refs BEFORE DELETE ON Files
-- BEGIN
--   DELETE FROM FileRefs WHERE File = OLD.Name;
-- END;

--- VIEWS ----------------------------------------------------------------------

CREATE VIEW IF NOT EXISTS posts_and_files
  AS
  SELECT
    Posts.Board,
    Posts.Number,
    Posts.Parent,

    Posts.Date,

    Posts.Name,
    Posts.Email,
    Posts.Subject,
    Posts.Text,
    
    Files.Name AS Filename,
    Files.Extension,
    Files.Size,
    Files.Width,
    Files.Height
  FROM Posts LEFT JOIN Files on Posts.FileId = Files.Id;

CREATE VIEW IF NOT EXISTS threads
  AS
  SELECT
    posts_and_files.*,

    ThreadInfo.LastBump,
    ThreadInfo.Sticky,
    ThreadInfo.Lock,
    ThreadInfo.Autosage,
    ThreadInfo.Cycle,
    ThreadInfo.ReplyCount
  FROM ThreadInfo
  JOIN posts_and_files ON posts_and_files.Number = ThreadInfo.Number 
  AND posts_and_files.Board = ThreadInfo.Board;