--- This is slightly modified picochan schema.

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

    LastBump        INTEGER                 DEFAULT NULL,
    Sticky          BOOLEAN                 DEFAULT NULL,
    Lock            BOOLEAN                 DEFAULT NULL,
    Autosage        BOOLEAN                 DEFAULT NULL,
    Cycle           BOOLEAN                 DEFAULT NULL,
    ReplyCount      INTEGER                 DEFAULT NULL        CHECK(ReplyCount IS NULL OR ReplyCount >= 0),

    PRIMARY KEY (Board,Number),
    FOREIGN KEY (Board) REFERENCES Boards (Name) ON DELETE CASCADE ON UPDATE CASCADE,
    FOREIGN KEY (Board,Parent) REFERENCES Posts (Board,Number) ON DELETE CASCADE,
    FOREIGN KEY (FileId) REFERENCES Files (Id) ON DELETE SET NULL,
    CHECK(
        (Parent IS NOT NULL AND LastBump IS NULL AND Sticky IS NULL AND Lock IS NULL 
        AND Autosage IS NULL AND Cycle IS NULL AND ReplyCount IS NULL) 
      OR
        (Parent IS NULL AND LastBump IS NOT NULL AND Sticky IS NOT NULL AND Lock IS NOT NULL 
        AND Autosage IS NOT NULL AND Cycle IS NOT NULL AND ReplyCount IS NOT NULL) 
      )
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
    Role            INTEGER     NOT NULL,
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

CREATE TABLE IF NOT EXISTS Captchas (
  Text              TEXT        NOT NULL,
  ExpireDate        INTEGER     NOT NULL    DEFAULT 0
);

--- INDEXES --------------------------------------------------------------------

CREATE UNIQUE INDEX IF NOT EXISTS filename_index ON Files (Name);

--- TRIGGERS -------------------------------------------------------------------


CREATE TRIGGER IF NOT EXISTS set_post_date AFTER INSERT ON Posts
BEGIN
    UPDATE Posts SET Date = strftime('%s','now') WHERE ROWID = NEW.ROWID;
END;

CREATE TRIGGER IF NOT EXISTS set_lastbump AFTER INSERT ON Posts
  WHEN NEW.Parent IS NULL
BEGIN
    UPDATE Posts SET LastBump = strftime('%s','now') WHERE ROWID = NEW.ROWID;
END;

CREATE TRIGGER IF NOT EXISTS bump_thread AFTER INSERT ON Posts
  WHEN NEW.Parent IS NOT NULL AND NEW.Email NOT LIKE '%sage%'
BEGIN
  UPDATE Posts SET LastBump = STRFTIME('%s', 'now') 
    WHERE Number = NEW.Parent AND Board = NEW.Board AND Autosage = FALSE;
END;

CREATE TRIGGER IF NOT EXISTS increment_post_number AFTER INSERT ON Posts
BEGIN
  UPDATE Posts SET Number = (SELECT MaxPostNumber + 1 FROM Boards WHERE Name = NEW.Board) WHERE ROWID = NEW.ROWID;
  UPDATE Boards SET MaxPostNumber = MaxPostNumber + 1 WHERE Name = NEW.Board;
  UPDATE Posts SET ReplyCount = ReplyCount + 1 WHERE NEW.Parent = Posts.Number AND NEW.Board = Posts.Board;
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

CREATE TRIGGER IF NOT EXISTS slide_thread BEFORE INSERT ON Posts
  WHEN (SELECT COUNT(*) FROM Posts WHERE Board = NEW.Board AND Parent IS NULL)
       >= (SELECT ThreadLimit FROM Boards WHERE Name = NEW.Board)
   AND NEW.Parent IS NULL
BEGIN
  DELETE FROM Posts
  WHERE Board = NEW.Board AND Parent IS NULL AND Sticky = FALSE
        AND LastBump = (SELECT MIN(LastBump) FROM Posts WHERE Board = NEW.Board AND Parent IS NULL);
END;

CREATE TRIGGER IF NOT EXISTS delete_cyclical BEFORE INSERT ON Posts
  WHEN (SELECT Cycle FROM Posts WHERE Board = NEW.Board AND Number = NEW.Parent)
  AND (SELECT ReplyCount FROM Posts WHERE Board = NEW.Board AND Number = NEW.Parent)
      >= (SELECT PostLimit FROM Boards WHERE Name = NEW.Board)
BEGIN
  DELETE FROM Posts WHERE Board = NEW.Board AND Number = (SELECT MIN(Number) FROM Posts WHERE Parent = NEW.Parent);
END;

CREATE TRIGGER IF NOT EXISTS delete_child_posts BEFORE DELETE ON Posts WHEN OLD.Parent IS NULL
BEGIN
  DELETE FROM Posts WHERE Board = OLD.Board AND Parent = OLD.Number;
END;

CREATE TRIGGER IF NOT EXISTS set_captcha_expiry AFTER INSERT ON Captchas
BEGIN
  UPDATE Captchas SET ExpireDate = strftime('%s', 'now','+15 minutes') WHERE ROWID = NEW.ROWID;
END;

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
    Files.Height,

    LastBump,
    Sticky,
    Lock,
    Autosage,
    Cycle,
    ReplyCount
  FROM Posts LEFT JOIN Files on Posts.FileId = Files.Id WHERE Parent IS NULL;
