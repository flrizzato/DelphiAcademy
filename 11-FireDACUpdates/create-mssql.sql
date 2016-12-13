--------------------------------------------------------------------------------
-- Embarcadero DB Change Manager Synchronization Script
-- FILE                : Schema-Archive-Job-MSSQL (FireDAC) Sep 9, 2016 10_07_41 AM.sql
-- DATE                : Sep 9, 2016 10:07:42 AM
-- 
-- SOURCE DATA SOURCE  : SQLServer-Local
--------------------------------------------------------------------------------
USE FireDAC
go
-- Create DDL for EventNotificationErrorsQueue has not been generated because it is a system object
-- Create DDL for QueryNotificationErrorsQueue has not been generated because it is a system object
-- Create DDL for ServiceBrokerQueue has not been generated because it is a system object
-- Create DDL for dbo has not been generated because it is a system object
CREATE TABLE dbo.Artist
(
    ArtistId int           NOT NULL,
    Name     nvarchar(120) COLLATE Latin1_General_CI_AS NULL,
    CONSTRAINT PK_Artist
    PRIMARY KEY CLUSTERED (ArtistId)
)
go
IF OBJECT_ID(N'dbo.Artist') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Artist >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Artist >>>'
go
CREATE TABLE dbo.Genre
(
    GenreId int           NOT NULL,
    Name    nvarchar(120) COLLATE Latin1_General_CI_AS NULL,
    CONSTRAINT PK_Genre
    PRIMARY KEY CLUSTERED (GenreId)
)
go
IF OBJECT_ID(N'dbo.Genre') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Genre >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Genre >>>'
go
CREATE TABLE dbo.Invoice_818aca6d
(
    InvoiceId         int           NOT NULL,
    CustomerId        int           NOT NULL,
    InvoiceDate       datetime      NOT NULL,
    BillingAddress    nvarchar(70)  COLLATE Latin1_General_CI_AS NULL,
    BillingCity       nvarchar(40)  COLLATE Latin1_General_CI_AS NULL,
    BillingState      nvarchar(40)  COLLATE Latin1_General_CI_AS NULL,
    BillingCountry    nvarchar(40)  COLLATE Latin1_General_CI_AS NULL,
    BillingPostalCode nvarchar(10)  COLLATE Latin1_General_CI_AS NULL,
    Total             numeric(10,2) NOT NULL
)
go
IF OBJECT_ID(N'dbo.Invoice_818aca6d') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Invoice_818aca6d >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Invoice_818aca6d >>>'
go
CREATE TABLE dbo.MediaType
(
    MediaTypeId int           NOT NULL,
    Name        nvarchar(120) COLLATE Latin1_General_CI_AS NULL,
    CONSTRAINT PK_MediaType
    PRIMARY KEY CLUSTERED (MediaTypeId)
)
go
IF OBJECT_ID(N'dbo.MediaType') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.MediaType >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.MediaType >>>'
go
CREATE TABLE dbo.Playlist
(
    PlaylistId int           NOT NULL,
    Name       nvarchar(120) COLLATE Latin1_General_CI_AS NULL,
    CONSTRAINT PK_Playlist
    PRIMARY KEY CLUSTERED (PlaylistId)
)
go
IF OBJECT_ID(N'dbo.Playlist') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Playlist >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Playlist >>>'
go
GRANT CONNECT TO dbo
go
CREATE USER FIREDAC FOR LOGIN FIREDAC
	WITH DEFAULT_SCHEMA = dbo
go
EXEC sp_addrolemember N'db_datareader', N'FIREDAC'
go
EXEC sp_addrolemember N'db_datawriter', N'FIREDAC'
go
EXEC sp_addrolemember N'db_ddladmin', N'FIREDAC'
go
IF USER_ID(N'FIREDAC') IS NOT NULL
    PRINT N'<<< CREATED USER FIREDAC >>>'
ELSE
    PRINT N'<<< FAILED CREATING USER FIREDAC >>>'
go
GRANT CONNECT TO FIREDAC
go
GRANT DELETE TO FIREDAC
go
GRANT EXECUTE TO FIREDAC
go
GRANT INSERT TO FIREDAC
go
GRANT SELECT TO FIREDAC
go
GRANT UPDATE TO FIREDAC
go
CREATE USER guest WITHOUT LOGIN
	WITH DEFAULT_SCHEMA = guest
go
IF USER_ID(N'guest') IS NOT NULL
    PRINT N'<<< CREATED USER guest >>>'
ELSE
    PRINT N'<<< FAILED CREATING USER guest >>>'
go
CREATE TABLE dbo.Album
(
    AlbumId  int           NOT NULL,
    Title    nvarchar(160) COLLATE Latin1_General_CI_AS NOT NULL,
    ArtistId int           NOT NULL,
    CONSTRAINT PK_Album
    PRIMARY KEY CLUSTERED (AlbumId)
)
go
IF OBJECT_ID(N'dbo.Album') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Album >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Album >>>'
go
CREATE NONCLUSTERED INDEX IFK_AlbumArtistId
    ON dbo.Album(ArtistId)
  WITH (
        PAD_INDEX = OFF
       )
go
IF EXISTS (SELECT * FROM sys.indexes WHERE object_id=OBJECT_ID(N'dbo.Album') AND name=N'IFK_AlbumArtistId')
    PRINT N'<<< CREATED INDEX dbo.Album.IFK_AlbumArtistId >>>'
ELSE
    PRINT N'<<< FAILED CREATING INDEX dbo.Album.IFK_AlbumArtistId >>>'
go
GRANT ALTER ON dbo.Album TO FIREDAC
go
GRANT CONTROL ON dbo.Album TO FIREDAC
go
GRANT DELETE ON dbo.Album TO FIREDAC
go
GRANT INSERT ON dbo.Album TO FIREDAC
go
GRANT REFERENCES ON dbo.Album TO FIREDAC
go
GRANT SELECT ON dbo.Album TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Album TO FIREDAC
go
GRANT UPDATE ON dbo.Album TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Album TO FIREDAC
go
CREATE TABLE dbo.Track
(
    TrackId      int           NOT NULL,
    Name         nvarchar(200) COLLATE Latin1_General_CI_AS NOT NULL,
    AlbumId      int           NULL,
    MediaTypeId  int           NOT NULL,
    GenreId      int           NULL,
    Composer     nvarchar(220) COLLATE Latin1_General_CI_AS NULL,
    Milliseconds int           NOT NULL,
    Bytes        int           NULL,
    UnitPrice    numeric(10,2) NOT NULL,
    CONSTRAINT PK_Track
    PRIMARY KEY CLUSTERED (TrackId)
)
go
IF OBJECT_ID(N'dbo.Track') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.Track >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.Track >>>'
go
GRANT ALTER ON dbo.Artist TO FIREDAC
go
GRANT CONTROL ON dbo.Artist TO FIREDAC
go
GRANT DELETE ON dbo.Artist TO FIREDAC
go
GRANT INSERT ON dbo.Artist TO FIREDAC
go
GRANT REFERENCES ON dbo.Artist TO FIREDAC
go
GRANT SELECT ON dbo.Artist TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Artist TO FIREDAC
go
GRANT UPDATE ON dbo.Artist TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Artist TO FIREDAC
go
ALTER TABLE dbo.Track
    ADD CONSTRAINT FK_TrackGenreId
    FOREIGN KEY (GenreId)
    REFERENCES dbo.Genre (GenreId)
go
GRANT ALTER ON dbo.Genre TO FIREDAC
go
GRANT CONTROL ON dbo.Genre TO FIREDAC
go
GRANT DELETE ON dbo.Genre TO FIREDAC
go
GRANT INSERT ON dbo.Genre TO FIREDAC
go
GRANT REFERENCES ON dbo.Genre TO FIREDAC
go
GRANT SELECT ON dbo.Genre TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Genre TO FIREDAC
go
GRANT UPDATE ON dbo.Genre TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Genre TO FIREDAC
go
GRANT ALTER ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT CONTROL ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT DELETE ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT INSERT ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT REFERENCES ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT SELECT ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT UPDATE ON dbo.Invoice_818aca6d TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Invoice_818aca6d TO FIREDAC
go
ALTER TABLE dbo.Track
    ADD CONSTRAINT FK_TrackMediaTypeId
    FOREIGN KEY (MediaTypeId)
    REFERENCES dbo.MediaType (MediaTypeId)
go
GRANT ALTER ON dbo.MediaType TO FIREDAC
go
GRANT CONTROL ON dbo.MediaType TO FIREDAC
go
GRANT DELETE ON dbo.MediaType TO FIREDAC
go
GRANT INSERT ON dbo.MediaType TO FIREDAC
go
GRANT REFERENCES ON dbo.MediaType TO FIREDAC
go
GRANT SELECT ON dbo.MediaType TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.MediaType TO FIREDAC
go
GRANT UPDATE ON dbo.MediaType TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.MediaType TO FIREDAC
go
GRANT ALTER ON dbo.Playlist TO FIREDAC
go
GRANT CONTROL ON dbo.Playlist TO FIREDAC
go
GRANT DELETE ON dbo.Playlist TO FIREDAC
go
GRANT INSERT ON dbo.Playlist TO FIREDAC
go
GRANT REFERENCES ON dbo.Playlist TO FIREDAC
go
GRANT SELECT ON dbo.Playlist TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Playlist TO FIREDAC
go
GRANT UPDATE ON dbo.Playlist TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Playlist TO FIREDAC
go
CREATE TABLE dbo.PlaylistTrack
(
    PlaylistId int NOT NULL,
    TrackId    int NOT NULL,
    CONSTRAINT PK_PlaylistTrack
    PRIMARY KEY NONCLUSTERED (PlaylistId,TrackId)
)
go
IF OBJECT_ID(N'dbo.PlaylistTrack') IS NOT NULL
    PRINT N'<<< CREATED TABLE dbo.PlaylistTrack >>>'
ELSE
    PRINT N'<<< FAILED CREATING TABLE dbo.PlaylistTrack >>>'
go
CREATE NONCLUSTERED INDEX IFK_PlaylistTrackTrackId
    ON dbo.PlaylistTrack(TrackId)
  WITH (
        PAD_INDEX = OFF
       )
go
IF EXISTS (SELECT * FROM sys.indexes WHERE object_id=OBJECT_ID(N'dbo.PlaylistTrack') AND name=N'IFK_PlaylistTrackTrackId')
    PRINT N'<<< CREATED INDEX dbo.PlaylistTrack.IFK_PlaylistTrackTrackId >>>'
ELSE
    PRINT N'<<< FAILED CREATING INDEX dbo.PlaylistTrack.IFK_PlaylistTrackTrackId >>>'
go
GRANT ALTER ON dbo.PlaylistTrack TO FIREDAC
go
GRANT CONTROL ON dbo.PlaylistTrack TO FIREDAC
go
GRANT DELETE ON dbo.PlaylistTrack TO FIREDAC
go
GRANT INSERT ON dbo.PlaylistTrack TO FIREDAC
go
GRANT REFERENCES ON dbo.PlaylistTrack TO FIREDAC
go
GRANT SELECT ON dbo.PlaylistTrack TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.PlaylistTrack TO FIREDAC
go
GRANT UPDATE ON dbo.PlaylistTrack TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.PlaylistTrack TO FIREDAC
go
CREATE NONCLUSTERED INDEX IFK_TrackAlbumId
    ON dbo.Track(AlbumId)
  WITH (
        PAD_INDEX = OFF
       )
go
IF EXISTS (SELECT * FROM sys.indexes WHERE object_id=OBJECT_ID(N'dbo.Track') AND name=N'IFK_TrackAlbumId')
    PRINT N'<<< CREATED INDEX dbo.Track.IFK_TrackAlbumId >>>'
ELSE
    PRINT N'<<< FAILED CREATING INDEX dbo.Track.IFK_TrackAlbumId >>>'
go
CREATE NONCLUSTERED INDEX IFK_TrackGenreId
    ON dbo.Track(GenreId)
  WITH (
        PAD_INDEX = OFF
       )
go
IF EXISTS (SELECT * FROM sys.indexes WHERE object_id=OBJECT_ID(N'dbo.Track') AND name=N'IFK_TrackGenreId')
    PRINT N'<<< CREATED INDEX dbo.Track.IFK_TrackGenreId >>>'
ELSE
    PRINT N'<<< FAILED CREATING INDEX dbo.Track.IFK_TrackGenreId >>>'
go
CREATE NONCLUSTERED INDEX IFK_TrackMediaTypeId
    ON dbo.Track(MediaTypeId)
  WITH (
        PAD_INDEX = OFF
       )
go
IF EXISTS (SELECT * FROM sys.indexes WHERE object_id=OBJECT_ID(N'dbo.Track') AND name=N'IFK_TrackMediaTypeId')
    PRINT N'<<< CREATED INDEX dbo.Track.IFK_TrackMediaTypeId >>>'
ELSE
    PRINT N'<<< FAILED CREATING INDEX dbo.Track.IFK_TrackMediaTypeId >>>'
go
GRANT ALTER ON dbo.Track TO FIREDAC
go
GRANT CONTROL ON dbo.Track TO FIREDAC
go
GRANT DELETE ON dbo.Track TO FIREDAC
go
GRANT INSERT ON dbo.Track TO FIREDAC
go
GRANT REFERENCES ON dbo.Track TO FIREDAC
go
GRANT SELECT ON dbo.Track TO FIREDAC
go
GRANT TAKE OWNERSHIP ON dbo.Track TO FIREDAC
go
GRANT UPDATE ON dbo.Track TO FIREDAC
go
GRANT VIEW DEFINITION ON dbo.Track TO FIREDAC
go
ALTER TABLE dbo.Track
    ADD CONSTRAINT FK_TrackAlbumId
    FOREIGN KEY (AlbumId)
    REFERENCES dbo.Album (AlbumId)
go
ALTER TABLE dbo.Album
    ADD CONSTRAINT FK_AlbumArtistId
    FOREIGN KEY (ArtistId)
    REFERENCES dbo.Artist (ArtistId)
go
ALTER TABLE dbo.PlaylistTrack
    ADD CONSTRAINT FK_PlaylistTrackPlaylistId
    FOREIGN KEY (PlaylistId)
    REFERENCES dbo.Playlist (PlaylistId)
go
ALTER TABLE dbo.PlaylistTrack
    ADD CONSTRAINT FK_PlaylistTrackTrackId
    FOREIGN KEY (TrackId)
    REFERENCES dbo.Track (TrackId)
go
