//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MusicPlayer.Utils;

interface
uses
{$IFDEF IOS}
  FMX.Types,
  iOSApi.MediaPlayer, iOSApi.Foundation, Macapi.Helpers,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
{$ENDIF}
  System.SysUtils,
  FMX.Graphics;

type
  TMPControllerType = (App, Ipod);
  TMPRepeatMode = (Default, None, One, All);
  TMPPlaybackState = (Stopped, Playing, Paused, Interrupted, SeekingForward, SeekingBackward);

  TMPSong = record
    Album: string;
    Artist: string;
    Duration: string;
    Title: string;
{$IFDEF ANDROID}
    Path: string;
    class function FromCursor(c: JCursor): TMPSong; static;
{$ENDIF}
{$IFDEF IOS}
    MPItem: MPMediaItem;
    class function FromMediaItem(Item: MPMediaItem): TMPSong; static;
{$ENDIF}
    class function EmptySong: TMPSong; static;
    class function DurationToString(duration: Single): string; static;
    function Equals(song : TMPSong): Boolean;
  end;

  TMPAlbum = record
    Name: string;
    Artist: string;
    Album_ID: Integer;
    Artwork: TBitmap;
    class function AllMusicAlbum: TMPAlbum; static;
  end;

  TOnSongChangeEvent = procedure (newIndex: Integer) of object;
  TOnProcessPlayEvent = procedure (newPos: Single) of object;
{$IFDEF IOS}
  function MPMediaItemPropertyTitle: NSString;
  function MPMediaItemPropertyAlbumTitle: NSString;
  function MPMediaItemPropertyArtist: NSString;
  function MPMediaItemPropertyArtwork: NSString;
  function MPMediaItemPropertyPlaybackDuration: NSString;
  function MPMediaItemPropertyMediaType: NSString;
  function MPMediaItemPropertyComposer: NSString;
  function MPMediaItemPropertyGenre: NSString;
  function MPMediaPlaylistPropertyName: NSString;
  function MPMediaItemPropertyPodcastTitle: NSString;
{$ENDIF}
implementation

{ TMPAlbum }

class function TMPAlbum.AllMusicAlbum: TMPAlbum;
begin
  Result.Name := 'All Songs';
  Result.Artist := '';
  Result.Album_ID := -1;
  Result.Artwork := nil;
end;

class function TMPSong.EmptySong: TMPSong;
begin
  Result.Album := '-';
  Result.Artist := '-';
  Result.Duration := '-';
  Result.Title := '-';
end;

function TMPSong.Equals(song: TMPSong): Boolean;
begin
  Result := (Artist = song.Artist) and (Album = song.Album) and (Duration = song.Duration) and (Title = song.Title);
end;

class function TMPSong.DurationToString(duration: Single): string;
var
  hours,
  minutes,
  seconds: Integer;
  secondsStr: string;
begin
  Result := '';
{$IFDEF IOS}
  hours := Trunc(duration) div (60*60);
  minutes := (Trunc(duration) mod (60*60)) div 60;
  seconds :=  Trunc(duration) mod 60;
{$ENDIF}
{$IFDEF ANDROID}
  hours := Trunc(duration) div (1000*60*60);
  minutes := (Trunc(duration) mod (1000*60*60)) div (1000*60);
  seconds :=  ((Trunc(duration) mod (1000*60*60)) mod (1000*60)) div 1000;
{$ENDIF}
  if hours > 0 then
    if minutes < 10 then
      Result := Result + hours.ToString + ':0'
    else
      Result := Result + hours.ToString + ':';

  if seconds < 10 then
    secondsStr := '0' + seconds.ToString
  else
    secondsStr := seconds.ToString;

  Result := Result + minutes.ToString + ':' + secondsStr;
end;
{$IFDEF ANDROID}
class function TMPSong.FromCursor(c: JCursor): TMPSong;
begin
  Result.Artist := JStringToString(c.getString(0));
  if Result.Artist = '<unknown>' then
    Result.Artist := 'Unknown';
  Result.Title := JStringToString(c.getString(1));
  Result.Path := JStringToString(c.getString(2));
  Result.Album := JStringToString(c.getString(3));
  Result.Duration := TMPSong.DurationToString(c.getFloat(4));
end;
{$ENDIF}

{$IFDEF IOS}
const
  libMediaPlayer = '/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer';

class function TMPSong.FromMediaItem(Item: MPMediaItem): TMPSong;
begin
  Result.Artist := NSStrToStr(TNSString.Wrap(Item.valueForProperty(MPMediaItemPropertyArtist)));
  Result.Album := NSStrToStr(TNSString.Wrap(Item.valueForProperty(MPMediaItemPropertyAlbumTitle)));
  Result.Title := NSStrToStr(TNSString.Wrap(Item.valueForProperty(MPMediaItemPropertyTitle)));
  Result.Duration := TMPSong.DurationTOString(TNSNumber.Wrap
      (Item.valueForProperty(MPMediaItemPropertyPlaybackDuration)).floatValue);
  Result.MPItem := Item;
  Result.MPItem.retain;
  if Result.Artist = '' then
      Result.Artist := 'Unknown';
end;

function MPMediaItemPropertyTitle: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyTitle');
end;

function MPMediaItemPropertyAlbumTitle: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyAlbumTitle');
end;

function MPMediaItemPropertyArtist: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyArtist');
end;

function MPMediaItemPropertyArtwork: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyArtwork');
end;

function MPMediaItemPropertyPlaybackDuration: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyPlaybackDuration');
end;

function MPMediaItemPropertyMediaType: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyMediaType');
end;

function MPMediaItemPropertyComposer: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyComposer');
end;

function MPMediaItemPropertyGenre: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyGenre');
end;

function MPMediaPlaylistPropertyName: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaPlaylistPropertyName');
end;

function MPMediaItemPropertyPodcastTitle: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyPodcastTitle');
end;
{$ENDIF}

end.
