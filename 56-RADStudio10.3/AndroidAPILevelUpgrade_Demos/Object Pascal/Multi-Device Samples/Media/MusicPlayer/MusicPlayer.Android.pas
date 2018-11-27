//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MusicPlayer.Android;

interface
{$IFDEF ANDROID}
uses
  FMX.Graphics,
  MusicPlayer.Utils,
  System.IoUtils, System.SysUtils, System.Classes,
  FMX.Types, FMX.Platform.Android,
  Androidapi.JNI.Os, Androidapi.JNI.Net,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.Helpers, Androidapi.JNI.App;
type
  TMusicPlayer = class
  private
  type
    TProcessThread = class (TThread)
    private
      [weak] FMusicPlayer: TMusicPlayer;
      FOnProcessPlay: TOnProcessPlayEvent;
      procedure DoProcessPlay;
    public
      constructor Create(CreateSuspended: Boolean; AMusicPlayer: TMusicPlayer; processHandler: TOnProcessPlayEvent);
      destructor Destroy; override;
      procedure Execute; override;
    end;
  protected
    class var FInstance: TMusicPlayer;
  private
    FCurrentIndex: Integer;
    FPlaylist: TArray<TMPSong>;
    FAlbums: TArray<TMPAlbum>;
    FMusicPlayer: JMediaPlayer;
    FPlayBackState: TMPPlaybackState;
    FRepeatMode: TMPRepeatMode;
    FShuffleMode: Boolean;
    FDefaultAlbumImage: TBitmap;
    FOnSongChange: TOnSongChangeEvent;
    FOnProcessPlay: TOnProcessPlayEvent;
    constructor Create(AType: TMPControllerType = TMPControllerType.App);
    procedure DoOnSongChange(newIndex: Integer);
    procedure DoOnProcessPlay(newPos: Single);
    procedure SetVolume(const Value: Single);
    procedure SetTime(const Value: Single);
    procedure SetRepeatMode(const Value: TMPRepeatMode);
    procedure SetShuffleMode(const Value: Boolean);
    function GetVolume: Single;
    function GetTime: Single;
    function GetRepeatMode: TMPRepeatMode;
    function GetDuration: Single;
    function GetPlaybackState: TMPPlaybackState;
    function GetShuffleMode: Boolean;
  public
    destructor Destroy; override;
    class procedure SetPlayerType(AType: TMPControllerType);
    class function DefaultPlayer: TMusicPlayer;
    property CurrentIndex: Integer read FCurrentIndex;
    property Volume: Single read GetVolume write SetVolume;
    property Time: Single read GetTime write SetTime;
    property Duration: Single read GetDuration;
    property PlaybackState: TMPPlaybackState read GetPlaybackState;
    property ShuffleMode: Boolean read GetShuffleMode write SetShuffleMode;
    property RepeatMode: TMPRepeatMode read GetRepeatMode write SetRepeatMode;
    property Playlist: TArray<TMPSong> read FPlaylist;
    property Albums: TArray<TMPAlbum> read FAlbums;
    property DefaultAlbumImage: TBitmap read FDefaultAlbumImage write FDefaultAlbumImage;
    property OnSongChange: TOnSongChangeEvent read FOnSongChange write FOnSongChange;
    property OnProcessPlay: TOnProcessPlayEvent read FOnProcessPlay write FOnProcessPlay;
    function GetAlbums: TArray<string>;
    function GetSongs: TArray<string>;
    function GetSongsInAlbum(AName: string): TArray<string>;
    function IsPlaying: Boolean;
    function CanSkipBack: Boolean;
    function CanSkipForward: Boolean;
    procedure PlaySong(path: string);
    procedure PlayByIndex(Index: Integer);
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Next;
    procedure Previous;
  end;
var
  NoArtBitmap: TBitmap;
{$ENDIF}
implementation
{$IFDEF ANDROID}
{ TMusicPlayer }

function TMusicPlayer.CanSkipBack: Boolean;
begin
  Result := (Length(FPlaylist) > 0) and (FCurrentIndex > 0) and
    (FPlayBackState in [TMPPlaybackState.Playing, TMPPlaybackState.Paused]);
end;

function TMusicPlayer.CanSkipForward: Boolean;
begin
  Result := False;
  if (Length(FPlaylist) = 0) or not (FPlayBackState in [TMPPlaybackState.Playing, TMPPlaybackState.Paused]) then
    Exit(Result);

  case RepeatMode of
    TMPRepeatMode.One:
      Result := FCurrentIndex in [Low(FPlaylist) .. High(FPlaylist)] ;
    TMPRepeatMode.Default,
    TMPRepeatMode.None:
      Result := FCurrentIndex in [Low(FPlaylist) .. High(FPlaylist)-1] ;
    TMPRepeatMode.All:
      Result := True;
  end;
end;

constructor TMusicPlayer.Create(AType: TMPControllerType);
begin
  MainActivity.setVolumeControlStream(TJAudioManager.JavaClass.STREAM_MUSIC);
  FMusicPlayer := TJMediaPlayer.Create;
  FPlayBackState := TMPPlaybackState.Stopped;
  FRepeatMode := TMPRepeatMode.All;
  FShuffleMode := False;
  FDefaultAlbumImage := TBitmap.CreateFromFile(TPath.Combine(TPath.GetDocumentsPath,'MusicNote.png'));
  TProcessThread.Create(True,self,DoOnProcessPlay).Start;
end;

class function TMusicPlayer.DefaultPlayer: TMusicPlayer;
begin
  if not Assigned(FInstance) then
    FInstance := TMusicPlayer.Create;
  Result := FInstance;
end;

destructor TMusicPlayer.Destroy;
begin
  FMusicPlayer.release;
end;

procedure TMusicPlayer.DoOnSongChange(newIndex: Integer);
begin
  if Assigned(FOnSongChange) then
    TThread.Queue(TThread.CurrentThread, procedure
      begin
        FOnSongChange(newIndex);
      end);
end;

procedure TMusicPlayer.DoOnProcessPlay(newPos: Single);
begin
  if Assigned(FOnProcessPlay) then
    TThread.Queue(TThread.CurrentThread, procedure
      begin
        FOnProcessPlay(newPos);
      end);
end;

function TMusicPlayer.GetAlbums: TArray<string>;
var
  projection: TJavaObjectArray<JString>;
  cursor: JCursor;
  artPath: String;
  tmpPath: String;
begin
  projection := TJavaObjectArray<JString>.Create(4);
  projection.Items[0] := TJAudio_AlbumColumns.JavaClass.ALBUM;
  projection.Items[1] := TJAudio_AlbumColumns.JavaClass.ARTIST;
  projection.Items[2] := StringToJString('_id');
  projection.Items[3] := TJAudio_AlbumColumns.JavaClass.ALBUM_ART;

  cursor := MainActivity.getContentResolver.query(
    TJAudio_Albums.JavaClass.EXTERNAL_CONTENT_URI,
    projection,
    nil,
    nil,
    nil);

  SetLength(Result, cursor.getCount);
  SetLength(FAlbums, cursor.getCount + 1);
  FAlbums[cursor.getCount] := TMPAlbum.AllMusicAlbum;
  while (cursor.moveToNext) do
  begin
    FAlbums[cursor.getPosition].Name := JStringToString(cursor.getString(0));
    FAlbums[cursor.getPosition].Artist := JStringToString(cursor.getString(1));
    FAlbums[cursor.getPosition].Album_ID := cursor.getInt(2);

    artPath := JStringToString(cursor.getString(3));
    if TFile.Exists(artPath) then
    begin
      try
        //Workaround for loading problems: copy to a file with correct extension.
        tmpPath := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,  'tmp.jpg');
        TFile.Copy(artPath, tmpPath);
        FAlbums[cursor.getPosition].Artwork := TBitmap.CreateFromFile(tmpPath);
        TFile.Delete(tmpPath);
      except
      end;
    end
    else
      FAlbums[cursor.getPosition].Artwork := FDefaultAlbumImage;

    Result[cursor.getPosition] := FAlbums[cursor.getPosition].Name;
  end;
  cursor.close;
end;

function TMusicPlayer.GetDuration: Single;
begin
  Result := FMusicPlayer.getDuration;
end;

function TMusicPlayer.GetPlaybackState: TMPPlaybackState;
begin
  Result := FPlayBackState;
end;

function TMusicPlayer.GetRepeatMode: TMPRepeatMode;
begin
  Result := FRepeatMode;
end;

function TMusicPlayer.GetShuffleMode: Boolean;
begin
  Result := FShuffleMode;
end;

function TMusicPlayer.GetSongs: TArray<string>;
var
  projection: TJavaObjectArray<JString>;
  cursor: JCursor;
  selection: JString;
begin
  selection := StringToJString(JStringToString(TJAudio_AudioColumns.JavaClass.IS_MUSIC) + ' != 0');

  projection := TJavaObjectArray<JString>.Create(5);

  projection.Items[0] := TJAudio_AudioColumns.JavaClass.ARTIST;
  projection.Items[1] := StringToJString('title');
  projection.Items[2] := StringToJString('_data');
  projection.Items[3] := TJAudio_AudioColumns.JavaClass.ALBUM;
  projection.Items[4] := TJAudio_AudioColumns.JavaClass.DURATION;

  cursor := MainActivity.getContentResolver.query(
    TJAudio_Media.JavaClass.EXTERNAL_CONTENT_URI,
    projection,
    selection,
    nil,
    nil);

  SetLength(Result,cursor.getCount);
  SetLength(FPlaylist, cursor.getCount);
  while (cursor.moveToNext) do
  begin
    FPlaylist[cursor.getPosition] := TMPSong.FromCursor(cursor);
    Result[cursor.getPosition] := Format('[%s]-[%s]', [FPlaylist[cursor.getPosition].Artist, FPlaylist[cursor.getPosition].Title]);
  end;
  cursor.close;
end;

function TMusicPlayer.GetSongsInAlbum(AName: string): TArray<string>;
var
  projection: TJavaObjectArray<JString>;
  cursor: JCursor;
  selection: JString;
begin
  if AName = TMPAlbum.AllMusicAlbum.Name then
  begin
    Result := GetSongs;
    Exit;
  end;

  selection := StringToJString(JStringToString(TJAudio_AudioColumns.JavaClass.IS_MUSIC) + ' != 0 and ' +
      JStringToString(TJAudio_AudioColumns.JavaClass.ALBUM) + ' = "' + AName + '"');

  projection := TJavaObjectArray<JString>.Create(5);

  projection.Items[0] := TJAudio_AudioColumns.JavaClass.ARTIST;
  projection.Items[1] := StringToJString('title');
  projection.Items[2] := StringToJString('_data');
  projection.Items[3] := TJAudio_AudioColumns.JavaClass.ALBUM;
  projection.Items[4] := TJAudio_AudioColumns.JavaClass.DURATION;

  cursor := MainActivity.getContentResolver.query(
    TJAudio_Media.JavaClass.EXTERNAL_CONTENT_URI,
    projection,
    selection,
    nil,
    nil);

  SetLength(Result,cursor.getCount);
  SetLength(FPlaylist, cursor.getCount);
  while (cursor.moveToNext) do
  begin
    FPlaylist[cursor.getPosition] := TMPSong.FromCursor(cursor);
    Result[cursor.getPosition] := Format('[%s]-[%s]', [FPlaylist[cursor.getPosition].Artist, FPlaylist[cursor.getPosition].Title]);
  end;
  cursor.close;
end;

function TMusicPlayer.GetTime: Single;
begin
  Result := FMusicPlayer.getCurrentPosition;
end;

function TMusicPlayer.GetVolume: Single;
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Result := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  Result := Result / AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
end;

procedure TMusicPlayer.Next;
begin
  case RepeatMode of
    TMPRepeatMode.One:
    begin
      Time := 0;
      Play;
    end;
    TMPRepeatMode.Default,
    TMPRepeatMode.None:
      if CurrentIndex = Length(FPlaylist) - 1  then
        FMusicPlayer.Stop
      else
      begin
        if ShuffleMode then
          PlayByIndex(Random(Length(FPlaylist)))
        else
          PlayByIndex(FCurrentIndex + 1);
      end;
    TMPRepeatMode.All:
      if FCurrentIndex = Length(FPlaylist) - 1 then
        PlayByIndex(0)
      else
      begin
        if FShuffleMode then
          PlayByIndex(Random(Length(FPlaylist)))
        else
          PlayByIndex(FCurrentIndex + 1);
      end;
  end;
  DoOnSongChange(FCurrentIndex);
end;

procedure TMusicPlayer.Pause;
begin
  FMusicPlayer.pause;
  FPlayBackState := TMPPlaybackState.Paused;
end;

procedure TMusicPlayer.Play;
begin
  if FPlayBackState = TMPPlaybackState.Stopped  then
    FMusicPlayer.prepare;
  FMusicPlayer.start;
  FPlayBackState := TMPPlaybackState.Playing;
end;

procedure TMusicPlayer.PlayByIndex(Index: Integer);
begin
  FCurrentIndex := Index;
  PlaySong(FPlaylist[FCurrentIndex].Path);
end;

function TMusicPlayer.IsPlaying: Boolean;
begin
  Result := FMusicPlayer.isPlaying;
end;

procedure TMusicPlayer.PlaySong(path: string);
begin
  Stop;
  FMusicPlayer.reset;
  FMusicPlayer.setDataSource(StringToJString(path));
  Play;
end;

procedure TMusicPlayer.Previous;
begin
  if (FCurrentIndex > 0) and (FCurrentIndex < Length(FPlaylist)) then
  begin
    PlayByIndex(FCurrentIndex -1);
    DoOnSongChange(FCurrentIndex);
  end;
end;

class procedure TMusicPlayer.SetPlayerType(AType: TMPControllerType);
begin
  // Do nothing
end;

procedure TMusicPlayer.SetRepeatMode(const Value: TMPRepeatMode);
begin
  FRepeatMode := Value;
end;

procedure TMusicPlayer.SetShuffleMode(const Value: Boolean);
begin
  FShuffleMode := Value;
end;

procedure TMusicPlayer.SetTime(const Value: Single);
begin
  FMusicPlayer.seekTo(Trunc(Value));
end;

procedure TMusicPlayer.SetVolume(const Value: Single);
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.setStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC,
    Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC) * Value), 0);
end;

procedure TMusicPlayer.Stop;
begin
  if FPlayBackState in [TMPPlaybackState.Playing, TMPPlaybackState.Paused] then
    FMusicPlayer.seekTo(0);
  FPlayBackState := TMPPlaybackState.Stopped;
  FMusicPlayer.stop;
  while FMusicPlayer.isPlaying do
    sleep(10);
  DoOnProcessPlay(0);
end;

{ TMusicPlayer.TProcessThread }

constructor TMusicPlayer.TProcessThread.Create(CreateSuspended: Boolean;
  AMusicPlayer: TMusicPlayer; processHandler: TOnProcessPlayEvent);
begin
  inherited Create(CreateSuspended);
  FOnProcessPlay := processHandler;
  FMusicPlayer := AMusicPlayer;
end;

destructor TMusicPlayer.TProcessThread.Destroy;
begin
  FMusicPlayer := nil;
  inherited;
end;

procedure TMusicPlayer.TProcessThread.Execute;
begin
  inherited;
  while Assigned(FMusicPlayer) do
  begin
    case FMusicPlayer.PlaybackState of
      Playing: DoProcessPlay;
      Stopped,
      Paused,
      Interrupted,
      SeekingForward,
      SeekingBackward: sleep(200);
    end;
  end;
end;

procedure TMusicPlayer.TProcessThread.DoProcessPlay;
var
  currentPos: Single;
begin
  currentPos := FMusicPlayer.Time;
  if Assigned(FOnProcessPlay) then
    FOnProcessPlay((currentPos/FMusicPlayer.Duration) * 100);


  if FMusicPlayer.IsPlaying then
    Sleep(200)
  else
    FMusicPlayer.Next;


end;
{$ENDIF}
end.
