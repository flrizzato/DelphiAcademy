//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit MusicPlayer.iOS;

interface
{$IFDEF IOS}
uses
  MusicPlayer.Utils,
  FMX.Graphics, FMX.Types,
  System.SysUtils, System.Classes, System.Types, System.IoUtils,
  iOSApi.MediaPlayer, iOSApi.Foundation, iOSApi.UIKit, FMX.Helpers.iOS,
  Macapi.Helpers;

type

  TMusicPlayer = class
  private
  type
    TProcessThread = class (TThread)
    private
      [weak] FMusicPlayer: TMusicPlayer;
      FLastItem: TMPSong;
      FOnProcessPlay: TOnProcessPlayEvent;
      procedure ProcessPlay;
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
    FMusicPlayer: MPMusicPlayerController;
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
    class procedure SetPlayerType(AType: TMPControllerType);
    class function DefaultPlayer: TMusicPlayer;
    destructor Destroy; override;
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
    function IndexOfNowPlayingItem: Integer;
    function GetAlbums: TArray<string>;
    function GetSongs: TArray<string>;
    function GetSongsInAlbum(AName: string): TArray<string>;
    function CanSkipBack: Boolean;
    function CanSkipForward: Boolean;
    procedure PlayByIndex(Index: Integer);
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Next;
    procedure Previous;
  end;


{$ENDIF}
implementation
{$IFDEF IOS}


{ TMusicPlayer }

function TMusicPlayer.CanSkipBack: Boolean;
begin
  Result := (Length(FPlaylist) > 0) and (FCurrentIndex > 0) and
    (TMPPlaybackState(FMusicPlayer.playbackState) in [TMPPlaybackState.Playing, TMPPlaybackState.Paused]);
end;

function TMusicPlayer.CanSkipForward: Boolean;
begin
  Result := False;
  if (Length(FPlaylist) = 0) or not
    (TMPPlaybackState(FMusicPlayer.playbackState) in [TMPPlaybackState.Playing, TMPPlaybackState.Paused]) then
    Exit(Result);

  case RepeatMode of
    TMPRepeatMode.One:
      Result := (Low(FPlaylist) <= FCurrentIndex) and (FCurrentIndex <= High(FPlaylist));
    TMPRepeatMode.Default,
    TMPRepeatMode.None:
      Result := (Low(FPlaylist) <= FCurrentIndex) and (FCurrentIndex <= (High(FPlaylist) - 1));
    TMPRepeatMode.All:
      Result := True;
  end;
end;

procedure TMusicPlayer.Pause;
begin
  FMusicPlayer.pause;
end;

constructor TMusicPlayer.Create(AType: TMPControllerType);
var
  LAlbumImageFile: string;
begin
  case AType of
    TMPControllerType.App:
      FMusicPlayer := TMPMusicPlayerController.Wrap
        (TMPMusicPlayerController.OCClass.applicationMusicPlayer);
    TMPControllerType.Ipod:
      FMusicPlayer := TMPMusicPlayerController.Wrap
        (TMPMusicPlayerController.OCClass.iPodMusicPlayer);
  end;
  LAlbumImageFile := TPath.Combine(TPath.GetDocumentsPath, 'MusicNote.png');
  if FileExists(LAlbumImageFile) then
    FDefaultAlbumImage := TBitmap.CreateFromFile(LAlbumImageFile);
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
  inherited;
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
  query: MPMediaQuery;
  i: Integer;
  Item: MPMediaItemCollection;
  artwork: MPMediaItemArtwork;
  art_img: UIImage;
  art_size: NSSize;
  pt: Pointer;
  bounds: TSizeF;
begin
  query := TMPMediaQuery.Wrap(TMPMediaQuery.OCClass.albumsQuery);
  SetLength(Result, query.collections.count);
  SetLength(FAlbums, query.collections.count + 1);
  FAlbums[query.collections.count] := TMPAlbum.AllMusicAlbum;
  for i := 0 to query.collections.count - 1 do
  begin
    Item := TMPMediaItemCollection.Wrap(query.collections.objectAtIndex(i));
    FAlbums[i].Name := NSStrToStr(TNSString.Wrap
      (Item.representativeItem.valueForProperty(MPMediaItemPropertyAlbumTitle)));
    FAlbums[i].Artist := NSStrToStr(TNSString.Wrap
      (Item.representativeItem.valueForProperty(MPMediaItemPropertyArtist)));
    FAlbums[i].Album_ID := i;

    pt := Item.representativeItem.valueForProperty(MPMediaItemPropertyArtwork);
    if pt <> nil then
    begin
      try
        artwork := TMPMediaItemArtwork.Wrap(pt);
        bounds := artwork.bounds.ToSizeF;
        art_size.width := bounds.cx;
        art_size.height := bounds.cy;
        art_img := artwork.imageWithSize(art_size);
        if art_img <> nil then
          FAlbums[i].Artwork := UIImageToBitmap(art_img,0, TSize.Create(Trunc(art_size.width), Trunc(art_size.height)));
      except
      end;
    end;
    if FAlbums[i].Artwork = nil then
      FAlbums[i].Artwork := FDefaultAlbumImage;

    Result[i] := FAlbums[i].Name;
  end;
end;

function TMusicPlayer.GetDuration: Single;
begin
  Result := TNSNumber.Wrap(FMusicPlayer.nowPlayingItem.valueForProperty(MPMediaItemPropertyPlaybackDuration)).floatValue;
end;

function TMusicPlayer.GetPlaybackState: TMPPlaybackState;
begin
  Result := TMPPlayBackState(FMusicPlayer.playbackState);
end;

function TMusicPlayer.GetRepeatMode: TMPRepeatMode;
begin
  Result := TMPRepeatMode(FMusicPlayer.repeatMode);
end;

function TMusicPlayer.GetShuffleMode: Boolean;
begin
  Result := (FMusicPlayer.shuffleMode = MPMusicShuffleModeSongs) or (FMusicPlayer.shuffleMode = MPMusicShuffleModeAlbums);
end;

function TMusicPlayer.GetSongs: TArray<string>;
var
  query: MPMediaQuery;
  i: Integer;
begin
  query := TMPMediaQuery.Wrap(TMPMediaQuery.OCClass.songsQuery);
  FMusicPlayer.setQueueWithQuery(query);
  SetLength(Result, query.items.count);
  SetLength(FPlaylist, query.items.count);
  for i := 0 to query.items.count - 1 do
  begin
    FPlaylist[i] := TMPSong.FromMediaItem(TMPMediaItem.Wrap(query.items.objectAtIndex(i)));
    Result[i] := Format('[%s]-[%s]', [FPlaylist[i].Artist, FPlaylist[i].Title]);
  end;
end;

function TMusicPlayer.GetSongsInAlbum(AName: string): TArray<string>;
var
  query: MPMediaQuery;
  i: Integer;
  predicate: MPMediaPropertyPredicate;
begin
  if AName = TMPAlbum.AllMusicAlbum.Name then
  begin
    Result := GetSongs;
    Exit;
  end;

  query := TMPMediaQuery.Wrap(TMPMediaQuery.Alloc.init);
  predicate :=TMPMediaPropertyPredicate.Wrap(TMPMediaPropertyPredicate.OCClass.predicateWithValue(TNSString.OCClass.stringWithString(StrToNSStr(AName)), MPMediaItemPropertyAlbumTitle));
  query.addFilterPredicate(predicate);
  query.setGroupingType(MPMediaGroupingAlbum);
  FMusicPlayer.setQueueWithQuery(query);

  SetLength(Result,query.items.count);
  SetLength(FPlaylist, query.items.count);
  for i := 0 to query.items.count - 1 do
  begin
    FPlaylist[i] := TMPSong.FromMediaItem(TMPMediaItem.Wrap(query.items.objectAtIndex(i)));
    Result[i] := Format('[%s]-[%s]', [FPlaylist[i].Artist, FPlaylist[i].Title]);
  end;
end;

function TMusicPlayer.GetTime: Single;
begin
  Result := FMusicPlayer.currentPlaybackTime;
end;

function TMusicPlayer.GetVolume: Single;
begin
  Result := FMusicPlayer.volume;
end;

function TMusicPlayer.IndexOfNowPlayingItem: Integer;
var
  i: Integer;
begin
  Result := Result.MaxValue;
  for i := 0 to Length(FPlaylist) -1 do
  begin
    if FPlaylist[i].Equals(TMPSong.FromMediaItem(FMusicPlayer.nowPlayingItem)) then
    begin
      Result := i;
      FCurrentIndex := i;
      Exit;
    end;
  end;
end;

procedure TMusicPlayer.Next;
begin
  FMusicPlayer.skipToNextItem;
  DoOnSongChange(IndexOfNowPlayingItem);
end;

procedure TMusicPlayer.Play;
begin
  if FMusicPlayer.playbackState = Ord(TMPPlaybackState.Stopped) then
    FMusicPlayer.setNowPlayingItem(FPlaylist[FCurrentIndex].MPItem);
  FMusicPlayer.play;
end;

procedure TMusicPlayer.PlayByIndex(Index: Integer);
begin
  Stop;
  if (Index >= 0) and (Index < Length(FPlaylist)) then
  begin
    FMusicPlayer.setNowPlayingItem(FPlaylist[Index].MPItem);
    FCurrentIndex := Index;
    Play;
    DoOnSongChange(Index);
  end;
end;

procedure TMusicPlayer.Previous;
begin
  FMusicPlayer.skipToPreviousItem;
  DoOnSongChange(IndexOfNowPlayingItem);
end;

class procedure TMusicPlayer.SetPlayerType(AType: TMPControllerType);
begin
  if Assigned(FInstance) then
    FInstance.DisposeOf;
  FInstance := TMusicPlayer.Create(AType);
end;

procedure TMusicPlayer.SetRepeatMode(const Value: TMPRepeatMode);
begin
  FMusicPlayer.SetRepeatMode(Ord(Value));
end;

procedure TMusicPlayer.SetShuffleMode(const Value: Boolean);
begin
  if Value then
    FMusicPlayer.setShuffleMode(MPMusicShuffleModeSongs)
  else
    FMusicPlayer.setShuffleMode(MPMusicShuffleModeOff);
end;

procedure TMusicPlayer.SetTime(const Value: Single);
begin
  FMusicPlayer.setCurrentPlaybackTime(Value);
end;

procedure TMusicPlayer.SetVolume(const Value: Single);
begin
  FMusicPlayer.SetVolume(Value);
end;

procedure TMusicPlayer.Stop;
begin
  FMusicPlayer.pause;
  DoOnProcessPlay(0);
end;


{ TMusicPlayer.TProcessThread }

constructor TMusicPlayer.TProcessThread.Create(CreateSuspended: Boolean;
  AMusicPlayer: TMusicPlayer; processHandler: TOnProcessPlayEvent);
begin
  inherited Create(CreateSuspended);
  FMusicPlayer := AMusicPlayer;
  FLastItem := TMPSong.EmptySong;
  FOnProcessPlay := processHandler;
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
      TMPPlaybackState.Playing: ProcessPlay;
      TMPPlaybackState.Stopped,
      TMPPlaybackState.Paused,
      TMPPlaybackState.Interrupted,
      TMPPlaybackState.SeekingForward,
      TMPPlaybackState.SeekingBackward: sleep(100);
    end;
  end;
end;

procedure TMusicPlayer.TProcessThread.ProcessPlay;
begin
  if Assigned(FOnProcessPlay) then
    if FMusicPlayer.Duration > 0 then
      FOnProcessPlay((FMusicPlayer.Time/FMusicPlayer.Duration) * 100);
  if FLastItem.Equals(TMPSong.EmptySong) then
  begin
    FLastItem := FMusicPlayer.Playlist[FMusicPlayer.CurrentIndex];
  end
  else
  begin
    if (FMusicPlayer.IndexOfNowPlayingItem <> -1)
    and (not FLastItem.Equals(FMusicPlayer.Playlist[FMusicPlayer.IndexOfNowPlayingItem])) then
    begin
      FLastItem := FMusicPlayer.Playlist[FMusicPlayer.CurrentIndex];
      FMusicPlayer.DoOnSongChange(FMusicPlayer.CurrentIndex);
      sleep(100);
    end
    else
    begin
      Sleep(100);
    end;
  end;
end;
{$ENDIF}
end.
