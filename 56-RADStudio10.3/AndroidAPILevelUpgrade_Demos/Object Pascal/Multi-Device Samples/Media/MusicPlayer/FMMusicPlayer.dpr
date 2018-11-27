//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program FMMusicPlayer;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MediaPlayerU in 'MediaPlayerU.pas' {FMXMusicPlayerFrm},
  MusicPlayer.Android in 'MusicPlayer.Android.pas',
  MusicPlayer.iOS in 'MusicPlayer.iOS.pas',
  MusicPlayer.Utils in 'MusicPlayer.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXMusicPlayerFrm, FMXMusicPlayerFrm);
  Application.Run;
end.
