// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

#include <fmx.h>
#ifdef __ANDROID__
    #include <Androidapi.Helpers.hpp>
    #include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "MediaPlayerU.h"

// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TTFMXMusicPlayerFrm *TFMXMusicPlayerFrm;
#ifdef __APPLE__
using namespace Musicplayer::Ios;
#endif
#ifdef __ANDROID__
using namespace Musicplayer::Android;
#endif
// ---------------------------------------------------------------------------
__fastcall TTFMXMusicPlayerFrm::TTFMXMusicPlayerFrm(TComponent *Owner) : TForm(Owner) {
#ifdef __ANDROID__
    FPermissionReadExternalStorage = JStringToString(TJManifest_permission::JavaClass->READ_EXTERNAL_STORAGE);
#endif
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::FormCreate(TObject *Sender) {
#ifdef __ANDROID__
    tcUITabs->TabPosition = TTabPosition::Top;
#endif
    TMusicPlayer::DefaultPlayer()->OnSongChange = SongChanged;
    TMusicPlayer::DefaultPlayer()->OnProcessPlay = DoUpdateUI;

    DynamicArray<String> permissions;
    permissions.Length = 1;
    permissions[0] = FPermissionReadExternalStorage;

    PermissionsService()->RequestPermissions(permissions, ReadStoragePermissionRequestResult, DisplayRationale);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::FormDestroy(TObject *Sender) {
    TMusicPlayer::DefaultPlayer()->OnSongChange = (Musicplayer::Utils::TOnSongChangeEvent)NULL;
    TMusicPlayer::DefaultPlayer()->OnProcessPlay = (Musicplayer::Utils::TOnProcessPlayEvent)NULL;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc) {
    // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
    // After the user sees the explanation, invoke the post-rationale routine to request the permissions
    TDialogService::ShowMessage("The app needs to read files from your device storage to show you the songs and albums available to you",
        [APostRationaleProc](TModalResult AKey)
        {
            APostRationaleProc->Invoke();
        });
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::ReadStoragePermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults) {
    // 1 permission involved: READ_EXTERNAL_STORAGE
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
    {
        TMusicPlayer::DefaultPlayer()->GetAlbums();
        TMusicPlayer::DefaultPlayer()->GetSongs();
        if (TMusicPlayer::DefaultPlayer()->Albums.Length >= 2) {
            lvAlbums->BeginUpdate();
            for (int i = 0; i < TMusicPlayer::DefaultPlayer()->Albums.Length; i++) {
                auto album = TMusicPlayer::DefaultPlayer()->Albums[i];
                auto item = lvAlbums->Items->Add();
                item->Text = album.Name;
                item->Detail = album.Artist;
                item->Bitmap = album.Artwork;
            }
            lvAlbums->EndUpdate();
            UpdateSongs();
            TMusicPlayer::DefaultPlayer()->ShuffleMode = swShuffleMode->IsChecked;
            RepeatItemsClick(All);
            StateChanged(Musicplayer::Utils::TMPPlaybackState::Stopped);
        }
        else
            TDialogService::ShowMessage("There is no music on this device");
    }
	else
		TDialogService::ShowMessage("Cannot read the music files on this device because the required permission is not granted");
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPrevClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Previous();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnNextClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Next();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::DoUpdateUI(float newPos) {
    TNotifyEvent handler = tbProgress->OnChange;
    tbProgress->OnChange = (TNotifyEvent)NULL;
    tbProgress->Value = newPos;
    tbProgress->OnChange = handler;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateNowPlaying(int newIndex) {
    lblArtistVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Artist;
    lblTitleVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Title;
    lblAlbumVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Album;
    lblDurationVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Duration;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateSongs() {
	lvSongs->BeginUpdate();
	lvSongs->Items->Clear();
	for (int i = 0; i < TMusicPlayer::DefaultPlayer()->Playlist.Length; i++) {
		Musicplayer::Utils::TMPSong song = TMusicPlayer::DefaultPlayer()->Playlist[i];
		TListViewItem *item = lvSongs->Items->Add();
		if (song.Artist != "Unknow") {
			item->Text = Format("%s - %s", ARRAYOFCONST((song.Artist, song.Title)));
		} else {
			item->Text = song.Title;
		}
	}
	lvSongs->EndUpdate();
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::SongChanged(int newIndex) {
    TNotifyEvent handler = lvSongs->OnChange;
    lvSongs->OnChange = (TNotifyEvent)NULL;
    lvSongs->ItemIndex = newIndex;
    UpdateNowPlaying(newIndex);
    lvSongs->OnChange = handler;
    StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::StateChanged(Musicplayer::Utils::TMPPlaybackState state) {
    btnPlay->Enabled = !(state == Musicplayer::Utils::TMPPlaybackState::Playing);
    btnPause->Enabled = !(state == Musicplayer::Utils::TMPPlaybackState::Paused || state == Musicplayer::Utils::TMPPlaybackState::Stopped);
    btnStop->Enabled = !(state == Musicplayer::Utils::TMPPlaybackState::Stopped);
    tbProgress->Enabled = !(state == Musicplayer::Utils::TMPPlaybackState::Paused || state == Musicplayer::Utils::TMPPlaybackState::Stopped);
    btnNext->Enabled = (!(state == Musicplayer::Utils::TMPPlaybackState::Stopped)) && TMusicPlayer::DefaultPlayer()->CanSkipForward();
    btnPrev->Enabled = (!(state == Musicplayer::Utils::TMPPlaybackState::Stopped)) && TMusicPlayer::DefaultPlayer()->CanSkipBack();
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::lvAlbumsChange(TObject *Sender) {
    TMusicPlayer::DefaultPlayer()->GetSongsInAlbum(TMusicPlayer::DefaultPlayer()->Albums[lvAlbums->ItemIndex].Name);
    UpdateSongs();
    tcUITabs->SetActiveTabWithTransition(tiSongs, TTabTransition::Slide);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPauseClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Pause();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Paused);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPlayClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Play();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnStopClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Stop();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Stopped);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::tbProgressChange(TObject *Sender) {
    TMusicPlayer::DefaultPlayer()->Time = (tbProgress->Value * TMusicPlayer::DefaultPlayer()->Duration) / 100;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::RepeatItemsClick(TObject *Sender) {
    if (dynamic_cast<TListBoxItem *>(Sender)) {
        for (int i = 0; i < SettingsList->Items->Count; i++) {
            SettingsList->ItemByIndex(i)->ItemData->Accessory = TListBoxItemData::TAccessory::aNone; ;
            TListBoxItem *item = (TListBoxItem *)Sender;
            if (item->Text == "All")
                TMusicPlayer::DefaultPlayer()->RepeatMode = Musicplayer::Utils::TMPRepeatMode::All;
            if (item->Text == "One")
                TMusicPlayer::DefaultPlayer()->RepeatMode = Musicplayer::Utils::TMPRepeatMode::One;
            if (item->Text == "None")
                TMusicPlayer::DefaultPlayer()->RepeatMode = Musicplayer::Utils::TMPRepeatMode::None;
            if (item->Text == "Default")
                TMusicPlayer::DefaultPlayer()->RepeatMode = Musicplayer::Utils::TMPRepeatMode::Default;
            item->ItemData->Accessory = TListBoxItemData::TAccessory::aCheckmark;
        }
    }
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::swShuffleModeSwitch(TObject *Sender) {
    TMusicPlayer::DefaultPlayer()->ShuffleMode = swShuffleMode->IsChecked;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::lvSongsChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->PlayByIndex(lvSongs->ItemIndex);
	UpdateNowPlaying(lvSongs->ItemIndex);
	tcUITabs->SetActiveTabWithTransition(tiNowPlaying, TTabTransition::Slide);
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::VolumeTrackBarChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Volume = VolumeTrackBar->Value;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::volTimerTimer(TObject *Sender) {
    TNotifyEvent LEvent = VolumeTrackBar->OnChange;
    VolumeTrackBar->OnChange = (TNotifyEvent)NULL;
    VolumeTrackBar->Value = TMusicPlayer::DefaultPlayer()->Volume;
    VolumeTrackBar->OnChange = LEvent;
}
// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnCloseSettingsClick(TObject *Sender) {
    mvSettings->HideMaster();
}
// ---------------------------------------------------------------------------
