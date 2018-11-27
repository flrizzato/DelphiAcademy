//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MediaPlayerUH
#define MediaPlayerUH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <FMX.MultiView.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#ifdef __APPLE__
#include "MusicPlayeriOS.h"
#endif
#ifdef __ANDROID__
#include "MusicPlayerAndroid.h"
#endif
#include "MusicPlayerUtils.h"

// ---------------------------------------------------------------------------
class TTFMXMusicPlayerFrm : public TForm {
__published: // IDE-managed Components
	TTabControl *tcUITabs;
	TTabItem *tiAlbums;
	TListView *lvAlbums;
	TTabItem *tiSongs;
	TListView *lvSongs;
	TTabItem *tiNowPlaying;
	TToolBar *tbNowPlaying;
	TButton *btnPrev;
	TButton *btnNext;
	TLayout *lyState;
	TButton *btnPlay;
	TButton *btnPause;
	TButton *btnStop;
	TLabel *lblArtist;
	TLabel *lblTitle;
	TLabel *lblAlbum;
	TLabel *lblDuration;
	TLabel *lblArtistVal;
	TLabel *lblTitleVal;
	TLabel *lblAlbumVal;
	TLabel *lblDurationVal;
	TTrackBar *tbProgress;
	TListBox *SettingsList;
	TListBoxGroupHeader *RepeatModes;
	TListBoxItem *All;
	TListBoxItem *One;
	TListBoxItem *None;
	TListBoxItem *Default;
	TListBoxGroupHeader *ShuffleMusic;
	TListBoxItem *ShufffleMode;
	TSwitch *swShuffleMode;
	TListBoxGroupHeader *VolumeHeader;
	TListBoxItem *VolumeListItem;
	TTrackBar *VolumeTrackBar;
	TMultiView *mvSettings;
	TButton *btnSettings;
	TLayout *lyProgressSettings;
	TTimer *volTimer;
	TToolBar *tbSettings;
	TLabel *lblSettings;
	TButton *btnCloseSettings;
	void __fastcall btnPrevClick(TObject *Sender);
	void __fastcall btnNextClick(TObject *Sender);
	void __fastcall lvAlbumsChange(TObject *Sender);
	void __fastcall btnPauseClick(TObject *Sender);
	void __fastcall btnPlayClick(TObject *Sender);
	void __fastcall btnStopClick(TObject *Sender);
	void __fastcall tbProgressChange(TObject *Sender);
	void __fastcall RepeatItemsClick(TObject *Sender);
	void __fastcall swShuffleModeSwitch(TObject *Sender);
	void __fastcall lvSongsChange(TObject *Sender);
	void __fastcall VolumeTrackBarChange(TObject *Sender);
	void __fastcall volTimerTimer(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall btnCloseSettingsClick(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
private: // User declarations
    String FPermissionReadExternalStorage;
    void __fastcall DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc);
    void __fastcall ReadStoragePermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults);
	void __fastcall DoUpdateUI(float newPos);
	void __fastcall UpdateNowPlaying(int newIndex);
	void __fastcall UpdateSongs();
	void __fastcall SongChanged(int newIndex);
	void __fastcall StateChanged(Musicplayer::Utils::TMPPlaybackState state);
public: // User declarations
	__fastcall TTFMXMusicPlayerFrm(TComponent *Owner);
};
// ---------------------------------------------------------------------------
extern PACKAGE TTFMXMusicPlayerFrm *TFMXMusicPlayerFrm;
// ---------------------------------------------------------------------------
#endif
