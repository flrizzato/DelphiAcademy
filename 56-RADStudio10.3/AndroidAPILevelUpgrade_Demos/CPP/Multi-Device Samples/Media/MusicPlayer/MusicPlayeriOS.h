//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MusicPlayeriOSH
#define MusicPlayeriOSH

#ifdef __APPLE__

#include <FMX.Graphics.hpp>
#include <FMX.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.IOUtils.hpp>
#include <iOSapi.MediaPlayer.hpp>
#include <iOSapi.Foundation.hpp>
#include <iOSapi.UIKit.hpp>
#include <FMX.Helpers.iOS.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Actions.hpp>
#include <vector>
#include "MusicPlayerUtils.h"

// ---------------------------------------------------------------------------
namespace Musicplayer {
namespace Ios {
// Types ---------------------------------------------------------------------

class TMusicPlayer : public TObject {
private:
	class TProcessThread : public System::Classes::TThread {
	private:
		TMusicPlayer* FMusicPlayer;

		Musicplayer::Utils::TMPSong FLastItem;
		Musicplayer::Utils::TOnProcessPlayEvent FOnProcessPlay;
		void __fastcall ProcessPlay(void);

	public:
		__fastcall TProcessThread(bool CreateSuspended,
			__strong TMusicPlayer* AMusicPlayer,
			Musicplayer::Utils::TOnProcessPlayEvent processHandler);
		__fastcall virtual ~TProcessThread(void);
		virtual void __fastcall Execute(void);
	};

protected:
	static TMusicPlayer* FInstance;

private:
	unsigned int FCurrentIndex;

	System::DynamicArray<Musicplayer::Utils::TMPSong>FPlaylist;
	System::DynamicArray<Musicplayer::Utils::TMPAlbum>FAlbums;
	Iosapi::Mediaplayer::_di_MPMusicPlayerController FMusicPlayer;
	Fmx::Graphics::TBitmap* FDefaultAlbumImage;
	Musicplayer::Utils::TOnSongChangeEvent FOnSongChange;
	Musicplayer::Utils::TOnProcessPlayEvent FOnProcessPlay;

protected:
	inline __fastcall TMusicPlayer(Utils::TMPControllerType AType);

private:
	__fastcall ~TMusicPlayer(void);
	void __fastcall DoOnSongChange(int newIndex);
	void __fastcall DoOnProcessPlay(float newPos);
	void __fastcall SetVolume(const float Value);
	void __fastcall SetTime(const float Value);
	void __fastcall SetRepeatMode(const Utils::TMPRepeatMode Value);
	void __fastcall SetShuffleMode(const bool Value);
	float __fastcall GetVolume(void);
	float __fastcall GetTime(void);
	Utils::TMPRepeatMode __fastcall GetRepeatMode(void);
	float __fastcall GetDuration(void);
	Utils::TMPPlaybackState __fastcall GetPlaybackState(void);
	bool __fastcall GetShuffleMode(void);

public:
	static void __fastcall SetPlayerType
		(Utils::TMPControllerType AType);
	static TMusicPlayer* __fastcall DefaultPlayer();
	__property unsigned CurrentIndex = {read = FCurrentIndex};
	__property float Volume = {read = GetVolume, write = SetVolume};
	__property float Time = {read = GetTime, write = SetTime};
	__property float Duration = {read = GetDuration};
	__property Utils::TMPPlaybackState PlaybackState = {
		read = GetPlaybackState};
	__property bool ShuffleMode = {
		read = GetShuffleMode, write = SetShuffleMode};
	__property Utils::TMPRepeatMode RepeatMode = {
		read = GetRepeatMode, write = SetRepeatMode};
	__property System::DynamicArray<Musicplayer::Utils::TMPSong>Playlist
		= {read = FPlaylist};
	__property System::DynamicArray<Musicplayer::Utils::TMPAlbum>Albums
		= {read = FAlbums};
	__property Fmx::Graphics::TBitmap* DefaultAlbumImage = {
		read = FDefaultAlbumImage, write = FDefaultAlbumImage};
	__property Musicplayer::Utils::TOnSongChangeEvent OnSongChange = {
		read = FOnSongChange, write = FOnSongChange};
	__property Musicplayer::Utils::TOnProcessPlayEvent OnProcessPlay = {
		read = FOnProcessPlay, write = FOnProcessPlay};

public:
	NativeUInt __fastcall IndexOfNowPlayingItem(void);
	System::DynamicArray<System::UnicodeString>__fastcall
		GetAlbums(void);
	System::DynamicArray<System::UnicodeString>__fastcall
		GetSongs(void);
	System::DynamicArray<System::UnicodeString>__fastcall
		GetSongsInAlbum(System::UnicodeString AName);
	bool __fastcall CanSkipBack();
	bool __fastcall CanSkipForward();
	void __fastcall PlayByIndex(int Index);
	void __fastcall Play(void);
	void __fastcall Stop(void);
	void __fastcall Pause(void);
	void __fastcall Next(void);
	void __fastcall Previous(void);
};
} /* namespace Ios */
} /* namespace Musicplayer */
#endif
#endif
