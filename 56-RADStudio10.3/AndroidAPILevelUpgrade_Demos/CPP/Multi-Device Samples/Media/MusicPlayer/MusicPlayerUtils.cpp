//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "MusicPlayerUtils.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#include "MusicPlayerUtils.h"

#ifdef _PLAT_IOS
#include <Macapi.Helpers.hpp>
#endif

namespace Musicplayer {
namespace Utils {
		// ---------------------------------------------------------------------------
#ifdef __APPLE__

TMPSong __fastcall TMPSong::FromMediaItem
	(Iosapi::Mediaplayer::_di_MPMediaItem Item) {
	TMPSong song;
	song.Artist = NSStrToStr(TNSString::Wrap(Item->valueForProperty(MPMediaItemPropertyArtist())));
	song.Album  = NSStrToStr(TNSString::Wrap(Item->valueForProperty(MPMediaItemPropertyAlbumTitle())));
	song.Title  = NSStrToStr(TNSString::Wrap(Item->valueForProperty(MPMediaItemPropertyTitle())));
	song.Duration =
		TMPSong::DurationToString
		(TNSNumber::Wrap(Item->valueForProperty
		(MPMediaItemPropertyPlaybackDuration()))->floatValue());
	song.MPItem = Item;
	song.MPItem->retain();
	if (song.Artist == "") {
		song.Artist = "Unknown";
	}
	return song;
}
#endif

#ifdef __ANDROID__

TMPSong __fastcall TMPSong::FromCursor
	(Androidapi::Jni::Graphicscontentviewtext::_di_JCursor c) {
	TMPSong song;
	song.Artist = JStringToString(c->getString(0));
	if (song.Artist == "<unknown>")
		song.Artist = "Unknown";
	song.Title = JStringToString(c->getString(1));
	song.Path = JStringToString(c->getString(2));
	song.Album = JStringToString(c->getString(3));
	song.Duration = TMPSong::DurationToString(c->getFloat(4));
	return song;
}
#endif

// ---------------------------------------------------------------------------
TMPSong TMPSong::EmptySong() {
	TMPSong song;
	song.Album = "-";
	song.Artist = "-";
	song.Duration = "-";
	song.Title = "-";
	return song;
}

// ---------------------------------------------------------------------------
System::UnicodeString TMPSong::DurationToString(float duration) {
	System::UnicodeString _return = "";
	System::UnicodeString _secondsStr = "";
#ifdef __APPLE__
	int hours = trunc(duration) / (60 * 60);
	int minutes = (static_cast<int>(trunc(duration)) % (60 * 60)) / 60;
	int seconds = static_cast<int>(trunc(duration)) % 60;
#endif
#ifdef __ANDROID__
	int hours = trunc(duration) / (1000 * 60 * 60);
	int minutes = (static_cast<int>(trunc(duration)) % (1000 * 60 * 60))
		/ (1000 * 60);
	int seconds =
		((static_cast<int>(trunc(duration)) % (1000 * 60 * 60)) %
		(1000 * 60)) / 1000;
#endif
	if (hours > 0) {
		if (minutes < 10) {
			_return += IntToStr(hours) + ":0";
		}
		else {
			_return += IntToStr(hours) + ":";
		}
	}
	if (seconds < 10) {
		_secondsStr = "0" + IntToStr(seconds);
	}
	else {
		_secondsStr = IntToStr(seconds);
	}
	return _return + IntToStr(minutes) + ":" + _secondsStr;
}

// ---------------------------------------------------------------------------
bool TMPSong::Equals(const TMPSong &song) {
	return (Artist == song.Artist) && (Album == song.Album) &&
		(Duration == song.Duration) && (Title == song.Title);
}

// ---------------------------------------------------------------------------
TMPAlbum TMPAlbum::AllMusicAlbum() {
	TMPAlbum album;
	album.Name = "All Songs";
	album.Artist = "";
	album.Album_ID = -1;
	album.Artwork = NULL;
	return album;
}
// ---------------------------------------------------------------------------
#ifdef __APPLE__
#define kLibMediaPlayer L"/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer"

_di_NSString MPMediaItemPropertyTitle() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyTitle");
}

_di_NSString MPMediaItemPropertyAlbumTitle() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyAlbumTitle");
}

_di_NSString MPMediaItemPropertyArtist() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyArtist");
}

_di_NSString MPMediaItemPropertyArtwork() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyArtwork");
}

_di_NSString MPMediaItemPropertyPlaybackDuration() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyPlaybackDuration");
}

_di_NSString MPMediaItemPropertyMediaType() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyMediaType");
}

_di_NSString MPMediaItemPropertyComposer() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyComposer");
}

_di_NSString MPMediaItemPropertyGenre() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyGenre");
}

_di_NSString MPMediaPlaylistPropertyName() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaPlaylistPropertyName");
}

_di_NSString MPMediaItemPropertyPodcastTitle() {
	return CocoaNSStringConst(kLibMediaPlayer, L"MPMediaItemPropertyPodcastTitle");
}
#endif
} /* namespace Utils */
} /* namespace Musicplayer */
