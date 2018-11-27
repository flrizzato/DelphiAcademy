//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MusicPlayerUtilsH
#define MusicPlayerUtilsH

#include <FMX.Types.hpp>
#ifdef __APPLE__
#include <iOSApi.MediaPlayer.hpp>
#include <iOSApi.Foundation.hpp>
#endif
#ifdef __ANDROID__
#include <Androidapi.JNI.JavaTypes.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#include <Androidapi.Helpers.hpp>
#endif
#include <System.SysUtils.hpp>
#include <FMX.Graphics.hpp>

// ---------------------------------------------------------------------------
namespace Musicplayer {
	namespace Utils {

		enum class TMPControllerType : unsigned char {
			App, Ipod
		};

		enum class TMPRepeatMode : unsigned char {
			Default, None, One, All
		};

		enum class TMPPlaybackState : unsigned char {
			Stopped, Playing, Paused, Interrupted, SeekingForward,
			SeekingBackward
		};

		struct TMPSong {
		public:
			System::UnicodeString Album;
			System::UnicodeString Artist;
			System::UnicodeString Duration;
			System::UnicodeString Title;
#ifdef __ANDROID__
			System::UnicodeString Path;
			static TMPSong __fastcall FromCursor
				(Androidapi::Jni::Graphicscontentviewtext::_di_JCursor c);
#endif
#ifdef __APPLE__
			Iosapi::Mediaplayer::_di_MPMediaItem MPItem;
			static TMPSong __fastcall FromMediaItem
				(Iosapi::Mediaplayer::_di_MPMediaItem Item);
#endif
			static TMPSong __fastcall EmptySong();
			static System::UnicodeString __fastcall DurationToString
				(float duration);
			bool __fastcall Equals(const TMPSong &song);
		};

		struct TMPAlbum {
		public:
			System::UnicodeString Name;
			System::UnicodeString Artist;

			int Album_ID;

			__strong Fmx::Graphics::TBitmap* Artwork;
			static TMPAlbum __fastcall AllMusicAlbum();
		};

		typedef void __fastcall(__closure * TOnSongChangeEvent)(int newIndex);

		typedef void __fastcall(__closure * TOnProcessPlayEvent)(float newPos);

		// template for Invoke TThreadProcedure
		template<typename M, typename P>
		class MyThreadMethod : public TCppInterfacedObject<TThreadProcedure>
		{
		private:
			M m_Method;
			P m_param;

		public:
			// constructor
			__inline __fastcall MyThreadMethod(M _m, P _p)
				: m_Method(_m), m_param(_p) {
			};

			void __fastcall Invoke(void) {
				m_Method(m_param);
			}
		};

#ifdef __APPLE__
		_di_NSString __fastcall MPMediaItemPropertyTitle(void);
		_di_NSString __fastcall MPMediaItemPropertyAlbumTitle(void);
		_di_NSString __fastcall MPMediaItemPropertyArtist(void);
		_di_NSString __fastcall MPMediaItemPropertyArtwork(void);
		_di_NSString __fastcall MPMediaItemPropertyPlaybackDuration(void);
		_di_NSString __fastcall MPMediaItemPropertyMediaType(void);
		_di_NSString __fastcall MPMediaItemPropertyComposer(void);
		_di_NSString __fastcall MPMediaItemPropertyGenre(void);
		_di_NSString __fastcall MPMediaPlaylistPropertyName(void);
		_di_NSString __fastcall MPMediaItemPropertyPodcastTitle(void);
#endif
	} /* namespace Utils */
} /* namespace Musicplayer */

#endif
