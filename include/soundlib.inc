#if defined _soundlib_ext_included
    #endinput
#endif
#define _soundlib_ext_included

/**
 * Opens a sound file.
 *
 * @note Sound files are closed with delete operator.
 *
 * @param file                File to open
 * @param valve if true, path is opened in Valve Filesystem.
 * @return                    A Handle to the sound file, null on open error.
 */
native Handle OpenSoundFile(const char[] path, bool valve=false);

/**
 * Gets the length of the sound file in seconds
 *
 * @param hndl            Handle to the sound file.
 * @return                The song length in seconds
 */
native int GetSoundLength(Handle soundfile);

/**
 * Get the Bit rate of sound (kbps)
 *
 * @param hndl            Handle to the sound file
 * @return                sound bitrate (int)
 */
native int GetSoundBitrate(Handle soundfile);

/**
 * Get the Sampling rate of sound (hz)
 *
 * @param hndl            Handle to the sound file
 * @return                sampling rate (int)
 */
native int GetSoundSamplingRate(Handle soundfile);

/**
 * Do not edit below this line!
 */
public Extension __ext_symphonia = 
{
    name = "SoundLib",
    file = "soundlib.ext",
#if defined AUTOLOAD_EXTENSIONS
    autoload = 1,
#else
    autoload = 0,
#endif
#if defined REQUIRE_EXTENSIONS
    required = 1,
#else
    required = 0,
#endif
};
