#include "font_load.h"
#ifdef _WIN32
#define _WIN32_WINNT 0x0501 /* need at least WinXP for this API, I think */
#include <windows.h>
#elif defined(__APPLE__)
#include <ApplicationServices/ApplicationServices.h>
#else /* Assume X11 with XFT/fontconfig - this will break on systems using legacy Xlib fonts */
#include <fontconfig/fontconfig.h>
#define USE_XFT 1
#endif

// Create some portability wrappers
#ifdef _WIN32
int load_private_font(_In_ LPCTSTR path) {
  return AddFontResourceEx(path, FR_PRIVATE, 0);
}
void unload_private_font(_In_ LPCTSTR path) {
  RemoveFontResourceEx(path, FR_PRIVATE, 0);
}

#elif defined(__APPLE__)
int load_private_font(const char *pf) {
  int result = 0;
  CFErrorRef errRef;
  // Make a URL from the font name given
  CFURLRef fontURL = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (const UInt8 *)pf, strlen(pf), false);
  // Try to load the font file
  if (CTFontManagerRegisterFontsForURL(fontURL, kCTFontManagerScopeProcess, &errRef)) {
    result = 1;
  } else {
    result = 0;
  }
  // discard the fontURL
  if (fontURL)
    CFRelease(fontURL);
  return result;
} // load_private_font

void unload_private_font(const char *pf) {
  CFErrorRef err;
  // Make a URL from the font name given
  CFURLRef fontURL = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (const UInt8 *)pf, strlen(pf), false);
  // Try to unregister the font
  CTFontManagerUnregisterFontsForURL(fontURL, kCTFontManagerScopeProcess, &err);
  if (fontURL)
    CFRelease(fontURL);
} // unload_private_font

#else /* Assume X11 with XFT/fontconfig - will break on systems using legacy Xlib fonts */
int load_private_font(const char* path) {
  return (int)FcConfigAppFontAddFile(0, (const FcChar8 *)(path));
}
void unload_private_font(const char* path) {
  FcConfigAppFontClear(0);
}
#endif
