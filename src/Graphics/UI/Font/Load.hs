module Graphics.UI.Font.Load
  (
    -- * Motivation
    --
    -- $Motivation

    -- * Quick Start
    --
    -- $QuickStart

    -- * The API
    --
    -- $TheAPI

    loadFont
  , unloadFont
  )
where
import Foreign.C.String
import Control.Applicative
import Foreign.C.Types
import qualified Graphics.UI.Font.LoadCWrapper as CWrapper

-- $Motivation
-- The goal of this library is to provide a simple way of giving an application access to a font that
-- isn't available system-wide allowing you to bundle fonts with your executable ensuring uniform look-and-feel
-- across platforms. It has been tested on Windows 10, Arch Linux and OSX Sierra

-- $QuickStart
-- While this package ships with a fully working [demo](https://github.com/deech/load-font#load-font-demo) the basic
-- usage is lost in all of the [GUI code](https://github.com/deech/load-font/blob/master/app/Main.hs).
-- And frankly the demo takes a while to build and pulls in a GUI library dependency so it's not worth it
-- unless you really want to convince yourself the library works. To get started with the minimum fuss
-- thrill at the [screenshot](https://github.com/deech/load-font#load-font-demo), take my word for it
-- and do the following:
--
--  - Add your fonts to the Cabal file's <https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-data-files data-files stanza> (completely optional, you can load fonts from anywhere on the file system but highly recommended for portability):
--
-- @
-- data-files:
--     fonts/*.ttf
-- @
--
--  - Add the @load-font@ dependency:
--
-- @
--   executable myAwesomeApp
--   ...
--     build-depends:
--       ...
--       , load-font
--       ...
--   ...
-- @
--
--  - Somewhere in your app code before you use the font :
--
-- @
--
--  import Paths_myAwesomeApp
--  ...
--  myAwesomeFunction = do
--    ...
--    fontPath <- getDataFileName "fonts/my-awesome-font.ttf"
--    loadFont fontPath
--    ...
--
-- @

-- |
-- Remove a private font located at 'FilePath' from the application.
--
-- For the most part you shouldn't need this function because private fonts are
-- automatically unloaded when the process exits but it's available in case
-- you're doing something more exotic like switching between two versions of the
-- same font.
--
-- On Windows and OSX this works as you would expect. But on Linux the only
-- available function is
-- <https://www.freedesktop.org/software/fontconfig/fontconfig-devel/fcconfigappfontclear.html FcConfigAppFontClear>
-- which ignores the 'FilePath' and removes /all/ private
-- fonts. This adheres violently to the Principle Of Greatest Surprise and in
-- the future I will transparently reload the other fonts but for now, caveat
-- computer.
unloadFont :: FilePath -> IO ()
unloadFont fp = withCString fp (\pathPtr -> CWrapper.unloadFont pathPtr)

-- |
-- Make a font located at some 'FilePath' available to your application. The font is automatically cleared from the font database with the process exits.
--
-- On Linux this uses
-- <https://www.freedesktop.org/software/fontconfig/fontconfig-devel/fcconfigappfontaddfile.html FcConfigAppFontAddFile>
-- under the hood and so assumes X11/Xft are available.
-- It should work fine on modern Linux systems but will break with old Xlib
-- legacy fonts.
--
-- Currently the error case just returns a pretty uninformative message
-- because underlying calls on Linux and Windows which are out of my control
-- only return 0 or 1 in case of failure or success. Given this it would make more sense that
-- the return type should be 'Maybe ()' but 'Either String ()' has two advantages
-- 1. It makes errors easier to collect when batch loading
-- 2. OSX has a <https://developer.apple.com/documentation/corefoundation/cferror-ru8 much nicer error> which I plan to expose in the future
loadFont :: FilePath -> IO (Either String ())
loadFont fp = withCString fp (\pathPtr -> do
                                 res <- CWrapper.loadFont pathPtr
                                 case res of
                                   Left _ -> return (Left ("Unable to load: " ++ fp))
                                   Right () -> return (Right ())
                             )
