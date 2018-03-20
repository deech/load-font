{-# LANGUAGE CPP #-}
module Graphics.UI.Font.LoadCWrapper
  (
    loadFont
  , unloadFont
  )
where
import Foreign.C.String
import Control.Applicative
import Foreign.C.Types
#include "font_load.h"
{# fun load_private_font as loadFont' { `CString' } -> `CInt' #}
loadFont :: CString -> IO (Either String ())
loadFont pathPtr = do
  res <- loadFont' pathPtr
  case res of
    0 -> return (Left "")
    _ -> return (Right ())
{# fun unload_private_font as unloadFont { `CString' } -> `()' #}
