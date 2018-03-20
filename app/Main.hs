{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.UI.Font.Load
import Paths_load_font
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

drawFonts :: Font -> Ref Box -> IO ()
drawFonts moonPhasesFont b = do
  label <- getLabel b
  bounds@(Rectangle (Position (X x') (Y y')) _) <- getRectangle b
  flcPushClip bounds
  flcSetColor blackColor
  flcSetFont helvetica (FontSize 18)
  flcDraw "It says \"Hello World!\" in Moon Phases:" (Position (X (x'+25)) (Y (y'+40)))
  flcSetFont moonPhasesFont (FontSize 30)
  flcDraw "Hello world!" (Position (X (x'+25)) (Y (y'+90)))
  flcPopClip

main :: IO ()
main = do
  fontPath <- getDataFileName "test-fonts/moon_phases.ttf"
  loadFont fontPath
  numFaces <- FL.setFonts Nothing
  let fonts = map Font [0 .. numFaces - 1]
  withFaces <- mapM
                 (\f -> do
                   (face,_) <- FL.getFontName f
                   return (face,f)
                 )
                 fonts
  case (lookup "Moon Phases" withFaces) of
    Nothing -> flMessage "The Moon Phases test font was not loaded!"
    Just f -> do
      win <- windowNew
               (Size (Width 400) (Height 120))
               Nothing
               (Just "Font Load Demo")
      setColor win whiteColor
      begin win
      b <- boxCustom
             (toRectangle (0,0,400,120))
             Nothing
             (Just (drawFonts f))
             Nothing
      setBox b FlatBox
      end win
      showWidget win
      _ <- FL.run
      return ()
