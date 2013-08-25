module Main where

import Graphics.UI.Gtk


main = do
  initGUI
  b <- builderNew
  builderAddFromFile b "ui.glade"
  bt1 <- builderGetObject b castToButton "button1"
  on bt1 buttonActivated $ putStrLn "Hello button"
  w <- builderGetObject b castToWindow "window1"
  widgetShowAll w
  mainGUI
  
  
