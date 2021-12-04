module Main where

import Graphics.UI.Threepenny

main :: IO ()
main = do
  startGUI defaultConfig showMessage

showMessage :: Window -> UI ()
showMessage window = do
  getBody window #+ [string "Hello, world!"]
  return ()
