{-# LANGUAGE TemplateHaskell #-}

module Main where

import UI.NCurses
import Data.Sequence

import Control.Lens.TH

data Direction = Up | Down | Left | Right

data SnakeState = SnakeState {
    _snake :: Seq (Int, Int),
    _grow :: Int,
    _direction :: Direction
}

makeLenses ''SnakeState

--  :: SnakeState -> 

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

