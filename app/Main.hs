{-# LANGUAGE TemplateHaskell #-}

module Main where

import UI.NCurses
-- import Data.Sequence

import Control.Lens.TH

data Direction = DUp | DDown | DLeft | DRight

data SnakeState = SnakeState {
    -- _snake :: Seq (Int, Int),
    _snake :: (Integer, Integer),
    _grow :: Int,
    _direction :: Direction
}

makeLenses ''SnakeState

initialSnakeState :: SnakeState
initialSnakeState = SnakeState { _snake = (0, 0), _grow = 0, _direction = DRight }

changeDirection :: Key -> Direction -> Direction
changeDirection KeyLeftArrow   DUp    = DLeft
changeDirection KeyLeftArrow   DDown  = DRight
changeDirection KeyLeftArrow   DLeft  = DDown
changeDirection KeyLeftArrow   DRight = DUp
changeDirection KeyRightArrow  DUp    = DRight
changeDirection KeyRightArrow  DDown  = DLeft
changeDirection KeyRightArrow  DLeft  = DUp
changeDirection KeyRightArrow  DRight = DDown
changeDirection _              d      = d

move :: Direction -> (Integer, Integer) -> (Integer, Integer)
move DUp    (r, c) = (r - 1, c    )
move DDown  (r, c) = (r + 1, c    )
move DLeft  (r, c) = (r,     c - 1)
move DRight (r, c) = (r,     c + 1)

directionToChar :: Direction -> Char
directionToChar DUp    = '^'
directionToChar DDown  = 'v'
directionToChar DLeft  = '<'
directionToChar DRight = '>'

drawCharX :: (Integer, Integer) -> Char -> Update ()
drawCharX (r, c) ch = do
    moveCursor r c
    drawGlyph $ Glyph ch []

main :: IO ()
main = runCurses $ snakeRun initialSnakeState

snakeRun :: SnakeState -> Curses ()
snakeRun (SnakeState {_snake = position, _direction = d }) = do

    w <- defaultWindow

    updateWindow w $ drawCharX position $ directionToChar d
    render

    ev <- getEvent w (Just 500)
    case ev of
        Just (EventCharacter 'q') -> return ()
        Just (EventCharacter 'Q') -> return ()
        _ ->
            let
                newDirection = case ev of
                    Just (EventSpecialKey k)  -> changeDirection k d
                    _ -> d
                newPosition = move newDirection position
            in do

                updateWindow w $ drawCharX position ' '
                render

                snakeRun $ SnakeState { _snake = newPosition, _grow = 0, _direction = newDirection }
