{-# LANGUAGE TemplateHaskell #-}

-- TODO: generation of rabbits
-- TODO: eating rabbits and growing
-- TODO: different speed when running vert vs horiz (?)

module Main where

import UI.NCurses
import Control.Monad

data Direction = DUp | DDown | DLeft | DRight

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

newColorIDX :: Color -> Color -> Integer -> Curses (Maybe ColorID)
newColorIDX fg bg cid = do
    colorOk <- supportsColor
    if colorOk
        then liftM Just $ newColorID fg bg cid
        else return Nothing

whenMaybe :: Applicative m => Maybe a -> (a -> m()) -> m ()
whenMaybe m f = maybe (pure ()) f m

oops :: Maybe ColorID -> String -> Curses ()
oops cid s = do
    w <- defaultWindow
    (r, c) <- updateWindow w $ windowSize
    let
        high = 3
        width = 2 + (toInteger $ length s)
        rnew = quot r 2 - 1
        cnew = quot c 2 - quot width 2
        in do
            wnew <- newWindow high width rnew cnew
            updateWindow wnew $ do
                moveCursor 1 1
                whenMaybe cid $ \ x -> setAttribute (AttributeColor x) True
                drawString s
                whenMaybe cid $ \ x -> setAttribute (AttributeColor x) False
            render
            void $ getEvent w Nothing
            closeWindow wnew
            render

snakeRun :: (Integer, Integer) -> Int -> Direction -> Curses ()
snakeRun position _ d = do

    w <- defaultWindow

    updateWindow w $ drawCharX position $ directionToChar d
    render

    ev <- getEvent w (Just 100)
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

                    snakeRun newPosition 0 newDirection

main :: IO ()
main = runCurses $ do
    redColorId <- newColorIDX ColorRed ColorDefault 1
    ex <- tryCurses $ do
        void $ setCursorMode CursorInvisible
        snakeRun (0, 0) 0 DRight
    case ex of
        Right () -> return ()
        Left e -> oops redColorId $ show e
