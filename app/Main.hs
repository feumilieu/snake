{-# LANGUAGE TemplateHaskell #-}

-- TODO: error when the snake eats himself
-- TODO: meaningful messages when the snake eats border
-- TODO: use state monad
-- TODO: accelerate key

module Main where

import Data.Sequence hiding (zip, length)
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import UI.NCurses

data Direction = DUp | DDown | DLeft | DRight
type Position = (Integer, Integer)

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

move :: Direction -> Position -> Position
move DUp    (r, c) = (r - 1, c    )
move DDown  (r, c) = (r + 1, c    )
move DLeft  (r, c) = (r,     c - 1)
move DRight (r, c) = (r,     c + 1)

directionToChar :: Direction -> Char
directionToChar DUp    = '^'
directionToChar DDown  = 'v'
directionToChar DLeft  = '<'
directionToChar DRight = '>'

drawCharX :: Char -> Position -> Update ()
drawCharX ch (r, c) = do
    moveCursor r c
    drawGlyph $ Glyph ch []

newColorIDX :: Color -> Color -> Integer -> Curses (Maybe ColorID)
newColorIDX fg bg cid = do
    colorOk <- supportsColor
    if colorOk
        then liftM Just $ newColorID fg bg cid
        else return Nothing

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe m f = maybe (pure ()) f m

newRabbitPosition :: Curses Position
newRabbitPosition = do
    w <- defaultWindow
    (r, c) <- updateWindow w $ windowSize
    rr <- liftIO $ randomRIO (0, r)
    rc <- liftIO $ randomRIO (0, c)
    return (rr, rc)

showRabbit :: Position -> Update ()
showRabbit = drawCharX '@'

oops :: Maybe ColorID -> String -> Curses ()
oops cid s = do
    w <- defaultWindow
    (r, c) <- updateWindow w $ windowSize
    let
        high = 3
        width = 2 + (toInteger $ length s)
        rnew = quot r 2 - 1
        cnew = quot c 2 - quot width 2
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

firstBodyTail :: Seq a -> (a, Seq a, a)
firstBodyTail s =
    case viewl s of
        EmptyL -> error "firstBodyTail: the length of the sequence is less than 1"
        f :< tt ->
            case viewr tt of
                EmptyR -> error "firstBodyTail: the length of the sequence is less than 2"
                b :> t -> (f, b, t)

snakeRun :: Seq Position -> Position -> Int -> Direction -> Curses ()
snakeRun snake rabbit grow direction = do

    w <- defaultWindow
    let (sfirst, sbody, slast) = firstBodyTail snake

    updateWindow w $ drawCharX (directionToChar direction) sfirst
    render

    ev <- getEvent w (Just 100)
    case ev of
        Just (EventCharacter 'q') -> return ()
        Just (EventCharacter 'Q') -> return ()
        _ -> do

            let
                newDirection = case ev of
                    Just (EventSpecialKey k)  -> changeDirection k direction
                    _ -> direction
                newSFirst = move newDirection sfirst

            (newRabbit, newGrow) <- if newSFirst == rabbit
                then do
                    nr' <- newRabbitPosition
                    updateWindow w $ showRabbit nr'
                    return (nr', grow + 3)
                else
                    return (rabbit, if grow == 0 then 0 else grow - 1)

            updateWindow w $ do
                when (grow == 0) $ drawCharX ' ' slast
                drawCharX snakeBodyChar sfirst

            snakeRun
                (newSFirst <| sfirst <| (if (grow == 0) then sbody else (sbody |> slast)))
                newRabbit newGrow newDirection

initialSnake :: Seq Position
initialSnake = fromList $ zip (repeat 0) [5, 4 .. 0]

snakeBodyChar :: Char
snakeBodyChar = '*'

main :: IO ()
main = runCurses $ do
    redColorId <- newColorIDX ColorRed ColorDefault 1
    rabbit <- newRabbitPosition
    w <- defaultWindow
    ex <- tryCurses $ do
        updateWindow w $ do
            showRabbit rabbit
            mapM_ (drawCharX snakeBodyChar) initialSnake
        void $ setCursorMode CursorInvisible
        snakeRun initialSnake rabbit 0 DRight
    case ex of
        Right () -> return ()
        Left e -> oops redColorId $ show e
