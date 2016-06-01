{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Sequence hiding (zip, length)
import Data.Maybe
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import System.Random
import UI.NCurses

data Direction = DUp | DDown | DLeft | DRight
type Position = (Integer, Integer)

data SnakeState = SnakeState
    { _snake     :: Seq Position
    , _rabbit    :: Position
    , _grow      :: Int
    , _direction :: Direction
    , _redID     :: Maybe ColorID
    }

makeLenses ''SnakeState

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

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (pure ()) f m

isElementOf :: Eq a => a -> Seq a -> Bool
isElementOf e s = isJust $ elemIndexL e s

newRabbitPosition :: Seq Position -> Curses Position
newRabbitPosition s = do
    w <- defaultWindow
    (r, c) <- updateWindow w $ windowSize
    rr <- liftIO $ randomRIO (0, r - 1)
    rc <- liftIO $ randomRIO (0, c - 1)
    if (rr, rc) `isElementOf` s then newRabbitPosition s else return (rr, rc)

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
        whenJust cid $ \ x -> setAttribute (AttributeColor x) True
        drawString s
        whenJust cid $ \ x -> setAttribute (AttributeColor x) False
    render
    void $ getEvent w Nothing
    closeWindow wnew
    render

dropLast :: Seq a -> Seq a
dropLast s =
    case viewr s of
        ss :> _ -> ss
        EmptyR -> error "dropLast: empty sequence"

isBorder :: Position -> Curses Bool
isBorder (r, c) = do
    w <- defaultWindow
    (wr, wc) <- updateWindow w $ windowSize
    return $ r < 0 || c < 0 || r >= wr || c >= wc

snakeRun :: StateT SnakeState (MaybeT Curses) ()
snakeRun = do

    w <- lift $ lift defaultWindow

    ev <- lift $ lift $ getEvent w (Just 100)

    case ev of
        Just (EventCharacter 'q') -> lift mzero
        Just (EventCharacter 'Q') -> lift mzero
        _ -> return ()

    d <- case ev of
        Just (EventSpecialKey k) -> direction <%= changeDirection k
        _ -> use direction

    oldHead :< _ <- liftM viewl $ use snake
    let newHead = move d oldHead

    s <- use snake
    red <- use redID
    when (newHead `isElementOf` s) $ do
        lift $ lift $ oops red "Don't eat yourself"
        lift mzero

    border <- lift $ lift $ isBorder newHead
    when border $ do
        lift $ lift $ oops red "Don't eat the borders"
        lift mzero

    snake %= (newHead <|)

    oldRabbit <- use rabbit
    when (newHead == oldRabbit) $ do
        nr' <- lift $ lift $ newRabbitPosition s
        lift $ lift $ updateWindow w $ showRabbit nr'
        rabbit .= nr'
        grow += 3

    _ :> l <- liftM viewr $ use snake
    g <- use grow

    lift $ lift $ updateWindow w $ do
        when (g == 0) $ drawCharX ' ' l
        drawCharX snakeBodyChar oldHead
        drawCharX (directionToChar d) newHead

    if g == 0
        then snake %= dropLast
        else grow -= 1

    lift $ lift render

    snakeRun

initialSnake :: Seq Position
initialSnake = fromList $ zip (repeat 0) [5, 4 .. 0]

initialDirection :: Direction
initialDirection = DRight

snakeBodyChar :: Char
snakeBodyChar = '*'

main :: IO ()
main = runCurses $ do
    redColorId <- newColorIDX ColorRed ColorDefault 1
    void $ setCursorMode CursorInvisible
    w <- defaultWindow
    initialRabbit <- newRabbitPosition initialSnake
    updateWindow w $ do
        showRabbit initialRabbit
        mapM_ (drawCharX snakeBodyChar) initialSnake
        let _ :> l = viewr initialSnake
        drawCharX (directionToChar initialDirection) l
    render
    void $ runMaybeT $ evalStateT snakeRun $
        SnakeState {
            _snake     = initialSnake,
            _rabbit    = initialRabbit,
            _grow      = 0,
            _direction = initialDirection,
            _redID     = redColorId
        }
