module Lib (
  deinitEditor,
  runEditor,
  initEditor,
) where

import Control.Exception (bracket_)
import Control.Monad (filterM, guard, replicateM_, unless, void, when)
import Control.Monad.State (StateT, gets, liftIO, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Bits ((.&.))
import Data.Char (chr, isDigit, ord)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybe)
import Data.Word (Word8)
import Foreign.C.Types (CSize (CSize))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import System.Console.Terminal.Size (Window (Window), fdSize, height, width)
import System.Posix.IO (fdReadBuf, fdWrite, stdInput, stdOutput)
import System.Posix.Terminal (
  TerminalAttributes,
  TerminalMode (
    CheckParity,
    EnableEcho,
    ExtendedFunctions,
    InterruptOnBreak,
    KeyboardInterrupts,
    MapCRtoLF,
    ProcessInput,
    ProcessOutput,
    StartStopOutput,
    StripHighBit
  ),
  TerminalState (WhenFlushed),
  getTerminalAttributes,
  setTerminalAttributes,
  withMinInput,
  withTime,
  withoutMode,
 )
import System.Posix.Types (Fd (..))


data Pos = Pos {x :: Int, y :: Int}


data EditorState = EditorState
  { origTermAttrs :: TerminalAttributes
  , window :: Window Int
  , cursorPos :: Pos
  }


type EditorM = StateT EditorState IO


version = "0.0.1"


initEditor :: IO EditorState
initEditor = do
  tState <- enterRawMode
  window <- getWindow
  return $
    EditorState
      { origTermAttrs = tState
      , window = window
      , cursorPos = Pos 0 0
      }


getWindow :: IO (Window Int)
getWindow = do
  win <- fdSize stdOutput
  maybe theHardWay return win
 where
  theHardWay = do
    _write "\x1b[999C\x1b[999B"
    _write "\x1b[6n"
    EscapeSeq bs <- readKey
    let (h, _ : w) = break (== ';') $ drop 2 (init bs)
    return $ Window (read h :: Int) (read w :: Int)


enterRawMode :: IO TerminalAttributes
enterRawMode = do
  attrs <- getTerminalAttributes stdInput
  let attrs' =
        foldl
          withoutMode
          (withMinInput (withTime attrs 1) 0)
          [ EnableEcho
          , ProcessInput
          , KeyboardInterrupts
          , StartStopOutput
          , ExtendedFunctions
          , MapCRtoLF
          , ProcessOutput
          ]
  setTerminalAttributes stdInput attrs' WhenFlushed
  return attrs


restoreTerminal :: EditorM ()
restoreTerminal = do
  origAttrs <- gets origTermAttrs
  liftIO $ setTerminalAttributes stdInput origAttrs WhenFlushed


_write = void . fdWrite stdOutput
write = liftIO . _write


data SpecialKey
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | PageUp
  | PageDown
  | Home
  | End
  | Delete
  deriving (Show, Eq)


data PressedKey
  = EscapeSeq String
  | SpecialKey SpecialKey
  | NormalKey Char
  deriving (Show, Eq)


readKey :: IO PressedKey
readKey = allocaBytes 1 _readKey
 where
  _readFallible :: Ptr Word8 -> MaybeT IO Char
  _readFallible mem = do
    lift $ poke mem 0
    nRead <- lift $ fdReadBuf stdInput mem $ CSize 1
    guard $ nRead == 1
    lift $ peek mem <&> chr . fromIntegral

  _readKey mem = do
    c <- runMaybeT $ _readFallible mem
    case c of
      Just '\x1b' -> _maybeEscapeSeq
      Just c' -> return $ NormalKey c'
      Nothing -> _readKey mem

  _maybeEscapeSeq = do
    maybeSeq <- allocaBytes 1 $ \mem -> runMaybeT $ do
      c0 <- _readFallible mem
      guard $ c0 `elem` ['[', 'O']
      c <- _readFallible mem
      if c0 == '['
        then case c of
          'A' -> return $ SpecialKey ArrowUp
          'B' -> return $ SpecialKey ArrowDown
          'C' -> return $ SpecialKey ArrowRight
          'D' -> return $ SpecialKey ArrowLeft
          'H' -> return $ SpecialKey Home
          'F' -> return $ SpecialKey End
          _ | isDigit c -> do
            '~' <- _readFallible mem
            return . SpecialKey $ case c of
              '1' -> Home
              '3' -> Delete
              '4' -> End
              '5' -> PageUp
              '6' -> PageDown
              '7' -> Home
              '8' -> End
          _ -> return $ NormalKey '\x1b'
        else case c of
          'H' -> return $ SpecialKey Home
          'F' -> return $ SpecialKey End
          _ -> return $ NormalKey '\x1b'
    return $ fromMaybe (NormalKey '\x1b') maybeSeq


processKeyPress :: PressedKey -> EditorM Bool
processKeyPress c =
  case c of
    NormalKey c' -> return $ ord c' /= ctrlKey 'q'
    SpecialKey k -> do
      Window h w <- gets window
      case k of
        ArrowUp -> modifyCursorPos $ \p -> p {y = max 0 $ y p - 1}
        ArrowLeft -> modifyCursorPos $ \p -> p {x = max 0 $ x p - 1}
        ArrowDown -> modifyCursorPos $ \p -> p {y = min (h - 1) $ y p + 1}
        ArrowRight -> modifyCursorPos $ \p -> p {x = min (w - 1) $ x p + 1}
        Home -> modifyCursorPos $ \p -> p {x = 0}
        End -> modifyCursorPos $ \p -> p {x = w - 1}
        _
          | k == PageDown || k == PageUp ->
            replicateM_ h . processKeyPress . SpecialKey $
              if k == PageDown then ArrowDown else ArrowUp
      return True
    _ -> return True
 where
  ctrlKey c = ord c .&. 0x1f

  modifyCursorPos :: (Pos -> Pos) -> EditorM ()
  modifyCursorPos f = modify $ \s0 -> s0 {cursorPos = f $ cursorPos s0}


moveCursorToTop = tell "\x1b[H"


type BatchedWriteM = WriterT String EditorM ()


drawScreen :: BatchedWriteM
drawScreen = do
  hideCursor
  moveCursorToTop
  drawRows
  positionCursor
  showCursor
 where
  hideCursor = tell "\x1b[?25l"
  showCursor = tell "\x1b[?25h"


batchedWrite :: BatchedWriteM -> EditorM ()
batchedWrite m = do
  ((), msg) <- runWriterT m
  write msg


positionCursor :: BatchedWriteM
positionCursor = do
  cpos <- gets cursorPos
  let posMsg = show (y cpos + 1) ++ ";" ++ show (x cpos + 1)
      msg = "\x1b[" ++ posMsg ++ "H"
  tell msg


drawRows :: BatchedWriteM
drawRows = do
  window <- gets window
  let buf =
        intercalate "\r\n" $
          [line window y ++ "\x1b[K" | y <- [0 .. height window - 1]]
  tell buf
 where
  line window y
    | y == (height window `div` 3) =
      let msg = "hscto editor -- version " ++ version
          paddingLen = (width window - length msg) `div` 2
       in if paddingLen <= 0
            then take (width window) msg
            else "~" ++ replicate (paddingLen - 1) ' ' ++ msg
    | otherwise = "~"


runEditor :: EditorM ()
runEditor = do
  batchedWrite drawScreen
  result <- liftIO readKey >>= processKeyPress
  when result runEditor


deinitEditor :: EditorM ()
deinitEditor = do
  restoreTerminal
  batchedWrite moveCursorToTop
