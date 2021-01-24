module Lib (
  deinitEditor,
  runEditor,
  initEditor,
) where

import Control.Exception (bracket_)
import Control.Monad (filterM, replicateM_, unless, void, when)
import Control.Monad.State (StateT, gets, liftIO, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Bits ((.&.))
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Maybe (maybe)
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
    bs <- readUntil $ ord 'R'
    let (h, _ : w) = break (== ';') $ chr <$> drop 2 bs
    return $ Window (read h :: Int) (read w :: Int)

  readUntil b = _readUntil [] b
  _readUntil acc b =
    readKey >>= \b' ->
      if b == b'
        then return $ reverse acc
        else _readUntil (b' : acc) b


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


readKey :: IO Int
readKey = liftIO $ allocaBytes 1 _readKey
 where
  _readKey mem = do
    poke mem 0
    nRead <- fdReadBuf stdInput mem $ CSize 1
    if nRead == 1
      then fromIntegral <$> peek mem
      else _readKey mem


processKeyPress :: EditorM Bool
processKeyPress =
  liftIO readKey >>= \c ->
    if c == ctrlKey 'q'
      then return False
      else do
        Window h w <- gets window
        case chr c of
          'w' -> modifyCursorPos $ \p -> p {y = max 0 $ y p - 1}
          'a' -> modifyCursorPos $ \p -> p {x = max 0 $ x p - 1}
          's' -> modifyCursorPos $ \p -> p {y = min (h - 1) $ y p + 1}
          'd' -> modifyCursorPos $ \p -> p {x = min (w - 1) $ x p + 1}
          _ -> return ()
        return True
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


batchedWrite :: WriterT String EditorM () -> EditorM ()
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
  result <- processKeyPress
  when result runEditor


deinitEditor :: EditorM ()
deinitEditor = do
  restoreTerminal
  batchedWrite moveCursorToTop
