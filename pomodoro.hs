import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO

data TimerState = Work | Break
  deriving (Eq)

instance Show TimerState where
  show Work = "Work"
  show Break = "Break"

startTimer :: Int -> Int -> IO ()
startTimer workTimeMinutes breakTimeMinutes = do
  hSetBuffering stdout NoBuffering
  clearScreen
  let workTime = workTimeMinutes * 60  -- Convert minutes to seconds
      breakTime = breakTimeMinutes * 60  -- Convert minutes to seconds
  setCursorPosition 1 0
  printTime workTime Work Red
  setCursorPosition 0 0
  putStrLn ("My pomodoro timer :V")
  threadDelay 2000000
  loop Work workTime breakTime workTime

  where
    loop state remaining breakTime workTime = do
      let (time, nextState) =
            if remaining <= 0
              then case state of
                     Work  -> (breakTime, Break)
                     Break -> (workTime, Work)
              else (remaining - 1, state)

      printTime time nextState (color nextState)

      -- Delay for 1 second (1 million microseconds)
      threadDelay 1000000

      loop nextState time breakTime workTime

    printTime :: Int -> TimerState -> Color -> IO ()
    printTime time state color = do
      clearScreen
      setCursorPosition 1 0
      setSGR [SetColor Foreground Vivid color]
      putStrLn (show state ++ " - " ++ show (time `div` 60) ++ "m " ++ show (time `mod` 60) ++ "s")

    color :: TimerState -> Color
    color Work  = Green
    color Break = Blue

main :: IO ()
main = startTimer 1 1

