module HaskBan.Curses where
  
  import UI.HSCurses.Curses
  import Data.List (intercalate)

  setupHaskBanGUI :: IO Window
  setupHaskBanGUI = do
    initCurses
    window <- initScr 
    echo False
    keypad window True
    mvWAddStr window 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
    return window

  youWonScreen = ["  ___                                           _  __   __            _    _               _ ",
                  " / _ \\                                         | | \\ \\ / /           | |  | |             | |",
                  "/ /_\\ \\__      _____ ___  ___  _ __ ___   ___  | |  \\ V /___  _   _  | |  | | ___  _ __   | |",
                  "|  _  |\\ \\ /\\ / / _ | __|/ _ \\| '_ ` _ \\ / _ \\ | |   \\ // _ \\| | | | | |/\\| |/ _ \\| '_ \\  | |",
                  "| | | | \\ V  V /  __|__ \\ (_) | | | | | |  __/ |_|   | | (_) | |_| | \\  /\\  / (_) | | | | |_|",
                  "\\_| |_/  \\_/\\_/ \\___|___/\\___/|_| |_| |_|\\___| (_)   \\_/\\___/ \\__,_|  \\/  \\/ \\___/|_| |_| (_)"]
    
  printYouWonScreen :: Window -> IO ()
  printYouWonScreen window = mvWAddStr window 0 0 $ intercalate "\n" youWonScreen
    
    
