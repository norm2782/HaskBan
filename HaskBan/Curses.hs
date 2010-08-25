module HaskBan.Curses where
  
  import UI.HSCurses.Curses

  setupHaskBanGUI :: IO Window
  setupHaskBanGUI = do
    initCurses
    window <- initScr 
    echo False
    keypad window True
    mvWAddStr window 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
    return window
    
