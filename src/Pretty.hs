module Pretty where

import           Text.PrettyPrint

banner :: String
banner = render $
  text ("\n" ++ ascii ++
  "\n\n Krill 0.1.0\n")
  where
    ascii = " |  ._ o | |\n |< |  | | | "
