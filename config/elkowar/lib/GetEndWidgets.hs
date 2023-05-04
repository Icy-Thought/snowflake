{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

import Network.HostName (getHostName)
import Text.RawString.QQ (r)

main :: IO ()
main = do
  hostname <- getHostName
  let lastWidget = case hostname of
        "desktop" -> ")"
        _ -> "(bat))"
  putStrLn $
    [r|
(box
    :halign "end"
    :space-evenly false
    :spacing "4"
    (rec)
    (net)
    (mem)
    (cpu)
|]
      ++ lastWidget
