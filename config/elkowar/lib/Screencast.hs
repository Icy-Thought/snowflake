{-# OPTIONS_GHC -Wall #-}

import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    SocketType (Stream),
    connect,
    defaultProtocol,
    socket,
    socketToHandle,
  )
import System.Environment (getEnv)
import System.IO (Handle, IOMode (ReadMode), hGetLine)
import Text.Parsec (char, digit, eof, many1, parse, string)
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  hisEnv <- getEnv "HYPRLAND_INSTANCE_SIGNATURE"
  let sockPath = "/tmp/hypr/" ++ hisEnv ++ "/.socket2.sock"
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix sockPath)
  handle <- socketToHandle sock ReadMode
  putStrLn "no-rec"
  loop handle

loop :: Handle -> IO ()
loop handle = do
  line <- hGetLine handle
  case parse screencastParser "" line of
    Left _ -> pure ()
    Right int
      | int == 1 -> putStrLn "rec"
      | otherwise -> putStrLn "no-rec"
  loop handle

screencastParser :: Parser Int
screencastParser = do
  _ <- string "screencast>>"
  x <- many1 digit
  _ <- char ','
  _ <- many1 digit <* eof
  return (read x)