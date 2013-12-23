import System.Posix.Signals

main :: IO ()
main = do
    pid <- readFile "/run/nginx.pid"
    signalProcess sigHUP (read pid)
