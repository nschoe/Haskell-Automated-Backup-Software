import Control.Concurrent

main = do
  a <- forkIO (write 'a')
  threadDelay 5000000
  killThread a

write c = putChar c >> write c