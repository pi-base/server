import Protolude (IO)

import qualified Server (start)

main :: IO ()
main = Server.defaultSettings >>= Server.start
