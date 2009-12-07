import Text.Printf

import Bustle.Noninteractive (run)
import Bustle.Stats (frequencies, TallyType)

format :: (Int, (TallyType, String)) -> String
format (c, (t, s)) = printf " %4d %6s %s" c (show t) s

main :: IO ()
main = run "bustle-count" frequencies format
