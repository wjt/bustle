import Text.Printf

import Bustle.Noninteractive (run)
import Bustle.Stats (methodTimes, TimeInfo(..))

format :: TimeInfo -> String
format (TimeInfo interface method total ncalls mean) =
    printf " %9.4f %3d %9.4f %s.%s" total ncalls mean interface method

main :: IO ()
main = run "bustle-time" methodTimes format
