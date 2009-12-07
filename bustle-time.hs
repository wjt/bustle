import Text.Printf

import Bustle.Noninteractive (run)
import Bustle.Stats (methodTimes)

format :: (String, Double, Int, Double) -> String
format (method, total, ncalls, mean) =
    printf " %9.4f %3d %9.4f %s" total ncalls mean method

main :: IO ()
main = run "bustle-time" methodTimes format
