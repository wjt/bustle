import Text.Printf

import Bustle.Noninteractive (run)
import Bustle.Stats (frequencies, TallyType, FrequencyInfo(..))

format :: FrequencyInfo -> String
format (FrequencyInfo c t s) = printf " %4d %6s %s" c (show t) s

main :: IO ()
main = run "bustle-count" frequencies format
