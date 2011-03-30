import Text.Printf

import Bustle.Noninteractive (run)
import Bustle.Stats (frequencies, TallyType, FrequencyInfo(..))

format :: FrequencyInfo -> String
format (FrequencyInfo c t i m) = printf " %4d %6s %s.%s" c (show t) i m

main :: IO ()
main = run "bustle-count" frequencies format
