import Text.Printf

import Bustle.Noninteractive (run, formatInterface)
import Bustle.Stats (frequencies, TallyType, FrequencyInfo(..))
import Data.Maybe (maybe)

format :: FrequencyInfo -> String
format (FrequencyInfo c t i m) =
    printf " %4d %6s %s.%s" c (show t) (formatInterface i) m

main :: IO ()
main = run "bustle-count" frequencies format
