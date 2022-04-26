# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Script to compute total duration in hours from a set of time windows.

# Resources
library(lubridate)

# Input is a list of times in the format shown below
input_times <- "9:37-11:26, 1:10-2:49, 3:33-4:37"

# Sum up the input times
tibble(windows = str_split(input_times, ",")[[1]]) %>%
    mutate(windows = str_squish(windows)) %>%
    separate(windows, into = c("start", "stop"), sep = "-", remove = FALSE) %>%
    mutate(across(c("start", "stop"), hm),
           duration = stop - start,
           time = hour(duration) + (minute(duration) / 60)) %>%
    pull(time) %>%
    sum()
