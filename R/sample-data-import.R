# For importing sample data sets
# Not intended for reproducibility
library(data.table)
library(tidyverse)
fm.201860.100 <- fread(file.path('~', 'Data', 'ad hoc', 'fm 100_201860.csv')) %>%
    as_tibble()

fm_nest <- fm.201860.100 %>%
    group_by(subject) %>%
    nest()
