# prepare data for visualization display
# Robert Kubinec
# Fri Apr 3

require(readr)
require(dplyr)
require(tidyr)


data_viz <- read_csv("data/data_viz.csv")

# add in fake heat map index

data_viz$severity_index <- rnorm(nrow(data_viz))


write_csv(data_viz,"data/data_viz_processed.csv")
