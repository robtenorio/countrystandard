library(devtools)
master_names <- read.csv("master_names_regex.csv", header=TRUE, stringsAsFactors=FALSE)
use_data(master_names, pkg = "countrystandard", internal = TRUE, overwrite=TRUE)
