# dev/dependencies.R

# Load devtools for development tools
library(devtools)

# Load testing packages
library(testthat)
library(roxygen2)

# Load tidyverse
library(dplyr)
library(tidyr)
library(haven)
library(tidyselect)
library(rlang)

library(labelled)
library(fastDummies)

library(INLA)

# Load your own package (reloads it from source)
devtools::document()
devtools::load_all()
