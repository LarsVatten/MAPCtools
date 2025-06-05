library(haven)
library(labelled)
library(dplyr)

# Uncleaned NHIS data:
NHIS_data <- read_dta("dev/testing/Data.dta")
message('Attached uncleaned NHIS data as "NHIS_data".')
NHIS_data <- remove_labels(NHIS_data)
NHIS_data <- NHIS_data %>% subset(age >= 25)
NHIS_data <- NHIS_data %>% rename(period=year)


# Cleaned NHIS data:
load("dev/testing/data.RData")
message('Attached cleaned NHIS data as "data".')

# Cleaned female data:
load("dev/testing/female_data.RData")
message('Attached cleaned female data as "female_data".')

# Cleaned male data:
load("dev/testing/male_data.RData")
message('Attached cleaned male data as "male_data".')


