# Process raw data into processed data

rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)


d1 <- readxl::read_excel("data/raw/Example questionnaire.xlsx",
                         skip = 5)

#--process into tidy data
d2 <-
  d1 |> 
  tidyr::fill(title, pesticide_load) |>
  dplyr::mutate(pesticide_load = as.numeric(pesticide_load),
                weight = as.numeric(weight)) |>
  dplyr::select(-notes, -pesticide_load, rating_numeric = rating_1to5)

data_example <- d2

data_example |> 
  saveRDS("data/processed/data_example.RDS")
