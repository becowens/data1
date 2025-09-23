library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)

finaldf <- df_cleaned

cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, is.numeric)]
