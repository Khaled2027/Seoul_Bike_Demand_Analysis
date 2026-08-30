getwd()

install.packages("tidyverse")
install.packages("psych")

library(readr)
library(lubridate)
library(psych)
library(dplyr)
library(ggplot2)

list.files()

df <- read_csv("SeoulBikeData.csv",
               locale= locale(encoding = "Windows-1252"))
head(df)
View(df)
str(df)
