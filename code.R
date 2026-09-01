getwd()

install.packages("tidyverse")
install.packages("psych")

library(readr)
library(lubridate)
library(psych)
library(dplyr)
library(ggplot2)


df <- read_csv("SeoulBikeData.csv",
               locale= locale(encoding = "Windows-1252"))

head(df)
View(df)
str(df)
# Returns the number of null values
sum(is.na(df))
# Returns the number of duplicate rows
sum(duplicated(df))

# Converts the datatype of 'Rented Bike Count' column into numeric type
df <-df %>%
  mutate(`Rented Bike Count` = as.numeric(`Rented Bike Count`))

# Returns a dataframe with Seasons grouped and 
# aggregated by the mean bike demand per season
df_groupby_season <- df %>%
  group_by(Seasons) %>%
  summarise(avgDemand=sum(`Rented Bike Count`)/length(Date))
df_groupby_sesson

# Displays a histogram based on 'Rented Bike Count' column
hist(df$`Rented Bike Count`,main=paste("Histogram of",
      "Rented Bike Count"),xlab='Number of rented bikes',col="lightblue") 


