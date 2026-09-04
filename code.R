getwd()

install.packages("tidyverse")
install.packages("psych")
install.packages("corrplot")

library(readr)
library(lubridate)
library(psych)
library(dplyr)
library(corrplot)
library(ggplot2)

#--------------Data loading & Exploration--------------
df <- read_csv("SeoulBikeData.csv",
               locale= locale(encoding = "Windows-1252"))
dim(df)
names(df)

head(df)
View(df)

str(df)
# Returns the number of null values
sum(is.na(df))
# Returns the number of duplicate rows
sum(duplicated(df))

# Returns the total number of values present in the 'Functioning Day' column
length_of_functioning_day <- length(df$`Functioning Day`)
# Returns the percentages of each unique value from 'Functioning Day'
percentage <- (table(df$`Functioning Day`)/length_of_functioning_day)*100
# Rounds to 2 decimal places
percentage <- round(percentage,2)
percentage

df_groupby_functioning_day <- df %>%
  select(`Rented Bike Count`,`Functioning Day`) %>%
  filter(`Functioning Day`=='No')
df_groupby_functioning_day
# -------------Data Preprocessing -------------

# Keeps rows where`Functioning Day' column is equal to 'Yes'
df <- df %>%
  filter(`Functioning Day`=='Yes')
# Drops the `Dew point temperature(°C)` column and `Functioning Day` column 
# from the dataframe
df <- df %>%
  select(!c(`Dew point temperature(°C)`,`Functioning Day`))
# Returns a Satistical summary 
describe(df)

# Switches the column data format into data format
df <-df %>%
  mutate(Date = as.Date(Date,format='%d/%m/%Y'))

# Returns a new column where days are numeric (Monday = 1, Sunday = 7)
df <-df %>%
  mutate(dayofweek = wday(Date,label = TRUE))

# Creates a new column called 'is_weekday' that contain two values: 1 (True) and 0 (False)
df$is_weekday <- ifelse(df$dayofweek=='Fri'|df$dayofweek=='Mon'|df$dayofweek=='Tue'|df$dayofweek=='Wed'|df$dayofweek=='Thu',1,0)
# Creates a new column called 'is_weekend' that contain two values: 1 (True) and 0 (False)
df$is_weekend <- ifelse(df$dayofweek=='Sat'|df$dayofweek=='Sun',1,0)

# Creates a new column called 'is_holiday' for encoding and it contains two values: 
# 1 (True) and 0 (False)
df$is_holiday <-ifelse(df$Holiday=='Holiday',1,0)

# feuture engineers 3 new columns
df$is_winter <- ifelse(df$Seasons=='Winter',1,0)
df$is_spring <- ifelse(df$Seasons=='Spring',1,0)
df$is_summer <- ifelse(df$Seasons=='Summer',1,0)


# Converts the datatype of 'Rented Bike Count' column into numeric type
df <-df %>%
  mutate(`Rented Bike Count` = as.numeric(`Rented Bike Count`))
# Drops the Date, Holiday, and dayofweek columns 
df <- df %>%
  select(!c(Date,Holiday,dayofweek))

df_numerical_columns <- df %>%
  select(!c(Seasons))
# Returns a Satistical summary 
describe(df_numerical_columns)

df_columns_for_corr <- cor(df_numerical_columns)

# Returns a dataframe with Seasons grouped and 
# aggregated by the mean bike demand per season
df_groupby_season <- df %>%
  group_by(Seasons) %>%
  summarise(avgSeasonDemand=round(sum(`Rented Bike Count`)/length(Date)))

avg_Demand<- df %>%
  summarise(avgDemand=round(sum(`Rented Bike Count`)/length(Date)))

# --------------EDA --------------

# Displays a histogram based on 'Rented Bike Count' column
hist(df$`Rented Bike Count`,main=paste("Histogram of",
      "Rented Bike Count"),xlab='Number of rented bikes',col="lightblue")

# Displays a correlation matrix 
corrplot(df_columns_for_corr)
#-------------- Data Viz for business stakeholders --------------
avg_Demand=unlist(avg_Demand)
# Displays a bar chart for the Average dail bike demand per Season
ggplot(df_groupby_season,aes(x=Seasons,y= avgSeasonDemand,fill = Seasons)) +
  geom_col() + 
  labs(x = "Seasons", y = "Average daily bike demand") +
  geom_hline(yintercept = avg_Demand,linetype = "dashed") +
  geom_text(aes(label = avgSeasonDemand),vjust=-0.5, colour = "black")


