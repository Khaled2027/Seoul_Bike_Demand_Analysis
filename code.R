getwd()

install.packages("tidyverse")
install.packages("psych")
install.packages("corrplot")
install.packages("car")

library(readr)
library(lubridate)
library(psych)
library(dplyr)
library(corrplot)
library(ggplot2)
library(car)

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

# Switches the column data format into date format
df <-df %>%
  mutate(Date = as.Date(Date,format='%d/%m/%Y'))

# Returns a new column where days are numeric (Monday = 1, Sunday = 7)
df <-df %>%
  mutate(dayofweek = wday(Date,label = TRUE))

# Creates a new column called 'is_weekday' that contain two values: 1 (True) and 0 (False)
df$is_weekday <- ifelse(df$dayofweek=='Fri'|df$dayofweek=='Mon'|df$dayofweek=='Tue'|df$dayofweek=='Wed'|df$dayofweek=='Thu',1,0)

# Creates a new column called 'is_holiday' for encoding and it contains two values: 
# 1 (True) and 0 (False)
df$is_holiday <-ifelse(df$Holiday=='Holiday',1,0)

# feuture engineers 3 new columns
df$is_winter <- ifelse(df$Seasons=='Winter',1,0)
df$is_spring <- ifelse(df$Seasons=='Spring',1,0)
df$is_summer <- ifelse(df$Seasons=='Summer',1,0)


df$is_snowfall <-ifelse(df$`Snowfall (cm)`==0,"No snowfall","Snowfall")

# Converts the datatype of 'Rented Bike Count' column into numeric type
df <-df %>%
  mutate(`Rented Bike Count` = as.numeric(`Rented Bike Count`))
# Drops the Date, Holiday, and dayofweek columns 
df <- df %>%
  select(!c(Holiday,dayofweek))

df_numerical_columns <- df %>%
  select(!c(Seasons,is_snowfall,Date))
# Returns a Satistical summary 
describe(df_numerical_columns)

df_columns_for_corr <- cor(df_numerical_columns)

# Returns a dataframe with Seasons grouped and 
# aggregated by the mean bike demand per season
df_groupby_season <- df %>%
  group_by(Seasons) %>%
  summarise(avgSeasonDemand=round(sum(`Rented Bike Count`)/length(Date)))

# Returns the overall average bike demand 
avg_Demand<- df %>%
  summarise(avgDemand=round(sum(`Rented Bike Count`)/length(Date)))

df_grouped_by_season_and_hour <- df %>%
  summarise(avgHourDemand=round(mean(`Rented Bike Count`))
            ,.by=c(Seasons,Hour))

df_grouped_by_season_and_temp <- df %>%
  summarise(avgDemand=round(mean(`Rented Bike Count`))
            ,.by=c(Seasons,`Temperature(°C)`))

df_inWinter <-df %>%
  filter(Seasons=='Winter')

# -------------- EDA --------------

# Displays a histogram based on 'Rented Bike Count' column
hist(df$`Rented Bike Count`,main=paste("Histogram of",
      "Rented Bike Count"),xlab='Number of rented bikes',col="lightblue")

# Displays a correlation matrix 
corrplot(df_columns_for_corr)
#-------------- Data Viz for business stakeholders --------------
avg_Demand=unlist(avg_Demand)

# Displays a bar chart for the Average daily bike demand per Season
ggplot(df_groupby_season,aes(x=Seasons,y= avgSeasonDemand
                             ,fill = Seasons)) +
  geom_col() + 
  labs(x = "Seasons", y = "Average demand") +
  geom_hline(yintercept = avg_Demand,linetype = "dashed") +
  geom_text(aes(label = avgSeasonDemand),vjust=-0.4, colour = "black")+
  theme_minimal() +
  ggtitle("Average Daily Bike Demand across Mulitple Seasons")

# Displays a linechart where x is the hour and y is the average demand per hour, colored by a line represting the Seasons
ggplot(df_grouped_by_season_and_hour,aes(x=Hour,y=avgHourDemand
                                         ,color=Seasons)) +
  geom_line() +
  expand_limits(y=0) +
  theme_minimal() +
  labs(x = "Hour of the day", y = "Average demand") +
  ggtitle("Average Hourly Bike Demand across Mulitple Seasons")

# Displays a scatterplot where x is tempertaure and y is the average bike demand
ggplot(df_grouped_by_season_and_temp,
       aes(x=`Temperature(°C)`,y=avgDemand,color=Seasons)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Temperature", y = "Average demand") +
  ggtitle("Temperture vs Average Demand (across seasons)") 
  
  
# Displays a boxplot that consists of 2 plots 
ggplot(df_inWinter,aes(x=is_snowfall,y=`Rented Bike Count`,
                       color=is_snowfall)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10() +
  #coord_flip() +
  theme_minimal() +
  labs(x = "Snowfall or No Snowfall", y = "Average demand") +
  ggtitle("Comparing the demand when it is snowing vs when there is no snow")


#--------------Hypothesis Testing --------------

# Test 1: Anova
df_aov <- aov(df$`Rented Bike Count`~ df$Seasons)
summary(df_aov)

# Tukey significant difference test
TukeyHSD(df_aov)

# Test 2: T-test for 'is_snowfall'
t.test(`Rented Bike Count`~is_snowfall,data=df)
# Test 3: T-test for 'is_holiday'
t.test(`Rented Bike Count`~is_holiday,data=df)

# Test 4: T-test for 'is_weekday'
t.test(`Rented Bike Count`~is_weekday,data=df)

#-------------- Modeling --------------

df_linear_regression <- df %>%
  select(!c(Date,Seasons,is_snowfall))
# Multivariable linear Regression Model
Linear_regresion_model <- lm(`Rented Bike Count`~.,df_linear_regression)

# Returns the calculate Variance Inflation Factor
vif(Linear_regresion_model)

summary(Linear_regresion_model)

par(mfrow = c(2,2))

plot(Linear_regresion_model)

# Create a dataframe with new values
prediction_data <- data.frame(Hour=9,`Temperature(°C)`= 6.0,`Humidity(%)`=34,
                       `Wind speed (m/s)`=3,`Visibility (10m)`=400,
                       `Solar Radiation (MJ/m2)`=1.12,`Rainfall(mm)`=0,
                       `Snowfall (cm)`=0,is_weekday=1,is_holiday=0,
                       is_winter=1,is_spring=0,is_summer=0,
                       check.names=FALSE)
# Gets the predicted value
Linear_Regression_prediction <- predict(Linear_regresion_model
                                      ,prediction_data)
# Displays the predicted value
Linear_Regression_prediction

