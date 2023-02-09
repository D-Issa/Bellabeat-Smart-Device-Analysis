# Install and Load Necessary Packages ####
install.packages("tidyverse")
library(tidyverse)

# Import Data ####
DailyActivity <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

DailyCalories <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

DailyIntensities <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

DailySteps <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

SleepDayData <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

WeightLog <- read.csv("C:/Users/issad/Desktop/Personal Projects/Google Certificate Case Studies/Track 1/Case 2/Fitbit_data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# Explore Data ####
## Use str() function to check data types and column names
str(DailyActivity)
str(DailyCalories)
str(DailyIntensities)
str(DailySteps)
str(SleepDayData)
str(WeightLog)

## Use head() function to look at some data values, could also use glimpse() function
head(DailyActivity)
head(DailyCalories)
head(DailyIntensities)
head(DailySteps)
head(SleepDayData)
head(WeightLog)

# Clean Data ####
## Correcting datatype for Date variables
DailyActivity <- transform(DailyActivity, ActivityDate = as.Date(ActivityDate, "%m/%d/%Y"))
DailyCalories <- transform(DailyCalories, ActivityDay = as.Date(ActivityDay, "%m/%d/%Y"))
DailyIntensities <- transform(DailyIntensities, ActivityDay = as.Date(ActivityDay, "%m/%d/%Y"))
DailySteps <- transform(DailySteps, ActivityDay = as.Date(ActivityDay, "%m/%d/%Y"))
SleepDayData <- transform(SleepDayData, SleepDay = as.Date(SleepDay, "%m/%d/%Y"))
WeightLog <- transform(WeightLog, Date = as.Date(Date, "%m/%d/%Y"))

## Check for Duplicates
sum(duplicated(DailyActivity))
sum(duplicated(DailyCalories))
sum(duplicated(DailyIntensities))
sum(duplicated(SleepDayData))
sum(duplicated(WeightLog))

## Identify NULL values
sum(is.na(DailyActivity))
sum(is.na(DailyCalories))
sum(is.na(DailyIntensities))
sum(is.na(SleepDayData))
sum(is.na(WeightLog))

## Remove Duplicates
SleepDayData <- unique(SleepDayData)
sum(duplicated(SleepDayData))

# Transform Data ####
## Adding column summing total active minutes
DailyActivity2 <- DailyActivity %>% mutate(TotalActiveMinutes = rowSums(DailyActivity[,11:13]))

# Aggregate Data ####
DailyActivity2 %>% 
  select(TotalSteps, TotalDistance, TotalActiveMinutes, SedentaryMinutes, Calories) %>% 
  summary()

SleepDayData %>% select(TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()

WeightLog %>% select(WeightPounds, BMI) %>% 
  summary()

# Search for Trends in Data ####
StepsVis <- ggplot(DailyActivity2) + 
  geom_point(mapping = aes(x=TotalSteps,y=Calories, color=TotalSteps)) +
  geom_smooth(mapping = aes(x=TotalSteps,y=Calories), color="orange") +
  labs(title = "Total Steps vs Calories Burned", x = "Total Steps Taken",
       y = "Total Calories Burned", caption = "Data collected By Mobius")

StepsVis

ActivityVis <- ggplot(DailyActivity2) + 
  geom_point(mapping = aes(x=TotalActiveMinutes/60,y=Calories), color="blue4") +
  geom_smooth(mapping = aes(x=TotalActiveMinutes/60,y=Calories), color="red") +
  labs(title = "Total Calorie Consumption vs Hours Active",
       x = "Total Hours Active", y = "Total Calories Burned",
       caption = "Data collected By Mobius")

ActivityVis

SleepVis <- ggplot(SleepDayData) + 
  geom_point(mapping = aes(x=TotalMinutesAsleep/60, y=TotalTimeInBed/60), color = "goldenrod2") +
  geom_smooth(mapping = aes(x=TotalMinutesAsleep/60, y=TotalTimeInBed/60)) +
  labs(title = "Hours Asleep vs In Bed", x = "Hours Asleep", y = "Hours In Bed",
       caption = "Data collected By Mobius")

SleepVis

WeightVis <- ggplot(WeightLog) + 
  geom_point(mapping = aes(x=WeightPounds, y = BMI), color="red") +
  geom_smooth(mapping = aes(x=WeightPounds, y = BMI), color="blue1") +
  labs(title = "Weight In Pounds vs BMI", caption = "Data collected By Mobius") 

WeightVis

ManualReportVis <- ggplot(WeightLog) +
  geom_bar(mapping = aes(x=IsManualReport,fill = IsManualReport)) +
  labs(title = "Number of Manual Reports vs Automatic Reports", y="Count",
       caption = "Data collected By Mobius")

ManualReportVis

# Create visualizations to Share With Stakeholders ####
StepsVis <- StepsVis + 
  annotate("text", x=28000, y=1500, label= "As total steps taken increases,",
           color="purple", fontface="bold",size=3.5) + 
  annotate("text", x=28000, y=1300, label= "more calories are burned.",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=15000, y=500, label="Most users take between 0-15,000",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=15000, y=300, label="(avg 7638) steps and burn 1,000-3,000",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=15000, y=100, label="(avg 2304) calories.",
           color="purple", fontface="bold",size=3.5)

ActivityVis <- ActivityVis +
  annotate("text", x=1, y=4800,label="As activity increases,",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=1, y=4600, label="more calories are burned.",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=7.5, y=900, label="Users on average are active for",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=7.5, y=700, label="3.8 hours and burn 2304 calories.",
           color="purple", fontface="bold",size=3.5)

SleepVis <- SleepVis +
  annotate("text", x=3, y=14.5, label="Generally, users get out",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=3, y=13.8, label="of bed shortly after waking up.",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=10, y=3.5, label="Users who get 7-9 hours",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=10, y=2.8, label="of sleep spend less time in bed.",
           color="purple", fontface="bold",size=3.5)

WeightVis <- WeightVis + 
  annotate("text", x=150, y=45, label="Generally, as user weight increases,",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x=150, y=43.5, label="so does their BMI.",
           color="purple", fontface="bold",size=3.5) 

ManualReportVis <- ManualReportVis + 
  annotate("text", x="FALSE", y=40, label="More users manually report weight",
           color="purple", fontface="bold",size=3.5) +
  annotate("text", x="FALSE", y=38, label="than automatically report.",
           color="purple", fontface="bold",size=3.5) 
StepsVis
ActivityVis
SleepVis
WeightVis
ManualReportVis