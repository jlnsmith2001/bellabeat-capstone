#download packages
library(ggplot2)
library(tidyverse)

#assign datasets to variables
weightLog <-read.csv("weightLogInfo_merged.csv")
activity <- read.csv("dailyActivity_merged.csv")

#reformat date to match correct "date" type
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
weightLog$Date <-as.Date(weightLog$Date, format = "%m/%d/%Y")

#reformat string to match for when i left join based on "Date" condition
activity <- activity %>%  rename(ActivityDate = Date)

#left join the tables to match based on user ID and date
activity_weight <- left_join(activity, weightLog, by = c("Id", "Date"))
                             
#create a new column called "weight_logged" which displays true or false values based on
#whether or not that user logged their weight on that specific day 
activity_weight <- activity_weight %>%  mutate(weight_logged = !is.na(WeightKg))

#return the result displaying the difference in calories burned between
#the two types of users and store it in a variable 

calories_summary <- activity_weight %>% 
    group_by(weight_logged) %>%  
    summarise(
         avg_calories = mean(Calories, na.rm = TRUE),
         count = n() 
       )
#graph difference in calories burned as a bar chart
ggplot(calories_summary, aes(x = weight_logged, y = avg_calories, fill = weight_logged)) +
     geom_bar(stat = "identity", width = 0.6) +
     labs(
         title = "Average Calories Burned: Logged vs Not Logged Weight",
         x = "Logged weight",
         y = "Average Calories Burned",
         fill = "Weight Log Status"
       ) +
     theme_minimal()


#graph differences in time spent engaging in strenuous exercise
activity_weight %>% 
     group_by(weight_logged) %>% 
     summarise(
         avg_active = mean(VeryActiveMinutes, na.rm = TRUE)
       ) %>% 
     ggplot(aes(x = weight_logged, y = avg_active, fill = weight_logged)) +
     geom_col() +
     labs(
         title = "Does tracking weight directly lead to more activity?",
         x = "Logged weight",
         y = "Average time of strenuous exercise (minutes)",
         fill = "Weight Log Status"
       ) +
     theme_minimal()

#graph differences in step count 
activity_weight %>%
     group_by(weight_logged) %>%
     summarise(
         avg_steps = mean(TotalSteps, na.rm = TRUE)
       ) %>% 
     ggplot(aes(x = weight_logged, y = avg_steps, fill = weight_logged)) +
     geom_col() +
       labs(
           title = "Average Steps taken: Logged vs Not Logged Weight",
           x = "Logged weight",
           y = "Average amount of steps taken",
           fill = "Weight Log Status "
         ) +
     theme_minimal()
                             