#importing necessary packages
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(stringr)

#importing dataset (Student Alchol Consumption) for math students only
math <- read.csv("C://Users/bdaet/Desktop/R_Tutorials/student-mat.csv")

math2 <- mutate(math, Total_Consumption = math$Dalc + math$Walc)

#some of the variables were interpreted as numerics when they are actually factors so we have to change them to factors
math$Dalc <- as.factor(math$Dalc)
math$Walc <- as.factor(math$Walc)
math$age <- as.factor(math$age)

#the failures variable is set up weirdly, it's numeric from 1-3 and then 4 means 4 or more
#to compensate for this I'm going to change it to a factor and change 4 to 4+ to avoid confusion
math$failures <- as.factor(math$failures)
math$failures <- str_replace(math$failures, pattern = "4", replacement = "4+")

math <- gather(math, Time_of_Week, Alc_Consumption, Dalc, Walc)
math$Time_of_Week <- str_replace(math$Time_of_Week, pattern = "Dalc", replacement = "Weekday")
math$Time_of_Week <- str_replace(math$Time_of_Week, pattern = "Walc", replacement = "Weekend")

#in this data set it turns out there are no students who failed 4+ classes so this wasn't actually necessary





#looking at weekday alcohol consumption by gender
mvf <- math %>%
          group_by(sex, Time_of_Week, Alc_Consumption) %>%
          summarise(Count = n())

male_v_female <- ggplot(mvf, aes(x = Alc_Consumption, y = Count, fill = sex)) +
                      geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
                      theme_bw() +
                      facet_wrap(~ Time_of_Week) +
                      scale_fill_manual(values = c("hotpink","royalblue")) +
                      ggtitle("Alcohol Consumption by Gender") +
                      xlab("Consumption Level (1 = Very Low, 5 = Very High)")
ggplotly(male_v_female)



#looking at alcohol consumption by age
#after looking at the result, it appears there is little to no data on ages 20-22 in this data set
#to make a more readable graph we're only going to focus on ages 15-19
ages <- math %>%
          filter(age %in% c(15:19)) %>%
          group_by(age, Alc_Consumption, Time_of_Week) %>%
          summarise(Count = n())

a <- ggplot(ages, aes(x = Alc_Consumption, y = Count, fill = Time_of_Week)) +
          geom_bar(stat = "identity", alpha = 0.65, position = "dodge") +
          theme_bw() +
          facet_wrap(~ age) +
          scale_fill_manual(values = c("skyblue", "limegreen")) +
          ggtitle("Alcohol Consumption by Age") +
          xlab("Consumption Level (1 = Very Low, 5 = Very High)") 
ggplotly(a)



#let's look at if participation in extra-curricular activities affects alcohol consumption during the week
extra_act <- math %>%
              group_by(activities, Time_of_Week, Alc_Consumption) %>%
              summarise(count = n())

ext <- ggplot(extra_act, aes(x = Alc_Consumption, y = count, fill = activities)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
          theme_bw() +
          facet_wrap(~ Time_of_Week) +
          scale_fill_manual(values = c("green", "orange")) +
          ggtitle("Effect Participation in Extra-Curricular Activities Has on Alcohol Consumption") +
          xlab("Consumption Level (1 = Very Low, 5 = Very High)")
ggplotly(ext)



#let's see if alcohol consumption leads to more absences from class
absences <- ggplot(math, aes(x = Alc_Consumption, y = absences)) +
               geom_boxplot(alpha = 0.65, color = "red4") +
               theme_bw() +
               facet_wrap(~ Time_of_Week) +
               ggtitle("Effect Alcohol Consumption has On Absences from Class") +
               xlab("Consumption Level (1 = Very Low, 5 = Very High)") +
               ylab("Number of Absences")
ggplotly(absences)


#let's see if weekday alcohol consumption leads to more failed classes
fail <- math %>%
        filter(Time_of_Week == "Weekday") %>%
        group_by(Alc_Consumption, failures) %>%
        summarise(Count = n())

fail <- spread(fail, failures, Count, fill = 0)
names(fail) <- c("Alcohol Consumption Level", "0 Failed Classes", 
                 "1 Failed Class", "2 Failed Classes", "3 Failed Classes")
fail
                
                
#let's see if alcohol consumption has any effect on final course grades
grades <- ggplot(math, aes(x = Alc_Consumption, y = G3)) +
              geom_boxplot(alpha = 0.65, color = "navy") +
              theme_bw() +
              facet_wrap(~ Time_of_Week) +
              ggtitle("Effect Alcohol Consumption has On Grades") +
              xlab("Consumption Level (1 = Very Low, 5 = Very High)") +
              ylab("Final Grade (on scale from 0-20)")
ggplotly(grades)



scattermath <- ggplot(math2, aes(Total_Consumption,G3)) + geom_jitter(aes(colour = goout)) +
  geom_smooth(color = "red") +
  ggtitle("Total Alcohol Consumption vs Final Math Grades") +
  labs(x = "Total Alcohol Consumption (Rating scaled out of 10)", 
       y = "Final Math Grade (Scaled out of 20)")

ggplotly(scattermath)

        
