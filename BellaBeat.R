install.packages("tidyverse")
library("tidyverse")
daily_activity<-read_csv("C:/Users/user/Documents/data analysis/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
View(daily_activity)
daily_sleep<-read_csv("C:/Users/user/Documents/data analysis/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
View(daily_sleep)
str(daily_activity)
str(daily_sleep)
#how many participants for day activity
n_distinct(daily_activity$Id)
#how many participants for night activity
n_distinct(daily_sleep$Id)
#checking for missing values
sum(is.na(daily_activity))
sum(is.na(daily_sleep))
#checking for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
#Remove duplicates
daily_sleep <-daily_sleep %>% distinct() %>% drop_na()
installed.packages("janitor")
library("janitor")
#making all variable names consistent
clean_names(daily_activity)
clean_names(daily_sleep)
view(daily_activity)
str(daily_activity$Date)
#can also usethis for date conversion
#daily_activity$ActivityDate<-mdy(daily_activity$ActivityDate)
daily_activity <- daily_activity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))
View(daily_activity)
daily_sleep <- daily_sleep %>%
  rename(Date = SleepDay) %>%
  mutate(Date = as_date(Date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
View(daily_sleep)
#observations in each data frame
nrow(daily_sleep)
#410
nrow(daily_activity)
#940
daily_activity %>% select(TotalSteps,TotalDistance,SedentaryMinutes) %>% summary()
daily_sleep %>% select(TotalSleepRecords,TotalMinutesAsleep,TotalTimeInBed) %>%summary() 
#visual presentation
ggplot(data=daily_activity,aes(x=TotalSteps,y=SedentaryMinutes,color=Calories))+geom_point()+labs(title="Calorie burn with comparison of Total steps to Sedentary minutes")
ggplot(data=daily_sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point(aes(color=Date))
#Merging the two datasets together
daily_data <- merge(daily_activity, daily_sleep, by=c ("Id", "Date"))
View(daily_data)
n_distinct(daily_data$Id)
#combine all the data
full_data<-merge(daily_activity,daily_sleep,by=c("Id","Date"),all=TRUE)
head(full_data)
View(full_data)
n_distinct(full_data$Id)
sum(is.na(full_data))
full_data<-full_data %>% mutate_if(is.numeric,~replace(.,is.na(.),0))
sum(is.na(full_data))
#format day of the week
format(as.Date(full_data$Date),"%w")
full_data$DayOfTheWeek = weekdays(as.Date(full_data$Date,format = "%Y-%m-%d"))
#Total minutes
full_data$TotalMinutes = full_data$VeryActiveMinutes+full_data$FairlyActiveMinutes+full_data$LightlyActiveMinutes+full_data$SedentaryMinutes
str(full_data)
#strftime to convert time to character objects
full_data$DayOfTheWeek = strftime(full_data$Date,'%A')
View(full_data)
full_data$TotalHours <- round((full_data$TotalMinutes/60), digits=2)
full_data %>%  
  select(TotalSteps,
         SedentaryMinutes,
         Calories) %>%
  summary()
#As distance increases calories reduce
p1=ggplot(data=full_data)+geom_point(mapping=aes(x=TotalDistance,y=Calories,color="red"))+labs(title="Distance vs calories")
#As number of steps increase The calories reduce
p2=ggplot(data=full_data)+
  geom_point(mapping = aes(x = TotalSteps, y =Calories),color="green")+
  labs(title="Total Steps vs. Calories")
#As the number of minutes increase the calories reduce
p3=ggplot(data=full_data)+
  geom_point(mapping = aes(x = TotalMinutes, y =Calories),color="red")+
  labs(title="Total Minutes vs. Calories")
full_data$TotalHours <- round(full_data$TotalMinutes/60,digits=2)
View(full_data)

install.packages("ggpubr")
library(ggpubr)
#SHARE
ggarrange(p1, p2,p3,ncol = 3, nrow = 1) 
full_data$DayOfTheWeek <- factor(full_data$DayOfTheWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data=full_data,aes(x=DayOfTheWeek,fill=DayOfTheWeek))+geom_bar(stat="count")+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+labs(x='Day of Week',y='Frequency',title='Number of time users logged in to use the app')
#calories for every step taken
full_data %>%  
  select(TotalSteps,
         Calories) %>%
  summary()
ggplot(data=full_data)+geom_point(mapping=aes(x=TotalSteps,y=Calories,color=TotalSteps))+scale_color_gradientn(colours = "rainbow"(6))+ geom_hline(yintercept = 2304, color = "red", linewidth = 1)+geom_vline(xintercept = 7638, color = "blue", linewidth = 1)+ geom_text(aes(x=10000, y=2100, label="Mean"), color="black", linewidth=5)+labs(title='Calories burned with every step taken',x='Steps taken',y='calories burned') 
#Calories burned for every hour logged
full_data %>% select(TotalHours,SedentaryMinutes,Calories) %>% summary()
ggplot(data = full_data) + geom_point(mapping = aes(x=TotalHours, y=Calories, color=TotalSteps)) + 
  scale_color_gradientn(colours = "rainbow"(3))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  geom_hline(aes(yintercept= 2304, linetype = "Average Hours"), colour= 'red', size=1)+
  geom_vline(aes(xintercept= 991/60, linetype = "Average Sedentary"), colour= 'purple', size=1)+
  geom_vline(aes(xintercept = 20.31, linetype = "Average Steps"), colour='blue', size=1)+
  scale_linetype_manual(name = "Statistics", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "purple")))) +
  labs(x='Hours logged',y='Calories burned',Title='Calories burned each hour')
  
Data_Ind <- full_data %>%
  summarise(Sum_VAM = sum(VeryActiveMinutes/1148807*100), 
            Sum_FAM = sum(FairlyActiveMinutes/1148807*100), 
            Sum_LAM = sum(LightlyActiveMinutes/1148807*100), 
            Sum_SEM = sum(SedentaryMinutes/1148807*100),
            Sum_TOTAL=sum(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)) %>% 
  round(digits = 2)
slices <- c(Data_Ind$Sum_VAM, Data_Ind$Sum_FAM, Data_Ind$Sum_LAM, Data_Ind$Sum_SEM)
lbls <- c("Very Active Min", "Fairly Active Min", "Lightly Active Min", "Sedentary Min")
pie(slices,labels=paste(lbls,slices,sep=" ","%"),col=rainbow(6), main="Pie Chart - % of Activity in Minutes")

    