
#DATA PROFFESIONAL SURVEY


setwd("C:/Users/stanley/Desktop/Stiles/R")
DataProfessionals <- read.csv('Professionals.csv')
DataProfessionals

#Libraries being use
library(dplyr)
library(tidyverse)
library(ggplot2)
library(likert)

#Viewing the dataset
View(DataProfessionals)
head(DataProfessionals)
tail(DataProfessionals)
summary(DataProfessionals)
str(DataProfessionals)
glimpse(DataProfessionals)



#checking for missing figures or characters within the dataset
is.na(DataProfessionals)
sum(is.na(DataProfessionals))

# The missing characters within the dataset are in columns which 
# will not be needed for the purpose of analysis of this data. 
# Therefore, these columns will not be part of the selected columns for the analysis'''

#####################################################################

#Spliting the FavoriteLan. coloumn 
#This is done for the other option to stand alone
#In order to get these variables to work with
# : C/C++, Jave, javaScript and Others

Dp <- separate(DataProfessionals,
               col = FavoriteLanguge,
               into = c("FavoriteLanguage","Others"),
               sep = ":")
view(Dp)



#Spliting the Job.Title coloumn 
#This is done for the other option to stand alone
#In order to get these variables to work with
# : Data Analyst, Data Architect, Data Engineer, Data Scientist, Databese Developer and Other

Dp1 <- separate(Dp,
                col = JobTitle,
                into = c("JobTitle.","Others 2"),
                sep = "-")
view(Dp1)




#Spliting the Industry coloumn 
#This is done for the other option to stand alone
#In order to get these variables to work with
# : Agriculture, Construction, Education, fiance, Healthcare

Dp2 <- separate(Dp1,
                col = Industry,
                into = c("Industry","Others 3"),
                sep = "-")




#Spliting the AnotherWork coloumn 
#This is done for the other option to stand alone
#In order to get these variables to work with
# : Better salary, Good Culture, Good Work/Life Balance and Remote Work
Dp3 <- separate(Dp2,
                col = AnotherWork,
                into = c("AnotherWork","Others 4"),
                sep = "-")



#Sliting Salary Column
# This is done in order to calculate the Average of the split columns 
# The Average will be use for the Analysis
Dp4 <- separate(Dp3,
                col = CurrentPay,
                into = c("Pay1","Pay2"),
                sep = "-")
view(Dp4)




# Converting values in the splits columns 
# From String to Numeric in order to Calculate the Average Pay

Dp4$Pay1 <- as.numeric(Dp4$Pay1)
Dp4$Pay2 <- as.numeric(Dp4$Pay2)



# Calculating the mean of the Split columns 
rowMeans(Dp4[,c(14,15)])


#creating A new column (AveragePay ) in the Dataset 

Dp5 <-Dp4%>%
  mutate(Income = rowMeans(Dp4[,c(14,15)]))
view(Dp5)




#Selection of the Columns necessary for the analysis

FinalDp <- Dp5%>%
  select(JobTitle., Industry ,FavoriteLanguage, Gender, Income,
         Difficulty, AnotherWork, Current.Age,Q6A,Q6B,Q6C,Q6D,Q6E,Q6F)
FinalDp
view(FinalDp)




#checking for missing figures or characters within the final data set

is.na(FinalDp)
sum(is.na(FinalDp))



#Remove Missing Numbers (NAs) since they are only 36.

FinalDp1 <- na.omit(FinalDp)
sum(is.na(FinalDp1))



#Factorization of Variables
FinalDp2 <- within(
  FinalDp1, {
    JobTitle. <- factor(JobTitle., labels = c("Data Analyst", "Data Architect", "Data Enginer", 
                                              "Data Scientist", "Database Developer",
                                              "Student/Looking/None", "Other"))
    Industry <- factor(Industry, labels = c("Agriculture", "Construction", "Education", "Finance",
                                            "Healthcare", "Real Estate", "Tech", "Telcommunication", "Other"))
    FavoriteLanguage <- factor(FavoriteLanguage, labels = c("C/C++", "Java", "JavaScript", "Python","R", "Other" ))
    Gender <- factor(Gender, labels = c("Male", "Female"))
    Difficulty <- factor(Difficulty, c("Difficult", "Easy", "Neither easy nor difficult", "Very Difficult",
                                       "Very Easy", "Other"))
    AnotherWork <- factor(AnotherWork, c("Better Salary", "Good Culture", "Good Work/Life Balance",
                                         "Remote Work", "Other"))
    
  })
####################################################################################


#Count of Python Users among the Professions

Python_Users <- FinalDp2%>%
  group_by(FavoriteLanguage)%>%
  filter(FavoriteLanguage=="Python")%>%
  count(JobTitle.)%>% 
  rename(Professionals = n)
Python_Users



#Count of R Users among the Professions
R_Users <- FinalDp2%>%
  group_by(FavoriteLanguage)%>%
  filter(FavoriteLanguage=="R")%>%
  count(JobTitle.)%>% 
  rename(Professionals = n)
R_Users



#Count of the number of workers who use in the variuos industries

Python_Users_Industry <- FinalDp2%>%
  group_by(FavoriteLanguage)%>%
  filter(FavoriteLanguage=="Python")%>%
  count(Industry)%>% 
  rename(Professionals = n)
Python_Users_Industry




#Count of the number of workers who use R users in the various industries

R_Users_Industry <- FinalDp2%>%
  group_by(FavoriteLanguage)%>%
  filter(FavoriteLanguage=="R")%>%
  count(Industry)%>% 
  rename(Professionals = n)
R_Users_Industry



#Count of Females Data Professionals

Female_Prof <- FinalDp2%>%
  group_by(Gender)%>%
  filter(Gender=="Female")%>%
  count(JobTitle.)%>% 
  rename(Females = n)
Female_Prof



#Count of Females Data Professionals

Female_Industry <- FinalDp2%>%
  group_by(Gender)%>%
  filter(Gender=="Female")%>%
  count(Industry)%>% 
  rename(Females = n)
Female_Industry




#Calculating the Average, Minimum and Maxmum Salary

FinalDp2%>%
  summarise(MeanPay = mean(Income),
            MinimunPay = min(Income),
            Maximumpay = max(Income))



#Calculating the Average, Minimum and Maxmum Age

FinalDp2%>%
  summarise(MeanAge = mean(Current.Age),
            MinimunAge = min(Current.Age),
            MaximumAge = max(Current.Age))




#######################################################################################

#VISUALIZATION OF RESULT

#Bar Chart showing the count of Data Professionals

ggplot(FinalDp2, aes(JobTitle.))+
  geom_bar()+
  ggtitle("Count of Job titles")+
  xlab("JobTitle.")+
  ylab("count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Bar Chart showing the count of Gender

ggplot(FinalDp2, aes(Gender))+
  geom_bar()+
  ggtitle("Count of Gender")+
  xlab("Gender")+
  ylab("count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Bar Chart showing the count of Data Professionals by Gender


ggplot(FinalDp2, aes(JobTitle.,fill=Gender))+
  geom_bar(position = "dodge",
           alpha = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Count of Data Professionals by Gender",
       x = "JobTitle",
       y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Bar Chart showing the count of Various Programming Languages by Gender

ggplot(FinalDp2, aes(FavoriteLanguage, fill = Gender))+
  geom_bar(position = "dodge",
           alpha = 1)+
  ggtitle("Count of Programming Languages by Gender")+
  xlab("Favoritelanguage")+
  ylab("count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Bar Chart showing the count of Professionals in the various Industries

ggplot(FinalDp2, aes(JobTitle.,fill=Industry))+
  geom_bar(position = "dodge",
           alpha = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Count of Professionals in the Industries",
       x = "JobTitle",
       y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Analysing the Response for questions on Happiness (Q6-Q11)

Qdata <- FinalDp%>% select(Q6A,Q6B,Q6C,Q6D,Q6E,Q6F)%>%
  mutate_if(is.character, as.factor)
 
Qdata2 <- Qdata %>% mutate_at(
  vars(starts_with("Q")),
  funs(case_when(
    .==0~0,
    .==1~1,
    .==2~2,
    .==3~3,
    .==4~4,
    .==5~5,
    .==6~6,
    .==7~7,
    .==8~8,
    .==9~9,
    .==10~10
  ))
  
  )
view(Qdata2)

Qdata2 <- data.frame(
  mutate_if(Qdata,is.numeric,as.factor) 
) #Changing Qdata2 into a factor

Qdata3 <- likert(Qdata2)
plot(Qdata3)

#Analyzing the responses for the Question on Difficulty

Diff <- FinalDp2%>%
  select(Difficulty)
View(Diff)



#Recoding the options under the Question

Diff_1 <-Diff%>% mutate_at(
  vars(starts_with("Difficulty")),
  funs(case_when(
    .=="difficult"~1,
    .=="Easy"~2,
    .=="Neither easy nor difficult"~3,
    .=="Very Difficult"~4,
    .=="VeryEasy"~5,
    .=="Other"~6
  ))
) 

Diff_2<- data.frame(
  mutate_if(Diff_1,is.numeric, as.factor) #Chaning Diff_2 into a factor
)
Diff_3 <- likert(Diff_2)
plot(Diff_3, main = "How difficult was it for you to break into Data?", ylab = "Difficulty")



#Analysing responses for the question on factors they consider when searching for new job

NewJob<- FinalDp2%>%
  select(AnotherWork)

NewJob1 <-NewJob%>% mutate_at(
  vars(starts_with("AnotherWork")),
  funs(case_when(
    .=="Better Salaryt"~1,
    .=="Good Culture"~2,
    .=="Good Work/Life Balance"~3,
    .=="Remote Work"~4,
    .=="Other"~5,
  ))
)

NewJob2<- data.frame(
  mutate_if(NewJob1,is.numeric, as.factor)    
)

NewJob3 <- likert(NewJob2)
plot(NewJob3)





# Plotting the distribution of the ages of the various professionals

ggplot(FinalDp2,aes(x=Current.Age))+
  geom_density()+
  ggtitle("Age Distribution")+
  xlab("Age")




# Plotting the distribution of the ages of the various professionals

ggplot(FinalDp2,aes(x= Income))+
  geom_density()+
  ggtitle("Income Distribution")+
  xlab("Income")





