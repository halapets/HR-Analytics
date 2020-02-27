#Reading all datasets in R
HRAnalytics <- read.csv("general_data.csv",stringsAsFactors = F)
employee_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

library(lubridate)
library(dplyr)
library(ggplot2)
library(caret)
library(gains)
library(irr)
library(car)


# Loading Libraries
library("anytime", lib.loc="~/R/win-library/3.5")

#merging datasets HRAnalytics and employee survey datasets
HRAnalytics <- merge(HRAnalytics,employee_survey,by.x = "EmployeeID",by.y = "EmployeeID")

#merging datasets HRAnalytics and Manager survey data
HRAnalytics <- merge(HRAnalytics,manager_survey,by.x = "EmployeeID",by.y = "EmployeeID")


#---------------------------------getting the average working time for each employee----------------------------
dim(in_time)
dim(out_time)

str(in_time)
str(out_time)

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"


#removing all null values
removeallnullcols<- function(df){
  df[,!apply(df,MARGIN = 2,FUN = function(x) all(is.na(x)))]
}

in_time <- removeallnullcols(in_time)
out_time <- removeallnullcols(out_time)

#Difference of intime & outtime can give us  number of hours worked

out_time[,2:250] <- lapply(out_time[,2:250],FUN = function(x) anytime(x,useR =T))
in_time[,2:250] <- lapply(in_time[,2:250],FUN = function(x) anytime(x,useR = T))

# Calculating #of Hours worked
Hours_Worked <- out_time[,2:250]-in_time[,2:250]
Hours_Worked$EmployeeID <- in_time$EmployeeID
Hours_Worked[,2:250] <- lapply(Hours_Worked[,2:250], function(x) as.numeric(x))

# Calculating means working hours of  the employees.
Mean_Hours_Worked <- as.data.frame(round(rowMeans(Hours_Worked[,2:250], na.rm = TRUE)))

#binding the Mean_Hours_Worked with HRAnalytics
HRAnalytics <- cbind(Mean_Hours_Worked,HRAnalytics)
colnames(HRAnalytics)[1] <- "Mean_Hours_Worked"
names(HRAnalytics)


#################################################Exploratory Data Analysis#############################################

summary(HRAnalytics)
colSums(is.na(HRAnalytics))

#TotalWorking Years, EnvironmentSatisfaction,Jobsatisfaction, NumCompaniesWorked and 
#WorkLifeBalance has missing values


#Converting Attrition Variable to numeric variable for further analysis

HRAnalytics$Attrition1 <- ifelse(HRAnalytics$Attrition == "Yes",1,0)

#-----------------------------------------HRAnalytics$Age------------------------------------------

summary(HRAnalytics$Age)
boxplot(HRAnalytics$Age)#No Outlier

ggplot(HRAnalytics,aes(x=Age,fill=as.factor(Attrition)))+
  geom_bar(stat = "count")+
  ggtitle("Attrition by Age")+ylab("Employees Count")+xlab("Age")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+labs(fill=Attrition)

#Looking at the graph,18-35 age group has higher attrition trend

#Creating Age groups below and above 36
HRAnalytics$AgeGroup <- HRAnalytics$Age
for(i in 1:nrow(HRAnalytics)){
  if(HRAnalytics$Age [i] < 36){
    HRAnalytics$AgeGroup[i] = "18-35"
  } else {
    HRAnalytics$AgeGroup[i] = "36-60"
  }
}

#Plotting Attrition by Age
ggplot(HRAnalytics,aes(x=AgeGroup,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  ggtitle("Attrition by Age")+ylab("Employee Count")+xlab("Age")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+labs(fill="Attrition")

#Attrition Rate is 2 times in age group 18-35 compared to age group 36-60.


#---------------------------------------HRAnalytics$BusinessTravel----------------------------------

table(HRAnalytics$BusinessTravel)

#verifying Attrition trend by Business Travel
BusinessTravel <- HRAnalytics%>%group_by(BusinessTravel)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round(total*100/count))

#   ggplot(BusinessTravel,aes(x=BusinessTravel,y=perc_Attrition,fill=BusinessTravel))+
#   geom_bar(stat = "identity")+ggtitle("Attrition by BusinessTravel")+
#   theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
#   theme(legend.position = "none")+
#   ylab("AttritionPercentage")+
#   geom_label(label= paste(BusinessTravel$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))
  
#Plotting Attrition by Business Trend
  ggplot(HRAnalytics,aes(x=BusinessTravel,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ ggtitle("Attrition by Business Travel")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#Employees who travel rarely are higher than who travel frequently.
#there is no significant pattern of attrition seen for Business Travel

#---------------------------------------HRAnalytics$Department-------------------------------------

table(HRAnalytics$Department)
  
#verifying Attrition trend by Department 
Department <- HRAnalytics%>% group_by(Department)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round(total*100/count))

#   ggplot(Department,aes(x=Department,y=perc_Attrition,fill=Department))+
#   geom_bar(stat = "identity")+ggtitle("Attrition by Department")+
#   theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
#   theme(legend.position = "none")+
#   ylab("AttritionPercentage")+
#   geom_label(label= paste(Department$perc_Attrition,"%",sep = ""),position = position_dodge(0.9))
  
#Plotting Attrition trend by Department
ggplot(HRAnalytics,aes(x=Department,fill=as.factor(Attrition)))+
    geom_bar(stat = "count",position = position_dodge())+
    theme_minimal()+ylab("Employee Count")+ ggtitle("Attrition by Department")+
    labs(fill="Attrition")+
    theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#R&D department has significantly higher employees compared to HR Department,
#there is no significant pattern of attrition seen for Department

#-------------------------------------HRAnalytics$DistanceFromHome---------------------------------

boxplot(HRAnalytics$DistanceFromHome)#No outliers

#Plotting Attrition trend by DistanceFromHome
ggplot(HRAnalytics,aes(x=DistanceFromHome,fill=as.factor(Attrition)))+
  geom_bar(stat = "count")+
  ggtitle("Attrition by DistanceFromHome")+ylab("Employees Count")+xlab("DistanceFromHome")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+labs(fill=Attrition)


#Looking at the graph, Attrition rate is high for distance less than 10 KMs.
#Distance from home does not seem to be the reason for Attrition

#Creating bin on Distance to see if we observe any strong attition pattern
Distance <- HRAnalytics%>%
  mutate(Groups=ntile(DistanceFromHome,3))%>%
  group_by(Groups)%>%
  summarise(Min_Distance=round(min(DistanceFromHome)),
            Max_Distance=round(max(DistanceFromHome)),
            total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))
Distance$Groups <- paste(Distance$Min_Distance,Distance$Max_Distance,sep="-")
colnames(Distance)[1] <- "Distance"

ggplot(Distance,aes(x=Distance,y=perc_Attrition,fill=as.factor(Distance)))+
  geom_bar(stat = "identity")+ggtitle("Attrition by Distance")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Average Distance")+ylab("Attrition Percentage")+labs(fill="Distance")+
  geom_label(label= paste(Distance$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Distance from Home does not show any significant pattern in terms of Attrition.

#---------------------------------------HRAnalytics$Education--------------------------------------
table(HRAnalytics$Education)

#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$Education[i] == 1){
    HRAnalytics$Education[i] = "Below College"
  } else if (HRAnalytics$Education[i] == 2){
    HRAnalytics$Education[i] = "College"
  } else if (HRAnalytics$Education[i] == 3){
    HRAnalytics$Education[i] = "Bachelor"
  } else if (HRAnalytics$Education[i] == 4){
    HRAnalytics$Education[i] = "Master"
  } else {
    HRAnalytics$Education[i] = "Doctor"
  }
}

##verifying Attrition trend by Education
Education1 <- HRAnalytics%>%group_by(Education)%>%summarise(total=sum(Attrition1),count=n(),
                            perc_Attrition=round((total*100)/count))%>%arrange(Education)


# ggplot(Education1,aes(x=Education ,y=perc_Attrition,
#                       fill=as.factor(Education )))+
#   geom_bar(stat = "identity")+ggtitle("Attrition by Education")+
#   theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
#   xlab("Education")+ylab("Attrition Percentage")+theme(legend.position = "none")+
#   geom_label(label= paste(Education1$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##plotting Attrition trend by Education
ggplot(HRAnalytics,aes(x=Education,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by Education")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#College Education level shows high attrition @19% compared to other levels
#Education level does not show any significant patten around Attrition


#--------------------------------------HRAnalytics$EducationField---------------------------------------
table(HRAnalytics$EducationField)

##verifying Attrition trend by Educationfield
Educationfield1 <- HRAnalytics%>%group_by(EducationField)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))


# ggplot(Educationfield1,aes(x=EducationField,y=perc_Attrition,fill=as.factor(EducationField)))+
#   geom_bar(stat = "identity")+ggtitle("Attrition by Educationfield")+
#   theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
#   xlab("Educationfield")+ylab("Attrition Percentage")+
#   geom_label(label= paste(Educationfield1$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##Plotting Attrition trend by Educationfield
ggplot(HRAnalytics,aes(x=EducationField,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by EducationField")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")
#HR Education Field with 81 employees only has the highest attrition rate @41% which 
#is not significant in terms of volume
#however, Lifescience, Marketing and Medical field has 16-17 % attrition rate which 
#is quite significant in terms of volume

#-----------------------------------Education and Educationfield-----------------------------------

#exploring the impact of education and Educationfield1 together on attrition

ggplot(HRAnalytics,aes(x=EducationField,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by Education and EducationField")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
  facet_grid(Education~.)

 
#Education field together and education level does not show any significant 
#trend/pattern around attrition


#--------------------------------------HRAnalytics$EmployeeCount----------------------------------

length(unique(HRAnalytics$EmployeeCount))#1

#not using Employee count for modelling as it has 1 values for every employee and 
#it is irrelevant from Attrition point of view

#-----------------------------------------HRAnalytics$EmployeeID-----------------------------------


length(unique(HRAnalytics$EmployeeID))#4410
#not using EmployeeID for modelling as it is irrelevant from Attrition point of view

#--------------------------------------HRAnalytics$Gender----------------------------------------
table(HRAnalytics$Gender)

##verifying Attrition trend by Gender
Gender <- HRAnalytics%>%group_by(Gender)%>%summarise(total=sum(Attrition1),count=n(),
                               perc_Attrition=round((total*100)/count))


# ggplot(Gender,aes(x=Gender,y=perc_Attrition,fill=as.factor(Gender)))+
#   geom_bar(stat = "identity")+ggtitle("Attrition by Gender")+
#   theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
#   theme(legend.position = "none")+
#   xlab("Gender")+ylab("Attrition Percentage")+
#   geom_label(label= paste(Gender$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##Plotting Attrition trend by Gender
ggplot(HRAnalytics,aes(x=Gender,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by Gender")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#Gender does not show any significant patten around Attrition

#-------------------------------------HRAnalytics$JobLevel----------------------------------------
table(HRAnalytics$JobLevel)
#with the increase in Job level, there is drop in employee count.
#Job level 1 & 2 has around 70% of employees

##verifying Attrition trend by JobLevel
JobL <- HRAnalytics%>%group_by(JobLevel)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

# ggplot(JobL,aes(x=JobLevel,y=perc_Attrition,fill=as.factor(JobLevel)))+
#   geom_bar(stat = "identity")+
#   ggtitle("Attrition by Joblevel")+theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
#   ylab("Attrition Percentage")+
#   geom_label(label= paste(JobL$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##Plotting Attrition trend by JobLevel
ggplot(HRAnalytics,aes(x=JobLevel,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by JobLevel")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#Job level 1 & 2 has around 70% of employees i.e. 3000+ employees. 
#Though Attrition rate across Job level does not show any significant variation but
#considering the volume at job level 1 & 2, we can focus on job level 1 & 2 
#to control the attrition 

#-----------------------------------HRAnalytics$JobRole------------------------------------------

table(HRAnalytics$JobRole)

##verifying Attrition trend by JobRole
JobR <- HRAnalytics%>%group_by(JobRole)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

# ggplot(JobR,aes(x=JobRole,y=perc_Attrition,fill=as.factor(JobRole)))+
#   geom_bar(stat = "identity")+
#   ggtitle("Attrition by JobRole")+theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
#   ylab("Attrition Percentage")+
#   geom_label(label= paste(JobR$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##Plotting Attrition trend by JobRole
ggplot(HRAnalytics,aes(x=JobRole,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by JobRole")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90,hjust= 0.5))

#research director job role has significant attrition rate(24%).
#However, no of employees in R & D department are low in volume representing 5% of employees
#There is no significant variation in the attrition rates of other job roles.

#-----------------------------------------HRAnalytics$MaritalStatus--------------------------------


table(HRAnalytics$MaritalStatus)

##verifying Attrition trend by Marital Status
MaritalStatus <- HRAnalytics%>%group_by(MaritalStatus)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

# ggplot(MaritalStatus,aes(x=MaritalStatus,y=perc_Attrition,fill=as.factor(MaritalStatus)))+
#   geom_bar(stat = "identity")+
#   ggtitle("Attrition by MaritalStatus")+theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
#   ylab("Attrition Percentage")+
#   geom_label(label= paste(MaritalStatus$perc_Attrition,"%",sep = ""),
#   position = position_dodge(0.9))

##Plotting Attrition trend by Marital Status
ggplot(HRAnalytics,aes(x=MaritalStatus,fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ 
  ggtitle("Attrition by MaritalStatus")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90,hjust= 0.5))

#Single status has higher atttrition rate @ 26% 
#Divorced and Married has Attrition Rates of 10 and 12% respectively.

#------------------------------------------HRAnalytics$MonthlyIncome----------------------------------
sum(is.na(HRAnalytics$MonthlyIncome))#no missing values

hist(HRAnalytics$MonthlyIncome,col = "Red",main = "Monthly Income")
#Monthly Income distribution is SKwed

boxplot(HRAnalytics$MonthlyIncome)#outliers present
summary(HRAnalytics$MonthlyIncome)
x <- boxplot(HRAnalytics$MonthlyIncome)
min(x$out)
max(x$out)
range(x$out)
length(x$out)/nrow(HRAnalytics)#7% outliers

#Outlier Treatment
HRAnalytics$MonthlyIncome[which(HRAnalytics$MonthlyIncome %in% x$out)] <- max(HRAnalytics$MonthlyIncome[HRAnalytics$MonthlyIncome<min(x$out)])


ggplot(HRAnalytics,aes(x=MonthlyIncome,fill=Attrition))+
  geom_histogram(bins = 30)+
  ggtitle("Attrition by Income")+ylab("Employees")+xlab("MonthlyIncome")+
  theme_minimal()+theme(legend.position = "bottom")+theme(plot.title = element_text(hjust=0.5))

#Attrition is higher in Lower income groups
  
#Trying to identify age, Income and Marital Status relation together with Attrition trend
ggplot(HRAnalytics,aes(x=Age,y= MonthlyIncome,col=Attrition,size=Attrition))+
  geom_point(alpha=0.4)+
  ggtitle("Attrition by MonthlyIncome and Age")+ylab("Monthly Income")+xlab("Age")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+facet_grid(MaritalStatus~.)

#High Attrition is observed in Married and 
#Single employees less than 40 years of age and with income less than 100000

#Correlation between MonthlyIncome and Age
cor(HRAnalytics$MonthlyIncome,HRAnalytics$Age)#-0.04

#------------------------------------HRAnalytics$NumCompaniesWorked----------------------------------
unique(HRAnalytics$NumCompaniesWorked)
summary(HRAnalytics$NumCompaniesWorked)#19 missing values

prop.table(table(HRAnalytics$NumCompaniesWorked))#35% of employees worked for 1 company.

#hence replacing missing value with 1 going by maxixmum frequency
HRAnalytics$NumCompaniesWorked[which(is.na(HRAnalytics$NumCompaniesWorked))] <- 1

##verifying Attrition trend by NumCompaniesWorked
NumCompaniesWorked <- HRAnalytics%>%group_by(NumCompaniesWorked)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(NumCompaniesWorked,aes(x=as.factor(NumCompaniesWorked),y=perc_Attrition,fill=as.factor(NumCompaniesWorked)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by NumCompaniesWorked")+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+
  geom_label(label= paste(NumCompaniesWorked$perc_Attrition,"%",sep = ""),
  position = position_dodge(0.9))

##Plotting Attrition trend by NumCompaniesWorked
ggplot(HRAnalytics,aes(x=as.factor(NumCompaniesWorked),fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ xlab("NumCompaniesWorked")+
  ggtitle("Attrition by NumCompaniesWorked")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90,hjust= 0.5))

#35% of employees have worked with 1 company where attrition rate is 19%.
#Looking at the volume of this category, focus on this group of employees would 
#result in solving attrition to a large extend.


#---------------------------------------HR Analytics$Over18--------------------------------------------
length(unique(HRAnalytics$Over18))#only one unique value
#hence not useful for analysis.

#-------------------------------------HRAnalytics$PercentSalaryHike-----------------------------
summary(HRAnalytics$PercentSalaryHike)
length(unique(HRAnalytics$PercentSalaryHike))#15 values
table(HRAnalytics$PercentSalaryHike)

##verifying Attrition trend by PercentSalaryHike
PercentSalaryHike <- HRAnalytics%>%group_by(PercentSalaryHike)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

##Plotting Attrition trend by PercentSalaryHike
ggplot(PercentSalaryHike,aes(x=as.factor(PercentSalaryHike),y=perc_Attrition,fill=as.factor(PercentSalaryHike)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by PercentSalaryHike")+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("PercentSalaryHike")+
  geom_label(label= paste(PercentSalaryHike$perc_Attrition,"%",sep = ""),
  position = position_dodge(0.9))

ggplot(HRAnalytics,aes(x=as.factor(PercentSalaryHike),fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ xlab("PercentSalaryHike")+
  ggtitle("Attrition by PercentSalaryHike")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#For percentage salary hike 15 and 25, the attrition rate is significantly high above 20%
#but the volume wise it covers 10% of the total Attrition. 
#Rest all level of salary hikes, no significant variation in attrition rate


#----------------------------------HRAnalytics$StandardHours----------------------------------
unique(HRAnalytics$StandardHours)#only one unique value

#Not considering for the modeling

#---------------------------------HRAnalytics$StockOptionLevel--------------------------------

unique(HRAnalytics$StockOptionLevel)
table(HRAnalytics$StockOptionLevel)

##verifying Attrition trend by StockOptionLevel
StockOptionLevel <- HRAnalytics%>%group_by(StockOptionLevel)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

##PlottingAttrition trend by StockOptionLevel
ggplot(StockOptionLevel,aes(x=StockOptionLevel,y=perc_Attrition,
                            fill=as.factor(StockOptionLevel)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by StockOptionLevel")+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("StockOptionLevel")+
  geom_label(label= paste(StockOptionLevel$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

ggplot(HRAnalytics,aes(x=as.factor(StockOptionLevel),fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ xlab("StockOptionLevel")+
  ggtitle("Attrition by StockOptionLevel")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

# Attrition rate across all stock options are more or less same (Around15-18%)
#However,79% of employees have stock option 1 & 2.
#Looking at the volume of these categories, focus on these groups would 
#result in solving attrition to a large extend.

#-------------------------------------------HRAnalytics$TotalWorkingYears-----------------------
unique(HRAnalytics$TotalWorkingYears)


#Checking outliers
boxplot(HRAnalytics$TotalWorkingYears)#outliers present
summary(HRAnalytics$TotalWorkingYears)
x <- boxplot(HRAnalytics$TotalWorkingYears)
min(x$out)
max(x$out)
range(x$out)
length(x$out)/nrow(HRAnalytics)#4.3% outliers

#Outlier Treatment
HRAnalytics$TotalWorkingYears[which(HRAnalytics$TotalWorkingYears %in% x$out)] <- 
  max(HRAnalytics$TotalWorkingYears[HRAnalytics$TotalWorkingYears<min(x$out)],na.rm = T)

#Missing Values
length(sum(is.na(HRAnalytics$TotalWorkingYears)))/nrow(HRAnalytics)#0.02% missing values

#Replacing missing values with mean

HRAnalytics$TotalWorkingYears[which(is.na(HRAnalytics$TotalWorkingYears))] <- mean(HRAnalytics$TotalWorkingYears,na.rm = T)
HRAnalytics$TotalWorkingYears <- round(HRAnalytics$TotalWorkingYears)

#Verifying the Attrition TRend with TotalWorkingYears
ggplot(HRAnalytics,aes(x=TotalWorkingYears,fill=Attrition))+
  geom_histogram(bins = 30)+
  ggtitle("Attrition by TotalWorkingYears")+xlab("TotalWorkingYears")+ylab("Employee")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(0,30,5))
#Attrition is higher with employees having less total working experience.


Totalworkingyears<- HRAnalytics%>%mutate(groups=ntile(TotalWorkingYears,3))%>%
  group_by(groups)%>%summarise(min_years=min(TotalWorkingYears),max_years=max(TotalWorkingYears),
                total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

Totalworkingyears$groups <- paste(Totalworkingyears$min_years,
                                  Totalworkingyears$max_years,sep="-")
colnames(Totalworkingyears)[1]<- "TotalWorkingYears"

ggplot(Totalworkingyears ,aes(x=as.factor(TotalWorkingYears),y=perc_Attrition,
                              fill=as.factor(TotalWorkingYears)))+  geom_bar(stat = "identity")+
  ggtitle("Attrition by Working experience")+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("Working experience")+
  geom_label(label=Totalworkingyears$perc_Attrition)

#Attrition with less than 7 years working experience, attrition rate is significantly high

#Conerting TotalWorking Years to Categorical Variable

#Creating TotalWorkingYearsGroup
HRAnalytics$TotalWorkingYearsGroup <- HRAnalytics$TotalWorkingYears
for(i in 1:nrow(HRAnalytics)){
  if(HRAnalytics$TotalWorkingYears [i] < 7){
    HRAnalytics$TotalWorkingYearsGroup[i] = "0-7"
  } else if (HRAnalytics$TotalWorkingYears [i] < 12){
    HRAnalytics$TotalWorkingYearsGroup[i] = "7-12"
  } else{
    HRAnalytics$TotalWorkingYearsGroup[i] = "12-28"
  }
}

#--------------------------------------HRAnalytics$TrainingTimesLastYear-----------------------------
unique(HRAnalytics$TrainingTimesLastYear)

table(HRAnalytics$TrainingTimesLastYear)

#Verifying the Attrition TRend with TrainingTimesLastYear
TrainingTimesLastYear <- HRAnalytics%>%group_by(TrainingTimesLastYear)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

#Plotting the Attrition TRend with TrainingTimesLastYear
ggplot(TrainingTimesLastYear,aes(x=TrainingTimesLastYear,y=perc_Attrition,
                            fill=as.factor(TrainingTimesLastYear)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by TrainingTimesLastYear")+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("TrainingTimesLastYear")+
  geom_label(label= paste(TrainingTimesLastYear$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

ggplot(HRAnalytics,aes(x=as.factor(TrainingTimesLastYear),fill=as.factor(Attrition)))+
  geom_bar(stat = "count",position = position_dodge())+
  theme_minimal()+ylab("Employee Count")+ xlab("TrainingTimesLastYear")+
  ggtitle("Attrition by TrainingTimesLastYear")+
  labs(fill="Attrition")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")

#Maximum Employees i.e.70% of employees have attained 2-3 trainings last year.
#Attrition Rate is 17-18% here
#Employees who have attained zero training last year have highest attrition rate i.e. 19%
#Employees who have attained 6 training last year have lowest attrition rate i.e.6% 

#------------------------------------HRAnalytics$YearsAtCompany------------------------------------
unique(HRAnalytics$YearsAtCompany)

#Checking outliers
boxplot(HRAnalytics$YearsAtCompany)#outliers present
summary(HRAnalytics$YearsAtCompany)
x <- boxplot(HRAnalytics$YearsAtCompany)
min(x$out)
max(x$out)
range(x$out)
length(x$out)/nrow(HRAnalytics)#7% outliers

#Outlier Treatment
HRAnalytics$YearsAtCompany[which(HRAnalytics$YearsAtCompany %in% x$out)] <- 
  max(HRAnalytics$YearsAtCompany[HRAnalytics$YearsAtCompany<min(x$out)],na.rm = T)

#Verifying the Attrition TRend with YearsAtCompany
ggplot(HRAnalytics,aes(x=YearsAtCompany,fill=Attrition))+
  geom_histogram(bins = 30)+
  ggtitle("Attrition by YearsAtCompany")+xlab("YearsAtCompany")+ylab("Employees")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(0,20,1))

#Verifying the Attrition TRend with YearsAtCompany by creating bins
YearsAtCompany<- HRAnalytics%>%mutate(groups=ntile(YearsAtCompany,2))%>%
  group_by(groups)%>%summarise(min_years=min(YearsAtCompany),max_years=max(YearsAtCompany),Avg_years=mean(YearsAtCompany),
  total=sum(Attrition1),count=n(),perc_Attrition=round(total*100/count))

YearsAtCompany$Category_Yearsatcompany <- 0
YearsAtCompany$Category_Yearsatcompany <- paste(YearsAtCompany$min_years,YearsAtCompany$max_years,sep="-")


ggplot(YearsAtCompany ,aes(x=as.factor(Category_Yearsatcompany),y=perc_Attrition,fill=as.factor(Category_Yearsatcompany)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by YearsAtCompany")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("YearsAtCompany")+
  geom_label(label= paste(YearsAtCompany$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Attrition is higher for employees with lower years at company.
#Employees with less than 5 years at company has relatively 
#higher attrition rate 21% than employees above 5 years at company

#Creating YearsAtCompanyGroup categorical variable
HRAnalytics$YearsAtCompanyGroup <- HRAnalytics$YearsAtCompany
for(i in 1:nrow(HRAnalytics)){
  if(HRAnalytics$YearsAtCompany [i] < 5){
    HRAnalytics$YearsAtCompanyGroup[i] = "0-5"
  } else{
    HRAnalytics$YearsAtCompanyGroup[i] = "5-18"
  }
}


#--------------------------------HRAnalytics$YearsSinceLastPromotion---------------------------------
unique(HRAnalytics$YearsSinceLastPromotion)
sum(is.na(HRAnalytics$YearsSinceLastPromotion))#no missing values
boxplot(HRAnalytics$YearsSinceLastPromotion)#Outliers Present
summary(HRAnalytics$YearsSinceLastPromotion)
x <- boxplot(HRAnalytics$YearsSinceLastPromotion)
min(x$out)
max(x$out)
range(x$out)
length(x$out)/nrow(HRAnalytics)#7% outliers

#Outlier Treatment
HRAnalytics$YearsSinceLastPromotion[which(HRAnalytics$YearsSinceLastPromotion %in% x$out)] <- 
  max(HRAnalytics$YearsSinceLastPromotion[HRAnalytics$YearsSinceLastPromotion<min(x$out)],na.rm = T)


#Verifying the Attrition TRend with YearsSinceLastPromotion by creating bins
YearsSinceLastPromotion<- HRAnalytics%>%mutate(groups=ntile(YearsSinceLastPromotion,3))%>%
  group_by(groups)%>%summarise(min_years=min(YearsSinceLastPromotion),max_years=max(YearsSinceLastPromotion),Avg_years=mean(YearsSinceLastPromotion),
  total=sum(Attrition1),count=n(),perc_Attrition=round(total*100/count))

YearsSinceLastPromotion$Category_YearsSinceLastPromotion <- 0
YearsSinceLastPromotion$Category_YearsSinceLastPromotion <- paste(YearsSinceLastPromotion$min_years,YearsSinceLastPromotion$max_years,sep="-")


ggplot(YearsSinceLastPromotion ,aes(x=as.factor(Category_YearsSinceLastPromotion),y=perc_Attrition,fill=as.factor(Category_YearsSinceLastPromotion)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by YearsSinceLastPromotion")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("YearsSinceLastPromotion")+
  geom_label(label= paste(YearsSinceLastPromotion$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Employees with no promotion have high attrition rates

#Creating YearsSinceLastPromotionGroup categorical variable
HRAnalytics$YearsSinceLastPromotionGroup <- HRAnalytics$YearsSinceLastPromotion
for(i in 1:nrow(HRAnalytics)){
  if(HRAnalytics$YearsSinceLastPromotion [i] <= 0){
    HRAnalytics$YearsSinceLastPromotionGroup[i] = "No Promotion"
  } else if(HRAnalytics$YearsSinceLastPromotion [i] < 2){
    HRAnalytics$YearsSinceLastPromotionGroup[i] = "Less than 2 Years since last promotion"
  } else {
    HRAnalytics$YearsSinceLastPromotionGroup[i] = "greater than 2 Years since last promotion"
  }
}

#--------------------------------------HRAnalytics$YearsWithCurrManager-------------------------

unique(HRAnalytics$YearsWithCurrManager)
boxplot((HRAnalytics$YearsWithCurrManager))#Outliers present
summary(HRAnalytics$YearsWithCurrManager)
x <- boxplot(HRAnalytics$YearsWithCurrManager)
min(x$out)
max(x$out)
range(x$out)
length(x$out)/nrow(HRAnalytics)#0.9% outliers

#Outlier Treatment
HRAnalytics$YearsWithCurrManager[which(HRAnalytics$YearsWithCurrManager %in% x$out)] <- 
  max(HRAnalytics$YearsWithCurrManager[HRAnalytics$YearsWithCurrManager<min(x$out)],na.rm = T)

#Verifying attrition trend with YearsWithCurrManager
ggplot(HRAnalytics,aes(x=YearsWithCurrManager,fill=Attrition))+
  geom_histogram(bins = 30)+
  ggtitle("Attrition by YearsWithCurrManager")+xlab("YearsWithCurrManager")+ylab("Employee")+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(0,20,1))

#Verifying attrition trend with YearsWithCurrManager by creating bins
YearsWithCurrManager<- HRAnalytics%>%mutate(groups=ntile(YearsWithCurrManager,3))%>%
  group_by(groups)%>%summarise(min_years=min(YearsWithCurrManager),max_years=max(YearsWithCurrManager),Avg_years=mean(YearsWithCurrManager),
  total=sum(Attrition1),count=n(),perc_Attrition=round(total*100/count))

YearsWithCurrManager$Category_YearsWithCurrManager <- 0
YearsWithCurrManager$Category_YearsWithCurrManager <- paste(YearsWithCurrManager$min_years,YearsWithCurrManager$max_years,sep="-")


ggplot(YearsWithCurrManager ,aes(x=as.factor(Category_YearsWithCurrManager),y=perc_Attrition,fill=as.factor(Category_YearsWithCurrManager)))+
  geom_bar(stat = "identity")+
  ggtitle("Attrition by YearsWithCurrManager")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+
  ylab("Attrition Percentage")+xlab("YearsWithCurrManager")+
  geom_label(label= paste(YearsWithCurrManager$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Employees less than 2 years with current manager have higher chances of attrition

#Creating YearsSinceLastPromotionGroup categorical variable
HRAnalytics$YearsWithCurrManagerGroup <- HRAnalytics$YearsWithCurrManager
for(i in 1:nrow(HRAnalytics)){
  if(HRAnalytics$YearsWithCurrManager [i] < 2){
    HRAnalytics$YearsWithCurrManagerGroup[i] = "Less than 2 years"
  } else if(HRAnalytics$YearsWithCurrManager [i] < 6){
    HRAnalytics$YearsWithCurrManagerGroup[i] = "Less than 6 Years"
  } else {
    HRAnalytics$YearsWithCurrManagerGroup[i] = "greater than 6 Years"
  }
}

#-------------------------------HRAnalytics$EnvironmentSatisfaction----------------------------
unique(HRAnalytics$EnvironmentSatisfaction)
table(HRAnalytics$EnvironmentSatisfaction)
sum(is.na(HRAnalytics$EnvironmentSatisfaction))/nrow(HRAnalytics)#0.5% missing values
#Replacing missing value with the value 3 of highest frequency (Value 3 represents high satisfaction)
HRAnalytics$EnvironmentSatisfaction[which(is.na(HRAnalytics$EnvironmentSatisfaction))] <- 3


#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$EnvironmentSatisfaction[i] == 1){
    HRAnalytics$EnvironmentSatisfaction[i] = "Low"
  } else if (HRAnalytics$EnvironmentSatisfaction[i] == 2){
    HRAnalytics$EnvironmentSatisfaction[i] = "Medium"
  } else if (HRAnalytics$EnvironmentSatisfaction[i] == 3){
    HRAnalytics$EnvironmentSatisfaction[i] = "High"
  } else {
    HRAnalytics$EnvironmentSatisfaction[i] = "Very High"
  }
}

#Verifying attrition trend with EnvironmentSatisfaction
EnvironmentSatisfaction <- HRAnalytics%>%group_by(EnvironmentSatisfaction)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(EnvironmentSatisfaction,aes(x=EnvironmentSatisfaction ,y=perc_Attrition,
                      fill=as.factor(EnvironmentSatisfaction )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by EnvironmentSatisfaction")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("EnvironmentSatisfaction")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(EnvironmentSatisfaction$perc_Attrition,"%",sep = ""),
  position = position_dodge(0.9))

#Low Environment Satisfaction leads to high Attrition rate (25%)

#-------------------------------HRAnalytics$JobSatisfaction----------------------------------
unique(HRAnalytics$JobSatisfaction)
length(sum(is.na(HRAnalytics$JobSatisfaction)))/nrow(HRAnalytics)#0.02% missing values
table(HRAnalytics$JobSatisfaction)

#Replacing missing value with the value 4 of highest frequency 
#(Value 4 represents very high satisfaction)
HRAnalytics$JobSatisfaction[which(is.na(HRAnalytics$JobSatisfaction))] <- 4

#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$JobSatisfaction[i] == 1){
    HRAnalytics$JobSatisfaction[i] = "Low"
  } else if (HRAnalytics$JobSatisfaction[i] == 2){
    HRAnalytics$JobSatisfaction[i] = "Medium"
  } else if (HRAnalytics$JobSatisfaction[i] == 3){
    HRAnalytics$JobSatisfaction[i] = "High"
  } else {
    HRAnalytics$JobSatisfaction[i] = "Very High"
  }
}

#Verifying attrition trend with JobSatisfaction
JobSatisfaction <- HRAnalytics%>%group_by(JobSatisfaction)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(JobSatisfaction,aes(x=JobSatisfaction ,y=perc_Attrition,
                                   fill=as.factor(JobSatisfaction )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by JobSatisfaction")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("JobSatisfaction")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(JobSatisfaction$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Low Job Satisfaction leads to higher attrition rate

#---------------------------------HRAnalytics$WorkLifeBalance-------------------------------------
unique(HRAnalytics$WorkLifeBalance)
length(sum(is.na(HRAnalytics$WorkLifeBalance)))/nrow(HRAnalytics)#0.02% missing values
table(HRAnalytics$WorkLifeBalance)

#Replacing missing value with the value 3 of highest frequency 
#(Value 4 represents better worklife balance)
HRAnalytics$WorkLifeBalance[which(is.na(HRAnalytics$WorkLifeBalance))] <- 3

#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$WorkLifeBalance[i] == 1){
    HRAnalytics$WorkLifeBalance[i] = "Bad"
  } else if (HRAnalytics$WorkLifeBalance[i] == 2){
    HRAnalytics$WorkLifeBalance[i] = "Good"
  } else if (HRAnalytics$WorkLifeBalance[i] == 3){
    HRAnalytics$WorkLifeBalance[i] = "Better"
  } else {
    HRAnalytics$WorkLifeBalance[i] = "Best"
  }
}

#Verifying attrition trend with WorkLifeBalance
WorkLifeBalance <- HRAnalytics%>%group_by(WorkLifeBalance)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(WorkLifeBalance,aes(x=WorkLifeBalance ,y=perc_Attrition,
                           fill=as.factor(WorkLifeBalance )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by WorkLifeBalance")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("WorkLifeBalance")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(WorkLifeBalance$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))


#Attrition rate is higher with Bad work life balance.However, the volume of Employees with
#bad work life balance is very less i.e. 5% of employees with bad worklife balance.

#-----------------------------HRAnalytics$JobInvolvement--------------------------------------------
unique(HRAnalytics$JobInvolvement)

#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$JobInvolvement[i] == 1){
    HRAnalytics$JobInvolvement[i] = "Low"
  } else if (HRAnalytics$JobInvolvement[i] == 2){
    HRAnalytics$JobInvolvement[i] = "Medium"
  } else if (HRAnalytics$JobInvolvement[i] == 3){
    HRAnalytics$JobInvolvement[i] = "High"
  } else {
    HRAnalytics$JobInvolvement[i] = "Very High"
  }
}

WorkLifeBalance
JobInvolvement <- HRAnalytics%>%group_by(JobInvolvement)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(JobInvolvement,aes(x=JobInvolvement ,y=perc_Attrition,
                           fill=as.factor(JobInvolvement )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by JobInvolvement")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("JobInvolvement")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(JobInvolvement$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#Attrition rate is higher with Low job involvement.However, the volume of Employees with
#Low job involvement is very less i.e. around 5% of employees with Low job involvement.

#--------------------------------HRAnalytics$PerformanceRating--------------------------------------
unique(HRAnalytics$PerformanceRating)

#labelling the Education Data as per data dictionary
for (i in 1:nrow(HRAnalytics)){
  if (HRAnalytics$PerformanceRating[i] == 3){
    HRAnalytics$PerformanceRating[i] = "Excellent"
  } else {
    HRAnalytics$PerformanceRating[i] = "Outstanding"
  }
}

#Verifying attrition trend with PerformanceRating
PerformanceRating <- HRAnalytics%>%group_by(PerformanceRating)%>%
  summarise(total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

ggplot(PerformanceRating,aes(x=PerformanceRating ,y=perc_Attrition,
                          fill=as.factor(PerformanceRating )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by PerformanceRating")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("PerformanceRating")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(PerformanceRating$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#16% attrition rate for Excellent rating which represents 80% of total attrition(588 employees)
#18% attrition rate for Outstanding rate which represents 20% of total attrition(123 employees)

#--------------------------------HRAnalytics$Mean_Hours_Worked------------------------------------

unique(HRAnalytics$Mean_Hours_Worked)

#Verifying Attrition trend with working Hours
WorkingHrs <- HRAnalytics%>%mutate(Working_Hours=ntile(Mean_Hours_Worked,3))%>%
  group_by(Working_Hours)%>%summarise(min=min(Mean_Hours_Worked),max=max(Mean_Hours_Worked),
  total=sum(Attrition1),count=n(),perc_Attrition=round((total*100)/count))

WorkingHrs$Working_Hours <- paste(WorkingHrs$min,WorkingHrs$max,sep="-")

ggplot(WorkingHrs,aes(x=Working_Hours ,y=perc_Attrition,
                          fill=as.factor(Working_Hours )))+
  geom_bar(stat = "identity")+ggtitle("Attrition by Working_Hours")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Working_Hours")+ylab("Attrition Percentage")+theme(legend.position = "none")+
  geom_label(label= paste(WorkingHrs$perc_Attrition,"%",sep = ""),
             position = position_dodge(0.9))

#With Increase in working hours, there is increase in attrition rate

#################################################################################################
#Creating a final dataset to be used for modelling
Attrition <- HRAnalytics[,-c(2,3,10,17,19,21,23:25,31)]

#REmoving Categorical Variable from Attrition dataset 
#and creating another dataframe as dummy variables will be created for them
Attrition1 <- Attrition[,-c(2,3,4,6,7:11,13,15:26)]
Attrition2 <- Attrition[,c(2,3,4,6,7:11,13,15:26)]

#converting couple of numerical variables which can be treated as categorical variables
#Joblevel,Stockoptionlevel and TrainingTimesLastyear
#In order to create dummy variables of these, converting them to factor
Attrition2$JobLevel <- as.factor(Attrition2$JobLevel)
Attrition2$StockOptionLevel <- as.factor(Attrition2$StockOptionLevel)
Attrition2$TrainingTimesLastYear <- as.factor(Attrition2$TrainingTimesLastYear)
Attrition2$NumCompaniesWorked <- as.factor(Attrition2$NumCompaniesWorked)


# # creating dummy variables for factor attributes
dummies<- data.frame(sapply(Attrition2,
                            function(x) data.frame(model.matrix(~x-1,data =Attrition2))[,-1]))

# Final dataset
Attrition1<- cbind(Attrition1,dummies)
View(Attrition1) #4410 obs. of  61 variables
names(Attrition1)

#Creating Dependent variable Attrition to Factor
Attrition1$Attrition <- as.factor(Attrition1$Attrition)

#Verifying the attrition %
prop.table(table(Attrition1$Attrition))#16% Attrition

############################################Building Model##########################################

#Creating train and test dataset
set.seed(1000)
index <- sample(nrow(Attrition1),nrow(Attrition1)*0.70,replace = F)
train_Attrition <- Attrition1[index,]
test_Attrition <- Attrition1[-index,]


#building the logistic regression model
model1 <- glm(Attrition~.,train_Attrition,family = "binomial")
summary(model1)
#AIC-2203.8

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 
??MASS
?step
?stepAIC

#Iteration 2
model2 <- stepAIC(model1, direction="both")

summary(model2)

vif(model2)
#AIC: 2158.2
 
#REmoving insignificant variables EnvironmentSatisfactionVery.High(insignificant),Joblevel5 etc 
#Iteration 3
model3 <- glm(Attrition ~ Mean_Hours_Worked + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x1 + 
                    NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                    NumCompaniesWorked.x7 + NumCompaniesWorked.x9 + TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.xLow + 
                    JobSatisfaction.xLow + 
                    JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                    WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                    AgeGroup + TotalWorkingYearsGroup.x12.28 + 
                    TotalWorkingYearsGroup.x7.12 + YearsAtCompanyGroup + YearsSinceLastPromotionGroup.xLess.than.2.Years.since.last.promotion + 
                    YearsSinceLastPromotionGroup.xNo.Promotion + YearsWithCurrManagerGroup.xLess.than.2.years + 
                    YearsWithCurrManagerGroup.xLess.than.6.Years, family = "binomial", 
                  data = train_Attrition)

summary(model3)
vif(model3)
#AIC: 2163.9


#REmoving below insignificant variables 
#JobRole.xHuman.Resources,TrainingTimesLastYear.x4,
#TrainingTimesLastYear.x5,JobInvolvement.xMedium
#Iteration 4
model4 <- glm(Attrition ~ Mean_Hours_Worked + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + 
                JobLevel.x5 + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + NumCompaniesWorked.x1 + 
                NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                NumCompaniesWorked.x7 + NumCompaniesWorked.x9 +  
                TrainingTimesLastYear.x6 + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + 
                AgeGroup + TotalWorkingYearsGroup.x12.28 + 
                TotalWorkingYearsGroup.x7.12 + YearsAtCompanyGroup + YearsSinceLastPromotionGroup.xLess.than.2.Years.since.last.promotion + 
                YearsSinceLastPromotionGroup.xNo.Promotion + YearsWithCurrManagerGroup.xLess.than.2.years + 
                YearsWithCurrManagerGroup.xLess.than.6.Years, family = "binomial", 
              data = train_Attrition)
summary(model4)
vif(model4)
#AIC: 2170


#Final Model-Model4

##################################################################################################
#------------------------------------------Model Evaluation-----------------------------------------

#predicted probabilities of Attrition for test data

test_pred = predict(model4, type = "response", 
                    test_Attrition[,-5])

table(test_Attrition$Attrition)#224 Attritions
prop.table(table(test_Attrition$Attrition))#16.9% attrition

summary(test_pred)#probabilities of Attrition ("Yes")
test_prob <- test_pred

#using probability cut off 50%
test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(as.numeric(test_Attrition$Attrition)==1,"No","Yes"))
#when I convert attrition from factor to numeric, 
#0 gets converted to 1 and 1 gets converted to 2, 
#Hence, 1 is treated as "No" and zero as "Yes"

table(test_actual_attr,test_pred_attr)
#Out of total 224 positives (attrition),52 are predicted correctly
#out of total 1099 negatives, 1069 are predicted correctly.

#using probability cut off 30%
test_pred_attr <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
table(test_actual_attr,test_pred_attr)
#Out of total 224 positives (attrition),109 are predicted correctly
#out of total 1099 negatives, 976 are predicted correctly.

#####################################Confusion Matrix###########################################
library(e1071)

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#with 30% probability cut off model is showing 82% accuracy

#Confusion Matrix and Statistics

#Reference
#Prediction  No Yes
#No  976 115
#Yes 123 109

#Accuracy : 0.8201          
#95% CI : (0.7983, 0.8405)
#No Information Rate : 0.8307          
#P-Value [Acc > NIR] : 0.8559          

#Kappa : 0.3694          
#Mcnemar's Test P-Value : 0.6500          

#Sensitivity : 0.48661         
#Specificity : 0.88808         
#Pos Pred Value : 0.46983         
#Neg Pred Value : 0.89459         
#Prevalence : 0.16931         
#Detection Rate : 0.08239         
#Detection Prevalence : 0.17536         
#Balanced Accuracy : 0.68734         

#'Positive' Class : Yes             

##################################################################################################
# Choosing the cut off (the optimal probability cut off)

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(0.01,0.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Choosing  a cutoff value of 0.1456566 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.1456566, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

#Confusion Matrix and Statistics

#Reference
#Prediction  No Yes
#No  777  67
#Yes 322 157

acc <- conf_final$overall[1]#70.6% accuracy

sens <- conf_final$byClass[1]#70.08% sensitivity

spec <- conf_final$byClass[2]#70.7% specificity


#work around gains and lift parameters
confusionMatrix_data <- cbind(test_cutoff_attr,test_actual_attr,test_pred)


##################################################################################################
### KS -statistic - Test Data 

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


library(ROCR)
#on test  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

plot(performance_measures_test,main="Area under the Curve-AUC")

performance_measures_test1<- performance(pred_object_test, "auc")

#Area under the curve is 70.09%. 
#False positive rate-0.2929936
#True positive rate-0.7008929


####################################################################
# Lift & Gain Chart 

# plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attr, test_pred, groups = 10)

#write.csv(Churn_decile,"Churn_decile.csv")


