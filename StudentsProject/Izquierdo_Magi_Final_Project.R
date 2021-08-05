# set directory where .csv files are located
#getwd()
#setwd("C:/Users/jik_6/Documents/R/win-library/3.5")
#read tables in.
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

# Merge Tables
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students


# Look for NA's or Outliers and remove
#install.packages("tidyverse")
library(tidyverse)

d3 %>%
  summarise(count = sum(is.na(.)))


par(mfrow=c(1,2))

boxplot(d3$age, main= "Boxplot of Age (15 to 22)")
#There are 3 values with outliers. Will remove them as will focus on under/to 18 years old.

boxplot(d3$absences.x, main="Boxplot of Absence")
plot(d3$absences.x,d3$age, main="Scatterplot of Age and Absence", ylab="Age", xlab = "Days of absence")
# Any datapoint higher than 20 is considered an outlier. In the scatterplot the most extreme
#values seems to be above 50. The reason of high absence is not disclosed but will 
#treat data as plausable.


boxplot(d3$G1.x, main="Boxplot of G1")

boxplot(d3$G2.x, main="Boxplot of G2")
# has outliers on 0 grades, probably linked to the most extreme value of Absence as it is close to 60, which
# is almost an schoolar trimester.  It is relevant that G1 does not have any outlier.

boxplot(d3$G3.x, main="Boxplot of G3")
# Outliers are grades 0.

plot(d3$absences.x,d3$G2.x, main="Scatterplot of G2 and Absence", ylab="Grade", xlab = "Days of absence")
plot(d3$absences.x,d3$G1.x, main="Scatterplot of G1 and Absence", ylab="Grade", xlab = "Days of absence")

#Remove age outliers

d3<-filter(d3,d3$age<19)

str(d3)
# from 382 to 369 students 
par(mfrow=c(1,3))

boxplot(d3$age, main= "Boxplot of Age (15 to 22)")
boxplot(d3$age, main= "Boxplot of Age (15 to 18)")

# Remove students with 0 Absences and 0 Grading.
d3<-d3[!(d3$absences.x==0 & d3$G1.x==0 & d3$G2.x==0 & d3$G3.x==0),]

str(d3)
# There seem to be some students that have atended the whole semester and graded 0 but not in all gradings.
# Probably have not attended the exam in some of the grading periods.




# Portuguese Languange

dev.off()

boxplot(d3$absences.y, main="Boxplot of Absence")
# Any datapoint higher than 15 is considered an outlier. In the scatterplot the most extreme
#values seems to be around 30. The reason of high absence is not disclosed but will 
#treat data as plausable.
plot(d3$absences.y,d3$age, main="Scatterplot of Age and Absence", ylab="Age", xlab = "Days of absence")


par(mfrow=c(1, 3))
boxplot(d3$G1.y, main="Boxplot of G1")
# Has 1 outlier at the 0 value.
boxplot(d3$G2.y, main="Boxplot of G2")
# has 1 outlier at each side, one at 5 and the higher one at 19.

boxplot(d3$G3.y, main="Boxplot of G3")
#Several outliers below 7 and one higher arround 19

par(mfrow=c(2, 2))

plot(d3$absences.y,d3$G2.y, main="Scatterplot of G2 and Absence", ylab="Grade", xlab = "Days of absence")
plot(d3$absences.y,d3$G1.y, main="Scatterplot of G1 and Absence", ylab="Grade", xlab = "Days of absence")

dev.off()

# Summary Statistics
#Student
summary(d3[1:13])
#Math
summary(d3[14:33])
#Por
summary(d3[34:53])

# Summary Stat MAth

Math_sup<-c(school=d3$schoolsup.x,Family=d3$famsup.x,Paid=d3$paid.x)
addmargins(prop.table(table(Math_sup)))

barplot(table(Math_sup), main = " General Math support",names.arg=c("NO", "YES"))

addmargins(prop.table(table(d3$failures.x)))
addmargins(prop.table(table(d3$absences.x)))
addmargins(prop.table(table(d3$higher.x)))
addmargins(prop.table(table(d3$activities.x)))
addmargins(prop.table(table(d3$romantic.x)))

# Summary Stat Por

Por_sup<-c(school=d3$schoolsup.y,Family=d3$famsup.y,Paid=d3$paid.y)
addmargins(prop.table(table(Por_sup)))

barplot(table(Por_sup),main = " General Por Lang. support",names.arg=c("NO", "YES"))

addmargins(prop.table(table(d3$failures.y)))
addmargins(prop.table(table(d3$absences.y)))
addmargins(prop.table(table(d3$higher.y)))
addmargins(prop.table(table(d3$activities.y)))
addmargins(prop.table(table(d3$romantic.y)))

# Summary Stat Both

par(mfrow=c(1, 2))
barplot(table(Math_sup), main = " General Math support",names.arg=c("NO", "YES"))
barplot(table(Por_sup),main = " General Por Lang. support",names.arg=c("NO", "YES"))


counts1 <- table(d3$schoolsup.x)
counts2 <- table(d3$famsup.x)
counts3 <- table(d3$paid.x)
counts4 <- table(d3$schoolsup.y)
counts5 <- table(d3$famsup.y)
counts6 <- table(d3$paid.y)

par(mfrow=c(3, 2))

barplot(counts1, main="Math School Sup",col=c("darkblue","red"))
barplot(counts4, main="Por School Sup",col=c("darkblue","red"))
barplot(counts2, main="Math Family Sup",col=c("darkblue","red"))
barplot(counts5, main="Por Family Sup",col=c("darkblue","red"))
barplot(counts3, main="Math Paid Sup",col=c("darkblue","red"))
barplot(counts6, main="Por Paid Sup",col=c("darkblue","red"))


# create the new dataset with Academia and Lifestyle variables and remove outliers.

d4<-d3 %>% 
  select(sex, age, nursery, studytime.x, failures.x, schoolsup.x,
         famsup.x,activities.x,paid.x,higher.x,romantic.x,freetime.x,
         goout.x,Walc.x,Dalc.x,absences.x, G1.x,G2.x,G3.x,studytime.y,
         failures.y, schoolsup.y,famsup.y,activities.y,paid.y,higher.y,
         romantic.y,freetime.y,goout.y,Walc.y,Dalc.y,absences.y, G1.y,G2.y,G3.y)

summary(d4)
str(d4)



# Divide Math - Por

d4M<-d4 %>% 
  select(sex, age, nursery, studytime.x, failures.x, schoolsup.x,
         famsup.x,activities.x,paid.x,higher.x,romantic.x,freetime.x,
         goout.x,Walc.x,Dalc.x,absences.x, G1.x,G2.x,G3.x)

d4P<-d4 %>% 
  select(sex, age, nursery, studytime.y,failures.y, schoolsup.y,famsup.y,
         activities.y,paid.y,higher.y,romantic.y,freetime.y,goout.y,Walc.y,
         Dalc.y,absences.y, G1.y,G2.y,G3.y)

str(d4M)
str(d4P)

##Mathematics

# Plot variables (integers to be ploted as follows) dont forget pairs plot for continuous.
str(d4M)


#??? transform integers into Factors

d4M[,"studytime.x"]<-as.numeric(factor(d4M[,"studytime.x"]))
class(d4M$studytime.x)
plot(d4M$studytime.x, main="Math Studytime", xlab="Level of Studytime")

d4M[,"failures.x"]<-as.numeric(factor(d4M[,"failures.x"]))
class(d4M$failures.x)
plot(d4M$failures.x, main="Math Past Failures", xlab="Number of failures")

d4M[,"freetime.x"]<-as.numeric(factor(d4M[,"freetime.x"]))
class(d4M$freetime.x)
plot(d4M$freetime.x, main="Math Free Time", xlab="Level of Free Time")

d4M[,"goout.x"]<-as.numeric(factor(d4M[,"goout.x"]))
class(d4M$goout.x)
plot(d4M$goout.x, main="Math Go Out Time", xlab="Go Out Time")

d4M[,"Walc.x"]<-as.numeric(factor(d4M[,"Walc.x"]))
class(d4M$Walc.x)
plot(d4M$Walc.x, main="Math Weekend Alcohol Consuption", xlab="Level of consumption")

d4M[,"Dalc.x"]<-as.numeric(factor(d4M[,"Dalc.x"]))
class(d4M$Dalc.x)
plot(d4M$Dalc.x, main="Math Daily Alcohol Consuption", xlab="Level of consumption")

str(d4M)

d4M$age<-as.numeric(d4M$age)
d4M$absences.x<-as.numeric(d4M$absences.x)
d4M$G1.x<-as.numeric(d4M$G1.x)
d4M$G2.x<-as.numeric(d4M$G2.x)
d4M$G3.x<-as.numeric(d4M$G3.x)

### Plotting Maths ###
dev.off()

d4Mc<-d4M %>% 
  select(age,absences.x,G1.x,G2.x,G3.x)
pairs(d4Mc)
cor(d4Mc)

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(d4Mc)


boxplot(d4M$G3.x~d4M$sex, main= "Boxplot of Math Sex", xlab= "Sex", ylab="Grade")
# Light difference with slightly higher grade for boys.
boxplot(d4M$G3.x~d4M$age, main= "Boxplot of Math Age", xlab= "Age", ylab="Grade")
#Age seems to affect negatively to the grade as median decreases gradually until 19 yo were almost
# nobody pass the subject.
boxplot(d4M$G3.x~d4M$nursery, main= "Boxplot of Math Nursery", xlab= "Attended Nursery", ylab="Grade")
# Does not seem to be significant difference
boxplot(d4M$G3.x~d4M$studytime.x, main= "Boxplot of Math Studytime", xlab= "Level of Studytime", ylab="Grade")
# As more time spended studing seems to perform better but the difference does not seem relevant.
boxplot(d4M$G3.x~d4M$failures.x, main= "Boxplot of Math Failures", xlab= "n of previous failures", ylab="Grade")
# Having previous failed classes decreases the grade.
boxplot(d4M$G3.x~d4M$schoolsup.x, main= "Boxplot of Math School Support", ylab="Grade")
#School support has a less variance of grade for students who has it but it does not seem to improve the grade.
boxplot(d4M$G3.x~d4M$famsup.x, main= "Boxplot of Math Family Support", ylab="Grade")
# Yes and NO are almost the same.
boxplot(d4M$G3.x~d4M$paid.x, main= "Boxplot of Math Paid Support", ylab="Grade")
#Does not seem to be a big difference between yes and no.
boxplot(d4M$G3.x~d4M$activities.x, main= "Boxplot of Math Activities", ylab="Grade")
# grades for "Yes" are slightly higher.
boxplot(d4M$G3.x~d4M$higher.x, main= "Boxplot of Math Higher Education", ylab="Grade", xlab = "")
# There is a considerable difference between students who want to go to Higher Education or not.
boxplot(d4M$G3.x~d4M$romantic.x, main= "Boxplot of Math Romantic", ylab="Grade")
# NOt having a relationship helps slightly to have better marks.
boxplot(d4M$G3.x~d4M$freetime.x, main= "Boxplot of Math Freetime", xlab= "Level of Freetime", ylab="Grade")
# Level 2 on Freetime seems to be a bit higher but not much.
boxplot(d4M$G3.x~d4M$goout.x, main= "Boxplot of Math Goout", xlab= "Level of Goout", ylab="Grade")
# Having a mid level of going out has a positive effect on the G3 variable respect not going out at all
# or going out very frequently.
boxplot(d4M$G3.x~d4M$Walc.x, main= "Boxplot of Math Weekend alcohol", xlab= "Level of consumption", ylab="Grade")
# Does not seem to have a large effect but the range of grades is decreased by the highest grades.
boxplot(d4M$G3.x~d4M$Dalc.x, main= "Boxplot of Math Daily alcohol", xlab= "Level of consumption", ylab="Grade")
# IT affect on the highest grades but medians are almost the same. 
plot(d4M$G3.x~d4M$absences.x, main= "Grade by Absences", ylab="Grade", xlab="n of Absences")
# Grade has a negative trend 
plot(d4M$G3.x~d4M$G1.x, main= "Grade 1 vs Grade 3", ylab="Grade3", xlab="Grade 1")
# Positive Trend with some values=0
plot(d4M$G3.x~d4M$G2.x, main= "Grade 2 vs Grade 3", ylab="Grade3", xlab="Grade 2")
# Positive Trend with some values=0

par(mfrow=c(1,3))
boxplot(d4M$G3.x~d4M$schoolsup.x, ylab="Grade", xlab = "School Support")
boxplot(d4M$G3.x~d4M$famsup.x, main= "Mathematics Supports", ylab="Grade", xlab = "Family Support")
boxplot(d4M$G3.x~d4M$paid.x, ylab="Grade", xlab = "Paid Support")
dev.off()


#### Portuguese ####

# transform integers into Factors

d4P[,"studytime.y"]<-as.numeric(factor(d4P[,"studytime.y"]))
class(d4P$studytime.y)
plot(d4P$studytime.y, main="Port Studytime", xlab="Level of Studytime")

d4P[,"failures.y"]<-as.numeric(factor(d4P[,"failures.y"]))
class(d4P$failures.y)
plot(d4P$failures.y, main="Port Past Failures", xlab="-Number of failures")

d4P[,"freetime.y"]<-as.numeric(factor(d4P[,"freetime.y"]))
class(d4P$freetime.y)
plot(d4P$freetime.y, main="Port Free Time", xlab="Level of Free Time")

d4P[,"goout.y"]<-as.numeric(factor(d4P[,"goout.y"]))
class(d4P$goout.y)
plot(d4P$goout.y, main="Port Go Out Time", xlab="Go Out Time")

d4P[,"Walc.y"]<-as.numeric(factor(d4P[,"Walc.y"]))
class(d4P$Walc.y)
plot(d4P$Walc.y, main="Port Weekend Alcohol Consuption", xlab="Level of consumption")

d4P[,"Dalc.y"]<-as.numeric(factor(d4P[,"Dalc.y"]))
class(d4P$Dalc.y)
plot(d4P$Dalc.y, main="Port Daily Alcohol Consuption", xlab="Level of consumption")

str(d4P)

d4P$age<-as.numeric(d4P$age)
d4P$absences.y<-as.numeric(d4P$absences.y)
d4P$G1.y<-as.numeric(d4P$G1.y)
d4P$G2.y<-as.numeric(d4P$G2.y)
d4P$G3.y<-as.numeric(d4P$G3.y)




### Plotting Portuguese Language ###

d4Pc<-d4P %>% 
  select(age,absences.y,G1.y,G2.y,G3.y)
pairs(d4Pc)
cor(d4Pc)

chart.Correlation(d4Pc)


boxplot(d4P$G3.y~d4P$sex, main= "Boxplot of Portuguese Lang. Sex", xlab= "Sex", ylab="Grade")
# Light difference with slightly higher grade for girls.
boxplot(d4P$G3.y~d4P$age, main= "Boxplot of Portuguese Lang. Age", xlab= "Age", ylab="Grade")
#Age seems to affect positively to the grade as median increase gradually. Exception is for 19 and 22
# where both are lower than the rest.
boxplot(d4P$G3.y~d4P$nursery, main= "Boxplot of Portuguese Lang. Nursery", xlab= "Attended Nursery", ylab="Grade")
# Does not seem to be significant difference
boxplot(d4P$G3.y~d4P$studytime.y, main= "Boxplot of Portuguese Lang. Studytime", xlab= "Level of Studytime", ylab="Grade")
# As more time spended studing seems to perform better.
boxplot(d4P$G3.y~d4P$failures.y, main= "Boxplot of Portuguese Lang. Failures", xlab= "n of previous failures", ylab="Grade")
# Having previous failed classes decreases the grade.
boxplot(d4P$G3.y~d4P$schoolsup.y, main= "Boxplot of Portuguese Lang. School Support", ylab="Grade")
#School support has a less variance of grade for students who has it but it does not seem to improve the grade.
boxplot(d4P$G3.y~d4P$famsup.y, main= "Boxplot of Portuguese Lang. Family Support", ylab="Grade")
# Yes and NO are almost the same.
boxplot(d4P$G3.y~d4P$paid.y, main= "Boxplot of Portuguese Lang. Paid Support", ylab="Grade")
#Does not seem to be a big difference between yes and no.
boxplot(d4P$G3.y~d4P$activities.y, main= "Boxplot of Portuguese Lang. Activities", ylab="Grade")
# grades for "Yes" are slightly higher.
boxplot(d4P$G3.y~d4P$higher.y, main= "Boxplot of Portuguese Lang. Higher Education", ylab="Grade", xlab = "")
# There is a considerable difference between students who want to go to Higher Education or not.
boxplot(d4P$G3.y~d4P$romantic.y, main= "Boxplot of Portuguese Lang. Romantic", ylab="Grade")
# It does not affect much the fact of having or not a relationship.
boxplot(d4P$G3.y~d4P$freetime.y, main= "Boxplot of Portuguese Lang. Freetime", xlab= "Level of Freetime", ylab="Grade")
# Level 2 on Freetime seems to be a bit higher but not much.
boxplot(d4P$G3.y~d4P$goout.y, main= "Boxplot of Portuguese Lang. Goout", xlab= "Level of Goout", ylab="Grade")
# Having a low/mid level of going out has a positive effect on the G3 variable respect not going out at all
# or going out very frequently.
boxplot(d4P$G3.y~d4P$Walc.y, main= "Boxplot of Portuguese Lang. Weekend alcohol", xlab= "Level of consumption", ylab="Grade")
# Does not seem to have a large effect but the tendency is to decrease with the level of consumption.
boxplot(d4P$G3.y~d4P$Dalc.y, main= "Boxplot of Portuguese Lang. Daily alcohol", xlab= "Level of consumption", ylab="Grade")
# IT affect on the highest grades but medians are almost the same. 
plot(d4P$G3.y~d4P$absences.y, main= "Grade by Absences", ylab="Grade", xlab="n of Absences")
# Grade has a negative trend 
plot(d4P$G3.y~d4P$G1.y, main= "Grade 1 vs Grade 3", ylab="Grade3", xlab="Grade 1")
# Positive Trend with some values=0
plot(d4P$G3.y~d4P$G2.y, main= "Grade 2 vs Grade 3", ylab="Grade3", xlab="Grade 2")
# Positive Trend with some values=0 for G3


### Compraison ###
par(mfrow=c(1,2))

#CORRELATION
#install.packages("corrplot")
library(corrplot)

chart.Correlation(d4Mc)
chart.Correlation(d4Pc)

Cor_Math<-cor(data.frame(model.matrix(~.-1, data=(d4M))))
Cor_Port<-cor(data.frame(model.matrix(~.-1, data=(d4P))))
dev.off()
corrplot(Cor_Math, method="color", type="upper",order="hclust",addCoef.col = "black", 
         tl.cex = 0.75,number.cex = 0.6,cl.cex = 1,diag=FALSE)
corrplot(Cor_Port,method="color", type="upper",order="hclust",addCoef.col = "black", 
         tl.cex = 0.75,number.cex = 0.6,cl.cex = 1,diag=FALSE)


#  SEX
boxplot(d4M$G3.x~d4M$sex, main= "Mathematics ", xlab= "Gender", ylab="Grade")
boxplot(d4P$G3.y~d4P$sex, main= "Portuguese Lang. ", xlab= "Gender",ylab="Grade")

#  AGE
boxplot(d4M$G3.x~d4M$age, main= "Mathematics", xlab= "Age", ylab="Grade")
boxplot(d4P$G3.y~d4P$age, main= "Portuguese Lang.", xlab= "Age", ylab="Grade")

#  STDYTIME
boxplot(d4M$G3.x~d4M$studytime.x, main= "Mathematics", xlab= "Level of Studytime", ylab="Grade")
boxplot(d4P$G3.y~d4P$studytime.y, main= "Portuguese Lang.", xlab= "Level of Studytime", ylab="Grade")

#  FAILUES
boxplot(d4M$G3.x~d4M$failures.x, main= "Mathematics", xlab= "n of previous failures", ylab="Grade")
boxplot(d4P$G3.y~d4P$failures.y, main= "Portuguese Lang.", xlab= "n of previous failures", ylab="Grade")

#  ROMANTIC
boxplot(d4M$G3.x~d4M$romantic.x, main= "Boxplot of Math Romantic", ylab="Grade")
boxplot(d4P$G3.y~d4P$romantic.y, main= "Boxplot of Portuguese Lang. Romantic", ylab="Grade")




################## Analysis Mathematics Regression ##############################
#install.packages("car")
#install.packages("mlbench")
#install.packages("caret")

# load libraries
library(mlbench)
library(caret)
library(car)

d4M<-d4M %>% 
  mutate(studytime.x=as.factor(studytime.x),
         failures.x=as.factor(failures.x),
         freetime.x =as.factor(freetime.x ),
         goout.x=as.factor(goout.x),
         Walc.x =as.factor(Walc.x),
         Dalc.x =as.factor(Dalc.x ))
str(d4M)

inTrain = createDataPartition(y = d4M$G3.x, p = .80, list = FALSE)
Mathtrain = d4M[inTrain,]
Mathtest = d4M[-inTrain,] 

str(Mathtrain)
str(Mathtest)


MathReg<-lm(G3.x~.,data=Mathtrain)
summary(MathReg)
plot(MathReg)

MathReg1<-lm(G3.x~.-famsup.x,data=Mathtrain)
summary(MathReg1)
plot(MathReg1)

MathReg2<-lm(G3.x~.-famsup.x-paid.x-freetime.x-goout.x,data=Mathtrain)
summary(MathReg2)
plot(MathReg2)

MathReg3<-lm(G3.x~.-famsup.x-paid.x-freetime.x-goout.x-sex-age-nursery,data=Mathtrain)
summary(MathReg3)
plot(MathReg3)

MathReg4<-lm(G3.x~.-famsup.x-paid.x-freetime.x-goout.x-sex-age-nursery,data=Mathtrain)
summary(MathReg4)
plot(MathReg4)


MathReg5<-lm(G3.x~.- freetime.x - nursery - sex - paid.x - 
               studytime.x - famsup.x - goout.x - age - schoolsup.x - Walc.x - 
               Dalc.x - factor(failures.x, exclude = c(3,4)), data = Mathtrain)
summary(MathReg5)
plot(MathReg5)

MathReg6<-lm(G3.x~ G2.x + G1.x + absences.x+romantic.x,data=Mathtrain)
summary(MathReg6)
plot(MathReg6)

par(mfrow=c(2,1))

anova(MathReg,MathReg1,MathReg2,MathReg3,MathReg4,MathReg6)
AIC(MathReg,MathReg1,MathReg2,MathReg3,MathReg4,MathReg5,MathReg6)
BIC(MathReg,MathReg1,MathReg2,MathReg3,MathReg4,MathReg5,MathReg6)
vif(MathReg6)
# Assumptions of normality are not met in regression. The best results are accomplished when removing all
# non academic varaibles except romantic situation.
library(stats)

pred<-predict(MathReg6,Mathtest) 
round_pred<-round(pred)
# Predict Accuracy
sum(round_pred==Mathtest$G3.x)/72*100
#34.72%

# Confusion Matrix
table(round_pred,Mathtest$G3.x)

################## Analysis Portuguese Regression ##############################
d4P<-d4P %>% 
  mutate(studytime.y=as.factor(studytime.y),
         failures.y=as.factor(failures.y),
         freetime.y =as.factor(freetime.y),
         goout.y=as.factor(goout.y),
         Walc.y =as.factor(Walc.y),
         Dalc.y =as.factor(Dalc.y ))
str(d4P)

inTrain = createDataPartition(y = d4P$G3.y, p = .80, list = FALSE)
Portrain = d4P[inTrain,]
Porttest = d4P[-inTrain,] 

str(Portrain)
str(Porttest)


PortReg<-lm(G3.y~.,data=Portrain)
summary(PortReg)
plot(PortReg)

PortReg1<-lm(G3.y~.-freetime.y-age,data=Portrain)
summary(PortReg1)
plot(PortReg1)

PortReg2<-lm(G3.y~.-freetime.y-nursery-sex-paid.y-studytime.y-famsup.y-age-schoolsup.y-Walc.y-Dalc.y
              +activities.y*higher.y+absences.y*G2.y,data=Portrain)
summary(PortReg2)
plot(PortReg2)

PortReg3<-lm(G3.y~ G2.y +higher.y*activities.y+failures.y,data=Portrain)
summary(PortReg3)
plot(PortReg3)


PortReg4<-lm(G3.y~ G2.y+factor(failures.y, exclude = c(4)),data=Portrain)
summary(PortReg4)
plot(PortReg4)

par(mfrow=c(2,1))

anova(PortReg,PortReg1,PortReg2,PortReg3,PortReg4)
AIC(PortReg,PortReg1,PortReg2,PortReg3,PortReg4)
BIC(PortReg,PortReg1,PortReg2,PortReg3,PortReg4)
vif(PortReg4)



# Assumptions of normality are not met in regression. 
#Surprisingly, G1.y is not statistically significant 

pred<-predict(PortReg4,Porttest) 
pred
predround<-round(pred)

predround[is.na(predround)] <- 0

plot(predround,Porttest$G3.y)
abline(a=0,b=1)

summary(pred)
summary(Porttest$G3.y)

lmtest<-lm(pred~Porttest$G3.y)
summary(lmtest)

sum((predround==Porttest$G3.y)==TRUE)
sum((predround==Porttest$G3.y)==FALSE)
table()

table1<-data.frame(predround,Porttest$G3.y)
table1

sum(predround==Porttest$G3.y)/73*100
#predict accuracy of 52.05%

# check predicting error
with(Porttest, table(pred, G3.y))
















######################################## CARET REGRESSION MATHEMATICS ####################################



# Create test and training data

inTrain = createDataPartition(y = d4M$G3.x, p = .80, list = FALSE)
training = d4M[inTrain,]
testing = d4M[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)

## control and seed
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

# Accuracy
metric <- "Accuracy"

# Preproces
preProcess=c("center", "scale")

### models

# Logistic Regression
set.seed(seed)
fit.glm <- train(G3.x~., data=dataset, method="glm", trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.x~., data=dataset, method="svmRadial", preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.x~., data=dataset, method="knn", preProc=c("center", "scale"), trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.x~., data=dataset, method="rpart", trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.x~., data=dataset, method="treebag", trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.x~., data=dataset, method="rf", trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.x~., data=dataset, method="gbm", trControl=control, verbose=FALSE)
?train

#BayesNaive cannot be run for regression

### 

results <- resamples(list(logistic=fit.glm,SVM=fit.svmRadial,KNN=fit.knn,BAGGED=fit.treebag, rf=fit.rf, BOOSTED=fit.gbm))
# Table comparison
summary(results)


### 
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


## nice plot #
#install.packages("rattle")
#install.packages("rpart")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
dev.off()

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

# PREDICT 

Pred<-predict(fit.rf, newdata = testing)
PredRound<-round(Pred)
sum(PredRound==testing$G3.x)/72*100

# Prediction Accuracy of 44.44%













######################################## CARET REGRESSION PORTUGUESE #####################################

inTrain = createDataPartition(y = d4P$G3.y, p = .80, list = FALSE)
training = d4P[inTrain,]
testing = d4P[-inTrain,] 


# rename dataset to keep code below generic
dataset <- training

str(dataset)


### models

# Logistic Regression
set.seed(seed)
fit.glm <- train(G3.y~., data=dataset, method="glm", trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.y~., data=dataset, method="svmRadial", preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.y~., data=dataset, method="knn", preProc=c("center", "scale"), trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.y~., data=dataset, method="rpart", trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.y~., data=dataset, method="treebag", trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.y~., data=dataset, method="rf", trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.y~., data=dataset, method="gbm", trControl=control, verbose=FALSE)


#BayesNaive cannot be run for regression

### 

results <- resamples(list(logistic=fit.glm,SVM=fit.svmRadial,KNN=fit.knn,BAGGED=fit.treebag, rf=fit.rf, BOOSTED=fit.gbm))
# Table comparison
summary(results)


### 
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)


#PREDICT

Pred<-predict(fit.rf, newdata = testing)
PredRound<-round(Pred)
sum(PredRound==testing$G3.y)/73*100

# Prediction Accuracy of 54.79%















######################################## CARET BINARY MATHEMATICS #######################################

# Create Binary G3.x
Binaryd4M<-d4M
Binaryd4M$G3.x<-factor(ifelse(Binaryd4M$G3.x >= 10, "Pass", "Fail"))

str(Binaryd4M)
# 2= Pass , 1= Fail

#Create Train and Test Datasets

inTrain = createDataPartition(y = Binaryd4M$G3.x, p = .80, list = FALSE)
training = Binaryd4M[inTrain,]
testing = Binaryd4M[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)


# MODELS

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(G3.x~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(G3.x~., data=dataset, method="glm", metric=metric, trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.x~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.x~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(G3.x~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.x~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.x~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.x~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.x~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(lda=fit.lda, logistic=fit.glm, svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Boosting best results at the price of long procesing time.

# LOGISTIC, CART, NB, KNN,SVM and LDA lower performance. RF, CART and LDA have outliers.

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

#PREDICT

Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 89.04%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 90.41%

#. Random forest has a better accuracy when compared to the test data.  










######################################## CARET BINARY PORTUGUESE #########################################

# Create Binary G3.y
Binaryd4P<-d4P
Binaryd4P$G3.y<-factor(ifelse(Binaryd4P$G3.y >= 10, "Pass", "Fail"))

str(Binaryd4P)
# 2= Pass , 1= Fail

inTrain = createDataPartition(y = Binaryd4P$G3.y, p = .80, list = FALSE)
training = Binaryd4P[inTrain,]
testing = Binaryd4P[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)


# MODELS

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(G3.y~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(G3.y~., data=dataset, method="glm", metric=metric, trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.y~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.y~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(G3.y~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.y~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.y~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.y~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.y~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(lda=fit.lda, logistic=fit.glm, svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Classification Tree best results.

# NB, KNN,SVM, GLM and LDA poor performance.

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

#PREDICT

Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 98.63%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 95.89%

Pred<-predict(fit.cart, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 95.89%

#???When compartng predictions with test dataset, Boosting is the most accurate technique. Random Forest does not 
# improve the accuracy for the Decision Tree.


# Portuguese Language has a Higher Rate of Pass in comparison with Mathematics. The Binary division does not
# gives much information as almost all the students pass both subjects. Will create a new 4 Level division
# to better clasify the students and create 2 groups of performance below and above 10.
# Groups will be: 
# High Fail 0<7
# Low Fail  7<10
# Low Pass 10<13
# High Pass 13<20

# High Fail and High Pass are bigger groups as it is expected to have less students as more extreme is the value.









######################################## CARET 4LEVEL MATHEMATICS #######################################

# Create 4Levels G3.x
Levelsd4M<-d4M
Levelsd4M$G3.x<-cut(Levelsd4M$G3.x, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

Levelsd4M$G3.x
str(Levelsd4M)
# 1=HighFail, 2=LowFail, 3=LowPass, 4=HighPass

#Create Training and Test datasets
inTrain = createDataPartition(y = Levelsd4M$G3.x, p = .80, list = FALSE)
training = Levelsd4M[inTrain,]
testing = Levelsd4M[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)

# MODELS

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(G3.x~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.x~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.x~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(G3.x~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.x~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.x~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.x~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.x~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(lda=fit.lda, svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


# Logistic has been deleted as predictive variables are not continuous, therefore cannot run the model.
# Boosting and RF takes a long processing time +10 sec
# Best Performer is RF along with GBM. 

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 84.93%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 80.82%

Pred<-predict(fit.cart, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 71.23%

# Summary shows better accuracy on RF but GBM has the best Accuracy when compared with the test data.
summary(fit.gbm)
# GBM main influence variables are: G2,G1,Absences, Age, Walc, Activities and Failures.

# Save predictive model for Shiny purposes
# saveRDS(fit.gbm,file="Math_Pred.rda")









######################################## CARET 4LEVEL PORTUGUESE #########################################
# Create 4Levels G3.y
Levelsd4P<-d4P
Levelsd4P$G3.y<-cut(Levelsd4P$G3.y, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

Levelsd4P$G3.y
str(Levelsd4P)
# 1=HighFail, 2=LowFail, 3=LowPass, 4=HighPass


#Create Training and Test datasets
inTrain = createDataPartition(y = Levelsd4P$G3.y, p = .80, list = FALSE)
training = Levelsd4P[inTrain,]
testing = Levelsd4P[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)


# MODELS

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(G3.y~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(G3.y~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(G3.y~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(G3.y~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(G3.y~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.y~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.y~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.y~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(lda=fit.lda, svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


# Logistic has been deleted as predictive variables are not continuous, therefore cannot run the model.
# Boosting and RF takes a long processing time +10 sec
# Best Performer is RF 


fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 79.45%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 76.71%

Pred<-predict(fit.cart, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 83.56%

# Summary shows better accuracy on RF but Classification Tree has the best Accuracy when compared with the test data.
# Classification Tree relies on G2, G1, Goout, Higher, Walc(5), Freetime (5)

summary(fit.gbm)
# GBM main influence variables are: G2,G1,Absences, Age, Goout, Schoolsup and Failures.

# Save predictive model for Shiny purposes
#  saveRDS(fit.rf,file="Port_Pred.rda")


##### Given the results, only a few variables are relevant on the ML techniques. Therefore, will reduce the data
##### to be more specific when requesting information from the APP, reduce to MAX 10 variables between Mathematics
##### and Portuguese.
##### Binary is the most accurate model but it lacks of information for Decision Making.









######################################## MATHEMATICS (4 LEVEL) WITHOUT G1 and G2 #######################################

# Create 4Levels G3.x
RedMath<-d4M %>%
  select(-G1.x,-G2.x)

LevelsRedMath<-RedMath
LevelsRedMath$G3.x<-cut(LevelsRedMath$G3.x, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

LevelsRedMath$G3.x
str(LevelsRedMath)
# 1=HighFail, 2=LowFail, 3=LowPass, 4=HighPass

#Create Training and Test datasets
inTrain = createDataPartition(y = LevelsRedMath$G3.x, p = .80, list = FALSE)
training = LevelsRedMath[inTrain,]
testing = LevelsRedMath[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)


# MODELS

# CART
set.seed(seed)
fit.cart <- train(G3.x~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.x~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.x~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.x~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(cart=fit.cart,bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

str(dataset)


Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 31.50%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 35.61%

Pred<-predict(fit.cart, newdata = testing)
sum(Pred==testing$G3.x)/73*100
# Prediction Accuracy of 30.13%

# Predictive Model with better accuracy is RF

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)

## Without G1 and G2 the most influence variables that can be known by teachers are: 
# Absences, Age, Activities, Paid, Studytime, Famsup.








######################################## PORTUGUESE (4 LEVEL) WITHOUT G1 and G2 #########################################
# Create 4Levels G3.y
RedPort<-d4P %>%
  select(-G1.y,-G2.y)

LevelsRedPort<-RedPort
LevelsRedPort$G3.y<-cut(LevelsRedPort$G3.y, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

LevelsRedPort$G3.y
str(LevelsRedPort)
# 1=HighFail, 2=LowFail, 3=LowPass, 4=HighPass


#Create Training and Test datasets
inTrain = createDataPartition(y = LevelsRedPort$G3.y, p = .80, list = FALSE)
training = LevelsRedPort[inTrain,]
testing = LevelsRedPort[-inTrain,] 

# rename dataset to keep code below generic
dataset <- training
str(dataset)

# MODELS


# CART
set.seed(seed)
fit.cart <- train(G3.y~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(G3.y~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(G3.y~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(G3.y~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Check models
results <- resamples(list(cart=fit.cart,bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


Pred<-predict(fit.gbm, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 50.68%

Pred<-predict(fit.rf, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 47.94%

Pred<-predict(fit.cart, newdata = testing)
sum(Pred==testing$G3.y)/73*100
# Prediction Accuracy of 42.46%

fancyRpartPlot(fit.cart$finalModel)
summary(fit.cart)
summary(fit.gbm)
summary(fit.rf)

## Without G1 and G2 the most influence variables are: 
# Absences, Age,Sex, Schoolsup, Romantic, Activities, Famsup, Higher, Failures 










############### Create the best predictive models fro both subjects ################# 
##################### and save them for Shiny purposes ####################### 


Export_Port<-Levelsd4P %>% 
  select(age,sex,G1.y,G2.y,G3.y,absences.y,failures.y,schoolsup.y,paid.y, famsup.y, activities.y, studytime.y, higher.y)

#New_Port<-LevelsPort_Model %>% 
#  mutate(age=as.factor(age),
#         G1.y=as.factor(G1.y),
#         G2.y=as.factor(G2.y),
#         absences.y=as.factor(absences.y))

str(Export_Port)
write.csv(Export_Port,"Export_Port.csv")


Export_Math<-Levelsd4M %>% 
  select(age,sex,G1.x,G2.x,G3.x,absences.x,failures.x,schoolsup.x,paid.x, famsup.x, activities.x, studytime.x, higher.x)

str(Export_Math)

write.csv(Export_Math,"Export_Math.csv")


# Predict models

# RF for Math
set.seed(seed)
fit.rf.Math <- train(G3.x~., data=New_Math, method="rf", metric=metric, trControl=control, verbose=FALSE)

saveRDS(fit.rf.Math, file="Math_Pred.rda")

#RF for Portuguese
fit.rf.Port <- train(G3.y~., data=New_Port, method="rf", metric=metric, trControl=control)
summary(fit.rf.Port)

saveRDS(fit.rf.Port, file="Port_Pred.rda")









                              
############################################## PCA ANALYSIS MATHEMATICS ###############################################
#install.packages("ggfortify")
library(ggfortify)

New_Math<-Levelsd4M %>% 
  select(age,sex,G1.x,G2.x,G3.x,absences.x,failures.x,schoolsup.x,paid.x, famsup.x, activities.x, studytime.x, higher.x)

str(New_Math)

New_Math<-New_Math %>% 
  mutate(schoolsup.x=as.numeric(schoolsup.x),
         activities.x=as.numeric(activities.x),
         famsup.x=as.numeric(famsup.x),
         sex=as.numeric(sex),
         failures.x=as.numeric(failures.x),
         paid.x=as.numeric(paid.x),
         studytime.x=as.numeric(studytime.x),
         higher.x=as.numeric(higher.x))


str(New_Math)

New_Math1<-New_Math %>% 
  select(-G3.x)


cov(New_Math1)
cor(New_Math1)


New_Math1_std<-scale(New_Math1)
cov(New_Math1_std)

d <- dist(New_Math1_std, method = "euclidean")


# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
?hclust
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = 0)

k = 4
n = nrow(New_Math1_std)
MidPoint = (hc1$height[n-k] + hc1$height[n-k+1]) / 2
abline(h = MidPoint, lty=2, col="red")


Math_clusters <- kmeans(New_Math1_std, centers = 4)

Math_clusters
barplot(Math_clusters$centers[,1], main = " Mathematics Cluster size")

barplot(Math_clusters$centers[1,], main = " Cluster 1 variable breakdown")
barplot(Math_clusters$centers[2,])
barplot(Math_clusters$centers[3,])
barplot(Math_clusters$centers[4,], main = " Cluster 4 variable breakdown")

pairs(New_Math1, col = Math_clusters$cluster)

pca<-prcomp(New_Math1_std)
summary(pca)
barplot(pca$rotation[,2])
barplot(pca$rotation[,1])

plot(pca,type="l")

plot(pca$x[,1],pca$x[,2],xlab="PC 1",ylab="PC 2",col = New_Math$G3.x)
plot(pca$x[,1],pca$x[,2],xlab="PC 1",ylab="PC 2",col = Math_clusters$cluster)


PCA_Math<-autoplot(pca, data = New_Math, colour ="G3.x",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5, loadings.label.colour="black")

PCA_Math+
  scale_colour_manual(values=c("HighPass" = "green", "LowPass" = "yellow", "LowFail"="orange", "HighFail"="red"))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
 
  


############################################## PCA ANALYSIS PORTUGUESE ###############################################


New_Port<-Levelsd4P %>% 
  select(age,sex,G1.y,G2.y,G3.y,absences.y,failures.y,schoolsup.y,paid.y, famsup.y, activities.y, studytime.y, higher.y)

str(New_Port)

New_Port<-New_Port %>% 
  mutate(schoolsup.y=as.numeric(schoolsup.y),
         activities.y=as.numeric(activities.y),
         famsup.y=as.numeric(famsup.y),
         sex=as.numeric(sex),
         failures.y=as.numeric(failures.y),
         paid.y=as.numeric(paid.y),
         studytime.y=as.numeric(studytime.y),
         higher.y=as.numeric(higher.y))


str(New_Port)

New_Port1<-New_Port %>% 
  select(-G3.y)


cov(New_Port1)
cor(New_Port1)


New_Port1_std<-scale(New_Port1)
cov(New_Port_std)

d <- dist(New_Port1_std, method = "euclidean")


# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
?hclust
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = 0)

k = 4
n = nrow(New_Port1_std)
MidPoint = (hc1$height[n-k] + hc1$height[n-k+1]) / 2
abline(h = MidPoint, lty=2, col="red")




Port_clusters <- kmeans(New_Port1_std, centers = 4)

Port_clusters

barplot(Port_clusters$centers[,1])

barplot(Port_clusters$centers[1,])
barplot(Port_clusters$centers[2,])
barplot(Port_clusters$centers[3,])
barplot(Port_clusters$centers[4,])

pairs(New_Port1, col = Port_clusters$cluster)

pca<-prcomp(New_Port1_std)
summary(pca)
barplot(pca$rotation[,2])
barplot(pca$rotation[,1])

plot(pca,type="l")

plot(pca$x[,1],pca$x[,2],xlab="PC 1",ylab="PC 2",col = New_Port$G3.y)
plot(pca$x[,1],pca$x[,2],xlab="PC 1",ylab="PC 2",col = Port_clusters$cluster)

PCA_Port<-autoplot(pca, data = New_Port, colour ="G3.y",
                   loadings = TRUE, loadings.colour = 'blue',
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.colour="black")

PCA_Port+
  scale_colour_manual(values=c("HighPass" = "green", "LowPass" = "yellow", "LowFail"="orange", "HighFail"="red"))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())







#Export Final Data for Shiny Project
write.csv(New_Port,'Portuguese_Language.csv')

write.csv(New_Math,'Mathematics.csv')






         ################################ SAND BOX ########################################

#######  NMDS TRYOUT (https://mb3is.megx.net/gustame) ######

New_Port<-LevelsPort_Model %>% 
  select(G3.y,G2.y,absences.y,age,schoolsup.y,romantic.y,activities.y,famsup.y)

New_Port<-LevelsPort_Model %>% 
  mutate(schoolsup.y=as.numeric(schoolsup.y),
         activities.y=as.numeric(activities.y),
         famsup.y=as.numeric(famsup.y),
         sex=as.numeric(sex),
         failures.y=as.numeric(failures.y),
         paid.y=as.numeric(paid.y),
         studytime.y=as.numeric(studytime.y))

  New_Port<-New_Port %>% 
  mutate(absences.y=absences.y^1/8)

str(New_Port)
summary(New_Port)



#install.packages("vegan")
library(vegan)

New_Port1<-New_Port %>% 
  select(-G3.y)

fm<-metaMDS(New_Port1,autotransform = FALSE)

fm
stressplot(fm)

plot(fm)

data.scores <- as.data.frame(scores(fm))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$G3.y <- New_Port$G3.y  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(fm, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

#to grouop Grading groups as a polygon shape in ggplot
HighPass <- data.scores[data.scores$G3.y == "HighPass", ][chull(data.scores[data.scores$G3.y == 
                                                                              "HighPass", c("NMDS1", "NMDS2")]), ]  # hull values for High Pass
LowPass <- data.scores[data.scores$G3.y == "LowPass", ][chull(data.scores[data.scores$G3.y == 
                                                                            "LowPass", c("NMDS1", "NMDS2")]), ]  # hull values for Low Pass
LowFail <- data.scores[data.scores$G3.y == "LowFail", ][chull(data.scores[data.scores$G3.y == 
                                                                            "LowFail", c("NMDS1", "NMDS2")]), ]  # hull values for Low Fail
HighFail <- data.scores[data.scores$G3.y == "HighFail", ][chull(data.scores[data.scores$G3.y == 
                                                                              "HighFail", c("NMDS1", "NMDS2")]), ]  # hull values for High Fail
hull.data <- rbind(HighPass, LowPass, LowFail, HighFail)  #combine 4 groups
hull.data

ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=G3.y),size=3, alpha = 0.5) + # add the point markers
  scale_colour_manual(values=c("HighPass" = "green", "LowPass" = "yellow", "LowFail"="orange", "HighFail"="red")) + #manual colours for date points
  scale_fill_manual(values=c("HighPass" = "green", "LowPass" = "yellow", "LowFail"="orange", "HighFail"="red")) + #manual colours for fill polygons
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),fontface="bold", size=4) +  # add the species labels
  coord_equal() + #important for the dimension of the NMDS
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position="top")





### Classification Tree: 4L Mathematics ###

#install.packages("ISLR")
#install.packages("tree")

library(ISLR)
library(tree)

# Select Main decision variables
Math_Model<-d4M %>% 
  select(age,sex,G1.x,G2.x,G3.x,absences.x,failures.x,schoolsup.x,paid.x, famsup.x, activities.x, studytime.x, higher.x)



# Create G3 Levels
LevelsMath_Model<-Math_Model
LevelsMath_Model$G3.x<-cut(LevelsMath_Model$G3.x, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

str(LevelsMath_Model)
## Classification Tree ##
set.seed(seed)
train <- sample(1:nrow(LevelsMath_Model), 297)
# 369 observations, train = 297 observ.
tree.Math <- tree(G3.x~., LevelsMath_Model, subset=train)
plot(tree.Math)
text(tree.Math, pretty=0)
tree.Math


#Predict
tree.pred <- predict(tree.Math, LevelsMath_Model[-train,], type="class")
# check predicting error
with(LevelsMath_Model[-train,], table(tree.pred, G3.x))
(13+12+16+18)/72
# Predict shows a result of 86.11% 


## Bagging  ###
library(ipred)
bagging.Math <- bagging(G3.x~., LevelsMath_Model, subset=train, coob = T)
#Predict Bagging
bagging.pred <- predict(bagging.Math, LevelsMath_Model[-train,], type="class")
# check predicting error
with(LevelsMath_Model[-train,], table(bagging.pred, G3.x))
(11+1+1)/72
# Confusion Matrix error, not doing proper diagonal. Find alternative (Jack)
sum(bagging.pred==LevelsMath_Model[-train,]$G3.x)/72*100

# Predict shows a result of 84.72% 

## Random Forest ##
library("randomForest")

RF.Math <- randomForest(G3.x~., LevelsMath_Model, subset=train)
#Plot RAndom FOrest

plot(RF.Math)
print(RF.Math)

#Predict Random Forest
RF.pred <- predict(RF.Math, LevelsMath_Model[-train,], type="response")
# check predicting error
table_Math_RF<-with(LevelsMath_Model[-train,], table(RF.pred, G3.x))
accuracy_Test_RF <- sum(diag(table_Math_RF)) / sum(table_Math_RF)
print(paste('Accuracy for Math test', accuracy_Test_RF*100))

# Predict shows a result of 81.94%


## Boosting ##
library("gbm")
?gbm
Boosting.Math <- gbm(G3.x~., data = LevelsMath_Model[train,], distribution = "gaussian", n.trees =10000, shrinkage = 0.01, interaction.depth = 4)
#Predict Boosting
Boosting.pred <- predict(Boosting.Math, LevelsMath_Model[-train,], n.trees =10000,type="response")
# check predicting error
Math_Roun<-round(Boosting.pred)
with(LevelsMath_Model[-train,], table(Math_Roun, G3.x))
(11+15+16+19)/72*100
# Predict shows a result of 84.72%

# Boosting has the same accuracy than the Classification Tree, other improvement techniques have a lower performance



### Classification Tree: 4L Portuguese ###


# Select Main decision variables
Port_Model<-d4P %>% 
  select(age,sex,G1.y,G2.y,G3.y,absences.y,failures.y,schoolsup.y,paid.y, famsup.y, activities.y, studytime.y, higher.y)

# Create G3 Levels
LevelsPort_Model<-Port_Model
LevelsPort_Model$G3.y<-cut(LevelsPort_Model$G3.y, br=c(-1,7,10,13,20), labels = c("HighFail","LowFail","LowPass","HighPass"))

## Classification Tree ##
set.seed(seed)
train <- sample(1:nrow(LevelsPort_Model), 297)
# 369 observations, train = 297 observ.
tree.Port <- tree(G3.y~., LevelsPort_Model, subset=train)
plot(tree.Port)
text(tree.Port, pretty=0)
tree.Port


#Predict
tree.pred <- predict(tree.Port, LevelsPort_Model[-train,], type="class")
# check predicting error
with(LevelsPort_Model[-train,], table(tree.pred, G3.y))
(1+9+28+17)/72*100
# Predict shows a result of 76.38% 


## Bagging  ###
library(ipred)
?bagging
bagging.Port <- bagging(G3.y~., LevelsPort_Model, subset=train, coob=T)
#Predict Bagging
bagging.pred <- predict(bagging.Port, LevelsPort_Model[-train,], type="class")
# check predicting error
with(LevelsPort_Model[-train,], table(bagging.pred, G3.y))
(1+1+4)/72*100
# Confusion Matrix error, not doing proper diagonal. Find alternative (Jack)
sum(bagging.pred==LevelsPort_Model[-train,]$G3.y)/72*100
# Predict shows a result of 84.72% 

## Random Forest ##
library("randomForest")

RF.Port <- randomForest(G3.y~., LevelsPort_Model, subset=train)
#Plot RAndom FOrest

plot(RF.Port)
print(RF.Port)

#Predict Random Forest
RF.pred <- predict(RF.Port, LevelsPort_Model[-train,], type="response")
# check predicting error
table_Port_RF<-with(LevelsPort_Model[-train,], table(RF.pred, G3.y))
accuracy_Test_RF <- sum(diag(table_Port_RF)) / sum(table_Port_RF)
print(paste('Accuracy for Math test', accuracy_Test_RF*100))

# Predict shows a result of 80.55%


## Boosting ##
library("gbm")
?gbm
Boosting.Port <- gbm(G3.y~., data = LevelsPort_Model[train,], distribution = "gaussian", n.trees =10000, shrinkage = 0.01, interaction.depth = 4)
#Predict Boosting
Boosting.pred <- predict(Boosting.Port, LevelsPort_Model[-train,], n.trees =10000,type="response")
# check predicting error
Port_Roun<-round(Boosting.pred)
with(LevelsPort_Model[-train,], table(Port_Roun, G3.y))
(1+11+22+18)/72*100
# Predict shows a result of 72.22%



# Bagging has the highest accuracy rate and the worst performer is Boosting.

