#Aleksi Tarkkonen, 10.11.2022, Assignment 3

#Read both student-mat.csv and student-por.csv into R (from the data folder) and explore 
#the structure and dimensions of the data. (1 point)

library(dplyr)
library(ggplot2)

por<-as.data.frame(student.por)
math<-as.data.frame(student.mat)
names(math)<-math[1,]
names(por)<-por[1,]
math<-math[-1,]
por<-por[-1,]
structure(math)
structure(por)
dim(math) #395 observations with 33 names
dim(por) #649 observations with 33 names

#Join the two data sets using all other variables than "failures", "paid", "absences", "G1", "G2",
#"G3" as (student) identifiers. Keep only the students present in both data sets. 
#Explore the structure and dimensions of the joined data. (1 point)

free_cols <- c("failures","paid","absences","G1","G2","G3")
join_cols <- setdiff(colnames(por), free_cols)
math_por <- inner_join(math, por, by = join_cols, suffix = c(".math", ".por"))
structure(math_por)
dim(math_por) #370 observations with 33 names

#Get rid of the duplicate records in the joined data set. Either a) copy the solution 
#from the exercise "3.3 The if-else structure" to combine the 'duplicated' answers in 
#the joined data, or b) write your own solution to achieve this task. (1 point)

math_por
math_por$age<-as.numeric(math_por$age)
math_por$Medu<-as.numeric(math_por$Fedu)
math_por$traveltime<-as.numeric(math_por$traveltime)
math_por$studytime<-as.numeric(math_por$studytime)
math_por$failures.math<-as.numeric(math_por$failures.math)
math_por$famrel<-as.numeric(math_por$famrel)
math_por$freetime<-as.numeric(math_por$freetime)
math_por$goout<-as.numeric(math_por$goout)
math_por$Dalc<-as.numeric(math_por$Dalc)
math_por$Walc<-as.numeric(math_por$Walc)
math_por$health<-as.numeric(math_por$health)
math_por$absences.math<-as.numeric(math_por$absences.math)
math_por$G1.math<-as.numeric(math_por$G1.math)
math_por$G2.math<-as.numeric(math_por$G2.math)
math_por$G3.math<-as.numeric(math_por$G3.math)
math_por$failures.por<-as.numeric(math_por$failures.por)
math_por$absences.por<-as.numeric(math_por$absences.por)
math_por$G1.por<-as.numeric(math_por$G1.por)
math_por$G2.por<-as.numeric(math_por$G2.por)
math_por$G3.por<-as.numeric(math_por$G3.por)









for(col_name in free_cols) {
  two_cols <- select(math_por, starts_with(col_name))
  first_col <- select(two_cols, 1)[[1]]
  if(is.numeric(first_col)) {
    alc[col_name] <- round(rowMeans(two_cols))
  } else {
    alc[col_name] <- first_col
  }
}
alc$age<-as.numeric(alc$age)
alc$Medu<-as.numeric(alc$Fedu)
alc$traveltime<-as.numeric(alc$traveltime)
alc$studytime<-as.numeric(alc$studytime)
alc$failures.math<-as.numeric(alc$failures.math)
alc$famrel<-as.numeric(alc$famrel)
alc$freetime<-as.numeric(alc$freetime)
alc$goout<-as.numeric(alc$goout)
alc$Dalc<-as.numeric(alc$Dalc)
alc$Walc<-as.numeric(alc$Walc)
alc$health<-as.numeric(alc$health)
alc$absences.math<-as.numeric(alc$absences.math)
alc$G1.math<-as.numeric(alc$G1.math)
alc$G2.math<-as.numeric(alc$G2.math)
alc$G3.math<-as.numeric(alc$G3.math)
alc$failures.por<-as.numeric(alc$failures.por)
alc$absences.por<-as.numeric(alc$absences.por)
alc$G1.por<-as.numeric(alc$G1.por)
alc$G2.por<-as.numeric(alc$G2.por)
alc$G3.por<-as.numeric(alc$G3.por)


#Take the average of the answers related to weekday and weekend alcohol consumption to create a 
#new column 'alc_use' to the joined data. Then use 'alc_use' to create a new logical column 
#'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE 
#otherwise). (1 point)

alc <- mutate(alc, alc_use =(Dalc + Walc) / 2)
g1 <- ggplot(data = alc, aes(x = alc_use))
g1 + geom_bar()
alc <- mutate(alc, high_use = alc_use > 2)

dim(alc) #370 obs with 35 variables, correct
library(tidyverse)
write.csv(alc,"C:\\Users\\Aleksi\\Home\\IODS-project\\data\\alc.csv", row.names = FALSE)
