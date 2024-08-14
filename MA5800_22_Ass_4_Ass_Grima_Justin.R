--------------
  title: "MA5800_22_Ass_4_Grima_Justin"
output: html_document
date: "2022-10-12"
editor_options: 
  chunk_output_type: console
--------------
# This script was provided in the zip file as pre-processing to merge the data sets/ CSV files.
#1=read.table("student-mat.csv",sep=";",header=TRUE)
#d2=read.table("student-por.csv",sep=";",header=TRUE)
#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu",
                    #"Mjob", "Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

#PART A
#Check RStudio Version, Load Packages and Dataset, and investigate
RStudio.Version() #Check RStudio type.
#Load packages
install.packages('groupdata2')
library(reshape)
library(dplyr)
library(tidyr)
library(tidyverse)
library(cluster)
library(ggplot2)
library(GGally)
library(groupdata2)
#Load document.
student_portmaths_df= read.csv('student_portmaths.csv') #load students_portmath.csv into R.
str(student_portmaths_df) #View structure of student: multivariate dataset (integer 
#character).
summary(student_portmaths_df) #View summary of data frame.
dim(student_portmaths_df) #View dimension of data frame.

#PartB
#Data tidying, cleaning, type conversion and variable transformation.
student_portmaths_df1 = student_portmaths_df #Make a copy of original data set.

#Basis of the question for the report some variables not being needed. I have chosen the appropriate variables (based on metadata) to conduct the report's data analysis and reduce 'noise' in the dataset. 
#Create a subset of desired variables from the original data for report data analysis (7. Data subsampling).
student_portmaths_df_subset = subset(student_portmaths_df1, select = c(X,school, schoolsup, famsup, paid, activities, studytime, higher, Medu, Fedu, G3, sub))

colnames(student_portmaths_df_subset) #Identify columns. There is no column name for 'Students' in the original data set. "X" has been automatically added as the column name for the student's number by R.

#Change column "X" name to "Student" and clean up other column names for organization and better undeertsanding of columns. (3. Data Cleaning).
student_portmaths_df_subset = rename(student_portmaths_df_subset , c("Student" = "X", "School"="school", "SchoolSup" = "schoolsup", "FamSup" = "famsup", "Paid" = "paid", "Activities" = "activities", "StudyTime" = "studytime", "Higher" = "higher", "MEdu" = "Medu", "FEdu" = "Fedu", "Final Grade(%)" = "G3", "Subject" = "sub"))

colnames(student_portmaths_df_subset) #View column names to ensure the right changes have been made.

#Convert final grades to a percentage score. Scores are originally from 0 - 20. For better understanding, change it to percentage. (9. Variable Transformation).
student_portmaths_df_subset$`Final Grade(%)` = ((student_portmaths_df_subset$`Final Grade(%)`/20)*100)

View(student_portmaths_df_subset) #Check data set with column name change and variable transformation.

#Part C
#Identify Missing variables (3. Data Cleaning)
Count_miss_val = function(x) sum(is.na(x))# Create function to count the number of missing values in a single data column (variable)
student_miss_vals = apply(student_portmaths_df_subset, MARGIN = 2, FUN = Count_miss_val) #look for NA in data set
student_miss_vals #Call function: 0 missing values for all variables in the data set.

#Identify if there are any duplicate rows (3. Data Cleaning)
sum(duplicated(student_portmaths_df_subset)) # [1] 0. Zero duplicates were found.

str(student_portmaths_df_subset) #View data set structure.

#Convert character type variables to factors (4. Type conversion)
convert_to_factors = c("School", "SchoolSup", "FamSup", "Paid", "Activities", "Higher","Subject") #Create variable to hold columns that will be converted from characters to factors.

student_portmaths_df_subset[,convert_to_factors] = lapply(student_portmaths_df_subset[,convert_to_factors], factor) #Convert chosen character type variables to factors.

#Convert factors to numeric variables (4. Type conversion). 
student_portmaths_df_subset$School = as.numeric(student_portmaths_df_subset$School)
student_portmaths_df_subset$SchoolSup = as.numeric(student_portmaths_df_subset$SchoolSup)
student_portmaths_df_subset$FamSup = as.numeric(student_portmaths_df_subset$FamSup)
student_portmaths_df_subset$Paid = as.numeric(student_portmaths_df_subset$Paid)
student_portmaths_df_subset$Activities = as.numeric(student_portmaths_df_subset$Activities)
student_portmaths_df_subset$Higher = as.numeric(student_portmaths_df_subset$Higher)
student_portmaths_df_subset$Subject = as.numeric(student_portmaths_df_subset$Subject)

str(student_portmaths_df_subset) #view subset after conversion

#Compute the sum of mother and father education rankings into one 'ParentCombinedEduRank' 
#variable (9. Create new variable through existing variable transformation).
ParentCombinedEduLevel = student_portmaths_df_subset$MEdu + student_portmaths_df_subset$FEdu
#Add variable to data subset.
student_portmaths_df_subset = student_portmaths_df_subset %>% add_column(ParentCombinedEduLevel)

#Remove 'MEdu' and 'FEdu' as they are not needed because we have combined them into one 
#variable (7. Data subset selection)
student_portmaths_df_subset = subset(student_portmaths_df_subset, select = -c(MEdu, FEdu))

View(student_portmaths_df_subset) #View final data set after tyding, cleaning, type conversion and variable transformation.

#Part D
#Data Subsampling and Spearman correlation check between variables in the data set.
  
#Create a new data frame without `Student` in it as it is not needed for determining 
#variable outliers because it serves as only identification and holds no real value 
#(7.Data subset selection).
subset_missing_student = subset(student_portmaths_df_subset, select = -(Student))

boxplot(subset_missing_student) #Use boxplot to determine if outliers in variables. Outliers 
#are present in the data set, therefore methods proceeding will be chosen based on robust 
#and ability to mitigate outliers.

#Spearman correlation between variables (1. Data Representation)
student_portmaths_corr= as.matrix(cor(student_portmaths_df_subset, method = 'spearman'))
View(student_portmaths_corr) #View correlation matrix.
ggcorr(student_portmaths_corr) #plot correlation matrix.

#Part E
#Group-based data summarization and exploratory visualisation using ggplot2.
#All schools
#PComEduRank vs Final Grade Average
  
#Calculate the 'Final Grade (%)' Mean for each group in the 'ParentCombinedEduRank' variable, for barplot visualisation (8. Group-based data summarization).
student_PComEduRank_grade_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Bar PLot (10. Exploratory visualisation using ggplot2).
ggplot(student_PComEduRank_grade_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `Grade_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Final Grade Average(%)") + ggtitle("Combined school and subjects Final Grade Average(%) vs\nParents Combined Education Level") + labs(fill = "Education\nLevel Scale\nHigh = 8\nNone = 0")

#GP School 
#PComEdu vs Final Grade Average
  
#7. Data Subset Selection (Gabriel Pereira School).
PComEdu_GP_School = filter(student_portmaths_df_subset, School == '1')

# Gabriel Pereira School students calculated the 'Final Grade (%)' mean for each group in the 
#'ParentCombinedEduRank' variable, for barplot visualisation (8. Group-based data 
#'summarisation).
GPStudent_PComEduRank_grade_means = PComEdu_GP_School %>% group_by(`ParentCombinedEduLevel`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(GPStudent_PComEduRank_grade_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `Grade_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Final Grade Average(%)") + ggtitle("Gabriel Pereira School Combined Final Grade Average(%)\nvs Parents Combined Education Level") + labs(fill = "Education\nLevel Scale\nHigh = 8\nNone = 0")

#MS School 
#PComEdu vs Final Grade Average
#7. Data Subset Selection (Mousinho da Silveira School).
PComEdu_MS_School = filter(student_portmaths_df_subset, School == '2')

# Mousinho da Silveira School students calculated the 'Final Grade (%)' mean for each group in the 'ParentCombinedEduRank' variable, for barplot visualisation (8. Group-based data summarization).
MSStudent_PComEduRank_grade_means = PComEdu_MS_School %>% group_by(`ParentCombinedEduLevel`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualisation using ggplot2).
ggplot(MSStudent_PComEduRank_grade_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `Grade_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Final Grade Average(%)") + ggtitle("Mousinho da Silveira School Combined Final Grade Average(%)\nvs Parents Combined Education Level") + labs(fill = "Education\nLevel Scale\nHigh = 8\nNone = 0")

#Math subject
#PComEdu vs Final Grade Average
#7. Data Subset Selection (Math Subject).
PComEdu_Math_Subject = filter(student_portmaths_df_subset, Subject == '1')

# Students' math subjects calculated the 'Final Grade (%)' mean for each group in the 'ParentCombinedEduRank' group, for barplot visualisation (8. Group-based data summarization).
Math_PComEduRank_grade_means = PComEdu_Math_Subject %>% group_by(`ParentCombinedEduLevel`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(Math_PComEduRank_grade_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `Grade_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Final Grade Average(%)") + ggtitle("Combined Schools Math Final Grade Average(%) vs Parents Combined Education Level") + labs(fill = "Education\nLevel Scale\nHigh = 8\nNone = 0")

#Portugese
#PComEdu vs Final Grade Average
#7. Data Subset Selection (Math Subject).
PComEdu_Portugese_Subject = filter(student_portmaths_df_subset, Subject == '2')

#Students Portuguese subject calculated the 'Final Grade (%)' mean for each group in the  'ParentCombinedEduRank' variable, for barplot visualisation (8. Group-based data summarization).
Portugese_PComEduRank_grade_means = PComEdu_Portugese_Subject %>% group_by(`ParentCombinedEduLevel`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualisation using ggplot2).
ggplot(Portugese_PComEduRank_grade_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `Grade_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Final Grade Average(%)") + ggtitle("Combined Schools Portugese Final Grade Average(%) vs\nParents Combined Education Level") + labs(fill = "Education\nLevel Scale\nHigh = 8\nNone = 0")

#Part F
#Final Grade Average vs Activities
#Students calculated the 'Final Grade Mean' for each group in the 'activities' variable, for barplot visualization (8. Group-based data summarization).
student_activities_grade_means = student_portmaths_df_subset %>% group_by(`Activities`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_activities_grade_means) + geom_bar(mapping = aes(x = `Activities`, y = `Grade_mean`, fill = `Activities`), stat = 'identity') + xlab("Extra-Curricular Activity Participation") + ylab("Final Grade Average(%)") + ggtitle("Overall Combined Final Grade Average(%) vs Extra-Curricular Activity Participation") + labs(fill = "Participation Scale\nYes = 2\nNo = 1\n")

#Final Grade Average vs FamSup
#Students calculated the 'Final Grade Mean' for each group in the 'Family Educational Support' variable, for barplot visualization (8. Group-based data summarisation).
student_FamSup_grade_means = student_portmaths_df_subset %>% group_by(`FamSup`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_FamSup_grade_means) + geom_histogram(mapping = aes(x = `FamSup`, y = `Grade_mean`, fill = `FamSup`), stat = 'identity') + xlab("Family Educational Support") + ylab("Final Grade Average(%)") + ggtitle("Overall Combined Final Grade Average(%) vs Family Educational Support") + labs(fill = "Family Support\nScale\nYes = 2\nNo = 1\n")

#Final Grade Average vs Higher
#Students calculated the 'Final Grade Mean' for each group in the 'Higher' variable, for barplot visualization (8. Group-based data summarization).
student_Higher_grade_means = student_portmaths_df_subset %>% group_by(`Higher`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_Higher_grade_means) + geom_histogram(mapping = aes(x = `Higher`, y = `Grade_mean`, fill = `Higher`), stat = 'identity') + xlab("Seeking Higher Education") + ylab("Final Grade Average(%)") + ggtitle("Overall Combined Final Grade Average(%) vs Seeking Higher Education") + labs(fill = "Seeking Higher\nEducation Scale\nYes = 2\nNo = 1\n")

#Final Grade Average vs SchoolSup
#Students calculated the 'Final Grade Mean' for each group in the 'School Education Support' variable, for barplot visualization (8. Group-based data summarization).
student_SchoolSup_grade_means = student_portmaths_df_subset %>% group_by(`SchoolSup`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_SchoolSup_grade_means) + geom_histogram(mapping = aes(x = `SchoolSup`, y = `Grade_mean`, fill = `SchoolSup`), stat = 'identity') + xlab("School Educational Support") + ylab("Final Grade Average(%)") + ggtitle("Overall Combined Final Grade Average(%) vs School Educational Support") + labs(fill = "School Educational\nSupport Scale\nYes = 2\nNo = 1\n")

#Final Grade Average vs Paid (Not used in the report due to page length restriction but briefly mentioned)
#Students calculated the 'Final Grade Mean' for each group in the 'Extra Paid Classes within the Course Subject (Math or Portuguese) ' variable, for barplot visualization (8. Group-based data summarization).
student_Paid_grade_means = student_portmaths_df_subset %>% group_by(`Paid`) %>% summarise(Grade_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_Paid_grade_means) + geom_histogram(mapping = aes(x = `Paid`, y = `Grade_mean`, fill = `Paid`), stat = 'identity') + xlab("Extra Paid Classes") + ylab("Final Grade Average(%)") + ggtitle("Overall Combined Final Grade Average(%) vs Extra Paid Classes") + labs(fill ="Extra Paid\nClasses Scale\nYes = 2\nNo = 1\n")

#Final Grade Average vs study time
#Students calculated the 'Final Grade Mean' for each grouping the 'Family Relationship' variable, for barplot visualization (8. Group-based data summarization).
student_study_means = student_portmaths_df_subset%>% group_by(`StudyTime`) %>% summarise(grades_mean =mean(`Final Grade(%)`))

#Barplot (10. Exploratory visualisation using ggplot2).
ggplot(student_study_means) + geom_bar(mapping = aes(x = `StudyTime`, y = `grades_mean`, fill = `StudyTime`), stat = 'identity') + xlab("Weekly Study Time") + ylab("Final Grade Average(%)") + ggtitle("Final Grade Average(%) vs Weekly Study Time") + labs(fill ="Education\nLevel Scale\n>10 hours = 4\n<2 hours = 1\n")

#Part G
#Group-based data summarization and exploratory visualisation using ggplot2.
#Exploratory Visualization between Parents Combined Education (PComEdu) Level vs Chosen Variables.
#PComEdu vs Activities
#Students calculated the 'Activity' mean for each group in of 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summation).
student_PComEdu_activities_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(activity_mean =mean(`Activities`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_PComEdu_activities_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `activity_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Average Activity Engagement\n(Yes = 2, No  = 1)") + ggtitle("Average Student Activity Engagement\nvs Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\nHigh = 8\nNone = 0") 

#PComEdu vs FamSup
#Students calculated the 'Family Educational Support' mean for each group in the 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summarization).
student_PComEdu_famsup_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(famsup_mean =mean(`FamSup`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_PComEdu_famsup_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `famsup_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Family Educational Support\n(Yes = 2, No  = 1)") + ggtitle("Average Family Educational Support\nvs Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\nHigh = 8\nNone = 0")

#PComEdu vs Higher
#Students calculated the 'Higher' education means for each group in the 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summarization).
student_PComEdu_higher_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(higher_mean =mean(`Higher`))

#Barplot (10. Exploratory visualization using ggplot2).
ggplot(student_PComEdu_higher_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `higher_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Student Desire for Higher Education Average\n(Yes = 2, No  = 1)") + ggtitle("Average Desire for Higher Education Based on Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\nHigh = 8\nNone = 0")

#PComEdu vs SchoolSup
#Students calculated the 'School Educational Support' mean for each group in the 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summarization).
student_PComEdu_schoolsup_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(schoolsup_mean =mean(`SchoolSup`))

#Barplot (10. Exploratory visualizations using ggplot2).
ggplot(student_PComEdu_schoolsup_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `schoolsup_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("School Educatinal Support Average\n(Yes = 2, No  = 1)") + ggtitle("Average School Educational Support\nvs Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\nHigh = 8\nNone = 0")

#PComEdu vs Paid (Not used in the report due to page length restriction but briefly mentioned)
#Students calculated extra 'Paid' classes mean for each group in the 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summarization).
student_PComEdu_paid_means = student_portmaths_df_subset %>% group_by(`ParentCombinedEduLevel`) %>% summarise(paid_mean =mean(`Paid`))

#Barplot (10. Exploratory visualizations using ggplot2).
ggplot(student_PComEdu_paid_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `paid_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Average Extra Paid Classes (Math & Portugese)\n(Yes = 2, No  = 1)") + ggtitle("Average for Extra Paid Classes Within Course Subject\nBased on Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\nHigh = 8\nNone = 0")

#PComEdu vs Studytime
#Students calculated the 'Study Time' mean for each group in the 'Parents Combined Education" variable, used for barplot visualization (8. Group-based data summarization).
student_study_means = student_portmaths_df_subset%>% group_by(`ParentCombinedEduLevel`) %>% summarise(study_mean =mean(`StudyTime`))

#Barplot (10. Exploratory visualisation using ggplot2).
ggplot(student_study_means) + geom_bar(mapping = aes(x = `ParentCombinedEduLevel`, y = `study_mean`, fill = `ParentCombinedEduLevel`), stat = 'identity') + xlab("Parents Combined Education Level") + ylab("Average Weekly Study Time\n(1: <2 hours, 2: 2 to 5 hours, 3: 5 to 10 hours)") + ggtitle("Average Weekly Study Time vs Parents Combined Education Level") + labs(fill ="Education\nLevel Scale\n>10 hours = 4\n<2 hours = 1\n")

