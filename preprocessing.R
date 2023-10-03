#Preprocessing

#setting up the directory
setwd("/Users/vishaknair/Downloads/")
getwd()

raw_data = read.csv("impure_heart (1).csv")

View(raw_data)

# age:			age
# sex:			1: male, 0: female
# cp:			  chest pain type, 1: typical angina, 2: atypical angina, 3: non-anginal pain, 4: asymptomatic
# trestbps:	resting blood pressure
# chol:			serum cholestoral in mg/dl
# fbs:			fasting blood sugar > 120 mg/dl
# restecg:	resting electrocardiographic results (values 0,1,2)
# thalach:	maximum heart rate achieved
# exang:		exercise induced angina
# oldpeak:	oldpeak = ST depression induced by exercise relative to rest
# slope:		the slope of the peak exercise ST segment
# ca:			  number of major vessels (0-3) colored by flourosopy
# thal:			thal: 3 = normal; 6 = fixed defect; 7 = reversable defect

#target:    0 = no heart disease; 1 = heart disease


class(raw_data)

nrow(raw_data)
ncol(raw_data)

dim(raw_data)

head(raw_data)

#summary of the Arguments
summ = lapply(raw_data, summary)
summ


#Number of missing values
#install.packages("tidyverse")
library(tidyverse)
missing = raw_data %>% summarise_all(~ sum(is.na(.)))
missing


#Missing value percentage
percen = function(x){
  return((x*100)/nrow(raw_data))
}
missing_per = lapply(missing, percen)
missing_per


#Number of people suffering from heart diseases:
num_dis = table(raw_data$target)
num_dis[2]


#Gender-wiser count
genderwise = raw_data %>% count(sex)
genderwise
ggplot(genderwise, aes(sex, y =n)) + geom_bar(stat = "identity") + labs(x="Gender", "Female",y="Frequency") + 
  geom_text(aes(label = signif(n)), nudge_y = 20) + 
  geom_text(aes(label = c("FeMale","Male")), nudge_y =50)

#Function to find mean gender-wise
mean_genderwise = function(a,d) {
  data1 = data.frame(a,d)
  data2 = na.omit(data1)
  return(aggregate(data2$a,list(data2$d), FUN=mean))
}

## Handling missing values:

# Deletion of NA values
data1 = na.omit(raw_data)
View(data1)

missing_del = data1 %>% summarise_all(~ sum(is.na(.)))
missing_del

# Imputation methods
#Dealing with missing values
h_data = raw_data[,-13] #Removing thal data as they are not required
h_data
tail(h_data,10)

#Numerical Data
#Replacing trestbps with mean of the respective sex
trestbps_mean_genderwise = mean_genderwise(h_data$trestbps, h_data$sex)
colnames(trestbps_mean_genderwise) <- c('sex','trestbps')
trestbps_mean_genderwise
h_data = left_join(h_data, trestbps_mean_genderwise, by = "sex")
h_data$trestbps = coalesce(h_data$trestbps.x, h_data$trestbps.y)
h_data = select(h_data, -trestbps.x, -trestbps.y)
h_data


#Replacing chol with mean of the respective sex
chol_mean_genderwise = mean_genderwise(h_data$chol, h_data$sex)
colnames(chol_mean_genderwise) <- c('sex','chol')
chol_mean_genderwise
h_data = left_join(h_data, chol_mean_genderwise, by = "sex")
h_data$chol = coalesce(h_data$chol.x, h_data$chol.y)
h_data = select(h_data,-chol.x,-chol.y)
h_data

#Replacing thalach with mean of the respective sex
thalach_mean_genderwise = mean_genderwise(h_data$thalach, h_data$sex)
colnames(thalach_mean_genderwise) <- c('sex','thalach')
thalach_mean_genderwise
h_data = left_join(h_data, thalach_mean_genderwise, by = "sex")
h_data$thalach = coalesce(h_data$thalach.x, h_data$thalach.y)
h_data = select(h_data,-thalach.x,-thalach.y)
h_data


#Checking the count of NA values
missing_imp = h_data %>% summarise_all(~ sum(is.na(.)))
missing_imp

#All the missing values have been dealt with.
new_h_data = h_data

# cholesterol (chol) gender-wise
ggplot(mean_genderwise(new_h_data$chol, h_data$sex), aes(x=x,y=Group.1 )) + geom_bar(stat = "identity") + labs(x="Cholesterol",y="Gender") + coord_flip()

# resting blood pressure (trestbps) gender-wise
ggplot(mean_genderwise(new_h_data$trestbps, h_data$sex), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="Blood Pressure",y="Gender") + coord_flip()

# max heart rate achieved (thalach) gender-wise
ggplot(mean_genderwise(new_h_data$thalach, h_data$sex), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="Max Heart Rate",y="Gender") + coord_flip()

