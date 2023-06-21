# Starting data aasignment two

#First we read data from our initial csv file (which has our EQI's with SDP and gini)

initialData <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/initialData.csv")
View(initialData)

#This data has 11442 obs of 30 variables!!

# Now we add sdp (i.e sdp ^ 2) and sdp3(i.e sdp^3) in our data for our regression model

#We are choosing AMOUNT OF HYDROGEN CARBONATE to be our EQI since it is closer to normal distribution as compared to others

df <- initialData
missing_eqi <- is.na(df$Amount.of.Hydrogencarbonate)
df <- df[!missing_eqi,]

missing_sdp <- is.na(df$SDP)
df <- df[!missing_sdp,]
View(df)

#df now contains our initial data without any NA values of sdp or our EQI
# (EQI)i,t = β0 + β1SDPi,t + ui,t
EQI_lm <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP, data = df)
summary(EQI_lm)

#N = 7179
#this was the first regression model we ran

#Since we have chosen Amount.of.hydrogencarbonate as our EQI and other indicators are useless now, modifying our data frame
# so that it is more convenient for us.

new_df <- subset(df, select = c(`District`, `State`, `YearCode`, `District.Code`, `State.Code`, `Amount.of.Hydrogencarbonate`, `district_year_id`, `SDP`, `Ginni.Index`))
View(new_df)

#running same model for test
EQI_lm <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP, data = new_df)
summary(EQI_lm)

#Ok so now our new_df variable has our EQI with other regressors!!!
#Removing the rows with NA as Ginni.Index so that we have only useful data

# Remove all rows with missing values in the "Gini.Index" column
new_df <- subset(new_df, !is.na(Ginni.Index))
#So now new_df has no NA entries. Our model would be enhanced!!

#Now time to add SDP^2 and SDP^3 in our data!!
new_df$sdp2 <- 0

# Looping through each row and assigning the unique identifier
for (i in 1:nrow(new_df)) {
  new_df$sdp2[i] <- (new_df$SDP[i] ^ 2)
}
#View(df5)

new_df$sdp3 <- 0

# Looping through each row and assigning the unique identifier
for (i in 1:nrow(new_df)) {
  new_df$sdp3[i] <- (new_df$SDP[i] ^ 3)
}

EQI_lm2 <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP + sdp2 + sdp3 + Ginni.Index, data = new_df)
summary(EQI_lm2)

#Right, no observation has any data missing. Now we will include new regressors based on power inequality
#First, merging our dataset with election margin victories data

new_df$MarginOfVictory <- NA

One <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/2014_win_margin.csv")
View(One)
sort(colnames(One))


for(i in 1:nrow(new_df)){
  if(new_df$District[i] %in% One$PC.Name & new_df$YearCode[i] >= 2014 & new_df$YearCode[i] <= 2018){
    new_df$MarginOfVictory[i] <- One$`Margin..`[One$PC.Name == new_df$District[i]]
  }
}

One2 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/2009_win_margin.csv")
View(One2)
sort(colnames(One2))

for(i in 1:nrow(new_df)){
  if(new_df$District[i] %in% One2$PC.Name & new_df$YearCode[i] >= 2009 & new_df$YearCode[i] <= 2013){
    new_df$MarginOfVictory[i] <- One2$`Margin..`[One2$PC.Name == new_df$District[i]]
  }
}

One3 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/2004_win_margin.csv")
View(One3)
sort(colnames(One3))

for(i in 1:nrow(new_df)){
  if(new_df$District[i] %in% One3$PC.Name & new_df$YearCode[i] >= 2004 & new_df$YearCode[i] <= 2008){
    new_df$MarginOfVictory[i] <- One3$`Margin..`[One3$PC.Name == new_df$District[i]]
  }
}

One4 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/1999_win_margin.csv")
View(One4)
sort(colnames(One4))

for(i in 1:nrow(new_df)){
  if(new_df$District[i] %in% One4$PC.Name & new_df$YearCode[i] >= 2000 & new_df$YearCode[i] <= 2003){
    new_df$MarginOfVictory[i] <- One4$`Margin..`[One4$PC.Name == new_df$District[i]]
  }
}

#Okay so now our data has been populated having margin of victories at the district level
#Removing all the NA values under marginOFVictory
df2 <- new_df[complete.cases(new_df$MarginOfVictory),]
View(df2)


#df2 is our current dataset having district level data of MarginOfVictory, our new regressor.


Two2 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/poverty_data_1993-2000.csv")
View(Two2)
colnames(Two2)
Two2[11, 1] <- "Jammu And Kashmir"

Two3 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/poverty_data_2004-11.csv")
View(Two3)
colnames(Two3)
Two3[11,1] <- "Jammu And Kashmir"

df2$percentagePoverty <- NA

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Two2$State.Union.Territory & df2$YearCode[i] >= 2000 & df2$YearCode[i] <= 2003){
    df2$percentagePoverty[i] <- Two2$`X.1`[Two2$State.Union.Territory == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Two3$State.Union.Territory & df2$YearCode[i] >= 2004 & df2$YearCode[i] <= 2008){
    df2$percentagePoverty[i] <- Two3$`X`[Two3$State.Union.Territory == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Two3$State.Union.Territory & df2$YearCode[i] >= 2009 & df2$YearCode[i] <= 2010){
    df2$percentagePoverty[i] <- Two3$`X.1`[Two3$State.Union.Territory == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Two3$State.Union.Territory & df2$YearCode[i] >= 2011 & df2$YearCode[i] <= 2018){
    df2$percentagePoverty[i] <- Two3$`X.2`[Two3$State.Union.Territory == df2$State[i]]
  }
}

# solving NA values using ALL INDIA data

for(i in 1:nrow(df2)){
  if(is.na(df2$percentagePoverty[i]) & df2$YearCode[i] >= 2000 & df2$YearCode[i] <= 2003){
    df2$percentagePoverty[i] <- Two2$`X.1`[Two2$State.Union.Territory == "ALL INDIA"]
  }
}


for(i in 1:nrow(df2)){
  if(is.na(df2$percentagePoverty[i]) & df2$YearCode[i] >= 2004 & df2$YearCode[i] <= 2008){
    df2$percentagePoverty[i] <- Two3$`X`[Two3$State.Union.Territory == "ALL INDIA"]
  }
}

for(i in 1:nrow(df2)){
  if(is.na(df2$percentagePoverty[i]) & df2$YearCode[i] >= 2009 & df2$YearCode[i] <= 2010){
    df2$percentagePoverty[i] <- Two3$`X.1`[Two3$State.Union.Territory == "ALL INDIA"]
  }
}

for(i in 1:nrow(df2)){
  if(is.na(df2$percentagePoverty[i]) & df2$YearCode[i] >= 2011 & df2$YearCode[i] <= 2018){
    df2$percentagePoverty[i] <- Two3$`X.2`[Two3$State.Union.Territory == "ALL INDIA"]
  }
}

any(is.na(df2$percentagePoverty))
#returns false, hence, we have eliminated all the NA values successfully
# NOw we have another regressor, namely percentagePoverty with us too :-)


#Now onto including third regressor i.e Human Development Index!!

Three <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/GDL-Subnational-HDI-data.csv")
View(Three)

Three[15,6] <- "Jammu And Kashmir"
Three[26,6] <- "Delhi"
Three[27,6] <- "Odisha"

df2$HDI <- NA

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2000){
    df2$HDI[i] <- Three$`X2000`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2001){
    df2$HDI[i] <- Three$`X2001`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2002){
    df2$HDI[i] <- Three$`X2002`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2003){
    df2$HDI[i] <- Three$`X2003`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2004){
    df2$HDI[i] <- Three$`X2004`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2005){
    df2$HDI[i] <- Three$`X2005`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2006){
    df2$HDI[i] <- Three$`X2006`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2007){
    df2$HDI[i] <- Three$`X2007`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2008){
    df2$HDI[i] <- Three$`X2008`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2009){
    df2$HDI[i] <- Three$`X2009`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2010){
    df2$HDI[i] <- Three$`X2010`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2011){
    df2$HDI[i] <- Three$`X2011`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2012){
    df2$HDI[i] <- Three$`X2012`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2013){
    df2$HDI[i] <- Three$`X2013`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2014){
    df2$HDI[i] <- Three$`X2014`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2015){
    df2$HDI[i] <- Three$`X2015`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2016){
    df2$HDI[i] <- Three$`X2016`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2017){
    df2$HDI[i] <- Three$`X2017`[Three$Region == df2$State[i]]
  }
}

for(i in 1:nrow(df2)){
  if(df2$State[i] %in% Three$Region & df2$YearCode[i] == 2018){
    df2$HDI[i] <- Three$`X2018`[Three$Region == df2$State[i]]
  }
}

# sum(is.na(df2$HDI))
# test <- df2[rowSums(is.na(df2)) > 0,]

any(is.na(df2$HDI))
#returns false, hence, we have eliminated all the NA values successfully
# NOw we have another regressor, namely HDI with us too :-)

#Now, onto the next regressor i.e Literacy rate!!

Four <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/Literacy_rate_2011 - Sheet1 (1).csv")
View(Four)
colnames(Four)

Four2 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/Literacy_rate_2001.csv")
View(Four2)
colnames(Four2)
Four2[13,1] <- "Odisha"
Four2[21,1] <- "Jammu And Kashmir"
Four2[38,1] <- "Telangana"
Four2[38,2] <- 60.5

Four3 <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/Literacy_rate_2021 - Sheet1.csv")
View(Four3)
colnames(Four3)
Four3[8,2] <- "Delhi"
Four3[30,2] <- "Jammu And Kashmir"
Four3[11,2] <- "Himachal Pradesh"
Four3[17,2] <- "Uttarakhand"
Four3[28,2] <- "Madhya Pradesh"
Four3[29,2] <- "Uttar Pradesh"
Four3[31,2] <- "Andhra Pradesh"
Four3[36,2] <- "Telangana"
Four3[36,1] <- 36
Four3[36,3] <- 67.0


df2$literacyRate <- NA
df3 <- df2

View(df3)

for(i in 1:nrow(df3)){
  if(df3$State[i] %in% Four2$X & df3$YearCode[i] >= 2000 & df3$YearCode[i] <= 2008){
    df3$literacyRate[i] <- Four2$`X.1`[Four2$X == df3$State[i]]
  }
}

for(i in 1:nrow(df3)){
  if(df3$State[i] %in% Four3$State..Union.Territory & df3$YearCode[i] >= 2014 & df3$YearCode[i] <= 2018){
    df3$literacyRate[i] <- Four3$`Literacy.Rate`[Four3$State..Union.Territory == df3$State[i]]
  }
}

for(i in 1:nrow(df3)){
  if(df3$State[i] %in% Four$State & df3$YearCode[i] >= 2009 & df3$YearCode[i] <= 2013){
    df3$literacyRate[i] <- Four$`Literacy.Rate`[Four$State == df3$State[i]]
  }
}

#Now to handle NA values
for(i in 1:nrow(df3)){
  if(is.na(df3$literacyRate[i])){
    df3$literacyRate[i] <- Four$`Literacy.Rate`[Four$State == "INDIA"]
  }
}


any(is.na(df3$literacyRate))
#returns false, hence, we have eliminated all the NA values successfully
# Now we have another regressor, namely literacyRate with us too :-)

#Our latest df is called df3

#We have our final dataset for our models ready!!
finalDataSet <- subset(df3, select = -c(test))
View(finalDataSet)

write.csv(finalDataSet, "finalData.csv", row.names = FALSE)

#Now onto further running models and interpretation

finalDataSet <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalData.csv")


# Earlier model

EQI_lm2 <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP + sdp2 + sdp3 + Ginni.Index, data = finalDataSet)
summary(EQI_lm2)

# ---------- Now new model start -----------

colnames(finalDataSet)


# Remove percentage sign and convert to numeric
finalDataSet$MarginOfVictory <- as.numeric(gsub("%", "", finalDataSet$MarginOfVictory))/100

finalDataSet$percentagePoverty <- as.numeric(gsub("%", "", finalDataSet$percentagePoverty))/100

finalDataSet$literacyRate <- as.numeric(gsub("%", "", finalDataSet$literacyRate))/100


#Now, we have all our regressors as numeric.

district_final_model <- lm(Amount.of.Hydrogencarbonate ~ SDP + Ginni.Index + sdp2 + sdp3 + MarginOfVictory + percentagePoverty + HDI + literacyRate, data = finalDataSet)
summary(district_final_model)

#Also, making state level dataset
library(dplyr)

state_data <- finalDataSet %>%
  group_by(State, YearCode) %>%
  summarise(across(c(Amount.of.Hydrogencarbonate, SDP, sdp2, sdp3, Ginni.Index, HDI, MarginOfVictory, percentagePoverty, literacyRate), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(YearCode = as.factor(YearCode))

View(state_data)

state_final_model <- lm(Amount.of.Hydrogencarbonate ~ SDP + Ginni.Index + sdp2 + sdp3 + MarginOfVictory + percentagePoverty + HDI + literacyRate, data = state_data)
summary(state_final_model)

write.csv(state_data, "finalStateData.csv", row.names = FALSE)

state_data_new <- state_data
state_data_new$Ginni.Index <- NA


gini <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/gini-Statewise - gini_2005_2015.csv")
View(gini)

gini[31, 1] <- "Telangana"
gini[31, 2] <- 0.394
gini[31, 3] <- 0.270

state_data_new$YearCode <- as.numeric(as.character(state_data_new$YearCode))

for(i in 1:nrow(state_data_new)){
  if(state_data_new$State[i] %in% gini$State & state_data_new$YearCode[i] >= 2000 & state_data_new$YearCode[i] <= 2009){
    state_data_new$Ginni.Index[i] <- gini$`X2005`[gini$State == state_data_new$State[i]]
  }
}

for(i in 1:nrow(state_data_new)){
  if(state_data_new$State[i] %in% gini$State & state_data_new$YearCode[i] >= 2010 & state_data_new$YearCode[i] <= 2018){
    state_data_new$Ginni.Index[i] <- gini$`X2015`[gini$State == state_data_new$State[i]]
  }
}

write.csv(state_data_new, "finalStateData.csv", row.names = FALSE)

View(state_data_new)

state_final_model_new <- lm(Amount.of.Hydrogencarbonate ~ SDP + Ginni.Index + sdp2 + sdp3 + MarginOfVictory + percentagePoverty + HDI + literacyRate, data = state_data_new)
summary(state_final_model_new)

#Checking collinearity between our predictor variables

library(corrplot)

any(is.na(finalDataSet))
# Create a correlation matrix
cor_matrix <- cor(finalDataSet[,c("Amount.of.Hydrogencarbonate", "SDP", "Ginni.Index", "sdp2", "sdp3", "MarginOfVictory", "percentagePoverty", "HDI", "literacyRate")])

cor_matrix[is.na(cor_matrix)] <- apply(cor_matrix, 2, mean, na.rm = TRUE)
# Create a correlation matrix plot
corrplot(cor_matrix, method = "circle", type = "lower", order = "hclust", tl.cex = 0.7)

cor_matrix_state <- cor(state_data_new[,c("Amount.of.Hydrogencarbonate", "SDP", "Ginni.Index", "sdp2", "sdp3", "MarginOfVictory", "percentagePoverty", "HDI", "literacyRate")])

cor_matrix_state[is.na(cor_matrix_state)] <- apply(cor_matrix_state, 2, mean, na.rm = TRUE)
# Create a correlation matrix plot
corrplot(cor_matrix_state, method = "circle", type = "lower", order = "hclust", tl.cex = 0.7)


#Including interaction between highly positively correlated terms

# Create interaction term between percentagePoverty and literacyRate
state_data_new$Poverty_Literacy <- state_data_new$percentagePoverty * state_data_new$literacyRate

# Create interaction term between literacyRate and HDI
state_data_new$Literacy_HDI <- state_data_new$literacyRate * state_data_new$HDI

# Run the new regression model with interaction terms
lm_new <- lm(Amount.of.Hydrogencarbonate ~ SDP + Ginni.Index + sdp2 + sdp3 + MarginOfVictory + percentagePoverty + HDI + literacyRate + Poverty_Literacy + Literacy_HDI, data = state_data_new)

# Print the summary of the new regression model
summary(lm_new)

#estimating non-linear relationsips
library(mgcv)

# fit a GAM model with splines for non-linear relationships
gam_model <- gam(Amount.of.Hydrogencarbonate ~ s(SDP) + s(Ginni.Index) + s(percentagePoverty) + s(MarginOfVictory) + s(HDI) + s(literacyRate) + s(sdp2) + s(sdp3) + s(Poverty_Literacy) + s(Literacy_HDI), data = state_data_new)

# summary of the model
summary(gam_model)

plot(gam_model)

write.csv(state_data_new, "finalStateData.csv", row.names = FALSE)

state_data_new <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalStateData.csv")

# Select the columns of interest
cols_of_interest <- c("SDP", "Ginni.Index", "percentagePoverty", "MarginOfVictory", "HDI", "literacyRate", "Amount.of.Hydrogencarbonate")

# Create a boxplot for each column
boxplot(state_data_new[,cols_of_interest], col="lightblue", main="Boxplot of Variables")

# Calculate the lower and upper limits for each variable using the Tukey's method
outliers <- data.frame(variable=character(), outlier_count=numeric(), stringsAsFactors = FALSE)
for (col in cols_of_interest) {
  q1 <- quantile(state_data_new[,col], 0.25)
  q3 <- quantile(state_data_new[,col], 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5*iqr
  upper_limit <- q3 + 1.5*iqr
  # Count the number of outliers in the dataset for each variable
  num_outliers <- sum(state_data_new[,col] < lower_limit | state_data_new[,col] > upper_limit)
  outliers <- rbind(outliers, data.frame(variable=col, outlier_count=num_outliers))
}

# Print the results
if (nrow(outliers) > 0) {
  cat("Outliers detected:\n")
  print(outliers)
} else {
  cat("No outliers detected.")
}

#Outliers detected for SDP, need to fix that
# will fix that by taking log of SDP everywhere
state_data_new$log_SDP <- log(state_data_new$SDP)

#Final lm
final_lm <- lm(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + MarginOfVictory + percentagePoverty + HDI + literacyRate + Poverty_Literacy + Literacy_HDI, data = state_data_new)

# Print the summary of the new regression model
summary(final_lm)

install.packages("caret")
library(caret)

vif_test <- na.omit(state_data_new) # remove missing values

# compute correlation matrix
correlation_matrix <- cor(vif_test[, c("log_SDP", "Ginni.Index", "sdp2", "sdp3", "MarginOfVictory", "percentagePoverty", "HDI", "literacyRate", "Poverty_Literacy", "Literacy_HDI")])

# compute VIF
vif <- 1/(1-correlation_matrix)^2

# print VIF
View(print(vif))
#check for multicollinearity done!!

#Final lm
final_lm <- lm(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI, data = state_data_new)

# Print the summary of the new regression model
summary(final_lm)

write.csv(state_data_new, "finalStateData.csv", row.names = FALSE)



# --------------------- Question 4 ---------------------------
finalDataSet <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalStateData.csv")

# load required libraries
library(dplyr)
library(tidyverse)
#library(car)
library(dplyr)

#Convert NA to Missing
finalDataSet$Amount.of.Hydrogencarbonate[is.na(finalDataSet$Amount.of.Hydrogencarbonate)] <- ""

#Convert to Numeric
finalDataSet$Amount.of.Hydrogencarbonate <-as.numeric(finalDataSet$Amount.of.Hydrogencarbonate)

#Defining the lists of North, South, East, West and Central States
North_States <- c("Jammu And Kashmir","Himachal Pradesh","Delhi","Punjab","Haryana","Rajasthan","Uttarakhand","Chandigarh","Uttar Pradesh")
South_States <- c("Kerala","Andhra Pradesh","Tamil Nadu","Telangana","Pondicherry","Karnataka")
East_States <- c("Assam","Manipur","Tripura","Sikkim","West Bengal","Odisha","Andaman And Nicobar Islands","Arunachal Pradesh","Bihar","Nagaland","Meghalaya","Mizoram")
West_States <- c("Gujarat","Maharashtra","Goa")
Central_States <- c("Madhya Pradesh","Chhattisgarh","Jharkhand")

# create a new column indicating the state_group of each state
finalDataSet$state_group <- NA
finalDataSet$state_group[finalDataSet$State %in% North_States] <- "North"
finalDataSet$state_group[finalDataSet$State %in% South_States] <- "South"
finalDataSet$state_group[finalDataSet$State %in% West_States] <- "West"
finalDataSet$state_group[finalDataSet$State %in% East_States] <- "East"
finalDataSet$state_group[finalDataSet$State %in% Central_States] <- "Central"

# subset data by state-groups
south_data <- finalDataSet %>% filter(state_group == "South")
north_data <- finalDataSet %>% filter(state_group == "North")
east_data <- finalDataSet %>% filter(state_group == "East")
west_data <- finalDataSet %>% filter(state_group == "West")
central_data <- finalDataSet %>% filter(state_group == "Central")

# perform a t-test to compare the mean environmental quality between North and South
t.test(north_data$Amount.of.Hydrogencarbonate, south_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between East and West
t.test(east_data$Amount.of.Hydrogencarbonate, west_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between North and Central
t.test(north_data$Amount.of.Hydrogencarbonate, central_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between Central and South
t.test(central_data$Amount.of.Hydrogencarbonate, south_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between North and East
t.test(north_data$Amount.of.Hydrogencarbonate, east_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between North and west
t.test(north_data$Amount.of.Hydrogencarbonate, west_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between South and East
t.test(south_data$Amount.of.Hydrogencarbonate, east_data$Amount.of.Hydrogencarbonate)

# perform a t-test to compare the mean environmental quality between South and west
t.test(south_data$Amount.of.Hydrogencarbonate, west_data$Amount.of.Hydrogencarbonate)

# Chow test
library(strucchange)

#perform a chow-test to compare regression coefficient between North and South
north_south_data <- rbind(north_data, south_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = north_south_data , type = "Chow")

#perform a chow-test to compare regression coefficient between east and west
east_west_data <- rbind(east_data, west_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = east_west_data, type = "Chow")

#perform a chow-test to compare regression coefficient between North and central
north_central_data <- rbind(north_data, central_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = north_central_data, type = "Chow")

#perform a chow-test to compare regression coefficient between Central and South
central_south_data <- rbind(central_data, south_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = central_south_data , type = "Chow")

#perform a chow-test to compare regression coefficient between North and East
north_east_data <- rbind(north_data, east_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = north_east_data, type = "Chow")

#perform a chow-test to compare regression coefficient between North and West
north_west_data <- rbind(north_data, west_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = north_west_data, type = "Chow")

#perform a chow-test to compare regression coefficient between East and South
east_south_data <- rbind(east_data, south_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data =east_south_data, type = "Chow")

#perform a chow-test to compare regression coefficient between West and South
west_south_data <- rbind(west_data, south_data)
sctest(Amount.of.Hydrogencarbonate ~ log_SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI,data = west_south_data, type = "Chow")


#------------------ Question 5 ---------------------
finalDataSet <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalStateData.csv")

EQI= finalDataSet$Amount.of.Hydrogencarbonate # EQI
SDP= finalDataSet$SDP #SDP

# Install and load the data.table package using pacman
library(pacman)
p_load(data.table)

# Run OLS regression
regression<-lm(formula = EQI ~ SDP, data = finalDataSet) #EQIi,t = ??0+??1SDPi,t + ui,t

alpha0_true <- coef(regression)[1] # True intercept
alpha1_true <- coef(regression)[2] #True slope

set.seed(1) #seed

#### Sample Size is 100 ####
n <- 100 #sample size
m <- 500 #Number of simulation

# Create empty vectors to store estimates
est_alpha0 <- rep(0, m)  #intercept
est_alpha1 <- rep(0, m)  #slope

# Begin Monte Carlo 
for (i in 1:m) {
  
  # Generate data with known parameter values
  u_i <- rnorm(n,mean = 0, sd = 2) #Error
  sdp <- rnorm(n, mean = 5, sd = 5) # Independent variable
  eqi <- alpha0_true + alpha1_true * sdp + u_i # Dependent variable
  
  # Formulate data.table
  data_i = data.table(Y=eqi, X=sdp)
  
  # Run OLS regression
  model <- lm(formula=eqi ~ sdp, data = data_i)
  
  # Store estimates
  est_alpha0[i] <- coef(model)[1]
  est_alpha1[i] <- coef(model)[2]
}

## Formulate data.table with columns alpha_0 and alpha_1
est_data <- data.table(alpha_0 = est_alpha0,alpha_1 = est_alpha1)

# Calculate mean and standard deviation of estimates
mean_alpha0 <- mean(est_alpha0)
mean_alpha1 <- mean(est_alpha1)
sd_alpha0 <- sd(est_alpha0)
sd_alpha1 <- sd(est_alpha1)

# Print results
cat("True alpha0:", alpha0_true, "\n")
cat("True alpha1:", alpha1_true, "\n\n")
cat("Estimated alpha0:", mean_alpha0, "Standard deviation:", sd_alpha0, "\n")
cat("Estimated alpha1:", mean_alpha1, "Standard deviation:", sd_alpha1, "\n")

# Visual Inception i.e. plotting histogram
hist(est_data[,alpha_0],xlim=c(240,244))
hist(est_data[,alpha_1],xlim=c(-0.2,0.2))

#### Sample Size is 1000
set.seed(1) #seed
n <- 1000 #sample size
m <- 500 #Number of simulation

# Create empty vectors to store estimates
est_alpha0 <- rep(0, m)  #intercept
est_alpha1 <- rep(0, m)  #slope

# Begin Monte Carlo 
for (i in 1:m) {
  
  # Generate data with known parameter values
  u_i <- rnorm(n,mean = 0, sd = 2) #Error
  sdp <- rnorm(n, mean = 5, sd = 5) # Independent variable
  eqi <- alpha0_true + alpha1_true * sdp + u_i # Dependent variable
  
  # Dormulate data.table
  data_i = data.table(Y=eqi, X=sdp)
  
  # Run OLS regression
  model <- lm(formula=eqi ~ sdp, data = data_i)
  
  # Store estimates
  est_alpha0[i] <- coef(model)[1]
  est_alpha1[i] <- coef(model)[2]
}

## Formulate data.table with columns alpha_0 and alpha_1
est_data <- data.table(alpha_0 = est_alpha0,alpha_1 = est_alpha1)

# Calculate mean and standard deviation of estimates
mean_alpha0 <- mean(est_alpha0)
mean_alpha1 <- mean(est_alpha1)
sd_alpha0 <- sd(est_alpha0)
sd_alpha1 <- sd(est_alpha1)

# Print results
cat("True alpha0:", alpha0_true, "\n")
cat("True alpha1:", alpha1_true, "\n\n")
cat("Estimated alpha0:", mean_alpha0, "Standard deviation:", sd_alpha0, "\n")
cat("Estimated alpha1:", mean_alpha1, "Standard deviation:", sd_alpha1, "\n")

# Visual Inception i.e. plotting histogram
hist(est_data[,alpha_0],xlim=c(240,244))
hist(est_data[,alpha_1],xlim=c(-0.2,0.2))

#### Sample Size is 2000
set.seed(1) #seed
n <- 2000 #sample size
m <- 500 #Number of simulation

# Create empty vectors to store estimates
est_alpha0 <- rep(0, m)  #intercept
est_alpha1 <- rep(0, m)  #slope

# Begin Monte Carlo 
for (i in 1:m) {
  
  # Generate data with known parameter values
  u_i <- rnorm(n,mean = 0, sd = 2) #Error
  sdp <- rnorm(n, mean = 5, sd = 5) # Independent variable
  eqi <- alpha0_true + alpha1_true * sdp + u_i # Dependent variable
  
  # Dormulate data.table
  data_i = data.table(Y=eqi, X=sdp)
  
  # Run OLS regression
  model <- lm(formula=eqi ~ sdp, data = data_i)
  
  # Store estimates
  est_alpha0[i] <- coef(model)[1]
  est_alpha1[i] <- coef(model)[2]
}

## Formulate data.table with columns alpha_0 and alpha_1
est_data <- data.table(alpha_0 = est_alpha0,alpha_1 = est_alpha1)

# Calculate mean and standard deviation of estimates
mean_alpha0 <- mean(est_alpha0)
mean_alpha1 <- mean(est_alpha1)
sd_alpha0 <- sd(est_alpha0)
sd_alpha1 <- sd(est_alpha1)

# Print results
cat("True alpha0:", alpha0_true, "\n")
cat("True alpha1:", alpha1_true, "\n\n")
cat("Estimated alpha0:", mean_alpha0, "Standard deviation:", sd_alpha0, "\n")
cat("Estimated alpha1:", mean_alpha1, "Standard deviation:", sd_alpha1, "\n")

# Visual Inception i.e. plotting histogram
hist(est_data[,alpha_0],xlim=c(240,244))
hist(est_data[,alpha_1],xlim=c(-0.2,0.2))

#------------------ Question 6 -----------------------
#-----------------------------------------------------
df <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalStateData.csv")
#View(df)
df[is.na(df)] <- 0
df <- subset(df, !SDP=="." & !sdp2=="." & !sdp3=="." & !Ginni.Index=="." & !MarginOfVictory=="." & !percentagePoverty=="." & !HDI=="." & !literacyRate==".")
# Extract the predictor variables (x1 to x8) and the response variable (y)
df$SDP<- as.numeric(log(df$SDP))
df$sdp2 <- as.numeric(df$sdp2)
df$sdp3 <- as.numeric(df$sdp3)
df$Ginni.Index <- as.numeric(df$Ginni.Index)
df$MarginOfVictory <- as.numeric(sub("%", "",df$MarginOfVictory))
df$percentagePoverty <- as.numeric(sub("%", "",df$percentagePoverty))
df$HDI <- as.numeric(df$HDI)
df$literacyRate <- as.numeric(sub("%", "",df$literacyRate))
df$Amount.of.Hydrogencarbonate <- as.numeric(df$Amount.of.Hydrogencarbonate)

#Final lm
final_lm <- lm(Amount.of.Hydrogencarbonate ~ SDP + Ginni.Index + sdp2 + sdp3 + HDI + literacyRate + percentagePoverty + MarginOfVictory, data = df)
# Print the summary of the new regression model
summary(final_lm)
# Define the log-likelihood function for normal distribution
log_likelihood <- function(params) {
  beta <- params[1:8]
  sigma <- exp(params[length(params)])
  x <- matrix(cbind(df$SDP,df$Ginni.Index, df$sdp2, df$sdp3,  df$HDI,df$literacyRate, df$percentagePoverty, df$MarginOfVictory),nrow=335,ncol=8)
  y <- df$Amount.of.Hydrogencarbonate
  calc <- predict(final_lm,df)
  n <- length(y)
  e <- y - calc
  
  loglik <- -n/2*log(2*pi*sigma^2) - 1/(2*sigma^2)*sum(e^2)
  return(-loglik)
}

# Define starting values for the parameters
start_vals <- c(coef(final_lm)[-1], log(sd(resid(final_lm))))

# Use the optimizing function to find the maximum likelihood estimates
results <- optim(par=start_vals,fn= log_likelihood,method="BFGS")

# Print the results
cat("Maximum likelihood estimates:\n")
cat(paste0("beta", 1:8, ": ", results$par[1:8], "\n"))
cat(paste0("alpha: ", exp(results$par[9]), "\n"))


# ------------ Question 7 ------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------

finalDataSet <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentTwo/finalStateData.csv")

#Convert NA to Missing
finalDataSet$Amount.of.Hydrogencarbonate[is.na(finalDataSet$Amount.of.Hydrogencarbonate)] <- ""

#Convert to Numeric
finalDataSet$Amount.of.Hydrogencarbonate <-as.numeric(finalDataSet$Amount.of.Hydrogencarbonate)

#Defining the lists of North, South, East, West and Central States
North_States <- c("Jammu And Kashmir","Himachal Pradesh","Delhi","Punjab","Haryana","Rajasthan","Uttarakhand","Chandigarh","Uttar Pradesh")
South_States <- c("Kerala","Andhra Pradesh","Tamil Nadu","Telangana","Pondicherry","Karnataka")
East_States <- c("Assam","Manipur","Tripura","Sikkim","West Bengal","Odisha","Andaman And Nicobar Islands","Arunachal Pradesh","Bihar","Nagaland","Meghalaya","Mizoram")
West_States <- c("Gujarat","Maharashtra","Goa")
Central_States <- c("Madhya Pradesh","Chhattisgarh","Jharkhand")

# create a new column indicating the state_group of each state
finalDataSet$state_group <- NA
finalDataSet$state_group[finalDataSet$State %in% North_States] <- "North"
finalDataSet$state_group[finalDataSet$State %in% South_States] <- "South"
finalDataSet$state_group[finalDataSet$State %in% West_States] <- "West"
finalDataSet$state_group[finalDataSet$State %in% East_States] <- "East"
finalDataSet$state_group[finalDataSet$State %in% Central_States] <- "Central"

# subset data by state-groups
south_data <- finalDataSet %>% filter(finalDataSet$state_group == "South")
north_data <- finalDataSet %>% filter(finalDataSet$state_group == "North")
east_data <- finalDataSet %>% filter(finalDataSet$state_group == "East")
west_data <- finalDataSet %>% filter(finalDataSet$state_group == "West")
central_data <- finalDataSet %>% filter(finalDataSet$state_group == "Central")

bartlett.test(Amount.of.Hydrogencarbonate ~ state_group, data = finalDataSet)


# The test statistic is Bartlett's K-squared, which is 38.155 with 4 degrees of freedom. 
# The p-value associated with this test statistic is 1.041e-07, which is much smaller than the
# commonly used significance level of 0.05. Therefore, we reject the null hypothesis of equal variances 
# across the state groups and conclude that there is evidence of heterogeneity of variances.

# Therefore, the variance VARIES largely across different state groups and our data is HETEROSKEDASTIC groupwise.

#-------
# If the variance differs significantly across state groups, the assumption of homoskedasticity of errors 
# (equal variance) is violated in OLS.
#-------


# ---- We will be using group-wise FEASIBLE GLS as  an alternative estimation strategy which can account for such a
# variance structure.

install.packages("plm")
library(plm)

# convert our data into a "panel data" format. This means that we need to create a "time" variable to indicate which observations belong to which state group. 
state_data_panel <- pdata.frame(finalDataSet, index = c("state_group"))

final_feasible <- plm(Amount.of.Hydrogencarbonate ~ log_SDP + sdp2 + sdp3 + HDI + literacyRate + Poverty_Literacy + Literacy_HDI, 
                      data = state_data_panel, model = "within", effect = "groupwise", index = c("state_group"))

summary(final_feasible)
#Result of our final feasible GLS. It is improved our simple OLS or MLE estimators.!!

#------ Project Fin ----------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

































