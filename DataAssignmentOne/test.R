final_merged_data <- read.csv("/home/ashu/Desktop/sem4/econometrics/DataAssignmentOne/final_merged_data.csv")
df <- final_merged_data

#Viewing the complete dataset
View(df)

#class(df)
#dim(df) 

#df is our dataframe which we are gonna use (and modify) in questions

# ------ Starting Ques5 here on -----

# Prepare detailed summary statistics for all the variables (tables; histogram; box-plot;
# shape of the distribution, skew). Are there any outliers?

# In our data, we have total of 22 variables, we will do analysis of each and every variable one by one
# First let's isolate each variable from our data set

amountArsenic <- df$Amount.of.Arsenic
amountCarbonate <- df$Amount.of.carbonate
amountCalcium <- df$Amount.of.Calcium
amountChloride <- df$Amount.of.Chloride
amountElecConductivity <- df$Amount.of.Electrical.Conductivity
amountFluorine <- df$Amount.of.Fluorine
amountIron <- df$Amount.of.Iron
amountHydrogenCarbonate <- df$Amount.of.Hydrogencarbonate
amountPotassium <- df$Amount.of.Potassium
amountMagnesium <- df$Amount.of.Magnesium
amountNitrate <- df$Amount.of.Nitrate
amountSodium <- df$Amount.of.Sodium
percentageSodium <- df$Percentage.of.Sodium
amountPhosphate <- df$Amount.of.Phosphate.Ion
amountSodiumCarbonate <- df$Amount.of.Residual.Sodium.Carbonate
amountSodiumAbsorptionRatio <- df$Amount.of.Sodium.absorption.ratio
amountSulfate <- df$Amount.of.Sulfate
amountSiliconDiOxide <- df$Amount.of.Silicon.dioxide
totalHardness <- df$Amount.of.Hardness.Total
totalAlkalinity <- df$Amount.of.Alkalinity.Total
totalDissolvedSolids <- df$Amount.of.Total.Dissolved.Solids
hydrogenPotential <- df$Amount.of.Potential.of.Hydrogen

#we have isolated all our variables
# Now we will prepare summary statistics for all our variables

# ----Amount of Arsenic----
df2 = df[df$Amount.of.Arsenic>0,]
#df2$Amount.of.Arsenic <- as.numeric(df2$Amount.of.Arsenic)
#View(df2)
summary(amountArsenic)
summary(df2$Amount.of.Arsenic)
df2$YearCode <- as.factor(df2$YearCode)
#We have made 2 histograms for arsenic, one using df and other using df2
ggplot(df2, aes(x = Amount.of.Arsenic)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of arsenic") +
  ylab("District count")
df3 <- df
df3$YearCode <- as.factor(df3$YearCode)
#df3[is.na(df3)] <- 0
ggplot(df3, aes(x = YearCode, y = Amount.of.Arsenic)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#Calculating the skewness of distribution
skewness(df3$Amount.of.Arsenic, na.rm = TRUE)

#----Amount of Carbonate----

summary(amountCarbonate)
df3 <- df
df3$YearCode <- as.factor(df3$YearCode)

ggplot(df3, aes(x = Amount.of.carbonate)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Carbonate") +
  ylab("District count")

#df3[is.na(df3)] <- 0
ggplot(df3, aes(x = YearCode, y = Amount.of.carbonate)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #+
#geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#Calculating the skewness of distribution
skewness(df$Amount.of.carbonate, na.rm = TRUE)
#View(amountCarbonate)

#----Amount of Calcium----

summary(amountCalcium)
df3 <- df
df3$YearCode <- as.factor(df3$YearCode)

ggplot(df3, aes(x = Amount.of.Calcium)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Calcium") +
  ylab("District count")

#df3[is.na(df3)] <- 0
ggplot(df3, aes(x = YearCode, y = Amount.of.Calcium)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #+
#geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#Calculating the skewness of distribution
skewness(df$Amount.of.Calcium, na.rm = TRUE)
#View(amountCalcium)

#----Amount of Chloride----

summary(amountChloride)
df3 <- df
df3$YearCode <- as.factor(df3$YearCode)

ggplot(df3, aes(x = Amount.of.Chloride)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Chloride") +
  ylab("District count")

#df3[is.na(df3)] <- 0
ggplot(df3, aes(x = YearCode, y = Amount.of.Chloride)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #+
#geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#Calculating the skewness of distribution
skewness(df$Amount.of.Chloride, na.rm = TRUE)

#----Amount of Electrical Conductivity----

summary(amountElecConductivity)

ggplot(df3, aes(x = Amount.of.Electrical.Conductivity)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Electrical Conductivity") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Electrical.Conductivity)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #+


skewness(df$Amount.of.Electrical.Conductivity, na.rm = TRUE)

#----Amount of Fluorine---

summary(amountFluorine)

ggplot(df3, aes(x = Amount.of.Fluorine)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Fluorine") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Fluorine)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Fluorine, na.rm = TRUE)

#----Amount of Iron---

summary(amountIron)

ggplot(df3, aes(x = Amount.of.Iron)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Iron") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Iron)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Iron, na.rm = TRUE)

#----Amount of Hydrogen Carbonate---

summary(amountHydrogenCarbonate)

ggplot(df3, aes(x = Amount.of.Hydrogencarbonate)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Hydrogen Carbonate") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Hydrogencarbonate)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Hydrogencarbonate, na.rm = TRUE)

#----Amount of Potassium---

summary(amountPotassium)

ggplot(df3, aes(x = Amount.of.Potassium)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Potassium") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Potassium)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Potassium, na.rm = TRUE)

#----Amount of Magnesium---

summary(amountMagnesium)

ggplot(df3, aes(x = Amount.of.Magnesium)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Magnesium") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Magnesium)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Magnesium, na.rm = TRUE)

#----Amount of Nitrate---

summary(amountNitrate)

ggplot(df3, aes(x = Amount.of.Nitrate)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Nitrate") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Nitrate)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Nitrate, na.rm = TRUE)

#----Amount of Sodium---

summary(amountSodium)

ggplot(df3, aes(x = Amount.of.Sodium)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Sodium") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Sodium)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Sodium, na.rm = TRUE)

#----Percentage of Sodium---

summary(percentageSodium)

ggplot(df3, aes(x = Percentage.of.Sodium)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Percentage of Sodium") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Percentage.of.Sodium)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Percentage.of.Sodium, na.rm = TRUE)

#----Amount of Phosphate ion---

summary(amountPhosphate)

ggplot(df3, aes(x = Amount.of.Phosphate.Ion)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount.of.Phosphate.Ion") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Phosphate.Ion)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Phosphate.Ion, na.rm = TRUE)

#----Amount of Residual Sodium Carbonate---

summary(amountSodiumCarbonate)

ggplot(df3, aes(x = Amount.of.Residual.Sodium.Carbonate)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount.of.Residual.Sodium.Carbonate") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Residual.Sodium.Carbonate)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Residual.Sodium.Carbonate, na.rm = TRUE)

#----Amount of Sodium Absorption Ratio---

summary(amountSodiumAbsorptionRatio)

ggplot(df3, aes(x = Amount.of.Sodium.absorption.ratio)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Sodium Absorption Ratio") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Sodium.absorption.ratio)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Sodium.absorption.ratio, na.rm = TRUE)

#----Amount of Sulfate---

summary(amountSulfate)

ggplot(df3, aes(x = Amount.of.Sulfate)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Sulfate") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Sulfate)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Sulfate, na.rm = TRUE)

#----Amount of Silicon Dioxide---

summary(amountSiliconDiOxide)

ggplot(df3, aes(x = Amount.of.Silicon.dioxide)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Silicon dioxide") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Silicon.dioxide)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Silicon.dioxide, na.rm = TRUE)

#----Amount of Hardness total---

summary(totalHardness)

ggplot(df3, aes(x = Amount.of.Hardness.Total)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Hardness Total") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Hardness.Total)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Hardness.Total, na.rm = TRUE)

#----Amount of Alkalinity total---

summary(totalAlkalinity)

ggplot(df3, aes(x = Amount.of.Alkalinity.Total)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Alkalinity Total") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Alkalinity.Total)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Alkalinity.Total, na.rm = TRUE)

#----Amount of total dissolved solids---

summary(totalDissolvedSolids)

ggplot(df3, aes(x = Amount.of.Total.Dissolved.Solids)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Total Dissolved Solids") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Total.Dissolved.Solids)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Total.Dissolved.Solids, na.rm = TRUE)

#----Amount of Potential of Hydrogen---

summary(hydrogenPotential)

ggplot(df3, aes(x = Amount.of.Potential.of.Hydrogen)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Amount of Potential of Hydrogen") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Amount.of.Potential.of.Hydrogen)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Amount.of.Potential.of.Hydrogen, na.rm = TRUE)

#----SDP---

summary(df$SDP)

ggplot(df3, aes(x = SDP)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("SDP") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = SDP)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$SDP, na.rm = TRUE)

#----Ginni Index---

summary(df$Ginni.Index)

ggplot(df3, aes(x = Ginni.Index)) + 
  geom_histogram(aes(color = YearCode), fill = 'cyan', 
                 position = 'identity', bins = 30) + xlab("Ginni Index") +
  ylab("District count")


ggplot(df3, aes(x = YearCode, y = Ginni.Index)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


skewness(df$Ginni.Index, na.rm = TRUE)


#-----Q5 Completed!!!!---


#-----Starting Question 6------

#Now, using this data, estimate the following regression for any one environmental
#quality indicator of your choice. Summarize the results in a table and interpret in
#plain English. Note that i indexes districts, t indexes years, and ui,t is random error.
#Environmental Quality Indicator (EQI)i,t = β0 + β1SDPi,t + ui,t

#We are choosing AMOUNT OF HYDROGEN CARBONATE to be our EQI since it is closer to normal distribution as compared to others

df4 <- df
missing_eqi <- is.na(df4$Amount.of.Hydrogencarbonate)
df4 <- df4[!missing_eqi,]

missing_sdp <- is.na(df4$SDP)
df4 <- df4[!missing_sdp,]

EQI_lm <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP, data = df4)
summary(EQI_lm)

#N = 7179
#Interpretation in doc!!!
#------Question 6 done------

#------Starting Question 7 -------

res <- resid(EQI_lm) #getting list of residuals
#length(res) gives 7179, hence, we have all the residuals

#Residual vs Fitted plot, useful for visually detecting hetereoskedasticity
plot(fitted(EQI_lm), res)
abline(0,0)
# => Our data is heteroskedastic

#Density plot, useful for visually checking whether or not the residuals are normally distributed
plot(density(res))
# We can see that the density plot roughly follows a bell shape, although it is slightly skewed to the right.

#Visualize the model residuals (i.e.,ûi,t) on a plot having the environmental quality
#indicator on Y-axis and SDP on the X-axis.

plot1 <- ggplot(df4, aes(x = SDP, y = Amount.of.Hydrogencarbonate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "SDP", y = "Environmental Quality Indicator(Amount of H2CO3) ") +
  ggtitle("Environmental Quality Indicator vs. SDP")
plot1


plot2 <- ggplot(df4, aes(x = SDP, y = res)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "SDP", y = "Model Residuals") +
  ggtitle("Model Residuals vs SDP")
plot2

predicted <- predict(EQI_lm)
plot3 <- ggplot(df4, aes(x = Amount.of.Hydrogencarbonate, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Actual Environmental Quality Indicator", y = "Predicted Environmental Quality Indicator") +
  ggtitle("Predicted vs Actual Environmental Quality Indicator") 
plot3

#Write interpretations and question 7 is done!!!
#-----Question 7 done------

#-----Question 8 ---------

# Plot a histogram of the residuals
hist(res, main = "Histogram of Model Residuals",col="cyan",freq=TRUE)

# Calculate the sum of the residuals
sum_residuals <- sum(res)
print(paste0("Sum of Residuals: ", sum_residuals))

#-----Question 9------
#Final Regression Model
#View(df4)
df5 <- df4
df5$sdp2 <- 0

# Looping through each row and assigning the unique identifier
for (i in 1:nrow(df5)) {
  df5$sdp2[i] <- (df5$SDP[i] ^ 2)
}
#View(df5)

df5$sdp3 <- 0

# Looping through each row and assigning the unique identifier
for (i in 1:nrow(df5)) {
  df5$sdp3[i] <- (df5$SDP[i] ^ 3)
}

EQI_lm2 <- lm(formula = Amount.of.Hydrogencarbonate ~ SDP + sdp2 + sdp3 + Ginni.Index, data = df5)
summary(EQI_lm2)