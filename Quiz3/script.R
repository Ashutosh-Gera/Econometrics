# In the quiz3.csv file, you are given data on road length (in kilometers) at the district-level in
# India. Specifically, we have total road length given by RL(i,year) where i = {1, 2, ... ... . 661}
# districts and year = {2011, 2012, ... . , 2018, 2021}.

# we have district codes from {1, 2, 3,..., 661}

# i = district code; year = yearcode

# First we load our data in a dataframe!

roadLength_districtLevel <- read.csv("/home/ash/Desktop/Econ/quiz3/quiz3.csv")
df <- roadLength_districtLevel

#View the complete dataset

#View(roadLength_districtLevel)
View(df)
#class(df) #Outputs data.frame
#dim(df)   #Outputs 2425  7
#head(df)

# We will be using df as our dataframe which we are gonna modify according to the questions

# ------ Starting questions --------

# First, defining a unique identifier for each row of data as follows: ID = i ∗ 10, 000 + year.

df$ID <- 0

# Looping through each row and assigning the unique identifier
for (i in 1:nrow(df)) {
  df$ID[i] <- (df$districtcode[i] * 10000) + df$yearcode[i]
}

#Now defining a trend variable as t = yearcode - 2010;

df$trend <- df$yearcode - 2010

# summary(df)

# Running the following regression model i  R:
# RL(ID) = B0 + B1*trend(ID) + u(id)

df$road_sum <- replace(df$road_sum, df$road_sum == '.', '')
df$road_sum <- as.numeric(df$road_sum)


road_lm <- lm(formula = road_sum ~ trend, data = df)
summary(road_lm)

# Ques 1 finished, now onto defining dummy variables

# Create DSouth dummy variable
df$DSouth <- ifelse(df$state %in% c("Andhra Pradesh", "Tamil Nadu", "Kerala", "Telangana"), 1, 0)

# Create DEast dummy variable
df$DEast <- ifelse(df$state %in% c("Assam", "Manipur", "Tripura", "Sikkim", "West Bengal", "Odisha"), 1, 0)

# Create DWest dummy variable
df$DWest <- ifelse(df$state %in% c("Gujarat", "Maharashtra", "Karnataka"), 1, 0)

# Create DNorth dummy variable
df$DNorth <- ifelse(df$state %in% c("Jammu And Kashmir", "Himachal Pradesh", "Delhi", "Punjab", "Haryana", "Rajasthan", "Uttarakhand"), 1, 0)

# Create DCentre dummy variable
df$DCentre <- ifelse(df$state %in% c("Madhya Pradesh", "Uttar Pradesh", "Chhattisgarh", "Jharkhand"), 1, 0)

# ---- All required dummy variables created and verified too!!------
# Some results from verifying that I observed:
# Haryana state has no data
# Some north eastern states like Mizoram, Meghalaya etc are in the data but NOT included in our dummy
# variable DEast, as given in the question statement. So, I am just following the question!

# Q3
# Running the following regression model:
# RL(ID) = γ0 + γ1(DSouth) + ε(ID) where ε(ID) is a random error.

road_lm_DSouth <- lm(formula = road_sum ~ DSouth, data = df)
summary(road_lm_DSouth)


# --- Ques 4 ---
# Running the below multiple linear regression model
# RLID = η0 + η1DSouth + η2DNorth + η3DEast + η4DWest + θID

road_mlrm <- lm(formula = road_sum ~ DSouth + DNorth + DEast + DWest, data = df)
summary(road_mlrm)

#--Ques 5 ---
# Running the below MLRM
# RL(ID) = α0 + α1trend(ID) + α2DSouth + α3DSouth ∗ trend + δ(ID) where δID is a random error.

road_mlrm_2 <- lm(formula = road_sum ~ trend + DSouth + (DSouth*trend), data = df)
summary(road_mlrm_2)




















