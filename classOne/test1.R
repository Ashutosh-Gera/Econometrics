a = 10
a

b = 1:10
b
class(b)

c = 1:50
c

x=c^2
x

?max

max(x)
min(x)

#summarizing x:
summary(x)

var(x)
(var(x))^(1/2)

#subsetting x
y = x[x<200]
y

#even-ordd parts of x
e <- subset(x, x%%2 == 0)
e

o <- subset(x, x%%2 == 1)
o


roadLength_districtLevel <- read.csv("/home/ash/Desktop/Econ/classOne/roadlength_districtlevel.csv")
df <- roadLength_districtLevel

#View the complete dataset:
View(roadLength_districtLevel)
#What type of an object is the data?
class(df)
dim(df)
head(df)
df[1, "state"]
head(df$state)

unique(df$state)
length(unique(df$state))
length(unique(df$district))
length(unique(df$yearcode))

#Today we will be working with road length:

rdLength <- df$road_sum
#Road length is a vector


#Histogram
hist(rdLength,
     main = "District-wise Road Length_2011-22",
     xlab = "Road Length in Kilometres",
     col = "darkmagenta",
     freq = TRUE
)

#Problem: Road length is non numeric
#COnvert "." to Misiing

rdLength <- replace(rdLength, rdLength=='.','')
#Does this solve the prpblem?

rdLength <- as.numeric(rdLength)

hist(rdLength,
     main = "Road Length",
     xlab = "kms",
     col = "cyan",
     freq = TRUE,
)

# summary(rdLength)

#Lets try drawing a boxplot

#First, curtail the distribution once again to 1000
df2 <- df[df$road_sum<1000,]

#Trend Regression
library(dplyr)

df3 <- df2 %>%
  mutate(trend = case_when(yearcode == 2011 ~ 1,
                           yearcode == 2012 ~ 2,
                           yearcode == 2013 ~ 3,
                           yearcode == 2014 ~ 4,
                           yearcode == 2015 ~ 5,
                           yearcode == 2016 ~ 6,
                           yearcode == 2017 ~ 7,
                           yearcode == 2018 ~ 8,
                           TRUE ~ 11))
df2$road_sum <- replace(df2$road_sum, df2$road_sum=='.','')
#Does this solve the prpblem?

df2$road_sum <- as.numeric(df2$road_sum)

df2$road_log <- log(df2$road_sum)
df3 <- df2 %>%
  mutate(trend = case_when(yearcode == 2011 ~ 1,
                           yearcode == 2012 ~ 2,
                           yearcode == 2013 ~ 3,
                           yearcode == 2014 ~ 4,
                           yearcode == 2015 ~ 5,
                           yearcode == 2016 ~ 6,
                           yearcode == 2017 ~ 7,
                           yearcode == 2018 ~ 8,
                           TRUE ~ 11))

#box plot
road_lm <- lm(formula = road_sum ~ trend, data = df3)
summary(road_lm)
    
#Predicted values(Y-Hat)
df3$yhat = 0.2804*(df3$trend)
# df3$uhat = 

#Impoting GW quality
gw <- NDAP_REPORT_7065

#take a subset
gw <- subset(gw, select = c(District,District.LGD.Code,Ground.Water.Station.Name,Ground.Water.Station.Latitude,Ground.Water.Station.Longitude,Amount.of.Calcium,YearCode))

#Drop NAs in district name
gw<-gw[!is.na(gw$District.LGD.Code),]

#Examine the District-year iD in the road length data
View(df3)

#Now, I want to genrate a similar dyid in the gw data
gw$dyid <- paste(gw$District, gw$YearCode)

#the default separator is the problem
gw$dyid <- paste(gw$District, gw$YearCode, sep ="")

#Take a summary of calcium amount at the DYID level:

#Convert calcium amount to a numeric variable:

gw[is.na(gw)] <- ''
gw$Amount.of.Calcium <- as.numeric(gw$Amount.of.Calcium)
View(gw)
#Take a mean by DYID:
gw_agg <- gw %>% group_by(dyid) %>%
  
  summarise(mean_calcium=mean(Amount.of.Calcium),
            
            .groups = 'drop')

#REDO:

gw_agg <- gw %>% group_by(dyid) %>%
  
  summarise(mean_calcium=mean(Amount.of.Calcium, na.rm = TRUE),
            
            .groups = 'drop')
#View(gw_agg)
summary(gw_agg$mean_calcium)
#View(gw)

#Have the duplicates been removed  yet?
length(unique(gw_agg$dyid))


#MERGING:

df4<-gw_agg

#I am here choosing to retain only those district-years which are available in both
#datasets:
  
df5 = merge(x = df3, y = df4, by = "dyid")
View(df5)
#What if I retain all district-years in the road length data, here the 'X' datset?

df6 = merge(x = df3, y = df4, by = "dyid", all.x = TRUE)

#scatter
#ggplot(df5, aes(x=road_sum, y = mean_calcium) + geom_point())
ggplot() +
  geom_boxplot(df5, mapping = aes(x = road_sum, y = mean_calcium))













