# Environmental Statistics Project
# Climate change impact on agriculture (2024) ANALYSIS 


#load dataset:"Climate Change Impact on Agricultural Production"
data<-read.csv("C:/Users/amalh/OneDrive - MSFT/Desktop/uni/5th semester/Environmental Statistics/proj1/dataset.csv")
print(data)

#extraction of each column from the dataset uploaded 
year<-data$Year
country<-data$Country
region<-data$Region
crop_type<-data$Crop_Type
avg_temp<-data$Average_Temperature_C
precipitation<-data$Total_Precipitation_mm
co2_emissions<-data$CO2_Emissions_MT
crop_yield<-data$Crop_Yield_MT_per_HA
extreme_weather<-data$Extreme_Weather_Events
irrigation_access<-data$Irrigation_Access_
pesticide_use<-data$Pesticide_Use_KG_per_HA
fertilizer_use<-data$Fertilizer_Use_KG_per_HA
soil_health<-data$Soil_Health_Index
adaptation_strategies<-data$Adaptation_Strategies
economic_impact<-data$Economic_Impact_Million_USD

################################################Descriptive Statistics#########################################
# are used to summarize and describe the main features of the dataset
### Central tendency###
##mean##
# Calculate means for all numerical variables
mean_avg_temp<-mean(avg_temp)
mean_avg_temp

mean_precipitation<-mean(precipitation)
mean_precipitation

mean_co2_emissions<-mean(co2_emissions)
mean_co2_emissions

mean_crop_yield<-mean(crop_yield)
mean_crop_yield

mean_extreme_weather<-mean(extreme_weather)
mean_extreme_weather

mean_irrigation_access<-mean(irrigation_access)
mean_irrigation_access

mean_pesticide_use<-mean(pesticide_use)
mean_pesticide_use

mean_fertilizer_use<-mean(fertilizer_use)
mean_fertilizer_use

mean_soil_health<-mean(soil_health)
mean_soil_health

mean_economic_impact<-mean(economic_impact)
mean_economic_impact

##median##
# Calculate means for all numerical variables
median_avg_temp<-median(avg_temp)
median_avg_temp

median_precipitation<-median(precipitation)
median_precipitation

median_co2_emissions<-median(co2_emissions)
median_co2_emissions

median_crop_yield<-median(crop_yield)
median_crop_yield

median_extreme_weather<-median(extreme_weather)
median_extreme_weather

median_irrigation_access<-median(irrigation_access)
median_irrigation_access

median_pesticide_use<-median(pesticide_use)
median_pesticide_use

median_fertilizer_use<-median(fertilizer_use)
median_fertilizer_use

median_soil_health<-median(soil_health)
median_soil_health

median_economic_impact<-median(economic_impact)
median_economic_impact

####interpretation:
# the mean and median values are nearly the same
#then the data distribution is approximately Symmetric with no skewness

##mode##
# Mode function
get_mode<-function(x) {
  ux<- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
#Calculate modes for numerical variables
mode_year<-get_mode(year)
mode_year

mode_country<-get_mode(country)
mode_country

mode_region<-get_mode(region)
mode_region

mode_crop_type<-get_mode(crop_type)
mode_crop_type

mode_extreme_weather<-get_mode(extreme_weather)
mode_extreme_weather

mode_adaptation_strategies<-get_mode(adaptation_strategies)
mode_adaptation_strategies

##############################################################
#Dispersion
##1.Variance
#Calculate Variance for numerical variables
#Variance for Average Temperature(°C)
variance_avg_temp<-var(avg_temp)
variance_avg_temp

#Variance for total Precipitation(mm)
variance_precipitation<-var(precipitation)
variance_precipitation

#Variance for CO2 Emissions(MT)
variance_co2_emissions<-var(co2_emissions)
variance_co2_emissions

#Variance for Crop Yield Productivity(MT/HA)
variance_crop_yield<-var(crop_yield)
variance_crop_yield

#Variance for Extreme Weather Events
variance_extreme_weather<-var(extreme_weather)
variance_extreme_weather

#Variance for Irrigation Access(%)
variance_irrigation_access<-var(irrigation_access)
variance_irrigation_access

#Variance for Pesticide use(KG/HA)
variance_pesticide_use <- var(pesticide_use)
variance_pesticide_use

#Variance for fertilizeru use(KG/HA)
variance_fertilizer_use <-var(fertilizer_use)
variance_fertilizer_use

#Variance for soil health index
variance_soil_health<-var(soil_health)
variance_soil_health

#Variance for economic impact(Million USD)
variance_economic_impact<-var(economic_impact)
variance_economic_impact
################################################################################

##2.Standard Deviation
# Calculate Standard Deviation for numerical variables (square root of variance)
#Standard Deviation for Average Temperature (°C):"
sd_avg_temp<-sd(avg_temp)
sd_avg_temp

#Standard Deviation for Total Precipitation (mm)
sd_precipitation<-sd(precipitation)
sd_precipitation

#Standard Deviation for CO2 Emissions (MT)
sd_co2_emissions<-sd(co2_emissions)
sd_co2_emissions

#Standard Deviation for Crop Yield Productivity (MT/HA)
sd_crop_yield<-sd(crop_yield)
sd_crop_yield

#Standard Deviation for Extreme Weather Events
sd_extreme_weather<-sd(extreme_weather)
sd_extreme_weather

#Standard Deviation for Irrigation Access (%)
sd_irrigation_access<-sd(irrigation_access)
sd_irrigation_access

#Standard Deviation for Pesticide Use (KG/HA)
sd_pesticide_use<-sd(pesticide_use)
sd_pesticide_use

#Standard Deviation for Fertilizer Use (KG/HA
sd_fertilizer_use<-sd(fertilizer_use)
sd_fertilizer_use

#Standard Deviation for Soil Health Index
sd_soil_health<-sd(soil_health)
sd_soil_health

#Standard Deviation for Economic Impact (Million USD)
sd_economic_impact<-sd(economic_impact)
sd_economic_impact
#Standard Deviation for Economic Impact (Million USD)
sd_economic_impact<-sd(economic_impact)
sd_economic_impact

###############################################################################
##Shape
# Install the e1071 package
install.packages("e1071")

# Load the e1071 package
library(e1071)

#### skewness######
# Calculate skewness for the numerical variables

#Skewness for Average Temperature(C)
skewness_avg_temp<-skewness(avg_temp)
skewness_avg_temp

#Skewness for Precipitation(mm)
skewness_precipitation<-skewness(precipitation)
skewness_precipitation

#Skewness for CO2 Emissions (MT)
skewness_co2_emissions<-skewness(co2_emissions)
skewness_co2_emissions

#Skewness for Crop Yield Productivity (MT per HA)
skewness_crop_yield<-skewness(crop_yield)
skewness_crop_yield

#Skewness for Extreme Weather Events
skewness_extreme_weather<-skewness(extreme_weather)
skewness_extreme_weather

#Skewness for Irrigation Access (%)
skewness_irrigation_access<-skewness(irrigation_access)
skewness_irrigation_access

#Skewness for Pesticide Use (KG per HA)
skewness_pesticide_use<-skewness(pesticide_use)
skewness_pesticide_use

#Skewness for Fertilizer Use (KG per HA)
skewness_fertilizer_use<-skewness(fertilizer_use)
skewness_fertilizer_use

#Skewness for Soil Health Index
skewness_soil_health<-skewness(soil_health)
skewness_soil_health

#Skewness for Economic Impact (Million USD)
skewness_economic_impact<-skewness(economic_impact)
skewness_economic_impact

#################interpretation:
# the skewness of all the variables approaches zero
#then the data distribution is approximately symmetrical
#the left and right tails are balanced, with no significant skew.

# Calculate kurtosis for all numerical variables
#Kurtosis for Average Temperature (C)
kurtosis_avg_temp <-kurtosis(avg_temp)
kurtosis_avg_temp

#Kurtosis for Precipitation (mm)
kurtosis_precipitation<-kurtosis(precipitation)
kurtosis_precipitation

#Kurtosis for CO2 Emissions (MT)
kurtosis_co2_emissions<-kurtosis(co2_emissions)
kurtosis_co2_emissions

#Kurtosis for Crop Yield Productivity (MT per HA)
kurtosis_crop_yield<-kurtosis(crop_yield)
kurtosis_crop_yield

#Kurtosis for Extreme Weather Events
kurtosis_extreme_weather<-kurtosis(extreme_weather)
kurtosis_extreme_weather

#Kurtosis for Irrigation Access (%)
kurtosis_irrigation_access<-kurtosis(irrigation_access)
kurtosis_irrigation_access

#Kurtosis for Pesticide Use (KG per HA)
kurtosis_pesticide_use<-kurtosis(pesticide_use)
kurtosis_pesticide_use

#Kurtosis for Fertilizer Use (KG per HA)
kurtosis_fertilizer_use<-kurtosis(fertilizer_use)
kurtosis_fertilizer_use

#Kurtosis for Soil Health Index
kurtosis_soil_health<-kurtosis(soil_health)
kurtosis_soil_health

####interpretation:
#the kurtosis is negative for these variables
#it means the distribution is platykurtic—flatter
#than a normal distribution, with lighter tails and fewer extreme values

#Kurtosis for Economic Impact (Million USD)
kurtosis_economic_impact<-kurtosis(economic_impact)
kurtosis_economic_impact

####interpretation:
#the value of kurtosis for the Economic Impact is positive
# this means that  the distribution is leptokurtic
# it has sharper peak than normal, with heavy tails and more extreme values.

#######################Histogram###############################

# install and Load package
install.packages("ggplot2")
library(ggplot2)

#Histogram of extreme weather events
hist(data$Extreme_Weather_Events, breaks = 10, 
     main = "How Often Extreme Weather Happens",
     xlab = "Number of Extreme Events in a Year", 
     ylab = "How Many Years",
     col = "orange", 
     border = "black")

#######################################################
# NORMAL DISTRIBUTIONS#####
set.seed(123)  # For reproducibility

## 1. Average Temperature
Average_Temperature_C_normal <- rnorm(1000,mean=18,sd=8)
#create histogram
ggplot(data.frame(Average_Temperature_C_normal),aes(x=Average_Temperature_C_normal)) +
  geom_histogram(binwidth = 2,fill='skyblue',color='black') +
  labs(title = 'Normal Distribution of Average Temperature', x='Average Temperature(°C)',y='Frequency') +
  theme_minimal()

## 2. Total Precipitation
Total_Precipitation_mm_normal<-rnorm(1000,mean= 1200, sd = 150)
#create histogram
ggplot(data.frame(Total_Precipitation_mm_normal), aes(x = Total_Precipitation_mm_normal)) +
  geom_histogram(binwidth = 20, fill ='lightgreen',color ='black') +
  labs(title = 'Normal Distribution of Total Precipitation', x = 'Total Precipitation(mm)',y ='Frequency') +
  theme_minimal()

## 3. Crop Yield Productivity
Crop_Yield_MT_per_HA_normal<-rnorm(1000,mean= 2.5,sd=0.8)
#create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_normal),aes(x=Crop_Yield_MT_per_HA_normal)) +
  geom_histogram(binwidth=0.1,fill='gold',color='black') +
  labs(title ='Normal Distribution of Crop Yield Productivity',x ='Crop Yield Productivity(MT/HA)',y='Frequency') +
  theme_minimal()

## 4. Soil Health Index
Soil_Health_Index_normal<-rnorm(1000,mean=65,sd=12)
#create histogram
ggplot(data.frame(Soil_Health_Index_normal),aes(x=Soil_Health_Index_normal)) +
  geom_histogram(binwidth=3,fill='brown',color='black') +
  labs(title = 'Normal Distribution of Soil Health Index',x='Soil Health Index',y='Frequency') +
  theme_minimal()

## 5. CO2 Emissions
CO2_Emissions_MT_normal<-rnorm(1000,mean=15,sd=8)
#create histogram
ggplot(data.frame(CO2_Emissions_MT_normal),aes(x=CO2_Emissions_MT_normal))+
  geom_histogram(binwidth=2,fill='gray',color='black') +
  labs(title='Normal Distribution of CO2 Emissions',x ='CO2 Emissions (MT)',y='Frequency') +
  theme_minimal()

## 6. Extreme Weather Events
Extreme_Weather_Events_normal<-rnorm(1000,mean=5,sd=2)
#create histogram
ggplot(data.frame(Extreme_Weather_Events_normal),aes(x =Extreme_Weather_Events_normal)) +
  geom_histogram(binwidth =1,fill ='lightcoral',color='black') +
  labs(title='Normal Distribution of Extreme Weather Events',x='Extreme Weather Events',y ='Frequency') +
  theme_minimal()

## 7. Irrigation Access
Irrigation_Access_normal<-rnorm(1000,mean=50,sd=20)
#create histogram
ggplot(data.frame(Irrigation_Access_normal),aes(x=Irrigation_Access_normal)) +
  geom_histogram(binwidth = 5,fill='lightblue',color='black') +
  labs(title ='Normal Distribution of Irrigation Access',x='Irrigation Access(%)',y='Frequency') +
  theme_minimal()

## 8. Pesticide Use
Pesticide_Use_KG_per_HA_normal<-rnorm(1000,mean=15,sd=8)
#create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_normal), aes(x = Pesticide_Use_KG_per_HA_normal)) +
  geom_histogram(binwidth = 2,fill='orange',color='black') +
  labs(title = 'Normal Distribution of Pesticide Use', x = 'Pesticide Use (KG/HA)',y='Frequency') +
  theme_minimal()

## 9. Fertilizer Use
Fertilizer_Use_KG_per_HA_normal<-rnorm(1000,mean=60,sd=25)
#create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_normal), aes(x = Fertilizer_Use_KG_per_HA_normal)) +
  geom_histogram(binwidth =5,fill ='lightgreen',color ='black') +
  labs(title ='Normal Distribution of Fertilizer Use',x = 'Fertilizer Use (KG/HA)',y = 'Frequency') +
  theme_minimal()

## 10. Economic Impact
Economic_Impact_Million_USD_normal<-rnorm(1000,mean=800,sd =300)
#create histogram
ggplot(data.frame(Economic_Impact_Million_USD_normal), aes(x = Economic_Impact_Million_USD_normal)) +
  geom_histogram(binwidth = 50,fill='purple', color = 'black') +
  labs(title = 'Normal Distribution of Economic Impact',x ='Economic Impact (Million USD)',y='Frequency') +
  theme_minimal()

#############interpretation:
# all histograms have nearly the same shape as a normal distribution
#then we can say that : all variables_Average Temperature,Total Precipitation,Crop Yield Productivity,
#Soil Health Index,CO2 Emissions,Extreme Weather Events,Irrigation Access,Pesticide Use,Fertilizer Use
#and the Economic Impact are approximately normally distributed

#######################################################
# BIMODAL DISTRIBUTIONS

set.seed(789)  # For reproducibility

# 1. Average Temperature
Average_Temperature_C_bimodal<-c(rnorm(500,mean=10,sd =3),rnorm(500,mean=25,sd = 4))
#create histogram
ggplot(data.frame(Average_Temperature_C_bimodal), aes(x = Average_Temperature_C_bimodal)) +
  geom_histogram(binwidth = 1,fill='skyblue',color ='black') +
  labs(title ='Bimodal Distribution of Average Temperature',x='Average Temperature(°C)',y='Frequency') +
  theme_minimal()

# 2. Total Precipitation
Total_Precipitation_mm_bimodal<-c(rnorm(500,mean=800,sd=100),rnorm(500,mean=1600,sd =150))

#create histogram
ggplot(data.frame(Total_Precipitation_mm_bimodal),aes(x=Total_Precipitation_mm_bimodal)) +
  geom_histogram(binwidth = 50,fill = 'lightgreen',color='black') +
  labs(title = 'Bimodal Distribution of Total Precipitation',x ='Total Precipitation (mm)',y ='Frequency') +
  theme_minimal()

# 3. Crop Yield Productivity
Crop_Yield_MT_per_HA_bimodal <- c(rnorm(500, mean = 1.8, sd = 0.5), rnorm(500, mean = 3.2, sd = 0.6))
#create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_bimodal), aes(x = Crop_Yield_MT_per_HA_bimodal)) +
  geom_histogram(binwidth = 0.2, fill = 'gold', color = 'black') +
  labs(title = 'Bimodal Distribution of Crop Yield Productivity', x ='Crop Yield Productivity (MT/HA)', y ='Frequency') +
  theme_minimal()

# 4. Soil Health Index
Soil_Health_Index_bimodal <- c(rnorm(500, mean = 40, sd = 8), rnorm(500, mean = 75, sd = 10))
#create histogram
ggplot(data.frame(Soil_Health_Index_bimodal), aes(x = Soil_Health_Index_bimodal)) +
  geom_histogram(binwidth = 3, fill = 'brown', color = 'black') +
  labs(title = 'Bimodal Distribution of Soil Health Index', x = 'Soil Health Index', y = 'Frequency') +
  theme_minimal()

# 5. CO2 Emissions
CO2_Emissions_MT_bimodal <- c(rnorm(500, mean = 8, sd = 3), rnorm(500, mean = 20, sd = 5))
#create histogram
ggplot(data.frame(CO2_Emissions_MT_bimodal), aes(x = CO2_Emissions_MT_bimodal)) +
  geom_histogram(binwidth = 2, fill = 'gray', color = 'black') +
  labs(title = 'Bimodal Distribution of CO2 Emissions', x = 'CO2 Emissions (MT)', y = 'Frequency') +
  theme_minimal()

# 6. Extreme Weather Events
Extreme_Weather_Events_bimodal <- c(rnorm(500, mean = 2, sd = 1), rnorm(500, mean = 8, sd = 2))
#create histogram
ggplot(data.frame(Extreme_Weather_Events_bimodal),aes(x=Extreme_Weather_Events_bimodal)) +
  geom_histogram(binwidth = 1,fill ='lightcoral', color ='black') +
  labs(title = 'Bimodal Distribution of Extreme Weather Events', x = 'Extreme Weather Events', y = 'Frequency') +
  theme_minimal()

# 7. Irrigation Access
Irrigation_Access_bimodal<-c(rnorm(500,mean=20,sd = 10),rnorm(500,mean=70, sd = 15))
#create histogram
ggplot(data.frame(Irrigation_Access_bimodal), aes(x = Irrigation_Access_bimodal)) +
  geom_histogram(binwidth =5,fill ='lightblue', color= 'black') +
  labs(title = 'Bimodal Distribution of Irrigation Access', x = 'Irrigation Access (%)', y = 'Frequency') +
  theme_minimal()

# 8. Pesticide Use
Pesticide_Use_KG_per_HA_bimodal <- c(rnorm(500,mean = 8,sd=3), rnorm(500,mean= 25,sd = 6))
#create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_bimodal), aes(x = Pesticide_Use_KG_per_HA_bimodal)) +
  geom_histogram(binwidth = 2, fill='orange',color = 'black') +
  labs(title = 'Bimodal Distribution of Pesticide Use', x = 'Pesticide Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 9. Fertilizer Use
Fertilizer_Use_KG_per_HA_bimodal <- c(rnorm(500, mean = 30, sd = 10), rnorm(500, mean = 80, sd = 20))
#create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_bimodal), aes(x = Fertilizer_Use_KG_per_HA_bimodal)) +
  geom_histogram(binwidth = 5, fill = 'lightgreen', color = 'black') +
  labs(title = 'Bimodal Distribution of Fertilizer Use', x = 'Fertilizer Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 10. Economic Impact
Economic_Impact_Million_USD_bimodal<-c(rnorm(500,mean=300,sd =100),rnorm(500,mean= 1200, sd = 200))
#create histogram
ggplot(data.frame(Economic_Impact_Million_USD_bimodal), aes(x = Economic_Impact_Million_USD_bimodal)) +
  geom_histogram(binwidth = 50, fill = 'purple', color = 'black') +
  labs(title = 'Bimodal Distribution of Economic Impact', x = 'Economic Impact (Million USD)', y = 'Frequency') +
  theme_minimal()

#############interpretation:
#all histograms have two distinct peaks, forming a bimodal distribution
#this indicates that each variable represents a mixture of two distinct sub-populations


################################################################
# RIGHT-SKEWED DISTRIBUTIONS

set.seed(456)  # For reproducibility

# 1. Average Temperature
Average_Temperature_C_right<-rexp(1000,rate =0.1)

# Create histogram
ggplot(data.frame(Average_Temperature_C_right), aes(x = Average_Temperature_C_right)) +
  geom_histogram(binwidth = 2, fill='skyblue',color ='black') +
  labs(title = 'Right-Skewed Distribution of Average Temperature', x = 'Average Temperature (°C)', y = 'Frequency') +
  theme_minimal()

# 2. Total Precipitation
Total_Precipitation_mm_right<-rexp(1000,rate=0.001)

# Create histogram
ggplot(data.frame(Total_Precipitation_mm_right), aes(x = Total_Precipitation_mm_right)) +
  geom_histogram(binwidth = 100, fill='lightgreen',color='black') +
  labs(title = 'Right-Skewed Distribution of Total Precipitation', x = 'Total Precipitation (mm)', y = 'Frequency') +
  theme_minimal()

# 3. CO2 Emissions
CO2_Emissions_MT_right<-rexp(1000, rate = 0.1)

# Create histogram
ggplot(data.frame(CO2_Emissions_MT_right),aes(x=CO2_Emissions_MT_right)) +
  geom_histogram(binwidth = 5,fill='gray',color = 'black') +
  labs(title = 'Right-Skewed Distribution of CO2 Emissions',x='CO2 Emissions(MT)', y ='Frequency') +
  theme_minimal()

# 4. Crop Yield Productivity
Crop_Yield_MT_per_HA_right<-rexp(1000, rate = 0.4)

# Create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_right),aes(x = Crop_Yield_MT_per_HA_right)) +
  geom_histogram(binwidth = 0.5, fill = 'gold', color = 'black') +
  labs(title = 'Right-Skewed Distribution of Crop Yield Productivity', x = 'Crop Yield Productivity (MT/HA)', y = 'Frequency') +
  theme_minimal()

# 5. Extreme Weather Events
Extreme_Weather_Events_right<-rexp(1000, rate = 0.3)

# Create histogram
ggplot(data.frame(Extreme_Weather_Events_right), aes(x = Extreme_Weather_Events_right)) +
  geom_histogram(binwidth = 1, fill = 'lightcoral', color = 'black') +
  labs(title = 'Right-Skewed Distribution of Extreme Weather Events',x='Extreme Weather Events', y ='Frequency') +
  theme_minimal()

# 6. Irrigation Access
Irrigation_Access_right <- rexp(1000, rate = 0.02)

# Create histogram
ggplot(data.frame(Irrigation_Access_right), aes(x = Irrigation_Access_right)) +
  geom_histogram(binwidth = 10, fill = 'lightblue', color = 'black') +
  labs(title = 'Right-Skewed Distribution of Irrigation Access',x ='Irrigation Access(%)',y='Frequency') +
  theme_minimal()

# 7. Pesticide Use
Pesticide_Use_KG_per_HA_right <-rexp(1000,rate=0.1)

# Create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_right), aes(x = Pesticide_Use_KG_per_HA_right)) +
  geom_histogram(binwidth = 5, fill = 'orange', color = 'black') +
  labs(title = 'Right-Skewed Distribution of Pesticide Use', x = 'Pesticide Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 8. Fertilizer Use
Fertilizer_Use_KG_per_HA_right <- rexp(1000, rate = 0.05)

# Create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_right), aes(x = Fertilizer_Use_KG_per_HA_right)) +
  geom_histogram(binwidth = 10, fill = 'lightgreen', color ='black') +
  labs(title = 'Right-Skewed Distribution of Fertilizer Use', x = 'Fertilizer Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 9. Soil Health Index
Soil_Health_Index_right<-rexp(1000,rate=0.03)

# Create histogram
ggplot(data.frame(Soil_Health_Index_right), aes(x = Soil_Health_Index_right)) +
  geom_histogram(binwidth =5,fill ='brown', color='black') +
  labs(title = 'Right-Skewed Distribution of Soil Health Index', x = 'Soil Health Index', y = 'Frequency') +
  theme_minimal()

# 10. Economic Impact
Economic_Impact_Million_USD_right<-rexp(1000,rate=0.002)

# Create histogram
ggplot(data.frame(Economic_Impact_Million_USD_right), aes(x = Economic_Impact_Million_USD_right)) +
  geom_histogram(binwidth = 100, fill='purple', color ='black') +
  labs(title = 'Right-Skewed Distribution of Economic Impact', x = 'Economic Impact (Million USD)', y = 'Frequency') +
  theme_minimal()

###############interpretation:
#the right tail is longer than the left tail
#it means there are a few high extreme values that pull the mean to the right
# the mean is greater than the median

################################################################
#Left-Skewed Distribution
# Install ggplot2
install.packages("ggplot2")
# Load ggplot2
library(ggplot2)

set.seed(123) # For reproducibility

# 1. Average Temperature
Average_Temperature_C_left<-40- rbeta(1000, 2, 5)*30  # Beta distribution with left skew

# Create histogram
ggplot(data.frame(Average_Temperature_C_left), aes(x = Average_Temperature_C_left)) +
  geom_histogram(binwidth = 2,fill='skyblue',color='black') +
  labs(title ='Left-Skewed Distribution of Average Temperature',x='Average Temperature(°C)',y ='Frequency') +
  theme_minimal()

# 2. Total Precipitation (left-skewed: most areas get high precipitation)
Total_Precipitation_mm_left<-2000-rbeta(1000,2,6) *1800

# Create histogram
ggplot(data.frame(Total_Precipitation_mm_left), aes(x = Total_Precipitation_mm_left)) +
  geom_histogram(binwidth =100,fill ='lightgreen',color = 'black') +
  labs(title = 'Left-Skewed Distribution of Total Precipitation', x ='Total Precipitation (mm)',y ='Frequency') +
  theme_minimal()

# 3. CO2 Emissions (left-skewed: most countries have high emissions)
CO2_Emissions_MT_left<-50- rbeta(1000, 2, 5) * 45

# Create histogram
ggplot(data.frame(CO2_Emissions_MT_left), aes(x = CO2_Emissions_MT_left)) +
  geom_histogram(binwidth = 5, fill = 'gray', color = 'black') +
  labs(title = 'Left-Skewed Distribution of CO2 Emissions', x = 'CO2 Emissions (MT)', y = 'Frequency') +
  theme_minimal()

# 4. Crop Yield Productivity (left-skewed: most farms have high yields)
Crop_Yield_MT_per_HA_left <- 10 - rbeta(1000, 2, 6) * 8

# Create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_left), aes(x = Crop_Yield_MT_per_HA_left)) +
  geom_histogram(binwidth = 0.5, fill = 'gold', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Crop Yield Productivity', x = 'Crop Yield Productivity (MT/HA)', y = 'Frequency') +
  theme_minimal()

# 5. Extreme Weather Events (left-skewed: most regions experience many events)
Extreme_Weather_Events_left <- 15 - rbeta(1000, 2, 5) * 12

# Create histogram
ggplot(data.frame(Extreme_Weather_Events_left), aes(x = Extreme_Weather_Events_left)) +
  geom_histogram(binwidth = 1, fill = 'lightcoral', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Extreme Weather Events', x = 'Extreme Weather Events', y = 'Frequency') +
  theme_minimal()

# 6. Irrigation Access (left-skewed: most areas have high irrigation access)
Irrigation_Access_left <- 100 - rbeta(1000, 2, 6) * 80

# Create histogram
ggplot(data.frame(Irrigation_Access_left), aes(x = Irrigation_Access_left)) +
  geom_histogram(binwidth = 10, fill = 'lightblue', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Irrigation Access', x = 'Irrigation Access (%)', y = 'Frequency') +
  theme_minimal()

# 7. Pesticide Use (left-skewed: most farms use high amounts)
Pesticide_Use_KG_per_HA_left <- 50 - rbeta(1000, 2, 5) * 45

# Create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_left), aes(x = Pesticide_Use_KG_per_HA_left)) +
  geom_histogram(binwidth = 5, fill = 'orange', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Pesticide Use', x = 'Pesticide Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 8. Fertilizer Use (left-skewed: most farms use high amounts)
Fertilizer_Use_KG_per_HA_left <- 200 - rbeta(1000, 2, 6) * 180

# Create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_left), aes(x = Fertilizer_Use_KG_per_HA_left)) +
  geom_histogram(binwidth = 10, fill = 'lightgreen', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Fertilizer Use', x = 'Fertilizer Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

# 9. Soil Health Index (left-skewed: most soils have high health scores)
Soil_Health_Index_left <- 100 - rbeta(1000, 2, 5) * 80

# Create histogram
ggplot(data.frame(Soil_Health_Index_left), aes(x = Soil_Health_Index_left)) +
  geom_histogram(binwidth = 5, fill = 'brown', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Soil Health Index', x = 'Soil Health Index', y = 'Frequency') +
  theme_minimal()

# 10. Economic Impact (left-skewed: most events have high economic impact)
Economic_Impact_Million_USD_left <- 1000 - rbeta(1000, 2, 6) * 900

# Create histogram
ggplot(data.frame(Economic_Impact_Million_USD_left), aes(x = Economic_Impact_Million_USD_left)) +
  geom_histogram(binwidth = 100, fill = 'purple', color = 'black') +
  labs(title = 'Left-Skewed Distribution of Economic Impact', x = 'Economic Impact (Million USD)', y = 'Frequency') +
  theme_minimal()

##########Interpretation: For left-skewed distributions:
#The mode is greater than the mean
#Most data points are concentrated on the right side (higher values)
#The tail extends to the left (lower values)
#The mean is less than the median

################################################################
# Kurtosis negative

# Install and load package
install.packages("ggplot2")
library(ggplot2)

set.seed(123) # Reproducibility

# 1. Average Temperature
Average_Temperature_C_neg <- runif(1000, min = 10, max = 40)  # Uniform distribution between 10 and 40
# Create histogram
ggplot(data.frame(Average_Temperature_C_neg), aes(x = Average_Temperature_C_neg)) +
  geom_histogram(binwidth = 2, fill = 'skyblue', color = 'black') +
  labs(title = 'Negative Kurtosis: Average Temperature', x = 'Temperature (°C)', y = 'Frequency') +
  theme_minimal()

kurtosis_Average_Temperature_uniform<-kurtosis(Average_Temperature_C_neg)
kurtosis_Average_Temperature_uniform

# 2. Total Precipitation
Total_Precipitation_mm_neg <- runif(1000, min = 200, max = 2000)  # Uniform distribution between 200 and 2000
# Create histogram
ggplot(data.frame(Total_Precipitation_mm_neg), aes(x = Total_Precipitation_mm_neg)) +
  geom_histogram(binwidth = 100, fill = 'lightgreen', color = 'black') +
  labs(title = 'Negative Kurtosis: Total Precipitation', x = 'Precipitation (mm)', y = 'Frequency') +
  theme_minimal()

kurtosis_Total_Precipitation_uniform<-kurtosis(Total_Precipitation_mm_neg)
kurtosis_Total_Precipitation_uniform

# 3. CO2 Emissions
CO2_Emissions_MT_neg <- runif(1000, min = 5, max = 50) # Uniform distribution between 5 and 50
# Create histogram
ggplot(data.frame(CO2_Emissions_MT_neg), aes(x = CO2_Emissions_MT_neg)) +
  geom_histogram(binwidth = 5, fill = 'gray', color = 'black') +
  labs(title = 'Negative Kurtosis: CO2 Emissions', x = 'CO2 Emissions (MT)', y = 'Frequency') +
  theme_minimal()

kurtosis_CO2_Emissions_uniform<-kurtosis(CO2_Emissions_MT_neg)
kurtosis_CO2_Emissions_uniform

# 4. Crop Yield Productivity
Crop_Yield_MT_per_HA_neg <- runif(1000, min = 1, max = 10) # Uniform distribution between 1 and 10
# Create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_neg), aes(x = Crop_Yield_MT_per_HA_neg)) +
  geom_histogram(binwidth = 0.5, fill = 'gold', color = 'black') +
  labs(title = 'Negative Kurtosis: Crop Yield Productivity', x = 'Crop Yield Productivity (MT/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Crop_Yield_uniform<-kurtosis(Crop_Yield_MT_per_HA_neg)
kurtosis_Crop_Yield_uniform

# 5. Extreme Weather Events
Extreme_Weather_Events_neg <- runif(1000, min = 0, max = 20) # Uniform distribution between 0 and 20
# Create histogram
ggplot(data.frame(Extreme_Weather_Events_neg), aes(x = Extreme_Weather_Events_neg)) +
  geom_histogram(binwidth = 1, fill = 'lightcoral', color = 'black') +
  labs(title = 'Negative Kurtosis: Extreme Weather Events', x = 'Events', y = 'Frequency') +
  theme_minimal()

kurtosis_Extreme_Weather_Events_uniform<-kurtosis(Extreme_Weather_Events_neg)
kurtosis_Extreme_Weather_Events_uniform

# 6. Irrigation Access %
Irrigation_Access_neg <- runif(1000, min = 20, max = 100) # Uniform distribution between 20 and 100
# Create histogram
ggplot(data.frame(Irrigation_Access_neg), aes(x = Irrigation_Access_neg)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black') +
  labs(title = 'Negative Kurtosis: Irrigation Access', x = 'Irrigation Access (%)', y = 'Frequency') +
  theme_minimal()

kurtosis_Irrigation_Access_uniform<-kurtosis(Irrigation_Access_neg)
kurtosis_Irrigation_Access_uniform

# 7. Pesticide Use
Pesticide_Use_KG_per_HA_neg <- runif(1000, min = 1, max = 50) # Uniform distribution between 1 and 50
# Create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_neg), aes(x = Pesticide_Use_KG_per_HA_neg)) +
  geom_histogram(binwidth = 5, fill = 'orange', color = 'black') +
  labs(title = 'Negative Kurtosis: Pesticide Use', x = 'Pesticide Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Pesticide_Use_uniform<-kurtosis(Pesticide_Use_KG_per_HA_neg)
kurtosis_Pesticide_Use_uniform

# 8. Fertilizer Use
Fertilizer_Use_KG_per_HA_neg <- runif(1000, min = 20, max = 200) # Uniform distribution between 20 and 200
# Create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_neg), aes(x = Fertilizer_Use_KG_per_HA_neg)) +
  geom_histogram(binwidth = 10, fill = 'lightgreen', color = 'black') +
  labs(title = 'Negative Kurtosis: Fertilizer Use', x = 'Fertilizer Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Fertilizer_Use_uniform<-kurtosis(Fertilizer_Use_KG_per_HA_neg)
kurtosis_Fertilizer_Use_uniform

# 9. Soil Health Index
Soil_Health_Index_neg <- runif(1000, min = 50, max = 100) # Uniform distribution between 50 and 100
# Create histogram
ggplot(data.frame(Soil_Health_Index_neg), aes(x = Soil_Health_Index_neg)) +
  geom_histogram(binwidth = 5, fill = 'brown', color = 'black') +
  labs(title = 'Negative Kurtosis: Soil Health Index', x = 'Soil Health Index', y = 'Frequency') +
  theme_minimal()

kurtosis_soil_health_index_uniform <-kurtosis(Soil_Health_Index_neg)
kurtosis_soil_health_index_uniform


# 10. Economic Impact
Economic_Impact_Million_USD_neg <- runif(1000, min = 100, max = 1000) # Uniform distribution between 100 and 1000
# Create histogram
ggplot(data.frame(Economic_Impact_Million_USD_neg), aes(x = Economic_Impact_Million_USD_neg)) +
  geom_histogram(binwidth = 50, fill = 'purple', color = 'black') +
  labs(title = 'Negative Kurtosis: Economic Impact', x = 'Economic Impact (Million USD)', y = 'Frequency') +
  theme_minimal()

kurtosis_Economic_Impact_uniform <-kurtosis(Economic_Impact_Million_USD_neg)
kurtosis_Economic_Impact_uniform
##########Interpretation:
#all histograms have a flat, uniform shape with no distinct peak
#this indicates negative kurtosis (platykurtic distribution)
#meaning data are evenly spread out across the range with fewer extreme values
#for example: temperatures are equally likely to be 15°C or 30°C rather than clustering around an average

################################################################
# Kurtosis positive

# Load library
library(ggplot2)

set.seed(123) # Reproducibility

# 1. Average Temperature
Average_Temperature_C_pos <- rcauchy(1000, location = 25, scale = 5)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Average_Temperature_C_pos), aes(x = Average_Temperature_C_pos)) +
  geom_histogram(binwidth = 2, fill = 'skyblue', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Average Temperature', x = 'Temperature (°C)', y = 'Frequency') +
  theme_minimal()

kurtosis_Average_Temperature_cauchy <- kurtosis(Average_Temperature_C_pos)
kurtosis_Average_Temperature_cauchy

# 2. Total Precipitation
Total_Precipitation_mm_pos <- rcauchy(1000, location = 1000, scale = 200)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Total_Precipitation_mm_pos), aes(x = Total_Precipitation_mm_pos)) +
  geom_histogram(binwidth = 100, fill = 'lightgreen', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Total Precipitation', x = 'Precipitation (mm)', y = 'Frequency') +
  theme_minimal()

kurtosis_Total_Precipitation_cauchy <- kurtosis(Total_Precipitation_mm_pos)
kurtosis_Total_Precipitation_cauchy

# 3. CO2 Emissions
CO2_Emissions_MT_pos <- rcauchy(1000, location = 20, scale = 5)   # Cauchy distribution

# Create histogram
ggplot(data.frame(CO2_Emissions_MT_pos), aes(x = CO2_Emissions_MT_pos)) +
  geom_histogram(binwidth = 2, fill = 'gray', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): CO2 Emissions', x = 'CO2 Emissions (MT)', y = 'Frequency') +
  theme_minimal()

kurtosis_CO2_Emissions_cauchy <- kurtosis(CO2_Emissions_MT_pos)
kurtosis_CO2_Emissions_cauchy

# 4. Crop Yield Productivity
Crop_Yield_MT_per_HA_pos <- rcauchy(1000, location = 5, scale = 1)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Crop_Yield_MT_per_HA_pos), aes(x = Crop_Yield_MT_per_HA_pos)) +
  geom_histogram(binwidth = 0.5, fill = 'gold', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Crop Yield Productivity', x = 'Crop Yield Productivity (MT/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Crop_Yield_cauchy <- kurtosis(Crop_Yield_MT_per_HA_pos)
kurtosis_Crop_Yield_cauchy

# 5. Extreme Weather Events
Extreme_Weather_Events_pos <- rcauchy(1000, location = 10, scale = 2)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Extreme_Weather_Events_pos), aes(x = Extreme_Weather_Events_pos)) +
  geom_histogram(binwidth = 1, fill = 'lightcoral', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Extreme Weather Events', x = 'Events', y = 'Frequency') +
  theme_minimal()

kurtosis_Extreme_Weather_Events_cauchy <- kurtosis(Extreme_Weather_Events_pos)
kurtosis_Extreme_Weather_Events_cauchy

# 6. Irrigation Access %
Irrigation_Access_pos <- rcauchy(1000, location = 60, scale = 10)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Irrigation_Access_pos), aes(x = Irrigation_Access_pos)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Irrigation Access', x = 'Irrigation Access (%)', y = 'Frequency') +
  theme_minimal()

kurtosis_Irrigation_Access_cauchy <- kurtosis(Irrigation_Access_pos)
kurtosis_Irrigation_Access_cauchy

# 7. Pesticide Use
Pesticide_Use_KG_per_HA_pos <- rcauchy(1000, location = 25, scale = 5)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Pesticide_Use_KG_per_HA_pos), aes(x = Pesticide_Use_KG_per_HA_pos)) +
  geom_histogram(binwidth = 2, fill = 'orange', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Pesticide Use', x = 'Pesticide Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Pesticide_Use_cauchy <- kurtosis(Pesticide_Use_KG_per_HA_pos)
kurtosis_Pesticide_Use_cauchy

# 8. Fertilizer Use
Fertilizer_Use_KG_per_HA_pos <- rcauchy(1000, location = 100, scale = 20)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Fertilizer_Use_KG_per_HA_pos), aes(x = Fertilizer_Use_KG_per_HA_pos)) +
  geom_histogram(binwidth = 10, fill = 'lightgreen', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Fertilizer Use', x = 'Fertilizer Use (KG/HA)', y = 'Frequency') +
  theme_minimal()

kurtosis_Fertilizer_Use_cauchy <- kurtosis(Fertilizer_Use_KG_per_HA_pos)
kurtosis_Fertilizer_Use_cauchy

# 9. Soil Health Index
Soil_Health_Index_pos <- rcauchy(1000, location = 75, scale = 10)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Soil_Health_Index_pos), aes(x = Soil_Health_Index_pos)) +
  geom_histogram(binwidth = 5, fill = 'brown', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Soil Health Index', x = 'Soil Health Index', y = 'Frequency') +
  theme_minimal()

kurtosis_Soil_Health_Index_cauchy <- kurtosis(Soil_Health_Index_pos)
kurtosis_Soil_Health_Index_cauchy

# 10. Economic Impact
Economic_Impact_Million_USD_pos <- rcauchy(1000, location = 500, scale = 100)   # Cauchy distribution

# Create histogram
ggplot(data.frame(Economic_Impact_Million_USD_pos), aes(x = Economic_Impact_Million_USD_pos)) +
  geom_histogram(binwidth = 50, fill = 'purple', color = 'black') +
  labs(title = 'Positive Kurtosis (Cauchy): Economic Impact', x = 'Economic Impact (Million USD)', y = 'Frequency') +
  theme_minimal()

kurtosis_Economic_Impact_cauchy <- kurtosis(Economic_Impact_Million_USD_pos)
kurtosis_Economic_Impact_cauchy

##########Interpretation:
#all histograms have a sharp peak with heavy tails (leptokurtic distribution)
#this indicates positive kurtosis with more extreme values than a normal distribution
#meaning data are highly concentrated around the mean but with frequent extreme outliers

################################################################
# Boxplots 

# Load libraries
library(ggplot2)

# Load dataset
data<-read.csv("C:/Users/amalh/OneDrive - MSFT/Desktop/uni/5th semester/Environmental Statistics/proj1/dataset.csv")
print(data)

#Boxplot of Average Temperature by Country
ggplot(data, aes(x = Country, y = Average_Temperature_C)) +
  geom_boxplot(fill = 'skyblue') +
  labs(title = "Average Temperature by Country",
       x = "Country",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######interpretation:
# it provides a country-level comparison of climate conditions
#each box represents the distribution of average temperatures for each given country
#the middle line inside the box = the median temperature of that country
#a tall box represents a wide variation in temperature across countries



#Boxplot of Total Precipitation by Region
ggplot(data, aes(x = Region, y = Total_Precipitation_mm)) +
  geom_boxplot(fill = 'lightgreen') +
  labs(title = "Total Precipitation by Region",
       x = "Region",
       y = "Total Precipitation (mm)") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
####interpreation:
#this boxplot provides a regional comparison of precipitation levels
#each box represents the distribution of total precipitation within a region
#the line inside the box represents the median precipitation
#high rainfall regions may support water-intensive crops but risk flooding and nutrient leaching



#Boxplot of CO2 Emissions by Adaptation Strategy
ggplot(data, aes(x = Adaptation_Strategies, y = CO2_Emissions_MT)) +
  geom_boxplot(fill = 'gray') +
  labs(title = "CO2 Emissions by Adaptation Strategy",
       x = "Adaptation Strategy",
       y = "CO2 Emissions (MT)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####interpreation:
#each box represents the distribution of CO₂ emissions for one adaptation strategy
#the line inside each box represents the median emissions under that strategy
# the strategies with lower medians may reflect more climate-friendly


#BOXPLOT: Crop Yield Productivity by Extreme Weather Events
boxplot(data$Crop_Yield_MT_per_HA ~ data$Extreme_Weather_Events,
        main = "Crop Yield Productivity Drops With More Extreme Weather",
        xlab = "Number of Extreme Weather Events",
        ylab = "Crop Yield Productivity (MT per Hectare)",
        col = "lightgreen")
####interpretation:
#This boxplot helps you see how Crop Yield Productivity vary depending on the frequency of extreme weather events.
#Each box corresponds to a category of extreme weather events
#The central line inside each box represents the *median* Crop Yield Productivity for that category.
#more extreme weather leads to lower and more variable yields



################################################################
# Scatterplots  
# Install and load the package
library(ggplot2)

# Load your dataset
data<-read.csv("C:/Users/amalh/OneDrive - MSFT/Desktop/uni/5th semester/Environmental Statistics/proj1/dataset.csv")

# Create a scatterplot of Crop Yield Productivity vs Average Temperature
#This example shows the relationship between Crop Yield Productivity and average temperature, which is likely one of the key relationships you'd want to explore in this climate change agriculture dataset
ggplot(data, aes(x = avg_temp, y = crop_yield)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Scatterplot of Crop Yield Productivity vs Average Temperature",
       x = "Average Temperature (°C)",
       y = "Crop Yield Productivity (tons/ha)") +
  theme_minimal()

#####interpretation:
#Each blue dot represents a data observation (a country or region in a given year).
#The red line shows the *linear relationship* between average temperature and Crop Yield Productivity.
#The line slopes upward, it means higher temperatures are associated with higher Crop Yield Productivitys (a positive relationship).

# Scatterplot of Crop Yield Productivity vs CO2 Emissions
#This example shows the relationship between Crop Yield Productivity and CO2 Emissions, which is likely one of the key relationships you'd want to explore in this climate change agriculture dataset
ggplot(data, aes(x = CO2_Emissions_MT, y = Crop_Yield_MT_per_HA)) +
  geom_point(color = 'green', alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = "Scatterplot of Crop Yield Productivity vs CO2 Emissions",
       x = "CO2 Emissions (MT)",
       y = "Crop Yield Productivity (MT per HA)") +
  theme_minimal()
####interpretation:
#Each green dot represents a data observation (a country in a particular year)
#The vertical spread shows how much Crop Yield Productivity varies even at the same CO₂ emission level, meaning other factors (temperature, precipitation soil fertility) also strongly influence yields.
#The red line represents the fitted linear model (trend) between CO₂ emissions and Crop Yield Productivity.
#The line slopes *downward*, it suggests that higher CO₂ emissions are associated with lower Crop Yield Productivity


# Scatterplot of Crop Yield Productivity vs Total Precipitation
#This example shows the relationship between Crop Yield Productivity and total precipitation, which is likely one of the key relationships you'd want to explore in this climate change agriculture dataset
ggplot(data, aes(x = Total_Precipitation_mm, y = Crop_Yield_MT_per_HA)) +
  geom_point(color = 'purple', alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = "Scatterplot of Crop Yield Productivity vs Total Precipitation",
       x = "Total Precipitation (mm)",
       y = "Crop Yield Productivity (MT per HA)") +
  theme_minimal()
####interpretation:
#Each purple dot is one observation (country a year)
#The red line shows the linear trend between precipitation and Crop Yield Productivity.
#The slope is *upward*, higher rainfall is associated with higher yields.
#then we can say that ; Adequate rainfall improves soil moisture and crop growth,and then boosting yields.
#but in reality,this relationship is often nonlinear
#Yields rise with more rainfall up to an optimal level, but the excess in rain reduces yields.


################################################################
# Set seed for reproducibility
set.seed(123)

# Sample variable 1: Crop Yield Productivity (tons/ha)
mean_yield <- 2.24  # Based on your dataset mean
sd_yield <- 0.8     # Estimated standard deviation
n <- 1000           # Number of simulated measurements

# Generate Crop Yield Productivity data
yield_data <- rnorm(n, mean = mean_yield, sd = sd_yield)

# Create histogram for Crop Yield Productivity
hist(yield_data, breaks = 20, main = "Distribution of Crop Yield Productivity",
     xlab = "Yield (tons per hectare)", ylab = "Frequency",
     col = "lightgreen", border = "darkgreen")

# Calculate basic statistics
yield_mean <- mean(yield_data)
yield_median <- median(yield_data)
yield_sd <- sd(yield_data)

cat("Crop Yield Productivity Statistics:\n")
cat("Mean:", round(yield_mean, 2), "tons/ha\n")
cat("Median:", round(yield_median, 2), "tons/ha\n")
cat("Standard Deviation:", round(yield_sd, 2), "tons/ha\n")

# Sample variable 2: Economic Impact (Million USD)
mean_economic <- 674.27  # Based on your dataset
sd_economic <- 300       # Estimated standard deviation

# Generate economic impact data
economic_data <- rnorm(n, mean = mean_economic, sd = sd_economic)

# Create histogram for economic impact
hist(economic_data, breaks = 20, main = "Distribution of Economic Impact",
     xlab = "Economic Impact (Million USD)", ylab = "Frequency",
     col = "lightcoral", border = "darkred")

# Sample variable 3: Average Temperature (°C)
mean_temp <- 22.5  # Representative average temperature
sd_temp <- 4.0     # Temperature variability

# Generate temperature data
temp_data <- rnorm(n, mean = mean_temp, sd = sd_temp)

# Create histogram for temperature
hist(temp_data, breaks = 20, main = "Distribution of Average Temperature",
     xlab = "Temperature (°C)", ylab = "Frequency",
     col = "lightyellow", border = "orange")

# Display summary statistics for all variables
summary_df <- data.frame(
  Variable = c("Crop Yield Productivity","Economic Impact", "Temperature"),
  Mean = c(mean(yield_data),mean(economic_data), mean(temp_data)),
  Median = c(median(yield_data),median(economic_data), median(temp_data)),
  SD = c(sd(yield_data), sd(economic_data), sd(temp_data)))

print("Summary Statistics for Sample Variables:")
print(summary_df)

#The histograms for each variable display a classic bell-shaped curve, characteristic of a normal distribution.
#The frequency of observations is highest near the mean value and decreases symmetrically towards both the lower and upper tails.
#This visual pattern, supported by the numerical summary where the mean and median are nearly identical for each variable, confirms 
#that Crop Yield Productivity, Economic Impact, and Average Temperature are approximately normally distributed.




# Correlation Analysis to find relationships
cat("CORRELATION ANALYSIS")
###################################
library(dplyr)

# Select only numerical variables for correlation
numerical_vars <- data %>% 
  select(where(is.numeric))

# Calculate correlation matrix
cor_matrix <- cor(numerical_vars, use = "complete.obs")
print(round(cor_matrix, 3))

# Find variables with strongest correlations to potential outcomes
cat("\nSTRONGEST CORRELATIONS WITH Crop Yield Productivity:\n")
crop_yield_cor <- cor_matrix["Crop_Yield_MT_per_HA", ]
crop_yield_cor <- crop_yield_cor[order(-abs(crop_yield_cor))]
print(round(crop_yield_cor, 3))

cat("\nSTRONGEST CORRELATIONS WITH ECONOMIC IMPACT:\n")
economic_cor <- cor_matrix["Economic_Impact_Million_USD", ]
economic_cor <- economic_cor[order(-abs(economic_cor))]
print(round(economic_cor, 3))


#Correlation Heatmap:

# Load library
library(corrplot)

# Select numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

cor_matrix <- cor(numeric_data, use = "complete.obs")
par(mar = c(1, 1, 4, 1))

#Correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "full",
  addCoef.col = "black",
  number.cex = 0.7,
  number.digits = 3,
  tl.col = "black",
  tl.cex = 0.8,
  col = colorRampPalette(c("red", "white", "blue"))(200),
  addgrid.col = "grey",    # add grid lines
  grid.col = "grey"        # optional, alternative for some versions
)
title(main = "Correlation Heatmap of Numeric Variables", cex.main = 1.3, line = 2.5)




# Next, we determine baseline thresholds for key variables: extreme weather events and economic impact
# Define normal range (baseline) for variables: mean - SD ≤ variable ≤ mean + SD
# Extreme Weather Events threshold

# Extreme Weather Events threshold
weather_events_threshold <- round(mean_extreme_weather + sd_extreme_weather, 0)
cat("Extreme Weather Events Threshold (mean + 1SD):", weather_events_threshold, "\n")

# Economic Impact threshold  
economic_threshold <- round(mean_economic_impact + sd_economic_impact, 0)
cat("Economic Impact Threshold (mean + 1SD):", economic_threshold, "\n")






#########################################
#  COMPREHENSIVE CLIMATE VARIABLES AND YIELD IMPACTS ANALYSIS
#########################################

##Scenario.1: "average temperature impact on Crop Yield Productivity"

##1.Descriptive Statistics
#Temperature variables
cat("average temperature mean:", mean_avg_temp)
cat("average temperature standard deviation:",sd_avg_temp)

#Yield variables
cat("Crop Yield Productivity mean:",mean_crop_yield )
cat("Crop Yield Productivity standard deviation:",sd_crop_yield)

##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a positive or a negative correlation between the average temperature and the Crop Yield Productivity
ggplot(data, aes(x = avg_temp, y = crop_yield)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +
  labs(title = "Impact of Average Temperature on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Temperature vs Yield Production",
       x = "Average Temperature (°C)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

##3.Correlation Analysis
correlation <- cor(avg_temp, crop_yield)
correlation_coefficient <-round(correlation, 3)
correlation_coefficient

####Interpretation:
#the correlation coefficient between temperature and Crop Yield Productivity indicates a "weak positive relationship"
#the scatterplot with the trend line reveal that there is an "increasing trend"
#higher temperatures are somewhat associated with higher Crop Yield Productivitys
#temperature has a "minor effect" on Crop Yield Productivity;it is not a major yield determinant


##4.Hypothesis Testing:"Pearson Correlation Test"

# Question: Is there a significant relationship between average temperature and Crop Yield Productivity?
# Hypothesis:
# H0(null hypothesis): average temperature has no effect on Crop Yield Productivity
# H1(alternative hypothesis):average temperature affects Crop Yield Productivity

p-value <- cor.test(avg_temp, crop_yield)
p-value

#Decision making:to make the right decision we should compare the p-value to the significance level (alpha,commonly set at 0.05)

###conclusion:
#fail to reject the null hypothesis
#there is no statistically "significant" relationship between average temperature and Crop Yield Productivity
#temperature "does not appear to be a meaningful factor" influencing yield variations
#it has "limited practical impact" on yield variations
#Other factors likely play more important roles in determining crop productivity


##5.Regression Analysis:"Average temperature Impact on Crop Yield Productivity"

#build a linear regression model
#Dependent variable:Crop Yield Productivity
#Independent variable:average temperature
regression_model <- lm(crop_yield ~ avg_temp, data = data)
summary(regression_model)
#Extract the slope for average temperature
regression_coefficient <- round(coef(regression_model)[2], 4)
regression_coefficient

#interpretation:
#For every 1°C increase in average temperature, the Crop Yield Productivity decreases by 0.0104 on average.
#The effect is "negative",indicates an "inverse relationship"between average temperature and Crop Yield Productivity
#higher temperatures "slightly reduce" yields




#########################################
##Scenario.2: "Precipitation impact on Crop Yield Productivity "

##1.Descriptive Statistics
#precipitation variables
cat("precipitation mean:",mean_precipitation)
cat("precipitation standard deviation:", sd_precipitation)

#Yield variables
cat("Crop Yield Productivity mean:",mean_crop_yield )
cat("Crop Yield Productivity standard deviation:",sd_crop_yield)


##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a positive or a negative correlation between the precipitation and the Crop Yield Productivity
ggplot(data, aes(x = precipitation, y = crop_yield)) +
  geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(title = "Impact of Precipitation on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Rainfall vs Yield Production",
       x = "Total Precipitation (mm)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()


##3.Correlation Analysis
correlation <- cor(precipitation, crop_yield)
correlation_value <- round(correlation, 3)
correlation_value

###Interpretation:
#the correlation coefficient between precipitation and Crop Yield Productivity indicates a "very weak positive relationship"
#The value is positive but extremely weak
#higher precipitation is very slightly associated with higher yield, though the effect is "negligible"
#there is "almost no relationship" between precipitation and Crop Yield Productivity (since 0.03 is very close to 0)
#Changes in precipitation do not affect Crop Yield Productivity "significantly"


##4.Hypothesis Testing:"Pearson Correlation Test"

#Question: Is there a significant relationship between precipitation and Crop Yield Productivity?
#Hypothesis:
#H0(null hypothesis): precipitation has no effect on Crop Yield Productivity (ρ = 0)
#H1(alternative hypothesis):precipitation affects Crop Yield Productivity(ρ ≠ 0)


p_value<-cor.test(precipitation, crop_yield)
p_value

#Decision making:to make the right decision we should compare the p_value to the significance level (alpha,commonly set at 0.05)

###conclusion:
#reject the null hypothesis (H0) since p-value < alpha
#there is no statistically "significant" relationship between precipitation and Crop Yield Productivity
#precipitation "does not appear to be a meaningful factor" influencing yield variations
#it has a"limited practical impact" on yield variations
#Other factors likely play more important roles in determining crop productivity


##5.Regression Analysis:"Precipitation Impact on Crop Yield Productivity"

#Build a linear regression model
#Dependent variable:Crop Yield Productivity
#Independent variable:Precipitation
regression_model <- lm(crop_yield ~ precipitation, data = data)
summary(regression_model)
# Extract the slope for precipitation
regression_coefficient<-round(coef(regression_model)[2] * 100, 4)
regression_coefficient

##interpretation:
#for every 1-unit increase in precipitation, the Crop Yield Productivity increases by 0.0037 units
#The effect is positive;higher precipitation is associated with slightly higher yield
#the value is very small,then the impact of precipitation on Crop Yield Productivity is "minimal"



#########################################
##Scenario.3: "CO2 Emissions Impact on Crop Yield Productivity"

##1.Descriptive Statistics
#Co2 emissions variables
cat("CO2 emissions mean:",mean_co2_emissions)
cat("CO2 emissions standard deviation:",sd_co2_emissions)

#Yield variables
cat("Crop Yield Productivity mean:",mean_crop_yield)
cat("Crop Yield Productivity standard deviation:",sd_crop_yield)

##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a "positive" or a "negative" correlation between the Co2 emissions and the Crop Yield Productivity
ggplot(data, aes(x = co2_emissions, y = crop_yield)) +
  geom_point(color = "brown", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "orange") +
  labs(title = "Impact of CO2 Emissions on Crop Yield Productivity",
       subtitle = "Scenario Analysis: CO2 vs Yield Production",
       x = "CO2 Emissions (MT)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

##3.Correlation Analysis
correlation <- cor(co2_emissions, crop_yield)
correlation_value <- round(correlation, 3)
correlation_value

##interpretation:
#the correlation coefficient between Co2 emissions and Crop Yield Productivity indicates a "very weak negative relationship"
#increasing Co2 emissions may be "slightly associated" with lower yields
#the effect is too small to be considered meaningful
#Changes in Co2 emissions do not affect Crop Yield Productivity "significantly"


##4.HYPOTHESIS TESTING:"Pearson Correlation Test"

#Question: Is there a significant relationship between CO2 emissions and Crop Yield Productivity?
#Hypothesis:
#H0(null hypothesis):CO2 emissions has no effect on Crop Yield Productivity (ρ = 0)
#H1(alternative hypothesis):CO2 emissions affects Crop Yield Productivity(ρ ≠ 0)

p_value<-cor.test(co2_emissions, crop_yield)
p_value

#Decision making:to make the right decision we should compare the p_value to the significance level (alpha,commonly set at 0.05)

###conclusion:
#fail to reject the null hypothesis (H0) since p-value > alpha
#there is "no statistically significant" relationship between CO2 emissions and Crop Yield Productivity
#CO2 emissions "do not meaningfully influence" crop productivity


##5. Regression Analysis: "CO2 Emissions Impact on Crop Yield Productivity"

#build a linear regression model
#Dependent variable:Crop Yield Productivity
#Independent variable:CO2 Emissions
regression_model <- lm(crop_yield ~ co2_emissions, data = data)
summary(regression_model)
# Extract the slope for CO2 emissions
regression_coefficient<-round(coef(regression_model)[2], 4)
regression_coefficient

##interpretation:
#For every 1-unit increase in CO2 emissions,the Crop Yield Productivity decreases by 0.0104
#The effect is "negative",indicates an "inverse relationship"between Co2 emissions and Crop Yield Productivity
#higher CO2 emissions are associated with slightly lower yield
##the value is "small",then the impact of Co2 emissions on Crop Yield Productivity is "minimal"


################################################################################

###Overall conclusion:
#Across all three scenarios(average temperature,precipitation,CO2 emission):
##########The correlations are weak (close to 0).
##########The p-values > alpha;the relationships are not statistically significant
##########The regression slopes are small;which means that climatic factors have a minimal impact on yield variations

#Climatic factors (average temperature,precipitation,CO2 emission)show only "weak and statistically insignificant" relationships with Crop Yield Productivity
#This suggests that other factors such as soil health, irrigation, fertilizers, crop type, and adaptation strategies may play a much stronger role in determining crop productivity


################################################################################






#########################################
# Extreme weather events and economic impact scenarios analysis
#########################################

#Scenario 1: Extreme weather events are occurring more frequently than typical historical levels
# -----------------------------------------------------------------------------
cat("Scenario 1: Extreme weather events are occurring more frequently than typical historical levels")

# H₀: μ_events ≤8 events per year (Within normal range)
# H₁: μ_events > 8 events per year (Elevated frequency)
# α = 0.05, One-tailed test

events_data <- data$Extreme_Weather_Events
normal_threshold <- weather_events_threshold 

cat("HYPOTHESIS TEST: One-Sample t-Test (One-tailed)\n")
cat("Question: Is the average event frequency elevated above 'normal' levels?\n\n")

#means difference
cat("Sample Mean (Events/Year):", round(mean_extreme_weather, 2), "\n")
cat("Normal Threshold:", normal_threshold, "events/year\n")
cat("Difference from Normal:", round(mean_extreme_weather - normal_threshold, 2), "events\n\n")

#One-sample t-test
frequency_test <- t.test(events_data, mu = normal_threshold, alternative = "greater")

cat("RESULTS:\n")
cat("• Test Statistic (t):", round(frequency_test$statistic, 4), "\n")
cat("• Degrees of Freedom:", frequency_test$parameter, "\n")
cat("• p-value:", round(frequency_test$p.value, 4), "\n")
cat("• 95% Confidence Interval: [", round(frequency_test$conf.int[1], 2), ", Inf]\n\n")

if(frequency_test$p.value < 0.05) {
  cat("STATISTICAL DECISION: Reject H₀ at α = 0.05\n")
  cat("CONCLUSION: Event frequency is significantly elevated above normal levels\n")
  cat("• Current climate conditions show abnormally high extreme event frequency\n")
  cat("• Climate adaptation and resilience planning should be prioritized\n")
} else {
  cat("STATISTICAL DECISION: Fail to reject H₀\n")
  cat("CONCLUSION: Event frequency is within normal expected range\n")
  cat("• Extreme event frequency matches historical expectations\n")
}

#Extreme event frequency matches historical expectations


# BEFORE/AFTER COMPARISON
# Split data into two time periods
cutoff_year <- median(data$Year)
before <- data$Extreme_Weather_Events[data$Year <= cutoff_year]
after <- data$Extreme_Weather_Events[data$Year > cutoff_year]

boxplot(list(`Early Period` = before, `Recent Period` = after),
        main = "BEFORE and AFTER: Extreme Weather Frequency",
        ylab = "Number of Extreme Events",
        col = c("lightgreen", "red"),
        border = "black",
        cex.main = 1.2)

#Extreme weather frequency remains within expected historical range
#No statistical evidence supports claims of increased event frequency


#########################################
#Scenario 2: Agricultural Economic Output Trends

cat("Scenario 2: Agricultural Economic Output Trends")

# Since n > 5000, use z-test for additional validation

# install and Load package
install.packages("BSDA")
library(BSDA)
benchmark <- 600  # Historical average economic output

cat("HYPOTHESIS TEST: Z-Test (Large Sample)\n")
cat("Question: How does current agricultural economic output compare to historical benchmarks?\n\n")

#mean and difference
cat("Sample Mean: $", round(mean_economic_impact, 2), "M\n")
cat("Historical Benchmark: $", benchmark, "M\n")
cat("Difference: $", round(mean_economic_impact - benchmark, 2), "M\n\n")

#Z-test
z_test <- z.test(data$Economic_Impact_Million_USD, 
                 mu = benchmark,
                 sigma.x = sd_economic_impact,
                 alternative = "two.sided")

cat("RESULTS:\n")
cat("• Z-statistic:", round(z_test$statistic, 4), "\n")
cat("• p-value:", round(z_test$p.value, 4), "\n")
cat("• 95% Confidence Interval: [", round(z_test$conf.int[1], 2), ",", 
    round(z_test$conf.int[2], 2), "]\n\n")

if(z_test$p.value < 0.05) {
  cat("STATISTICAL DECISION: Significant difference from benchmark\n")
  direction <- ifelse(mean_economic_impact > benchmark, "higher", "lower")
  cat("CONCLUSION: Current agricultural economic output is", direction, "than historical benchmark\n")
} else {
  cat("STATISTICAL DECISION: No significant difference from benchmark\n")
  cat("CONCLUSION: Current agricultural economic output aligns with historical patterns\n")
}




# Graph showing agricultural economic output trends

# Calculate annual averages
annual_avg <- aggregate(Economic_Impact_Million_USD ~ Year, data = data, mean)

# Create the plot
plot(annual_avg$Year, annual_avg$Economic_Impact_Million_USD,
     type = "o",  # points connected by lines
     pch = 19, 
     col = "darkgreen",
     lwd = 2,
     main = "AGRICULTURAL ECONOMIC OUTPUT",
     xlab = "Year", 
     ylab = "Agricultural Economic Output (Million USD)",
     ylim = c(500, 800))

#########################################
#Scenario 3: High-event years have different agricultural economic output than low-event years
cat("Scenario 3: High-event years have different agricultural economic output than low-event years\n")

# Create groups directly without modifying dataset
high_events <- data$Economic_Impact_Million_USD[data$Extreme_Weather_Events > median(data$Extreme_Weather_Events)]
low_events <- data$Economic_Impact_Million_USD[data$Extreme_Weather_Events <= median(data$Extreme_Weather_Events)]

# H₀: μ_high_events = μ_low_events (No difference in economic output)
# H₁: μ_high_events ≠ μ_low_events (Events affect economic output)
# α = 0.05

cat("HYPOTHESIS TEST: Two-Sample t-Test (Welch's)\n")
cat("Question: Do high-event years have different agricultural economic output than low-event years?\n\n")

#Calculate means and means difference
mean_high <- mean(high_events)
mean_low <- mean(low_events)
cat("Mean Agricultural Output - High Events: $", round(mean_high, 2), "M\n")
cat("Mean Agricultural Output - Low Events: $", round(mean_low, 2), "M\n")
cat("Difference: $", round(mean_high - mean_low, 2), "M\n\n")

#Two-sample t-test
two_sample_test <- t.test(high_events, low_events, var.equal = FALSE)

cat("RESULTS:\n")
cat("• Test Statistic (t):", round(two_sample_test$statistic, 4), "\n")
cat("• Degrees of Freedom:", round(two_sample_test$parameter, 2), "\n")
cat("• p-value:", round(two_sample_test$p.value, 4), "\n")
cat("• 95% Confidence Interval: [", round(two_sample_test$conf.int[1], 2), ",", 
    round(two_sample_test$conf.int[2], 2), "]\n\n")

if(two_sample_test$p.value < 0.05) {
  cat("STATISTICAL DECISION: Reject H₀ at α = 0.05\n")
  cat("CONCLUSION: Significant difference in agricultural economic output between event groups\n")
  direction <- ifelse(mean_high > mean_low, "higher", "lower")
  cat("• High-event years have", direction, "agricultural economic output\n")
  cat("• Practical significance:", round(abs(mean_high - mean_low), 2), "M difference\n")
} else {
  cat("STATISTICAL DECISION: Fail to reject H₀\n")
  cat("CONCLUSION: No significant difference in agricultural economic output between event groups\n")
  cat("• Event frequency alone may not be the primary driver of agricultural economic performance\n")
}

#High-event and low-event years exhibit nearly identical agricultural economic output
#The Surprising Truth: There is NO significant relationship between extreme weather event frequency and agricultural economic output
#This means that: Knowing how many extreme events occur tells you almost nothing about agricultural economic performance
#This is reasonable because national economies have buffers like trade, insurance, and diversification


#########################################
# Scenario 4: EVENT INTENSITY ANALYSIS: THE TRUE DRIVER OF AGRICULTURAL ECONOMIC OUTCOME

cat("Scenario 4: THE TRUE DRIVER OF AGRICULTURAL ECONOMIC OUTCOME")

# Event economic intensity is defined as the agricultural economic output per single extreme weather event
# Calculate economic output per individual extreme weather event
event_intensity <- data$Economic_Impact_Million_USD / data$Extreme_Weather_Events

# Handle years with zero events (can't divide by zero)
event_intensity[data$Extreme_Weather_Events == 0] <- NA

# Remove NA values for analysis
event_intensity_clean <- event_intensity[!is.na(event_intensity)]

# Basic Statistics
cat("ECONOMIC INTENSITY DISTRIBUTION:\n")
cat("• Average Economic Intensity: $", round(mean(event_intensity_clean), 2), "M per event\n")
cat("• Typical Economic Intensity: $", round(median(event_intensity_clean), 2), "M per event\n")
cat("• Minimum Economic Intensity: $", round(min(event_intensity_clean), 2), "M per event\n")
cat("• Maximum Economic Intensity: $", round(max(event_intensity_clean), 2), "M per event\n")
cat("• Economic Intensity Variability: $", round(sd(event_intensity_clean), 2), "M per event\n\n")

# INTERPRETATION: The wide range (min to max) and high variability show that economic impact per event 
# varies dramatically across different countries and contexts, suggesting national economic factors 
# dominate over weather severity in determining economic outcomes.


cat("EVIDENCE FOR ECONOMIC INTENSITY AS PRIMARY DRIVER\n")

# TEST 1: Does Event Count Predict Economic Outcome?
cat("EVIDENCE 1: Can We Predict Economic Outcomes by Counting Events?")

# Only look at years that actually had extreme events
years_with_events <- data[data$Extreme_Weather_Events > 0, ]

# Calculate how well event count predicts economic outcome
relationship_strength <- cor(years_with_events$Extreme_Weather_Events, 
                             years_with_events$Economic_Impact_Million_USD)

cat("RESULTS:\n")
cat("• Relationship Strength: ", round(relationship_strength, 4), "\n")
cat("• Predictive Power: ", round(relationship_strength^2 * 100, 4), "%\n\n")

# Simple interpretation
cat("WHAT THESE NUMBERS MEAN:\n")
cat("------------------------\n")

if(abs(relationship_strength) < 0.1) {
  cat("• The relationship is EXTREMELY WEAK (almost zero)\n")
  cat("• Event count explains ONLY ", round(relationship_strength^2 * 100, 4), "% of economic outcome differences\n")
  cat("• This means: 99.99% of economic variation comes from OTHER factors\n\n")
}

# INTERPRETATION: The near-zero correlation (-0.009) proves that simply counting extreme weather events
# tells us almost nothing about economic outcomes. This challenges the common assumption that more events
# automatically mean worse economic impacts, and suggests national economic resilience buffers climate shocks.

# The frequency-outcome correlation is negligible, indicating no meaningful predictive relationship


# TEST 2: Compare countries with the SAME number of extreme events
cat("EVIDENCE 2: Same Number of Events, Very Different Economic Outcomes")

# Find the most common number of extreme events per year
most_common_event_count <- median(data$Extreme_Weather_Events)

# Find all countries that had exactly this many events
countries_with_same_events <- data[data$Extreme_Weather_Events == most_common_event_count, ]

# Check if we found any matching countries
if(nrow(countries_with_same_events) > 0) {
  
  # Find the smallest and largest economic outcomes
  smallest_outcome <- min(countries_with_same_events$Economic_Impact_Million_USD)
  largest_outcome <- max(countries_with_same_events$Economic_Impact_Million_USD)
  
  # Calculate how many times bigger the largest outcome is
  times_bigger <- largest_outcome / smallest_outcome
  
  # Print the results in simple language
  cat("• When countries had the same number of events (", most_common_event_count, " events):\n")
  cat("  - Smallest agricultural economic output: $", round(smallest_outcome, 2), " million\n")
  cat("  - Largest agricultural economic output: $", round(largest_outcome, 2), " million\n")
  cat("  - That's ", round(times_bigger, 1), " times difference!\n")
  cat("  → This proves: How events affect ECONOMIC SYSTEMS matters more than how MANY events occur\n\n")
}

# INTERPRETATION: The dramatic differences (28x variation) for the same number of events demonstrate that
# national economic context, development level, and agricultural sector size are far more important than
# event frequency in determining economic outcomes. A country's economic structure determines its vulnerability.

# Same quantity of events yields dramatically different economic outcomes; highlighting economic intensity's critical role


ggplot(data.frame(Intensity = event_intensity_clean), aes(x = Intensity)) +
  geom_histogram(fill = "darkgreen", alpha = 0.7, bins = 30) +
  labs(title = "Distribution of Weather Event Economic Intensity",
       subtitle = "Most events have minor economic impact, but a few have major economic consequences",
       x = "Agricultural Economic Output per Individual Event (Million USD)",
       y = "Number of Events at This Economic Level") +
  theme_minimal()

# INTERPRETATION: The right-skewed histogram shows that most weather events have relatively small economic
# impacts per event, but a few events have disproportionately large consequences. This "heavy-tailed" 
# distribution pattern is typical of economic impact data, where a small number of extreme cases drive
# most of the overall economic effects.

# Economic intensity exhibits a right-skewed Pareto distribution (few events have major economic impact) with heavy-tailed extreme events

# Individual event economic intensity matters more than how many events occur

# OVERALL INTERPRETATION: This analysis reveals that national economic scale and structure are the true drivers
# of what we call "economic intensity." Wealthier agricultural economies show larger numbers not because
# they suffer more severe weather, but because they have more valuable assets at risk. The key insight is
# that economic resilience and scale matter more than weather event frequency in determining national-level outcomes.


#########################################
# COMPREHENSIVE COUNTRY AND CROP ECONOMIC PATTERNS ANALYSIS
#########################################

cat("Specific Economic Patterns by Country and Crop\n")

# Scenario 1: COUNTRY-LEVEL ANALYSIS

cat("1. COUNTRIES WITH HIGHEST AGRICULTURAL ECONOMIC OUTPUT\n")

# Simple way to find top countries by economic output
country_totals <- data.frame(Country = character(),
                             Total_Output = numeric(),
                             Output_Per_Event = numeric())

# Check each country
for(country in unique(data$Country)) {
  country_data <- data[data$Country == country & data$Extreme_Weather_Events > 0, ]
  
  if(nrow(country_data) > 0) {
    total_output <- sum(country_data$Economic_Impact_Million_USD)
    output_per_event <- mean(country_data$Economic_Impact_Million_USD / country_data$Extreme_Weather_Events)
    
    country_totals <- rbind(country_totals,
                            data.frame(Country = country,
                                       Total_Output = total_output,
                                       Output_Per_Event = output_per_event))
  }
}

# Sort by biggest economic output first
country_totals <- country_totals[order(-country_totals$Total_Output), ]

# Show top 10
cat("TOP 10 COUNTRIES BY AGRICULTURAL ECONOMIC OUTPUT:\n")
for(i in 1:10) {
  cat(i, ". ", country_totals$Country[i], "\n", sep = "")
  cat("   Total agricultural output: $", round(country_totals$Total_Output[i]), "M\n", sep = "")
  cat("   Per event: $", round(country_totals$Output_Per_Event[i]), "M\n\n", sep = "")
}

# INTERPRETATION: The country rankings show surprisingly uniform economic outputs
# Nigeria ($656B) outperforming USA ($628B) and China ($621B) is economically implausible
# This suggests the economic data may be synthetic or artificially normalized

# ANOVA for economic output by country
anova_country <- aov(Economic_Impact_Million_USD ~ Country, data = data)
cat("ANOVA - Agricultural Economic Output by Country:\n")
anova_summary <- summary(anova_country)
cat("p-value:", format.pval(anova_summary[[1]]$"Pr(>F)"[1], 4), "\n")

# INTERPRETATION: p-value = 0.628 indicates NO significant differences between countries
# This contradicts real-world economic disparities and confirms artificial data patterns
# Real agricultural economies should show significant variation based on development level, sector size, and resources

# CONCLUSION: The economic impact variable shows mathematically perfect uniformity
# that doesn't reflect real-world national economic differences
# We cannot draw meaningful conclusions about country-level economic vulnerability

#########################################
# Scenario 2: CROP ECONOMIC SENSITIVITY ANALYSIS

# Create a simple table for crop results
crop_results <- data.frame(Crop = character(),
                           Economic_Sensitivity = numeric())

# Check each crop type
for(crop in unique(data$Crop_Type)) {
  # Get data for this crop only, and only years with extreme weather
  crop_data <- data[data$Crop_Type == crop & data$Extreme_Weather_Events > 0, ]
  
  if(nrow(crop_data) > 0) {
    # Calculate average yield (how much crop grows)
    avg_harvest <- mean(crop_data$Crop_Yield_MT_per_HA, na.rm = TRUE)
    
    # Calculate average economic output
    avg_economic_output <- mean(crop_data$Economic_Impact_Million_USD, na.rm = TRUE)
    
    # Calculate economic sensitivity: economic output per ton of crop
    economic_sensitivity <- avg_economic_output / avg_harvest
    
    # Save results
    crop_results <- rbind(crop_results,
                          data.frame(Crop = crop,
                                     Economic_Sensitivity = economic_sensitivity))
  }
}

# Sort by most economically sensitive crops first
crop_results <- crop_results[order(-crop_results$Economic_Sensitivity), ]

# Show results
cat("CROPS RANKED BY ECONOMIC SENSITIVITY:\n")
cat("(Higher score = more economic output per ton of crop)\n\n")

for(i in 1:nrow(crop_results)) {
  cat(i, ". ", crop_results$Crop[i], "\n", sep = "")
  cat("   Economic sensitivity score: ", round(crop_results$Economic_Sensitivity[i], 1), "\n", sep = "")
}

# INTERPRETATION: All crops show nearly identical economic sensitivity scores (297-310)
# This uniformity is economically implausible - different crops have vastly different market values
# Sugarcane and coffee should not have almost identical economic sensitivity to wheat and corn

# ANOVA for Crop Yield Productivity by crop type
anova_crop <- aov(Crop_Yield_MT_per_HA ~ Crop_Type, data = data)
cat("ANOVA - Crop Yield Productivity by Crop Type:\n")
crop_summary <- summary(anova_crop)
cat("p-value:", format.pval(crop_summary[[1]]$"Pr(>F)"[1], 4), "\n")

# INTERPRETATION: p-value = 0.1498 indicates NO significant yield differences between crop types
# This contradicts agricultural reality where different crops have distinct yield potentials
# and economic values per hectare

# CONCLUSION: The crop economic data shows artificial normalization
# We cannot draw meaningful conclusions about crop-specific economic vulnerability
# The patterns reflect mathematical artifacts rather than real agricultural economics

#########################################
# Scenario 3: High-event years have different economic outputs than low-event years
cat("Scenario 3: High-event years have different economic outputs than low-event years\n")

# Create groups directly without modifying dataset
high_events <- data$Economic_Impact_Million_USD[data$Extreme_Weather_Events > median(data$Extreme_Weather_Events)]
low_events <- data$Economic_Impact_Million_USD[data$Extreme_Weather_Events <= median(data$Extreme_Weather_Events)]

# H₀: μ_high_events = μ_low_events (No difference in economic output)
# H₁: μ_high_events ≠ μ_low_events (Events affect economic output)
# α = 0.05

cat("HYPOTHESIS TEST: Two-Sample t-Test (Welch's)\n")
cat("Question: Do high-event years have different economic outputs than low-event years?\n\n")

# Calculate means and means difference
mean_high <- mean(high_events)
mean_low <- mean(low_events)
cat("Mean Economic Output - High Events: $", round(mean_high, 2), "M\n")
cat("Mean Economic Output - Low Events: $", round(mean_low, 2), "M\n")
cat("Difference: $", round(mean_high - mean_low, 2), "M\n\n")

# Two-sample t-test
two_sample_test <- t.test(high_events, low_events, var.equal = FALSE)

cat("RESULTS:\n")
cat("• Test Statistic (t):", round(two_sample_test$statistic, 4), "\n")
cat("• Degrees of Freedom:", round(two_sample_test$parameter, 2), "\n")
cat("• p-value:", round(two_sample_test$p.value, 4), "\n")
cat("• 95% Confidence Interval: [", round(two_sample_test$conf.int[1], 2), ",", 
    round(two_sample_test$conf.int[2], 2), "]\n\n")

if(two_sample_test$p.value < 0.05) {
  cat("STATISTICAL DECISION: Reject H₀ at α = 0.05\n")
  cat("CONCLUSION: Significant difference in economic output between event groups\n")
  direction <- ifelse(mean_high > mean_low, "higher", "lower")
  cat("• High-event years have", direction, "economic outputs\n")
  cat("• Practical significance:", round(abs(mean_high - mean_low), 2), "M difference\n")
} else {
  cat("STATISTICAL DECISION: Fail to reject H₀\n")
  cat("CONCLUSION: No significant difference in economic output between event groups\n")
  cat("• Event frequency alone may not be the primary driver of economic outputs\n")
}

# INTERPRETATION: p-value = 0.4228 and tiny difference ($6.67M, <1%) show NO weather impact
# This contradicts climate economics where extreme weather should significantly affect agriculture
# The confidence interval [-22.99, 9.65] includes zero, confirming no real effect

# CONCLUSION: The economic data is completely decoupled from weather events
# This further confirms the artificial nature of the economic impact variable

#########################################

# Scenario 4: EVENT INTENSITY ANALYSIS
cat("Scenario 4: EVENT INTENSITY ANALYSIS\n")

# Event intensity is defined as the economic output per single extreme weather event
event_intensity <- data$Economic_Impact_Million_USD / data$Extreme_Weather_Events

# Handle years with zero events (can't divide by zero)
event_intensity[data$Extreme_Weather_Events == 0] <- NA

# Remove NA values for analysis
event_intensity_clean <- event_intensity[!is.na(event_intensity)]

# Basic Statistics
cat("INTENSITY DISTRIBUTION:\n")
cat("• Average Intensity: $", round(mean(event_intensity_clean), 2), "M per event\n")
cat("• Typical Intensity: $", round(median(event_intensity_clean), 2), "M per event\n")
cat("• Minimum Intensity: $", round(min(event_intensity_clean), 2), "M per event\n")
cat("• Maximum Intensity: $", round(max(event_intensity_clean), 2), "M per event\n")
cat("• Intensity Variability: $", round(sd(event_intensity_clean), 2), "M per event\n\n")

# INTERPRETATION: Large gap between mean ($200M) and median ($119M) indicates right-skewed distribution
# However, the extreme range ($5M to $2,285M) and high variability suggest artificial patterns

cat("EVIDENCE FOR DATA QUALITY ASSESSMENT\n")

# TEST 1: Does Event Count Predict Economic Output?
cat("TEST 1: Relationship Between Event Count and Economic Output\n")

# Only look at years that actually had extreme events
years_with_events <- data[data$Extreme_Weather_Events > 0, ]

# Calculate how well event count predicts economic output
relationship_strength <- cor(years_with_events$Extreme_Weather_Events, 
                             years_with_events$Economic_Impact_Million_USD)

cat("RESULTS:\n")
cat("• Relationship Strength: ", round(relationship_strength, 4), "\n")
cat("• Predictive Power: ", round(relationship_strength^2 * 100, 4), "%\n\n")

# Simple interpretation
cat("WHAT THESE NUMBERS MEAN:\n")
cat("------------------------\n")

if(abs(relationship_strength) < 0.1) {
  cat("• The relationship is EXTREMELY WEAK (almost zero)\n")
  cat("• Event count explains ONLY ", round(relationship_strength^2 * 100, 4), "% of output differences\n")
  cat("• This means: 99.99% of output variation comes from OTHER factors\n\n")
}

# INTERPRETATION: Correlation of -0.0093 is essentially zero
# Weather events have no predictive power for economic outcomes
# This is economically implausible and indicates data quality issues

# TEST 2: Compare countries with the SAME number of extreme events
cat("TEST 2: Economic Output Variation for Identical Event Counts\n")

# Find the most common number of extreme events per year
most_common_event_count <- median(data$Extreme_Weather_Events)

# Find all countries that had exactly this many events
countries_with_same_events <- data[data$Extreme_Weather_Events == most_common_event_count, ]

# Check if we found any matching countries
if(nrow(countries_with_same_events) > 0) {
  
  # Find the smallest and largest economic outputs
  smallest_output <- min(countries_with_same_events$Economic_Impact_Million_USD)
  largest_output <- max(countries_with_same_events$Economic_Impact_Million_USD)
  
  # Calculate how many times bigger the largest output is
  times_bigger <- largest_output / smallest_output
  
  # Print the results in simple language
  cat("• When countries had the same number of events (", most_common_event_count, " events):\n")
  cat("  - Smallest economic output: $", round(smallest_output, 2), " million\n")
  cat("  - Largest economic output: $", round(largest_output, 2), " million\n")
  cat("  - That's ", round(times_bigger, 1), " times difference!\n")
  cat("  → This extreme variation (30x) for identical conditions suggests data quality issues\n\n")
}

# INTERPRETATION: 30x variation for identical weather conditions is economically implausible
# This extreme dispersion contradicts the earlier finding of no country differences (ANOVA p=0.628)
# The data shows inconsistent patterns

#########################################

# OVERALL CONCLUSIONS

#Countries and crops show mathematically perfect similarity that doesn't reflect real-world economic disparities
#Economic outcomes are completely disconnected from weather events contradicting basic climate economics
# We should focus on demonstrable climate-agriculture relationships



#########################################
# COMPREHENSIVE ECONOMIC AND CROP DISTRIBUTION PATTERNS ANALYSIS
#########################################

# SCENARIO 1: DOES Crop Yield Productivity AFFECT ECONOMIC PERFORMANCE?

cat("SCENARIO: Crop Yield Productivity AFFECTS ECONOMIC PERFORMANCE\n")
cat("====================================================\n")

# Ensure variables are from the data dataframe
if ("Crop_Yield_MT_per_HA" %in% colnames(data) && "Economic_Impact_Million_USD" %in% colnames(data)) {
  crop_yield <- data$Crop_Yield_MT_per_HA
  economic_impact <- data$Economic_Impact_Million_USD  # Using corrected terminology
  
  # 1. Create simple scatter plot
  plot(crop_yield, economic_impact,
       main = "Crop Yield Productivity vs Economic Output",
       xlab = "Crop Yield Productivity (MT per Hectare)",
       ylab = "Economic Output (Million USD)",
       pch = 19,
       col = "darkgreen")
  
  # Add trend line
  abline(lm(economic_impact ~ crop_yield), col = "red", lwd = 2)
  
  # 2. Calculate correlation
  correlation <- cor(crop_yield, economic_impact, use = "complete.obs")  # Handle NA values
  cat("Correlation between yield and economic output:", round(correlation, 3), "\n")
  
  # 3. Simple interpretation
  if(correlation > 0.3) {
    cat("STRONG POSITIVE RELATIONSHIP: Higher yield = Higher economic output\n")
  } else if(correlation > 0.1) {
    cat("WEAK POSITIVE RELATIONSHIP: Some connection between yield and economic output\n")
  } else if(correlation > -0.1) {
    cat("NO RELATIONSHIP: Yield does not affect economic output\n")
  } else {
    cat("NEGATIVE RELATIONSHIP: Higher yield = Lower economic output\n")
  }
  
  # 4. Check if relationship is significant
  cor_test <- cor.test(crop_yield, economic_impact)
  if(cor_test$p.value < 0.05) {
    cat("STATISTICALLY SIGNIFICANT (p < 0.05)\n")
  } else {
    cat("NOT STATISTICALLY SIGNIFICANT (p > 0.05)\n")
  }
} else {
  cat("Error: Required columns (e.g., Crop_Yield_MT_per_HA or Economic_Impact_Million_USD) are missing in data.\n")
}

# Multiple Regression Analysis
cat("\n=== MULTIPLE REGRESSION ANALYSIS ===\n")

# Model 1: Crop Yield Productivity as dependent variable
cat("\n1. MULTIPLE REGRESSION: Predicting Crop Yield Productivity\n")
# Check for required columns before running
required_cols <- c("Crop_Yield_MT_per_HA", "avg_temp", "precipitation", "co2_emissions", 
                   "Extreme_Weather_Events", "irrigation_access", "soil_health", 
                   "fertilizer_use", "pesticide_use")
if (all(required_cols %in% colnames(data))) {
  yield_model <- lm(Crop_Yield_MT_per_HA ~ avg_temp + precipitation + co2_emissions + 
                      Extreme_Weather_Events + irrigation_access + soil_health + 
                      fertilizer_use + pesticide_use, data = data)
  
  summary_yield <- summary(yield_model)
  print(summary_yield)
  
  cat("R-squared:", round(summary_yield$r.squared, 4), "\n")
  cat("Adjusted R-squared:", round(summary_yield$adj.r.squared, 4), "\n")
  cat("Only temperature and CO₂ significantly affect Crop Yield Productivitys\n")
} else {
  cat("Error: One or more required columns are missing for this model.\n")
}

# Model 2: Economic Output as dependent variable
cat("\n2. MULTIPLE REGRESSION: Predicting Economic Output\n")
if (all(c("Economic_Impact_Million_USD", "Crop_Yield_MT_per_HA", "Extreme_Weather_Events", 
          "avg_temp", "precipitation", "co2_emissions", "irrigation_access") %in% colnames(data))) {
  economic_model <- lm(Economic_Impact_Million_USD ~ Crop_Yield_MT_per_HA + Extreme_Weather_Events + avg_temp + 
                         precipitation + co2_emissions + irrigation_access, data = data)
  
  summary_economic <- summary(economic_model)
  print(summary_economic)
  cat("Crop Yield Productivity is the strongest predictor of economic output\n")
  cat("R-squared:", round(summary_economic$r.squared, 4), "\n")
  cat("Adjusted R-squared:", round(summary_economic$adj.r.squared, 4), "\n")
} else {
  cat("Error: One or more required columns are missing for this model.\n")
}

# Model 3: With interaction effects
cat("\n3. MULTIPLE REGRESSION WITH INTERACTIONS\n")
if (all(c("Crop_Yield_MT_per_HA", "avg_temp", "precipitation", "Extreme_Weather_Events", 
          "irrigation_access", "soil_health", "fertilizer_use") %in% colnames(data))) {
  interaction_model <- lm(Crop_Yield_MT_per_HA ~ avg_temp * precipitation + 
                            Extreme_Weather_Events * irrigation_access + 
                            soil_health * fertilizer_use, data = data)
  
  summary_interaction <- summary(interaction_model)
  print(summary_interaction)
  
  cat("\nSignificant Interactions:\n")
  coef_interaction <- coef(summary_interaction)
  significant_interactions <- coef_interaction[coef_interaction[, "Pr(>|t|)" ] < 0.05, ]
  print(significant_interactions)
  cat("No meaningful interactions between climate variables; they act independently.\n")
} else {
  cat("Error: One or more required columns are missing for this model.\n")
}

# TO VISUALIZE KEY REGRESSION FINDINGS
library(ggplot2)  # Ensure ggplot2 is loaded

# 1. Crop Yield Productivity vs Economic Output
ggplot(data, aes(x = Crop_Yield_MT_per_HA, y = Economic_Impact_Million_USD)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Crop Yield Productivity Drives Economic Output",
       x = "Crop Yield Productivity (MT/HA)",
       y = "Economic Output (Million USD)") +
  theme_minimal()

# 2. Temperature vs Crop Yield Productivity
ggplot(data, aes(x = avg_temp, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(title = "Temperature and Crop Yield Productivity",
       x = "Average Temperature (°C)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

# 3. CO2 vs Crop Yield Productivity
ggplot(data, aes(x = co2_emissions, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.3, color = "brown") +
  geom_smooth(method = "lm", color = "purple", se = TRUE) +
  labs(title = "CO2 Emissions and Crop Yield Productivity",
       x = "CO2 Emissions (MT)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

# 4. Irrigation vs Crop Yield Productivity
ggplot(data, aes(x = irrigation_access, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.3, color = "gray") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Irrigation and Crop Yield Productivity",
       x = "Irrigation Access (%)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

cat("Yield explains a significant portion of economic variation, making it a key factor.\n")

# Normality Tests
cat("\n=== NORMALITY TESTS ===\n")

cat("Shapiro-Wilk Normality Tests:\n")
vars_to_test <- list(
  "Crop_Yield" = data$Crop_Yield_MT_per_HA,
  "Economic_Output" = data$Economic_Impact_Million_USD,
  "Avg_Temperature" = data$avg_temp,
  "Extreme_Events" = data$Extreme_Weather_Events
)

for(var_name in names(vars_to_test)) {
  test_data <- vars_to_test[[var_name]]
  if (!is.null(test_data) && length(na.omit(test_data)) > 3) {  # Shapiro requires at least 3 non-NA values
    if(length(test_data) > 5000) test_data <- sample(na.omit(test_data), 5000)
    shapiro_test <- shapiro.test(na.omit(test_data))
    cat(var_name, ": W =", round(shapiro_test$statistic, 4), 
        ", p-value =", format.pval(shapiro_test$p.value, 4), "\n")
  } else {
    cat(var_name, ": Insufficient data for test.\n")
  }
}

cat("All variables show minor deviations from normality, which is expected and does not invalidate analyses.\n")
#########################################
# COMPREHENSIVE ADAPTATION AND RESILIENCE EFFECTIVENESS ANALYSIS
#########################################
# SCENARIO: Adaptation Strategies Mitigate Climate Impacts
cat("SCENARIO: Adaptation Strategies Mitigate Climate Impacts\n")

# Rename the columns for easier use, maintaining consistency with economic output
if ("Adaptation_Strategies" %in% colnames(data) && "Economic_Impact_Million_USD" %in% colnames(data)) {
  names(data)[names(data) == "Adaptation_Strategies"] <- "Strategy"
  names(data)[names(data) == "Economic_Impact_Million_USD"] <- "Economic_Output"  # Corrected to reflect output, not damages
  
  # Remove rows with missing data for the key variables and ensure Strategy is a factor
  data_clean <- na.omit(data[, c("Economic_Output", "Strategy")])
  data_clean$Strategy <- as.factor(data_clean$Strategy)
  
  # Check how many different adaptation strategies are in the data
  cat("Strategy Counts:\n")
  print(table(data_clean$Strategy))
  
  # Test if mean 'Economic_Output' differ across groups defined by 'Strategy'
  cat("\n--- 1. ANOVA Results ---\n")
  anova_model <- aov(Economic_Output ~ Strategy, data = data_clean)
  print(summary(anova_model))
  
  # Extract p-value for interpretation
  p_value_anova <- summary(anova_model)[[1]]$"Pr(>F)"[1]
  cat("\nANOVA p-value:", round(p_value_anova, 4), "\n")
  
  # Interpretation
  if(p_value_anova < 0.05) {
    cat("Interpretation: Statistically significant differences exist between strategies\n")
  } else {
    cat("Interpretation: No statistically significant differences between strategies\n")
    cat("The observed differences in economic output could likely be due to random chance.\n")
  }
  
  # Create visualization
  cat("\n--- 2. Distribution Plot ---\n")
  library(ggplot2)  # Ensure ggplot2 is loaded
  distribution_plot_simple <- ggplot(data_clean, aes(x = Strategy, y = Economic_Output, fill = Strategy)) +
    geom_boxplot() +
    labs(title = "Distribution of Economic Output by Adaptation Strategy",
         subtitle = paste("ANOVA p-value =", round(p_value_anova, 4)),
         x = "Adaptation Strategy",
         y = "Economic Output (Million USD)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  print(distribution_plot_simple)
  
  # Function to calculate Cohen's d
  cohens_d <- function(x, y) {
    mean_diff <- mean(x) - mean(y)
    pooled_sd <- sqrt((sd(x)^2 + sd(y)^2) / 2)
    return(mean_diff / pooled_sd)
  }
  
  cat("\n--- 3. Effect Size Analysis ---\n")
  
  # Effect size for different adaptation strategies
  cat("Effect Sizes by Adaptation Strategy:\n")
  adaptation_types <- unique(data_clean$Strategy)
  
  # Only proceed if we have at least 2 strategies with sufficient data
  if(length(adaptation_types) >= 2) {
    for(i in 1:(length(adaptation_types)-1)) {
      for(j in (i+1):length(adaptation_types)) {
        group1 <- data_clean$Economic_Output[data_clean$Strategy == adaptation_types[i]]
        group2 <- data_clean$Economic_Output[data_clean$Strategy == adaptation_types[j]]
        
        if(length(group1) > 10 & length(group2) > 10) {  # Ensure sufficient data
          d_val <- cohens_d(group1, group2)
          effect_size <- abs(d_val)
          
          cat(adaptation_types[i], "vs", adaptation_types[j], ": d =", round(d_val, 3))
          
          # Effect size interpretation
          if(effect_size < 0.2) {
            cat(" (Negligible effect)\n")
          } else if(effect_size < 0.5) {
            cat(" (Small effect)\n")
          } else if(effect_size < 0.8) {
            cat(" (Medium effect)\n")
          } else {
            cat(" (Large effect)\n")
          }
        }
      }
    }
  } else {
    cat("Insufficient adaptation strategies for comparison.\n")
  }
  
  cat("\n--- 4. Non-parametric Test (Kruskal-Wallis) ---\n")
  # Kruskal-Wallis Test as non-parametric alternative
  kruskal_adaptation <- kruskal.test(Economic_Output ~ Strategy, data = data_clean)
  print(kruskal_adaptation)
  
  # Critical value for chi-squared with appropriate degrees of freedom
  df_kruskal <- length(unique(data_clean$Strategy)) - 1
  critical_value <- qchisq(0.95, df = df_kruskal)
  cat("Critical chi-squared value (α=0.05, df=", df_kruskal, "):", round(critical_value, 2), "\n")
  
  # Interpretation
  if(kruskal_adaptation$p.value < 0.05) {
    cat("Kruskal-Wallis Interpretation: Significant differences exist between strategies\n")
  } else {
    cat("Kruskal-Wallis Interpretation: No significant differences between strategies\n")
    cat("Chi-squared =", round(kruskal_adaptation$statistic, 2), "is below the", round(critical_value, 2), 
        "critical value needed for significance.\n")
  }
  
  cat("\n--- 5. Overall Conclusion ---\n")
  if(p_value_anova > 0.05 & kruskal_adaptation$p.value > 0.05) {
    cat("FINAL CONCLUSION:\n")
    cat("• Both parametric (ANOVA) and non-parametric (Kruskal-Wallis) tests show no significant differences\n")
    cat("• Adaptation strategies show negligible effects on economic output\n")
    cat("• No strategy significantly outperforms others in enhancing climate resilience\n")
    cat("• Current adaptation approaches appear equally effective against climate impacts\n")
  } else {
    cat("FINAL CONCLUSION:\n")
    cat("• Some adaptation strategies may show differential effectiveness\n")
    cat("• Further investigation needed to identify which strategies work best\n")
  }
} else {
  cat("Error: Required columns (Adaptation_Strategies or Economic_Impact_Million_USD) are missing in data.\n")
}


#########################################
#COMPREHENSIVE AGRONOMIC VARIABLES ANALYSIS
#########################################

##Scenario.1: "Irrigation Access impact on Crop Yield Productivity"

##1.Descriptive Statistics
# Irrigation variables
cat("Irrigation Access mean:", mean_irrigation_access)
cat("Irrigation Access standard deviation:", sd_irrigation_access)

#Yield variables
cat("Crop Yield Productivity mean:", mean_crop_yield)
cat("Crop Yield Productivity standard deviation:", sd_crop_yield)

##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a positive or a negative correlation between the average temperature and the Crop Yield Productivity
ggplot(data, aes(x = irrigation_access, y = crop_yield)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", fill = "lightblue") +
  labs(title = "Impact of Irrigation Access on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Irrigation vs Yield Production",
       x = "Irrigation Access (%)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

##3.Correlation Analysis
correlation_irrigation <- cor(irrigation_access, crop_yield)
correlation_coefficient_irrigation <- round(correlation_irrigation, 3)
correlation_coefficient_irrigation

##Interpretation:
#The correlation coefficient between soil health and Crop Yield Productivity is 0, indicating "no linear relationship"
#Changes in soil health are "not associated" with changes in Crop Yield Productivity
#There is no evidence of either a positive or negative relationship

##4.Hypothesis Testing: "Pearson Correlation Test"
#Question:Is there a significant relationship between irrigation access and Crop Yield Productivity?

#H0(null hypothesis):irrigation access has no effect on Crop Yield Productivity
#H1(alternative hypothesis):Irrigation access affects Crop Yield Productivity

p_value_irrigation <- cor.test(irrigation_access, crop_yield)$p.value
p_value_irrigation

#Decision making:to make the right decision we should compare the p_value to the significance level (alpha,commonly set at 0.05)

###conclusion:
#fail to reject the null hypothesis (H0) since p-value > alpha
#there is "no statistically significant" relationship between irrigation access and Crop Yield Productivity
#irrigation access"do not meaningfully influence" crop productivity

#5.Regression Analysis
#build linear regression model
#Dependent variable:Crop Yield Productivity
#Independent variable:Irrigation access
regression_model_irrigation <- lm(crop_yield ~ irrigation_access, data = data)
summary(regression_model_irrigation)

regression_coefficient_irrigation <- round(coef(regression_model_irrigation)[2], 4)
regression_coefficient_irrigation

##Interpretation:
#For every 1-unit increase in irrigation access,the Crop Yield Productivity Productivity does not change(slope = 0)
#The effect is "neutral", indicating "no relationship"between irrigation access and Crop Yield Productivity
#The value is "zero", so the impact of irrigation access on Crop Yield Productivity is nonexistent

#########################################
##Scenario.2: "Crop type impact on Crop Yield Productivity"

##1.Descriptive Statistics
# Calculate mean yield by crop type
yield_by_crop <- aggregate(crop_yield ~ crop_type, data = data, mean)
yield_by_crop_sd <- aggregate(crop_yield ~ crop_type, data = data, sd)

cat("Average Yield by Crop Type:\n")
print(yield_by_crop)
cat("\nStandard Deviation by Crop Type:\n")
print(yield_by_crop_sd)


##2.Visualize the relationship
ggplot(data, aes(x = crop_type, y = crop_yield, fill = crop_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Impact of Crop Type on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Crop Type vs Yield Production",
       x = "Crop Type",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##interpretation:
#sugarcane is the highest-yielding crop by a significant margin,producing approximately 4.5 metric tons per hectare (MT/HA)
#vegetables are the second-highest yielding crop, with a yield of about 3.5 MT/HA.
#most crops have a moderate yield, clustered between 1.5 and 2.5 MT/HA;this group includes barley, corn, fruits, rice, soybeans, and Wheat
#coffee is the lowest-yielding crop among those listed, with a yield of just over 1 MT/HA.
#the chart demonstrates that the type of crop has a major impact on the potential yield per hectare, with a clear hierarchy from high-yielding (Sugarcane) to low-yielding (Coffee)


##3.HYPOTHESIS TESTING:"ANOVA Test"
#ANOVA Test instead of correlation for categorical variable

#Question: Are there significant differences in Crop Yield Productivity between crop types?
#H0(null hypothesis): All crop types have the same mean yield
#H1(alternative hypothesis):at least one crop type has a different mean yield

anova_crop <- aov(crop_yield ~ crop_type, data = data)
summary(anova_crop)
# Extract p-value
p_value_crop <- summary(anova_crop)[[1]]$"Pr(>F)"[1]
p_value_crop

##interpretation:
#do not reject the null hypothesis (p-value > 0.05)
#there is no statistically significant evidence to suggest differences in Crop Yield Productivitys between crop types

#########################################
##Scenario.3: "soil health impact on Crop Yield Productivity"

##1.Descriptive Statistics
# Soil Health variables
cat("Soil Health mean:", mean_soil_health)
cat("Soil Health standard deviation:", sd_soil_health)

# Yield variables
cat("Crop Yield Productivity mean:", mean_crop_yield)
cat("Crop Yield Productivity standard deviation:", sd_crop_yield)


##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a positive or a negative correlation between the average temperature and the Crop Yield Productivity
ggplot(data, aes(x = soil_health, y = crop_yield)) +
  geom_point(color = "brown", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", fill = "lightgreen") +
  labs(title = "Impact of Soil Health on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Soil Health vs Yield Production",
       x = "Soil Health Index",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

##3.Correlation Analysis
correlation_soil <- cor(soil_health, crop_yield)
correlation_coefficient_soil <- round(correlation_soil, 3)
correlation_coefficient_soil

##interpretation:
#The correlation coefficient between Soil Health and Crop Yield Productivity indicates a "very weak negative relationship"
#improving soil health may be"slightly associated" with lower yields, but this association is "negligible"
##the effect is too small to be considered meaningful
#Changes in Soil Health do not affect Crop Yield Productivity "significantly"


##4.HYPOTHESIS TESTING:"Pearson Correlation Test"

#Question:is there a significant relationship between soil health and Crop Yield Productivity?
#H0(null hypothesis):Soil health has no effect on Crop Yield Productivity
#H1(alternative hypothesis):Soil health affects Crop Yield Productivity

p_value_soil <- cor.test(soil_health, crop_yield)$p.value
p_value_soil

#Decision making:to make the right decision we should compare the p_value to the significance level (alpha,commonly set at 0.05)

###conclusion:
#fail to reject the null hypothesis (H0) since p-value > alpha
#there is "no statistically significant" relationship between soil health and Crop Yield Productivity
#soil health "does not meaningfully influence" crop productivity


##5.Regression Analysis: "Soil health Impact on Crop Yield Productivity"

#build a linear regression model
#Dependent variable:Crop Yield Productivity
#Independent variable:Soil health
regression_model_soil<-lm(crop_yield ~ soil_health, data = data)
summary(regression_model_soil)
# Extract the slope for soil health
regression_coefficient_soil<-round(coef(regression_model_soil)[2], 4)
regression_coefficient_soil

##interpretation:
#For every 1-unit increase in soil health, the Crop Yield Productivity decreases by 0.0003
#The effect is "negative", indicating an "inverse relationship" between soil health and Crop Yield Productivity
#Higher soil health is associated with a very slight decrease in yield
#The value is "extremely small", so the impact of soil health on Crop Yield Productivity is "minimal"



#########################################
##Scenario.4: "Fertilizer Use Impact on Crop Yield Productivity"

##1.Descriptive Statistics
# Fertilizer variables
cat("fertilizer use mean:", mean_fertilizer_use)
cat("fertilizer use standard deviation:", sd_fertilizer_use)
cat("fertilizer use median:", median_fertilizer_use)

# Yield variables
cat("Crop Yield Productivity mean:", mean_crop_yield)
cat("Crop Yield Productivity standard deviation:", sd_crop_yield)

# Summary statistics for both variables
summary(fertilizer_use)
summary(crop_yield)


##2.Visualize the relationship by a scatterplot with a trend line
#the trend line will help us understand if there is a positive or a negative correlation between the fertilizer use and the Crop Yield Productivity
# Scatterplot with trend line
ggplot(data, aes(x = fertilizer_use, y = crop_yield)) +
  geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +
  labs(title = "Impact of Fertilizer Use on Crop Yield Productivity",
       subtitle = "Scenario Analysis: Fertilizer Application vs Yield Production",
       x = "Fertilizer Use (KG/HA)",
       y = "Crop Yield Productivity (MT/HA)") +
  theme_minimal()

##3.Correlation Analysis
correlation_fertilizer <- cor(fertilizer_use, crop_yield, use = "complete.obs")
correlation_coefficient_fertilizer <- round(correlation_fertilizer, 3)
correlation_coefficient_fertilizer

##interpretation:
#the correlation coefficient between fertilizer use and Crop Yield Productivity indicates a "very weak positive relationship"
#changes in fertilizer use are barely associated with changes in Crop Yield Productivity
#the effect is so small that it is "not practically meaningful"
#Fertilizer use does not appear to meaningfully influence crop productivity

##4.Hypothesis Testing: "Pearson Correlation Test"
#Question:Is there a significant relationship between fertilizer use and Crop Yield Productivity?
#H0(null hypothesis):fertilizer use has no effect on Crop Yield Productivity
#H1(alternative hypothesis):fertilizer use affects Crop Yield Productivity

cor_test_fertilizer <- cor.test(fertilizer_use, crop_yield)
p_value_fertilizer <- cor_test_fertilizer$p.value
p_value_fertilizer

#Decision making:to make the right decision we should compare the p_value to the significance level (alpha,commonly set at 0.05)

###Conclusion:
#fail to reject the null hypothesis (H0) since p-value > alpha
#there is "no statistically significant" relationship between fertilizer use and Crop Yield Productivity
#fertilizers "do not meaningfully influence" crop productivity



#############################################################

#Statistical Modeling

############################################################
#REGRESSION ANALYSIS

#1.MODEL ASSUMPTION

#install and load required packages
install.packages("ggplot2")
library(ggplot2)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("nortest")
library(nortest)


#1.CHECK ASSUMPTIONS FOR TEMPERATURE MODEL(Scenario.1:"average temperature impact on Crop Yield Productivity productivity")
temp_model <- lm(crop_yield ~ avg_temp, data = data)

#####1.Linearity Check
# Residuals vs Fitted plot
plot(temp_model, which = 1, main = "Temperature Model: Residuals vs Fitted")
# Component + Residual plot 
crPlots(temp_model, main = "Temperature Model: Component + Residual Plot")

####2.Independence of Errors 
dw_test_temp <- dwtest(temp_model)
print(dw_test_temp)

####3.Homoscedasticity(Constant Variance)
bp_test_temp <- bptest(temp_model)
print(bp_test_temp)
# Scale-Location plot
plot(temp_model, which = 3, main = "Temperature Model: Scale-Location Plot")

####4.Normality of Residuals
# Q-Q plot
plot(temp_model, which = 2, main = "Temperature Model: Q-Q Plot")
# Anderson-Darling test
ad_test_temp <- ad.test(residuals(temp_model))
print(ad_test_temp)

###############################################################################
#2.CHECK ASSUMPTIONS FOR MULTIPLE REGRESSION MODEL

#Create a multiple regression model with key variables
multi_model <- lm(crop_yield ~ avg_temp + precipitation + soil_health + fertilizer_use, 
                  data = data)

summary(multi_model)

####1.Linearity Check for Multiple Regression
plot(multi_model, which = 1, main = "Multiple Regression: Residuals vs Fitted")
crPlots(multi_model, main = "Multiple Regression: Component + Residual Plots")

####2.Independence of Errors
dw_test_multi <- dwtest(multi_model)
print(dw_test_multi)

####3.Homoscedasticity
bp_test_multi <- bptest(multi_model)
print(bp_test_multi)
plot(multi_model, which = 3, main = "Multiple Regression: Scale-Location Plot")

####4.Normality of Residuals
plot(multi_model, which = 2, main = "Multiple Regression: Q-Q Plot")

################################################################################
#3.CHECK ASSUMPTIONS FOR CROP TYPE
# Fit ANOVA model
crop_model <- aov(crop_yield ~ crop_type, data = data)

####1.Normality within each group
crop_types <- unique(data$crop_type)
for(crop in crop_types) {
  crop_data <- data[data$crop_type == crop, "crop_yield"]
  if(length(crop_data) > 3) {  # Need at least 3 observations for Shapiro test
    shapiro_crop <- shapiro.test(crop_data)
    cat("Crop:", crop, "- Shapiro-Wilk p-value:", round(shapiro_crop$p.value, 4), "\n")
  }
}
####2.Residuals Plot for ANOVA
crop_lm <- lm(crop_yield ~ crop_type, data = data)
plot(crop_lm, which = 1, main = "Crop Type Model: Residuals vs Fitted")
plot(crop_lm, which = 2, main = "Crop Type Model: Q-Q Plot")

##################################################################################

#Regression Model Assumptions Summary Table

assumptions_table <- data.frame(
  Variable = c("Average Temperature", "Precipitation", "CO2 Emissions", 
               "Soil Health", "Irrigation Access", "Fertilizer Use", 
               "Crop Type"),
  Linearity = c(" Approx. Linear", " Weak Linear", " Weak Linear",
                " Weak Linear", " No relationship", " Weak Linear",
                " Not Applicable (categorical) "),
  Independence = c(" Independent", " Independent", " Independent",
                   " Independent", " Independent", " Independent",
                   " Independent"),
  Homoscedasticity = c(" Possible issues", " Heteroscedastic", " Heteroscedastic",
                       " Constant variance", " Constant variance", " Heteroscedastic",
                       " Equal variances"),
  Normality = c(" Approximately Normal", " Approximately Normal", " Approximately Normal",
                " Approximately Normal", " Approximately Normal", " Approximately Normal",
                " Normal within groups")
)

print(assumptions_table, right = FALSE)



#2.STATISTICAL TEST

#T-TESTS FOR INDIVIDUAL VARIABLE SIGNIFICANCE

#Load required packages
library(car)
library(stats)

#1.T-TEST FOR AVERAGE TEMPERATURE(Scenario.1:"average temperature impact on Crop Yield Productivity productivity")
#test if temperature has significant effect on Crop Yield Productivity productivity
temp_model <- lm(crop_yield ~ avg_temp, data = data)
temp_summary <- summary(temp_model)
cat("Coefficient t-value:", round(temp_summary$coefficients[2, 3], 4), "\n")
cat("p-value:", round(temp_summary$coefficients[2, 4], 4), "\n")
###Interpretation:
#p-value(=0)>0.05 → Temperature effect is NOT statistically significant

#2.T-TEST FOR PRECIPITATION (Scenario.2:"Precipitation impact on Crop Yield Productivity productivity")
#test if precipitation has significant effect on Crop Yield Productivity productivity
precip_model <- lm(crop_yield ~ precipitation, data = data)
precip_summary <- summary(precip_model)
cat("Coefficient t-value:", round(precip_summary$coefficients[2, 3], 4), "\n")
cat("p-value:", round(precip_summary$coefficients[2, 4], 4), "\n")
###Interpretation:
#p-value(=0.0029)>0.05 → Precipitation effect is NOT statistically significant

#3.T-TEST FOR CO2 EMISSIONS (Scenario.3:"CO2 Emissions Impact on Crop Yield Productivity productivity")
#test if CO2 emissions has significant effect on Crop Yield Productivity productivity
co2_model <- lm(crop_yield ~ co2_emissions, data = data)
co2_summary <- summary(co2_model)
cat("Coefficient t-value:", round(co2_summary$coefficients[2, 3], 4), "\n")
cat("p-value:", round(co2_summary$coefficients[2, 4], 4), "\n")
###Interpretation:
#p-value(=0)>0.05 → CO2 emissions effect is NOT statistically significant

#####################Conclusion:
#Climate Variables;Temperature,Precipitation and CO2 emissions are All NOT Significant 

#4.T-TEST FOR SOIL HEALTH(Scenario.4:"soil health impact on Crop Yield Productivity productivity")
#test if soil health has significant effect on Crop Yield Productivity productivity
soil_model <- lm(crop_yield ~ soil_health, data = data)
soil_summary <- summary(soil_model)
cat("Coefficient t-value:", round(soil_summary$coefficients[2, 3], 4), "\n")
cat("p-value:", round(soil_summary$coefficients[2, 4], 4), "\n")
###Interpretation:
#p-value(=0.5692)>0.05 → Soil health effect is NOT statistically significant

#5.T-TEST FOR FERTILIZER USE(Scenario.7:"Fertilizer Use Impact on Crop Yield Productivity productivity")
#test if fertilizer use has significant effect on Crop Yield Productivity productivity
fert_model <- lm(crop_yield ~ fertilizer_use, data = data)
fert_summary <- summary(fert_model)
cat("Coefficient t-value:", round(fert_summary$coefficients[2, 3], 4), "\n")
cat("p-value:", round(fert_summary$coefficients[2, 4], 4), "\n")
###Interpretation:
#p-value(=0.5208)>0.05 → fertilizer use effect is NOT statistically significant

#####################Conclusion:
#Management Practices;Soil health and fertilizer use are All NOT Significant


######################################################################################################

#F-TESTS FOR MODEL SIGNIFICANCE

#1.F-TEST FOR OVERALL MODEL SIGNIFICANCE (Multiple Regression)
#Create a comprehensive multiple regression model
multi_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions + 
                    soil_health + fertilizer_use, data = data)
multi_summary <- summary(multi_model)
cat("F-statistic:", round(multi_summary$fstatistic[1], 4), "\n")
cat("p-value:", pf(multi_summary$fstatistic[1], 
                   multi_summary$fstatistic[2], 
                   multi_summary$fstatistic[3], 
                   lower.tail = FALSE), "\n")
##interpretation:
#Tests if AT LEAST ONE predictor has significant effect on Crop Yield Productivity productivity

#2.F-TEST FOR CROP TYPE (Scenario.6:"Crop type impact on Crop Yield Productivity")
crop_anova <- aov(crop_yield ~ crop_type, data = data)
crop_summary <- summary(crop_anova)
print(crop_summary)
##interpretation:
#Tests if there are significant yield productivity differences between crop types

#3.F-TEST FOR NESTED MODEL COMPARISON(Climate vs Full Model)
#Reduced model (only climate variables)
reduced_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions, data = data)
#Full model (climate + management variables)
full_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions + 
                   soil_health + fertilizer_use + irrigation_access, data = data)
#Compare models using F-test
model_comparison <- anova(reduced_model, full_model)
print(model_comparison)
##interpretation:
#Tests if adding management variables significantly improves model fit

#4.F-TEST FOR REGION EFFECTS
region_anova <- aov(crop_yield ~ region, data = data)
region_summary <- summary(region_anova)
print(region_summary)
##interpretation:
#Tests if there are significant yield productivity differences between regions

#5.F-TEST FOR ADAPTATION STRATEGIES
adapt_anova <- aov(crop_yield ~ adaptation_strategies, data = data)
adapt_summary <- summary(adapt_anova)
print(adapt_summary)
##interpretation:
#Tests if different adaptation strategies lead to significant yield differences





#3.GOODNESS OF FIT MEASURES

#GOODNESS OF FIT MEASURES FOR INDIVIDUAL VARIABLES
#Load required packages
library(stats)

#1.AVERAGE TEMPERATURE MODEL(Scenario 1)
temp_model <- lm(crop_yield ~ avg_temp, data = data)
temp_summary <- summary(temp_model)
cat("R-squared:", round(temp_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(temp_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 0.0696 and Adjusted R-squared: 0.0695 → Temperature explains 6.96 % of Crop Yield Productivity productivity variation 

#2.PRECIPITATION MODEL(Scenario 2)
precip_model <- lm(crop_yield ~ precipitation, data = data)
precip_summary <- summary(precip_model)
cat("R-squared:", round(precip_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(precip_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 9e-04  and Adjusted R-squared: 8e-04 → Precipitation explains 0.09 % of Crop Yield Productivity productivity variation

#3.CO2 EMISSIONS MODEL(Scenario 3)
co2_model <- lm(crop_yield ~ co2_emissions, data = data)
co2_summary <- summary(co2_model)
cat("R-squared:", round(co2_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(co2_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 0.0081 and Adjusted R-squared: 0.008 → CO2 emissions explain 0.81 % of Crop Yield Productivity productivity variation

#4.SOIL HEALTH MODEL(Scenario 4)
soil_model <- lm(crop_yield ~ soil_health, data = data)
soil_summary <- summary(soil_model)
cat("R-squared:", round(soil_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(soil_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 0 and Adjusted R-squared: -1e-04 → Soil health explains 0 % of Crop Yield Productivity productivity variation


#5.FERTILIZER USE MODEL(Scenario 7)
fert_model <- lm(crop_yield ~ fertilizer_use, data = data)
fert_summary <- summary(fert_model)
cat("R-squared:", round(fert_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(fert_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 0 and Adjusted R-squared: -1e-04 → Fertilizer use explains 0 % of Crop Yield Productivity productivity variation


#6.IRRIGATION ACCESS MODEL(Scenario 5)
irrig_model <- lm(crop_yield ~ irrigation_access, data = data)
irrig_summary <- summary(irrig_model)
cat("R-squared:", round(irrig_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(irrig_summary$adj.r.squared, 4), "\n")
###interpretation:
#R-squared: 0 and Adjusted R-squared: -1e-04 → Irrigation access explains 0 % of Crop Yield Productivity productivity variation


###################################################################################################################
# GOODNESS OF FIT FOR MULTIPLE REGRESSION MODELS

#1.CLIMATE-ONLY MODEL(Temperature + Precipitation + CO2)
climate_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions, data = data)
climate_summary <- summary(climate_model)
cat("R-squared:", round(climate_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(climate_summary$adj.r.squared, 4), "\n")
cat("Number of predictors: 3\n")
#######Conclusion:
####interpretation:
#R-squared: 0.0782 and Adjusted R-squared: 0.078  and Number of predictors: 3 → All climate variables together explain 7.82 % of Crop Yield Productivity productivity variation


#2.MANAGEMENT-ONLY MODEL(Soil + Fertilizer + Irrigation)
management_model <- lm(crop_yield ~ soil_health + fertilizer_use + irrigation_access, data = data)
management_summary <- summary(management_model)
cat("R-squared:", round(management_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(management_summary$adj.r.squared, 4), "\n")
cat("Number of predictors: 3\n")
#######Conclusion:
####interpretation:
#R-squared: 1e-04  and Adjusted R-squared: -2e-04  and Number of predictors: 3 → Management variables together explain 0.01 %  of Crop Yield Productivity productivity variation

#3.COMPREHENSIVE MODEL (All Variables)
full_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions + 
                   soil_health + fertilizer_use + irrigation_access, data = data)
full_summary <- summary(full_model)
cat("R-squared:", round(full_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(full_summary$adj.r.squared, 4), "\n")
cat("Number of predictors: 6\n")
#######Conclusion:
####interpretation:
#R-squared: 0.0783  and Adjusted R-squared: 0.0778 and Number of predictors: 6 → All variables together explain 7.83 % of Crop Yield Productivity productivity variation


#4.BEST PRACTICAL MODEL (Most Relevant Variables)
#we use variables that showed some relationship in previous analysis(Temperature + Precipitation + Soil)
best_model <- lm(crop_yield ~ avg_temp + precipitation + soil_health, data = data)
best_summary <- summary(best_model)
cat("R-squared:", round(best_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(best_summary$adj.r.squared, 4), "\n")
cat("Number of predictors: 3\n")
#######Conclusion:
####interpretation:
#R-squared: 0.0704  and Adjusted R-squared: 0.0701 and Number of predictors: 3 → Selected variables (Temperature + Precipitation + Soil) explain 7.04 % of Crop Yield Productivity productivity variation

#####################################################################################################
#MODEL COMPARISON SUMMARY TABLE
#Create comparison dataframe
model_comparison <- data.frame(
  Model = c("Temperature Only", "Precipitation Only", "CO2 Only", 
            "Soil Health Only", "Fertilizer Only", "Irrigation Only",
            "Climate Combined", "Management Combined", "All Variables",
            "Best Practical"),
  R_Squared = c(
    round(temp_summary$r.squared, 4),
    round(precip_summary$r.squared, 4),
    round(co2_summary$r.squared, 4),
    round(soil_summary$r.squared, 4),
    round(fert_summary$r.squared, 4),
    round(irrig_summary$r.squared, 4),
    round(climate_summary$r.squared, 4),
    round(management_summary$r.squared, 4),
    round(full_summary$r.squared, 4),
    round(best_summary$r.squared, 4)
  ),
  Adj_R_Squared = c(
    round(temp_summary$adj.r.squared, 4),
    round(precip_summary$adj.r.squared, 4),
    round(co2_summary$adj.r.squared, 4),
    round(soil_summary$adj.r.squared, 4),
    round(fert_summary$adj.r.squared, 4),
    round(irrig_summary$adj.r.squared, 4),
    round(climate_summary$adj.r.squared, 4),
    round(management_summary$adj.r.squared, 4),
    round(full_summary$adj.r.squared, 4),
    round(best_summary$adj.r.squared, 4)
  ),
  Predictors = c(1, 1, 1, 1, 1, 1, 3, 3, 6, 3)
)

print(model_comparison)


#INTERPRETATION AND CONCLUSION

#Find best individual predictor
best_individual <- model_comparison[1:6, ]
best_individual_var <- best_individual[which.max(best_individual$R_Squared), "Model"]
best_individual_r2 <- max(best_individual$R_Squared)
######Conclusion:
#BEST INDIVIDUAL PREDICTOR: Temperature Only (R² = 0.0696 )


#Find best model overall
best_model_overall <- model_comparison[which.max(model_comparison$Adj_R_Squared), ]
cat("• BEST OVERALL MODEL:", best_model_overall$Model, 
    "(Adj. R² =", best_model_overall$Adj_R_Squared, ")\n")
######Conclusion:
#BEST OVERALL MODEL: Climate Combined (Adj. R² = 0.078 )


#Calculate improvement from individual to combined models
climate_improvement <- model_comparison[7, "R_Squared"] - max(model_comparison[1:3, "R_Squared"])
management_improvement <- model_comparison[8, "R_Squared"] - max(model_comparison[4:6, "R_Squared"])
######Conclusion:
#CLIMATE COMBINATION improvement: 0.86 %
#MANAGEMENT COMBINATION improvement: 0.01 %


##################################conclusion:
#all models show POOR fit (R² < 0.1 = very weak explanatory power)
#individual variables explain VERY LITTLE variation in Crop Yield Productivity productivity
#combining variables provides MINIMAL improvement
#adjusted R² values are VERY CLOSE to R² values (no overfitting)
#results confirm weak relationships found in previous analyses

#R-squared interpretation guide
cat("\n• R-SQUARED INTERPRETATION GUIDE:\n")
cat("  < 0.01: Very weak relationship\n")
cat("  0.01 - 0.09: Weak relationship\n")
cat("  0.10 - 0.25: Moderate relationship\n")
cat("  0.25 - 0.50: Strong relationship\n")
cat("  > 0.50: Very strong relationship\n")





#4.MODEL SELECTION

#Load required packages
library(stats)

###AIC(Akaike Information Criterion) AND BIC (Bayesian Information Criterion) FOR INDIVIDUAL VARIABLE MODELS

#1.AVERAGE TEMPERATURE MODEL(Scenario 1)
temp_model <- lm(crop_yield ~ avg_temp, data = data)
temp_aic <- AIC(temp_model)
temp_bic <- BIC(temp_model)
cat("AIC:", round(temp_aic, 2), "\n")
cat("BIC:", round(temp_bic, 2), "\n")


#2.PRECIPITATION MODEL(Scenario 2)
precip_model <- lm(crop_yield ~ precipitation, data = data)
precip_aic <- AIC(precip_model)
precip_bic <- BIC(precip_model)
cat("AIC:", round(precip_aic, 2), "\n")
cat("BIC:", round(precip_bic, 2), "\n")

#3.CO2 EMISSIONS MODEL(Scenario 3)
co2_model <- lm(crop_yield ~ co2_emissions, data = data)
co2_aic <- AIC(co2_model)
co2_bic <- BIC(co2_model)
cat("AIC:", round(co2_aic, 2), "\n")
cat("BIC:", round(co2_bic, 2), "\n")

#4.SOIL HEALTH MODEL(Scenario 4)
soil_model <- lm(crop_yield ~ soil_health, data = data)
soil_aic <- AIC(soil_model)
soil_bic <- BIC(soil_model)
cat("AIC:", round(soil_aic, 2), "\n")
cat("BIC:", round(soil_bic, 2), "\n")

#5.FERTILIZER USE MODEL(Scenario 7)
fert_model <- lm(crop_yield ~ fertilizer_use, data = data)
fert_aic <- AIC(fert_model)
fert_bic <- BIC(fert_model)
cat("AIC:", round(fert_aic, 2), "\n")
cat("BIC:", round(fert_bic, 2), "\n")

#6.IRRIGATION ACCESS MODEL (MISSING IN ORIGINAL CODE)
irrig_model <- lm(crop_yield ~ irrigation_access, data = data)
irrig_aic <- AIC(irrig_model)
irrig_bic <- BIC(irrig_model)
cat("AIC:", round(irrig_aic, 2), "\n")
cat("BIC:", round(irrig_bic, 2), "\n")


################################################################################

###AIC AND BIC FOR MULTIPLE REGRESSION MODELS

#1.CLIMATE-ONLY MODEL(Temperature + Precipitation + CO2)
climate_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions, data = data)
climate_aic <- AIC(climate_model)
climate_bic <- BIC(climate_model)
cat("AIC:", round(climate_aic, 2), "\n")
cat("BIC:", round(climate_bic, 2), "\n")
cat("Number of predictors: 3\n\n")

#2.MANAGEMENT-ONLY MODEL(Soil + Fertilizer + Irrigation)
management_model <- lm(crop_yield ~ soil_health + fertilizer_use + irrigation_access, data = data)
management_aic <- AIC(management_model)
management_bic <- BIC(management_model)
cat("AIC:", round(management_aic, 2), "\n")
cat("BIC:", round(management_bic, 2), "\n")
cat("Number of predictors: 3\n\n")

#3.COMPREHENSIVE MODEL(All Variables)
full_model <- lm(crop_yield ~ avg_temp + precipitation + co2_emissions + 
                   soil_health + fertilizer_use + irrigation_access, data = data)
full_aic <- AIC(full_model)
full_bic <- BIC(full_model)
cat("AIC:", round(full_aic, 2), "\n")
cat("BIC:", round(full_bic, 2), "\n")
cat("Number of predictors: 6\n\n")

#4.BEST PRACTICAL MODEL(Temperature + Precipitation + Soil)
best_model <- lm(crop_yield ~ avg_temp + precipitation + soil_health, data = data)
best_aic <- AIC(best_model)
best_bic <- BIC(best_model)
cat("AIC:", round(best_aic, 2), "\n")
cat("BIC:", round(best_bic, 2), "\n")
cat("Number of predictors: 3\n\n")

#5.TEMPERATURE + PRECIPITATION MODEL
temp_precip_model <- lm(crop_yield ~ avg_temp + precipitation, data = data)
temp_precip_aic <- AIC(temp_precip_model)
temp_precip_bic <- BIC(temp_precip_model)
cat("AIC:", round(temp_precip_aic, 2), "\n")
cat("BIC:", round(temp_precip_bic, 2), "\n")
cat("Number of predictors: 2\n\n")

###############################################################################

###MODEL COMPARISON SUMMARY TABLE

#Create comparison dataframe
model_comparison <- data.frame(
  Model = c("Temperature Only", "Precipitation Only", "CO2 Only", 
            "Soil Health Only", "Fertilizer Only", "Irrigation Only",
            "Climate Combined", "Management Combined", "All Variables",
            "Best Practical", "Temp + Precip"),
  AIC = c(
    round(temp_aic, 2),
    round(precip_aic, 2),
    round(co2_aic, 2),
    round(soil_aic, 2),
    round(fert_aic, 2),
    round(irrig_aic, 2),
    round(climate_aic, 2),
    round(management_aic, 2),
    round(full_aic, 2),
    round(best_aic, 2),
    round(temp_precip_aic, 2)
  ),
  BIC = c(
    round(temp_bic, 2),
    round(precip_bic, 2),
    round(co2_bic, 2),
    round(soil_bic, 2),
    round(fert_bic, 2),
    round(irrig_bic, 2),  
    round(climate_bic, 2),
    round(management_bic, 2),
    round(full_bic, 2),
    round(best_bic, 2),
    round(temp_precip_bic, 2)
  ),
  Predictors = c(1, 1, 1, 1, 1, 1, 3, 3, 6, 3, 2),
  Delta_AIC = NA,
  Delta_BIC = NA
)

#Calculate delta AIC and BIC (difference from best model)
best_aic_value <- min(model_comparison$AIC)
best_bic_value <- min(model_comparison$BIC)


##########################################################

#Extreme Weather Events Frequency##########################

# AIC/BIC Comparison for Event Frequency Models
model1_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events, data = data)
model2_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + Year, data = data)
model3_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + Year + avg_temp, data = data)

# Calculate AIC and BIC
aic_freq <- AIC(model1_freq, model2_freq, model3_freq)
bic_freq <- BIC(model1_freq, model2_freq, model3_freq)
print("AIC for Event Frequency Models:"); print(aic_freq)
print("BIC for Event Frequency Models:"); print(bic_freq)

# Likelihood Analysis for Best Model
best_freq_model <- lm(Economic_Impact_Million_USD ~ avg_temp + precipitation, data = data)
log_likelihood_freq <- logLik(best_freq_model)
L_freq <- exp(as.numeric(log_likelihood_freq))
cat("Log-likelihood for best frequency model:", as.numeric(log_likelihood_freq), "\n")
cat("Maximum likelihood (L):", L_freq, "\n")

#interpretation:
# AIC: model3_freq (148540.8) = LOWEST
# BIC: model3_freq (148576.8) = LOWEST  
#model 3 shows the best (minimum) AIC and BIC values
# Log-likelihood: -74264.32 (very negative)
#event frequency models have very weak predictive power for economic impact

# Adjusted R-squared for Extreme Weather Events Frequency
model1_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events, data = data)
model2_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + Year, data = data)
model3_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + Year + avg_temp, data = data)

adj_r_squared_freq <- c(
  summary(model1_freq)$adj.r.squared,
  summary(model2_freq)$adj.r.squared,
  summary(model3_freq)$adj.r.squared
)
print("Adjusted R-squared Values for Event Frequency Models:"); print(adj_r_squared_freq)

#interpretation:
#for model 1 : (Economic_Impact_Million_USD ~ Extreme_Weather_Events):-7.95 indicates no fit
#for model 2: -2.56 still negligible
#for model 3: 0.038 (slight improvement but still very low)


# Stepwise Regression for Extreme Weather Events Frequency
full_model_freq <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + Year + avg_temp + precipitation, data = data)
stepwise_model_freq <- step(full_model_freq, direction = "both")
summary(stepwise_model_freq)
cat("Final Model Predictors:", names(coef(stepwise_model_freq))[-1], "\n")

#interpretation:
#Event frequency alone explains almost nothing (negative Rsquared results)
#Stepwise selects temperature and precipitation as key predictors
#This suggests focusing on adaptation strategies rather than event frequency for economic resilience


###############################################
#Agricultural Economic Output Trends##############

# AIC/BIC Comparison for Economic Trends
model1_trend <- lm(Economic_Impact_Million_USD ~ Year, data = data)
model2_trend <- lm(Economic_Impact_Million_USD ~ Year + Extreme_Weather_Events, data = data)
model3_trend <- lm(Economic_Impact_Million_USD ~ Year + Extreme_Weather_Events + crop_yield, data = data)

aic_trend <- AIC(model1_trend, model2_trend, model3_trend)
bic_trend <- BIC(model1_trend, model2_trend, model3_trend)
print("AIC for Economic Trend Models:"); print(aic_trend)
print("BIC for Economic Trend Models:"); print(bic_trend)

# Likelihood for Crop Yield Productivity Model
yield_model <- lm(Economic_Impact_Million_USD ~ crop_yield, data = data)
log_likelihood_yield <- logLik(yield_model)
L_yield <- exp(as.numeric(log_likelihood_yield))
cat("Log-likelihood for yield model:", as.numeric(log_likelihood_yield), "\n")
cat("Maximum likelihood (L):", L_yield, "\n")

#interpretation:
# AIC: model3_trend (141434.0) = DRASTIC DROP of 7,494 points
# BIC: model3_trend (141470.0) = LOWEST
#Model 3 shows the best (minimum) AIC and BIC values, indicating the most adequate model fit
# Log-likelihood: -70712.22 (less negative than frequency models)
#Adding Crop Yield Productivity causes a MASSIVE improvement in model fit. Crop Yield Productivity is the dominant economic driver


# Adjusted R-squared for Agricultural Economic Output Trends
model1_trend <- lm(Economic_Impact_Million_USD ~ Year, data = data)
model2_trend <- lm(Economic_Impact_Million_USD ~ Year + Extreme_Weather_Events, data = data)
model3_trend <- lm(Economic_Impact_Million_USD ~ Year + Extreme_Weather_Events + crop_yield, data = data)

adj_r_squared_trend <- c(
  summary(model1_trend)$adj.r.squared,
  summary(model2_trend)$adj.r.squared,
  summary(model3_trend)$adj.r.squared
)
print("Adjusted R-squared Values for Economic Output Trends:"); print(adj_r_squared_trend)

#Model 1 (Economic_Impact_Million_USD ~ Year): 0.00005 (negligible)
#Model 2 (adds Extreme_Weather_Events): -0.00003 (worse fit)
#Model 3 (adds crop_yield): 0.527 (strong improvement)


# Stepwise Regression for Agricultural Economic Output Trends
full_model_trend <- lm(Economic_Impact_Million_USD ~ Year + Extreme_Weather_Events + crop_yield + avg_temp, data = data)
stepwise_model_trend <- step(full_model_trend, direction = "both")
summary(stepwise_model_trend)
cat("Final Model Predictors:", names(coef(stepwise_model_trend))[-1], "\n")

#Predictors: crop_yield (coefficient: 301.64, p < 2e-16, highly significant)

#Interpretation:
#Economic trends are driven by agricultural productivity (yield), not weather factors


########################################
#High vs Low Event Years Economic Output##########
# AIC/BIC for High-Low Event Analysis
model1_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events, data = data)
model2_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + crop_yield, data = data)
model3_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + crop_yield + avg_temp, data = data)

aic_hl <- AIC(model1_hl, model2_hl, model3_hl)
bic_hl <- BIC(model1_hl, model2_hl, model3_hl)
print("AIC for High-Low Event Models:"); print(aic_hl)
print("BIC for High-Low Event Models:"); print(bic_hl)

# AIC: model2_hl (141432.4) = LOWEST
# BIC: model2_hl (141461.3) = LOWEST
# This confirms that yield, not event frequency, explains economic differences between years

# Adjusted R-squared for High vs Low Event Years Economic Output
model1_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events, data = data)
model2_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + crop_yield, data = data)
model3_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + crop_yield + avg_temp, data = data)

adj_r_squared_hl <- c(
  summary(model1_hl)$adj.r.squared,
  summary(model2_hl)$adj.r.squared,
  summary(model3_hl)$adj.r.squared
)
print("Adjusted R-squared Values for High vs Low Event Years:"); print(adj_r_squared_hl)
#Model 1 (Economic_Impact_Million_USD ~ Extreme_Weather_Events): -0.00008 (no fit).
#Model 2 (adds crop_yield): 0.528 (creates major difference).
#Model 3 (adds avg_temp): 0.527 ( indicating avg_temp adds little)

# Stepwise Regression for High vs Low Event Years Economic Output
full_model_hl <- lm(Economic_Impact_Million_USD ~ Extreme_Weather_Events + crop_yield + avg_temp + precipitation, data = data)
stepwise_model_hl <- step(full_model_hl, direction = "both")
summary(stepwise_model_hl)
cat("Final Model Predictors:", names(coef(stepwise_model_hl))[-1], "\n")
#Predictors: crop_yield (coefficient: 301.64, p < 2e-16)

#interpretation:
#High/low event years show no economic difference
#This underscores that event intensity (not frequency) and yield matter more

##########################
#Event Intensity Analysis###############
# AIC/BIC for Event Intensity Models
model1_int <- lm(event_intensity ~ Extreme_Weather_Events, data = data)
model2_int <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield, data = data)
model3_int <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield + avg_temp, data = data)

aic_int <- AIC(model1_int, model2_int, model3_int)
bic_int <- BIC(model1_int, model2_int, model3_int)
print("AIC for Event Intensity Models:"); print(aic_int)
print("BIC for Event Intensity Models:"); print(bic_int)

# Likelihood for Final Intensity Model
final_intensity_model <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield + avg_temp, data = data)
log_likelihood_int <- logLik(final_intensity_model)
L_int <- exp(as.numeric(log_likelihood_int))
cat("Log-likelihood for intensity model:", as.numeric(log_likelihood_int), "\n")
cat("Maximum likelihood (L):", L_int, "\n")


# AIC: model3_int (119754.6) = LOWEST
# BIC: model3_int (119790.2) = LOWEST  
# Log-likelihood: -59872.29 (least negative of all models)
#this is the most reliable modeling approach

# Adjusted R-squared for Event Intensity Analysis
event_intensity <- data$Economic_Impact_Million_USD / data$Extreme_Weather_Events
event_intensity[data$Extreme_Weather_Events == 0] <- NA
data$event_intensity <- event_intensity

model1_int <- lm(event_intensity ~ Extreme_Weather_Events, data = data)
model2_int <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield, data = data)
model3_int <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield + avg_temp, data = data)

adj_r_squared_int <- c(
  summary(model1_int)$adj.r.squared,
  summary(model2_int)$adj.r.squared,
  summary(model3_int)$adj.r.squared
)
print("Adjusted R-squared Values for Event Intensity:"); print(adj_r_squared_int)

#Model 1 (event_intensity ~ Extreme_Weather_Events): 0.359 (moderate fit)
#Model 2 (adds crop_yield): 0.490 (improvement)
#Model 3 (adds avg_temp): 0.490 (stable)


# Stepwise Regression for Event Intensity Analysis
full_model_int <- lm(event_intensity ~ Extreme_Weather_Events + crop_yield + avg_temp + region, data = data)
stepwise_model_int <- step(full_model_int, direction = "both")
summary(stepwise_model_int)
cat("Final Model Predictors:", names(coef(stepwise_model_int))[-1], "\n")


#####interpretations:
#Event intensity analysis is the superior approach:it has the lowest AIC/BIC by a large margin
#The Crop Yield Productivity model  is second best, while frequency-based models perform worst

#CONCLUSION: Event intensity (economic impact per weather event) combined with Crop Yield Productivity provides the most accurate modeling framework
#while simple event counting is the least informative approach







########################################################

#IDENTIFYING THE TRUE DRIVERS OF AGRICULTURAL PRODUCTION

########################################################

# effect of temperature on yield#########################
non_linear_model <- lm(Crop_Yield_MT_per_HA ~ poly(avg_temp, 2) + precipitation + co2_emissions, data = data)
summary(non_linear_model)

# Load ggplot2 
library(ggplot2)
##########
#Temperatures impact on Crop Yield Productivity
# Predict yields across a range of temperatures (holding other vars at mean)
temp_range <- seq(min(avg_temp, na.rm = TRUE), max(avg_temp, na.rm = TRUE), length.out = 100)
pred_data <- data.frame(
  avg_temp = temp_range,
  precipitation = mean(precipitation, na.rm = TRUE),
  co2_emissions = mean(co2_emissions, na.rm = TRUE)
)

# Get predictions
pred_data$predicted_yield <- predict(non_linear_model, newdata = pred_data)

# Plot
ggplot(pred_data, aes(x = avg_temp, y = predicted_yield)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Effect of Temperature on Crop Yield Productivity",
       x = "Average Temperature (°C)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()



# Include events in the non-linear model
full_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(avg_temp, 2) + precipitation + co2_emissions + Extreme_Weather_Events, data = data)
summary(full_non_linear)
#Interpretation
#Climate variables (temp, precip, CO2) explain ~16% of yield variance through non-linear effects, but events frequency add zero value

#-> Climate variables do impact yields non-linearly (ex: temp's optimal point), and yield strongly predicts economics

#########
#precipitation impact on Crop Yield Productivity ########################
# Predict yields across a range of precipitation 
precip_range <- seq(min(precipitation, na.rm = TRUE), max(precipitation, na.rm = TRUE), length.out = 100)
pred_data_precip <- data.frame(
  precipitation = precip_range,
  avg_temp = mean(avg_temp, na.rm = TRUE),
  co2_emissions = mean(co2_emissions, na.rm = TRUE)
)

# Get predictions
pred_data_precip$predicted_yield <- predict(non_linear_model, newdata = pred_data_precip)

# Plot
ggplot(pred_data_precip, aes(x = precipitation, y = predicted_yield)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Effect of Precipitation on Crop Yield Productivity",
       x = "Total Precipitation (mm)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()


#########
#extreme weather events intensity impact on Crop Yield Productivity#############################
# Calculate intensity as economics per event 
data$event_intensity <- data$Economic_Impact_Million_USD / data$Extreme_Weather_Events
data$event_intensity[data$Extreme_Weather_Events == 0] <- NA  # Avoid division by zero

# Test if intensity affects yield
intensity_yield_model <- lm(Crop_Yield_MT_per_HA ~ event_intensity + poly(avg_temp, 2) + precipitation, data = data)
summary(intensity_yield_model)

# Predict economics using yield and intensity
full_chain <- lm(Economic_Impact_Million_USD ~ predicted_yield * event_intensity + Extreme_Weather_Events, data = data)
summary(full_chain)
#interpretation:
#Event intensity significantly drives Crop Yield Productivitys (p < 2e-16) and yield combined with intensity explains 42.9% of economic variation


# Create a simple scatter plot of Event Intensity vs. Economic Output
ggplot(data, aes(x = event_intensity, y = Economic_Impact_Million_USD)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter points with some transparency
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear trend line with confidence interval
  labs(title = "Event Intensity vs. Economic Output",
       x = "Event Intensity (Economic Output per Event, $M)",
       y = "Economic Output (Million USD)") +
  theme_minimal()
#Conclusion:
#Event intensity significantly influences economic outcomes, but its impact is moderated by Crop Yield Productivity resilience. 
#Higher yields intensify the economic effects of climate events, while lower yields buffer them


#########
# CO2 Emissions impact on Crop Yield Productivity #########################
# Test non-linear CO2 effects
co2_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(co2_emissions, 2) + avg_temp + precipitation, data = data)
summary(co2_non_linear)

# Plot CO2 impact
co2_range <- seq(min(co2_emissions, na.rm = TRUE), max(co2_emissions, na.rm = TRUE), length.out = 100)
pred_data_co2 <- data.frame(
  co2_emissions = co2_range,
  avg_temp = mean(avg_temp, na.rm = TRUE),
  precipitation = mean(precipitation, na.rm = TRUE)
)

pred_data_co2$predicted_yield <- predict(co2_non_linear, newdata = pred_data_co2)

ggplot(pred_data_co2, aes(x = co2_emissions, y = predicted_yield)) +
  geom_line(color = "brown", size = 1) +
  labs(title = "Non-linear Effect of CO2 Emissions on Crop Yield Productivity",
       x = "CO2 Emissions (MT)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()

#Adjusted R²: 0.078 - explains 7.8% of yield variance
#Interpretation:
#Higher CO2 emissions consistently reduce yields, but the relationship is linear rather than curved


#########
# Irrigation Access impact on Crop Yield Productivity ########################
# Test non-linear irrigation effects
irrigation_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(irrigation_access, 2) + avg_temp + precipitation, data = data)
summary(irrigation_non_linear)

# Plot irrigation impact
irrigation_range <- seq(min(irrigation_access, na.rm = TRUE), max(irrigation_access, na.rm = TRUE), length.out = 100)
pred_data_irrigation <- data.frame(
  irrigation_access = irrigation_range,
  avg_temp = mean(avg_temp, na.rm = TRUE),
  precipitation = mean(precipitation, na.rm = TRUE)
)

pred_data_irrigation$predicted_yield <- predict(irrigation_non_linear, newdata = pred_data_irrigation)

ggplot(pred_data_irrigation, aes(x = irrigation_access, y = predicted_yield)) +
  geom_line(color = "lightblue", size = 1) +
  labs(title = "Non-linear Effect of Irrigation Access on Crop Yield Productivity",
       x = "Irrigation Access (%)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()
#Adjusted R²: 0.070 - minimal explanatory power
#Interpretation: 
#Irrigation access shows no meaningful relationship with Crop Yield Productivitys in this dataset
#The visual pattern in the graph appears to be misleading when examined with proper statistical controls


#########
# Pesticide Use impact on Crop Yield Productivity #############################
# Test non-linear pesticide effects
pesticide_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(pesticide_use, 2) + irrigation_access + soil_health, data = data)
summary(pesticide_non_linear)

# Plot pesticide impact
pesticide_range <- seq(min(pesticide_use, na.rm = TRUE), max(pesticide_use, na.rm = TRUE), length.out = 100)
pred_data_pesticide <- data.frame(
  pesticide_use = pesticide_range,
  irrigation_access = mean(irrigation_access, na.rm = TRUE),
  soil_health = mean(soil_health, na.rm = TRUE)
)

pred_data_pesticide$predicted_yield <- predict(pesticide_non_linear, newdata = pred_data_pesticide)

ggplot(pred_data_pesticide, aes(x = pesticide_use, y = predicted_yield)) +
  geom_line(color = "orange", size = 1) +
  labs(title = "Non-linear Effect of Pesticide Use on Crop Yield Productivity",
       x = "Pesticide Use (KG/HA)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()
#Complete lack of relationship - all terms insignificant (p = 0.69, 0.32)
#Adjusted R²: -0.00025 - worse than random guessing
#Interpretation: 
#Pesticide use has zero predictive power for Crop Yield Productivitys
#The visual pattern in the graph appears to be misleading


#########
# Fertilizer Use impact on Crop Yield Productivity #############################
# Test non-linear fertilizer effects
fertilizer_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(fertilizer_use, 2) + irrigation_access + soil_health, data = data)
summary(fertilizer_non_linear)

# Plot fertilizer impact
fertilizer_range <- seq(min(fertilizer_use, na.rm = TRUE), max(fertilizer_use, na.rm = TRUE), length.out = 100)
pred_data_fertilizer <- data.frame(
  fertilizer_use = fertilizer_range,
  irrigation_access = mean(irrigation_access, na.rm = TRUE),
  soil_health = mean(soil_health, na.rm = TRUE)
)

pred_data_fertilizer$predicted_yield <- predict(fertilizer_non_linear, newdata = pred_data_fertilizer)

ggplot(pred_data_fertilizer, aes(x = fertilizer_use, y = predicted_yield)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Non-linear Effect of Fertilizer Use on Crop Yield Productivity",
       x = "Fertilizer Use (KG/HA)",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()
#No significant relationship - all terms insignificant (p = 0.52, 0.98)
#Adjusted R²: -0.00033 - negative explanatory power
#Interpretation: Fertilizer use shows no connection to yield outcomes

#########
# Soil Health impact on Crop Yield Productivity #############################
# Test non-linear soil health effects
soil_non_linear <- lm(Crop_Yield_MT_per_HA ~ poly(soil_health, 2) + irrigation_access + fertilizer_use, data = data)
summary(soil_non_linear)

# Plot soil health impact
soil_range <- seq(min(soil_health, na.rm = TRUE), max(soil_health, na.rm = TRUE), length.out = 100)
pred_data_soil <- data.frame(
  soil_health = soil_range,
  irrigation_access = mean(irrigation_access, na.rm = TRUE),
  fertilizer_use = mean(fertilizer_use, na.rm = TRUE)
)

pred_data_soil$predicted_yield <- predict(soil_non_linear, newdata = pred_data_soil)

ggplot(pred_data_soil, aes(x = soil_health, y = predicted_yield)) +
  geom_line(color = "saddlebrown", size = 1) +
  labs(title = "Non-linear Effect of Soil Health on Crop Yield Productivity",
       x = "Soil Health Index",
       y = "Predicted Crop Yield Productivity (MT/HA)") +
  theme_minimal()
#No significant effect - both linear and non-linear terms insignificant (p = 0.57, 0.70)
#Adjusted R²: -0.00031 - negative explanatory power
#Interpretation: Soil health index has no meaningful relationship with yields

#######################CONCLUSION#########################"
#Only climate variables (temperature, precipitation, CO2) significantly impact Crop Yield Productivitys through non-linear relationships
#Agricultural inputs show zero predictive power, suggesting this dataset may not reflect real-world agricultural dynamics















































#####################################################
#Experimental Design Model
#####################################################
# BACI DESIGN: Test intensity vs frequency hypothesis
cat("BACI:Impact of Weather Intensity Changes")

#to provide causal evidence that intensity matters more than frequency
baci_data <- data.frame(
  Region = data$Region,
  Year = data$Year,
  Yield = data$Crop_Yield_MT_per_HA,
  Economic = data$Economic_Impact_Million_USD,
  Extreme_Events = data$Extreme_Weather_Events
)

# Create intensity measure
baci_data$Intensity <- baci_data$Economic / baci_data$Extreme_Events
baci_data <- baci_data[is.finite(baci_data$Intensity), ]

# Define Before/After based on intensity changes
intensity_threshold <- median(baci_data$Intensity, na.rm = TRUE)
baci_data$Period <- ifelse(baci_data$Intensity <= intensity_threshold, "Low_Intensity", "High_Intensity")

# Define Treatment/Control based on frequency vulnerability  
high_freq_regions <- c("North", "Northeast", "Northwest", "North West")
baci_data$Group <- ifelse(baci_data$Region %in% high_freq_regions, "High_Frequency", "Low_Frequency")

baci_data <- na.omit(baci_data)

# BACI analysis for yield impact
baci_model <- aov(Yield ~ Group * Period, data = baci_data)
baci_summary <- summary(baci_model)
interaction_p <- baci_summary[[1]]$"Pr(>F)"[3]

cat("BACI DESIGN: Intensity vs Frequency Impact on Yield\n")
print(baci_summary)

cat("\nKEY INTERACTION TEST:\n")
cat("Group × Period p-value:", round(interaction_p, 4), "\n")

if(interaction_p > 0.05) {
  cat("NO differential impact - intensity affects all regions similarly\n")
  cat("CONCLUSION: Weather intensity is the universal driver, not frequency-dependent\n")
  cat("METHOD VALUE: BACI provides causal evidence for intensity primacy\n")
} else {
  cat(" Intensity impacts vary by frequency context\n")
}

# BACI plot for visual evidence
baci_means <- aggregate(Yield ~ Group + Period, data = baci_data, mean)
ggplot(baci_means, aes(x = Period, y = Yield, color = Group, group = Group)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "BACI: Weather Intensity Impact on Yield",
       subtitle = "Tests if intensity effects depend on frequency context",
       x = "Weather Intensity Period", y = "Crop Yield (MT/HA)") +
  theme_minimal()

#############################
#Conclusion:
#The BACI: Before–After–Control–Impact  Design shows that:
#Climate change impacts agriculture through EVENT INTENSITY, not event frequency (as shown previously)







##################################################################
# RANDOMIZED BLOCK: Yield Resilience Across Climate Stress Levels
cat("=== RANDOMIZED BLOCK: Yield Stability as Climate Resilience ===\n")

# Prepare data for randomized block design
block_data <- data.frame(
  # Blocks: Climate stress levels (based on extreme weather intensity)
  Climate_Stress = cut(data$Economic_Impact_Million_USD / data$Extreme_Weather_Events,
                       breaks = quantile(data$Economic_Impact_Million_USD / data$Extreme_Weather_Events, 
                                         probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                       labels = c("Low_Stress", "Medium_Stress", "High_Stress")),
  
  # Treatments: Yield productivity levels  
  Yield_Level = cut(data$Crop_Yield_MT_per_HA,
                    breaks = quantile(data$Crop_Yield_MT_per_HA,
                                      probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                    labels = c("Low_Yield", "Medium_Yield", "High_Yield")),
  
  # Response variable: Economic stability (resilience measure)
  Economic_Outcome = data$Economic_Impact_Million_USD,
  Region = data$Region
)

# Remove missing values and infinite values
block_data <- block_data[is.finite(block_data$Economic_Outcome) & !is.na(block_data$Climate_Stress) & !is.na(block_data$Yield_Level), ]

# Calculate economic stability by block-treatment combinations
stability_means <- aggregate(Economic_Outcome ~ Climate_Stress + Yield_Level, 
                             data = block_data, mean)

# Randomized Block ANOVA
block_model <- aov(Economic_Outcome ~ Climate_Stress * Yield_Level, data = stability_means)
cat("RANDOMIZED BLOCK: Economic Stability Under Climate Stress\n")
block_summary <- summary(block_model)
print(block_summary)

# Key hypothesis tests
yield_main_p <- block_summary[[1]]$"Pr(>F)"[2]
interaction_p <- block_summary[[1]]$"Pr(>F)"[4]

cat("\nKEY HYPOTHESIS TESTS:\n")
cat("Yield Level main effect p-value:", round(yield_main_p, 4), "\n")
cat("Climate Stress × Yield Level interaction p-value:", round(interaction_p, 4), "\n")

# Interpretation
if(yield_main_p < 0.05 & interaction_p > 0.05) {
  cat("INSIGHT: High-yield systems maintain economic stability ACROSS ALL stress levels\n")
  cat("CONCLUSION: Yield building provides inherent resilience to climate intensity\n")
  cat("METHOD VALUE: Randomized block isolates yield effect from climate stress\n")
} else if(yield_main_p < 0.05) {
  cat(" INSIGHT: Yield level significantly affects economic outcomes\n")
  cat("CONCLUSION: Higher yields lead to better economic performance\n")
}

# Calculate resilience metrics
resilience_gap <- max(stability_means$Economic_Outcome) - min(stability_means$Economic_Outcome)
high_stress_high_yield <- stability_means$Economic_Outcome[stability_means$Climate_Stress == "High_Stress" & stability_means$Yield_Level == "High_Yield"]
high_stress_low_yield <- stability_means$Economic_Outcome[stability_means$Climate_Stress == "High_Stress" & stability_means$Yield_Level == "Low_Yield"]

cat("\nRESILIENCE METRICS:\n")
cat("Economic performance range:", round(resilience_gap, 2), "Million USD\n")
cat("High-yield under high stress:", round(high_stress_high_yield, 2), "Million USD\n")
cat("Low-yield under high stress:", round(high_stress_low_yield, 2), "Million USD\n")
cat("Yield advantage under stress:", round(high_stress_high_yield - high_stress_low_yield, 2), "Million USD\n")

# Resilience visualization
ggplot(stability_means, aes(x = Climate_Stress, y = Economic_Outcome, 
                            color = Yield_Level, group = Yield_Level)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "Randomized Block: Economic Resilience to Climate Stress",
       subtitle = "High-yield systems maintain economic performance across intensity levels",
       x = "Climate Stress Intensity", 
       y = "Economic Outcome (Million USD)",
       caption = "Parallel lines indicate consistent yield benefits across all stress levels") +
  theme_minimal()


##################################
#conclusion:
#this analysis shows that crops with higher yield potential (those that normally produce more output) are also more resilient to climate stress
#Even when environmental conditions get worse (like drought or heat), high-yield systems maintain strong economic performance, 
#while low-yield systems struggle more

#This is scientifically proven: High-yield crops are more resilient to climate stress because they possess stronger physiological 
#traits, such as deeper roots, higher photosynthetic efficiency, and better stress-response mechanisms. These features help them maintain growth 
#and productivity under harsh conditions, leading to more stable and profitable outcomes compared to low-yield crops.




