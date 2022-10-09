#START
install.packages("tidyverse")             # Install tidyverse package
install.packages("corrplot")
library(tidyverse)                      # Load tidyverse package
library(corrplot)

# Reading datasets
country_code_mapping <- read.csv("Metadata_Country.csv")
data1 <- read.csv("smoking.csv", header = FALSE) #GDP per capita, PPP, table import w/o header
data2 <- read.csv("alcohol.csv", header = FALSE) #
data3 <- read.csv("pollution.csv", header = FALSE) #
data4 <- read.csv("population.csv", header = FALSE) #
data5 <- read.csv("total-cancer-deaths-by-type.csv") #


# Removing the top 4 rows and making row 4 as header for GDP Per Capita Table
colnames(data1) <- data1[3, ] 
data1 <- data1 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Health Expenditure per capita table
colnames(data2) <- data2[3, ] 
data2 <- data2 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Hospital beds per capita table
colnames(data3) <- data3[3, ] 
data3 <- data3 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Hospital beds per capita table
colnames(data4) <- data4[3, ] 
data4 <- data4 %>% slice(4:n())

# Replacing Missing value of smoking
smoking <- data1                        # Duplicate data frame
smoking$`2017`[is.na(smoking$`2017`)] <- rowMeans(smoking[,c(45:61)], na.rm = TRUE)[is.na(smoking$`2017`)]  # Replace by row means
smoking  

# Replacing Missing value of alcohol
alcohol <- data2                   # Duplicate data frame
alcohol$`2017`[is.na(alcohol$`2017`)] <- rowMeans(alcohol[,c(45:61)], na.rm = TRUE)[is.na(alcohol$`2017`)]  # Replace by row means
alcohol                                  # Print new data frame

# Replacing Missing value of pollution
pollution <- data3                       # Duplicate data frame
pollution$`2017`[is.na(pollution$`2017`)] <- rowMeans(pollution[,c(45:61)], na.rm = TRUE)[is.na(pollution$`2017`)]  # Replace by row means
pollution     

# Replacing Missing value of population
population <- data4                         # Duplicate data frame
population$`2017`[is.na(population$`2017`)] <- rowMeans(population[,c(45:61)], na.rm = TRUE)[is.na(population$`2017`)]  # Replace by row means
population  

# Replacing Missing value of cancer_deaths
data5 <- data5 %>% filter(data5$Year == 2017)
cancer_deaths <- data5[,c(1,2,3,7)] 

# Renaming column
names(smoking)[names(smoking) == '2017'] <- 'Smoking'
names(smoking)[names(smoking) == '2017'] <- 'Smoking'
names(smoking)[names(smoking) == '2017'] <- 'Smoking'
names(alcohol)[names(alcohol) == '2017'] <- 'Alcohol_consumption'
names(pollution)[names(pollution) == '2017'] <- 'Air_pollution'
names(population)[names(population) == '2017'] <- 'Population'
names(cancer_deaths)[names(cancer_deaths) == 'Lung_cancer'] <- 'Lung_cancer_death'


# Merging all datasets
m1 = select(merge(x = country_code_mapping, y = pollution, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE),c(1,2,3,5,'Air_pollution'))
m2 = select(merge(x=m1, y=alcohol, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE), c(1:5,'Alcohol_consumption'))
m3 = select(merge(x=m2, y=smoking, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE), c(1:6,'Smoking'))
m4 = select(merge(x=m3, y=cancer_deaths, by.x = "Country.Code", by.y = "Code", all.x = TRUE), c(1:7,'Lung_cancer_death'))
final_data = select(merge(x=m4, y=population, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE),c(1:8,"Population"))


# EDA

# Finding top 6 rows
head(final_data)

# Summarizing the descriptive statistics
summary(final_data)

# Checking number of rows and columns
dim(final_data)

# Removing NA values
final_data <- na.omit(final_data)

# Plot b/w gdp_per_capita vs hos_bed_per_1000
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Smoking) +
  geom_point(shape = "circle", size = 1.5, colour = "Red") +
  theme_minimal()

# Plot b/w gdp_per_capita vs health_exp_per_capita
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Alcohol_consumption) +
  geom_point(shape = "circle", size = 1.5, colour = "Green") +
  theme_minimal()

# Plot b/w gdp_per_capita vs Dr_per_10000
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Air_pollution) +
  geom_point(shape = "circle", size = 1.5, colour = "Blue") +
  theme_minimal()

final_data <- final_data %>% filter(final_data$Lung_cancer_death <60000)

final_data$Lung_cancer_death_rate <- final_data$Lung_cancer_death*100000/final_data$Population
head(final_data)


# Finding correlation b/w variables
cor(final_data[c(5,6,7,10)])

# Plotting correlation matrix graph
corrplot(cor(final_data[c(5,6,7,10)]), method = 'color')   #Method 1: By showing shading
corrplot(cor(final_data[c(5,6,7,10)]), method = 'number')  #Method 2: By showing correlation number

#END