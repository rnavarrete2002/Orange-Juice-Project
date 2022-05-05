#Data Analytics
#Rebeca Navarrete
#R Script
library(tidyverse)


#Getting the Orange Juice Dataset from GitHub
#This is a data set abbreviation of the OJ data set found in https://vincentarelbundock.github.io/Rdatasets/datasets.html
urlfile <- "https://raw.githubusercontent.com/rnavarrete2002/OrangeJuice-Project/main/Orange%20Juice%20Dataset.csv"

# Read the data into R and name it orangeJuice.
orangeJuice <- read_csv(url(urlfile))

################################################################################
#Since the Price and SalePrice variables have a numeric type, while our Type variable 
#(used to identify the type of orange juice) has a character type; thus, we proceed to 
#change the Type variable into a factor using the "as.factor" function. 
orangeJuice$Type <- as.factor(orangeJuice$Type)

#We use the _head_function to confirm our changes and look at the first six lines of the data set.
head(orangeJuice)

################################################################################
#Creates a boxplot of the sales price of Citrus Hill and Minute Maid orange juice
p <- ggplot(orangeJuice, aes (x=Type, y=SalePrice)) + geom_boxplot(aes(fill=Type))
p + labs (title="Orange Juice Boxplot")

#Creates a boxplot of the price of Citrus Hill and Minute Maid orange juice
p <- ggplot(orangeJuice, aes (x=Type, y=Price)) + geom_boxplot(aes(fill=Type))
p + labs (title="Orange Juice Boxplot")


#The two boxplots above show that the price and the sales price of the Minute Maid
#Orange juice is more expensive than the price and the sales price of the Citrus Hill.
#Likewise, it shows that distribution range of Minute Maid is bigger than the 
#distribution range of Citrus Hill in the mentioned variables.

################################################################################
#Creates a variance and a two-sample t-test of the sales price of Citrus Hill and Minute Maid orange juice
CH <- orangeJuice %>% filter(Type == "CH") %>% select(SalePrice)                #CH stands for Citrus Hill
MM <- orangeJuice %>% filter(Type == "MM") %>% select(SalePrice)                #MM stands for Minute Maid
mean(CH$SalePrice)  # CH is actually a dataframe with two columns so we create the SalePrice vector
mean(MM$SalePrice)  # MM is actually a dataframe with two columns so we create the SalePrice vector
t.test(CH$SalePrice,MM$SalePrice)
var.test(CH$SalePrice,MM$SalePrice)


#From the output we see the p-value < 2.2e-16. Since 2.2e-16 is less than 0.05 (alpha), 
#we reject the null hypothesis that the group variances are the same.
#What does this mean? It means, we have sufficient evidence to say the variance for 
#sale price for the two orange juice types are different.
#Likewise, the p-value shows the results are significant.

#Creates a variance and two-sample t-test of the price of Citrus Hill and Minute Maid orange juice
CH <- orangeJuice %>% filter(Type == "CH") %>% select(Price)  # CH is actually a dataframe with two columns so we create the Price vector
MM <- orangeJuice %>% filter(Type == "MM") %>% select(Price)  # MM is actually a dataframe with two columns so we create the Price vector
t.test(CH$Price,MM$Price)
var.test(CH$Price,MM$Price)


#From the output we see the p-value < 2.2e-16. Since 2.2e-16 is less than 0.05 (alpha), 
#we reject the null hypothesis that the group variances are the same.
#What does this mean? It means, we have sufficient evidence to say the variance for 
#price for the two orange juice types are different.
#Likewise, the p-value shows the results are significant.

################################################################################
#Creates a linear function with output Price and input SalePrice.
#Uses the summary function to display summaries of the fitting functions.
model <- lm(orangeJuice$Price ~ orangeJuice$SalePrice)
summary(model)

#The summary shows that the y-intercept is 1.13, the slope is 0.45, the p-value is 
#<2e-16 and the residual standard error is 0.13.

#The linear model that will predict price based on sales price is 
#Price = 0.45SalePrice + 1.13 

#We create a visualization
Vis <- orangeJuice %>% ggplot()
Vis + geom_point(aes(SalePrice,Price),col="deeppink") +
  geom_abline(slope = 0.45,intercept = 1.13,col = "black") +
  labs(title="Linear Model: Price as a function of Sale Price")

#The visualization shows that there is a positive relationship between the price 
#and the sales price of Minute Maid and Citrus Hill orange juice. As the sale price 
#increases, the price increases too.
