
#------------------------------------------------------------------
# Ames Housing Data Analysis.
# Gurjeet Singh
# Exploratory Data Analysis and Regression Models
#------------------------------------------------------------------

# Loading and attaching different packages.
require(moments)
require(ggplot2)
require(stats)
require(gridExtra)
require(GGally)


#checking the working directory
getwd()

#------------------------------------------------------------------
# Pre-Requirements
#------------------------------------------------------------------

#Loading the Abalones data into mydata
ames.df<- read.csv("ames_housing_data.csv",header = TRUE ,stringsAsFactors = FALSE)

#Checking the top and bottom 5 records from data
head(ames.df)
tail(ames.df)

#Checking the structure of the data such as variables and type.
str(ames.df)
summary(ames.df)
# Show the distribution (and levels) of a discrete/factor type variable;
table(ames.df$LotShape)

# Note that table() will suppress the count of missing values;
# Here is how we force table() to show the missing values as a level;
table(ames.df$Fence,useNA=c('always'))


#------------------------------------------------------------------
#Requirement #1: Define the Sample Population
#------------------------------------------------------------------

# Create a waterfall of drop conditions;
# Work the data frame as a 'table' like you would in SAS or SQL;

ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                        ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                        ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                        ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                        ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                        ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                        ifelse(ames.df$GrLivArea >4000,'07: LT 4000 SqFt',
                        ifelse(ames.df$BedroomAbvGr <1,'08: No Bedroom',
                        ifelse(ames.df$KitchenAbvGr <1,'09: No Kitchen',
                        ifelse(ames.df$FullBath <1,'10: No Full Bath',
                        ifelse(ames.df$Utilities!='AllPub','11: Not Public Utilities',
                          '99: Eligible Sample')
                        ))))))))));



table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
matrx <- as.matrix(waterfall,7,1)
colnames(matrx) <- c("Total Count")

matrx


# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition);



#------------------------------------------------------------------
#Requirement #1.a: # Create a list of interesting predictor variables
#------------------------------------------------------------------

#picking 20 varibles for Data Quality Check
ames.Sample <- data.frame(eligible.population$SID,eligible.population$PID,eligible.population$LotFrontage, eligible.population$LotArea, 
                              eligible.population$LotConfig, eligible.population$Neighborhood
                            , eligible.population$HouseStyle, eligible.population$OverallQual, eligible.population$OverallCond, eligible.population$YearBuilt
                            , eligible.population$YearRemodel, eligible.population$Exterior1, eligible.population$BsmtFinSF1, eligible.population$BsmtFinSF2
                            , eligible.population$CentralAir, eligible.population$GrLivArea, eligible.population$BsmtFullBath, eligible.population$BsmtHalfBath
                            , eligible.population$FullBath, eligible.population$HalfBath, eligible.population$BedroomAbvGr, eligible.population$TotRmsAbvGrd
                            , eligible.population$Fireplaces, eligible.population$GarageCars, eligible.population$GarageArea, eligible.population$WoodDeckSF
                            , eligible.population$OpenPorchSF, eligible.population$EnclosedPorch, eligible.population$ThreeSsnPorch, eligible.population$ScreenPorch
                            , eligible.population$PoolArea, eligible.population$MoSold, eligible.population$YrSold, eligible.population$SaleCondition
                            , eligible.population$SalePrice)

#renaming the columns
colnames(ames.Sample) <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood',
                             'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
                             'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
                             'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
                             'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
                             'ScreenPorch','PoolArea','MoSold','YrSold','SaleCondition','SalePrice');

#checkign the structure of the data frame
str(ames.Sample)

#creating a matrix with column names for report
field.Names <- as.matrix(colnames(ames.Sample),35,1)

#Renaming the field name
colnames(field.Names) <- "Field Names"

#getting the results.
field.Names

#Using summary to perform data quality check.
# reviewing min, max, mean, median, and outlier
summary(ames.Sample)


#------------------------------------------------------------------
#Requirement #1.b: Delete observations with missing values 
#------------------------------------------------------------------


sample.df <- na.omit(ames.Sample);

# Check the change in dimension;
dim(ames.Sample)
dim(sample.df)

dim(ames.Sample)-dim(sample.df)


#------------------------------------------------------------------
#Requirement #1.c: Define some discrete variables and indicator variables 
#------------------------------------------------------------------


# Define total square footage
sample.df$TotalSqftCalc <- sample.df$BsmtFinSF1+sample.df$BsmtFinSF2+sample.df$GrLivArea;

# Define total bathrooms
sample.df$TotalBathCalc <- sample.df$BsmtFullBath + 0.5*sample.df$BsmtHalfBath ++
  + sample.df$FullBath + 0.5*sample.df$HalfBath;


# Corner lot indicator
# ifelse(condition,valueTrue,valueFalse)
sample.df$CornerLotInd <- ifelse(sample.df$LotConfig=='Corner',1,0);

# Check how the indicator is assigned
table(sample.df$CornerLotInd,sample.df$LotConfig)

# Define two indicators for fire places
table(sample.df$Fireplaces)

# Intercept Adjustment for a single fireplace
sample.df$FireplaceInd1 <- ifelse((sample.df$Fireplaces>0)&(sample.df$Fireplaces<2),1,0);
table(sample.df$FireplaceInd1,sample.df$Fireplaces)

# Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceInd2 <- ifelse((sample.df$Fireplaces>1),1,0);
table(sample.df$FireplaceInd2,sample.df$Fireplaces)

# Additive Intercept Adjustment for a single fireplace
sample.df$FireplaceAdder1 <- ifelse((sample.df$Fireplaces>0),1,0);

# Additive Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceAdder2 <- ifelse((sample.df$Fireplaces>1),1,0);

table(sample.df$FireplaceAdder1,sample.df$Fireplaces)
table(sample.df$FireplaceAdder2,sample.df$Fireplaces)



# Central Air Indicator
sample.df$CentralAirInd <- ifelse(sample.df$CentralAir=='Y',1,0);
table(sample.df$CentralAirInd) 
# Looks like this is not useful since almost all homes have central air


# Exterior Siding Type
sample.df$BrickInd <- ifelse(sample.df$Exterior1=='BrkFace',1,0);
sample.df$VinylSidingInd <- ifelse(sample.df$Exterior1=='VinylSd',1,0);

# Pool Indicator
sample.df$PoolInd <- ifelse(sample.df$PoolArea>0,1,0);

# Wood Deck Indicator
sample.df$WoodDeckInd <- ifelse(sample.df$WoodDeckSF>0,1,0);

# Porch Indicator - Open Porch OR Screen Porch
sample.df$PorchInd <- ifelse((sample.df$OpenPorchSF>0)||(sample.df$ScreenPorch>0),1,0);

# Quality Index
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond;

table(sample.df$QualityIndex)


# Year Sold Indicators
sample.df$I2006 <- ifelse(sample.df$YrSold==2006,1,0);
sample.df$I2007 <- ifelse(sample.df$YrSold==2007,1,0);
sample.df$I2008 <- ifelse(sample.df$YrSold==2008,1,0);
sample.df$I2009 <- ifelse(sample.df$YrSold==2009,1,0);
sample.df$I2010 <- ifelse(sample.df$YrSold==2010,1,0);

table(sample.df$YrSold)
table(sample.df$I2006)
table(sample.df$I2007)
table(sample.df$I2008)
table(sample.df$I2009)
table(sample.df$I2010)


# List out sample.df
str(sample.df)


#------------------------------------------------------------------
#Requirement #1.d: Add a train/test flag to split the sample  
#------------------------------------------------------------------


sample.df$u <- runif(n=dim(sample.df)[1],min=0,max=1);
sample.df$train <- ifelse(sample.df$u<0.70,1,0);

# Check the counts on the train/test split
table(sample.df$train)

# Check the train/test split as a percentage of whole
table(sample.df$train)/dim(sample.df)[1]



#------------------------------------------------------------------
#Requirement #1.e:  Save data frame as an .RData data object   
#------------------------------------------------------------------


# Save the R data frame as an .RData object
saveRDS(sample.df,file=("ames_sample.RData"))



#------------------------------------------------------------------
#Requirement #2:  Simple Linear Regression Models
#------------------------------------------------------------------


# Loading and attaching different packages.
require(moments)
require(ggplot2)
require(stats)
require(gridExtra)
require(GGally)

# Read (or reload) the .RData object as an R data frame
sample.df <- readRDS(file= "ames_sample.RData")



# Check it
str(sample.df)

#------------------------------------------------------------------
#Requirement #2.a:  Perform Exploratory Data Analysis 
#------------------------------------------------------------------


corr.Var <- data.frame(sample.df$LotArea, sample.df$YearBuilt, sample.df$YearRemodel, 
                       sample.df$TotRmsAbvGrd, sample.df$TotalBathCalc,
                       sample.df$TotalSqftCalc, sample.df$GarageArea,
                       sample.df$SalePrice )

colnames(corr.Var) <- c('LotArea','YearBuilt', 'YearRemodel',
                        'TotRmsAbvGrd','TotalBathCalc',
                        'TotalSqft', "GarageArea", 'SalePrice');


#plotting the correction in scatter plot matrix
#picking TotalSQFT and TotalBathCalc due to high corr value
ggpairs(cor(corr.Var[,1:8]))


#creating a scatter plot
ggplot(data = sample.df, aes(x = sample.df$TotalSqftCalc, y = sample.df$SalePrice/1000)) + 
        labs(x="Total SQFT", y="Sale Price (000)" ) + 
        geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
        ggtitle("Sale Price by SQFT") +
        theme_bw()



#creating a scatter plot
ggplot(data = sample.df, aes(x = sample.df$TotalBathCalc, y = sample.df$SalePrice/1000)) + 
          labs(x="Total Bathroom", y="Sale Price (000)" ) + 
          geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
          ggtitle("Sale Price by Total Bathroom") +
          theme_bw()

#------------------------------------------------------------------
#Requirement #2.b:  fit two simple linear regression models
#------------------------------------------------------------------

##
# MOdel 1
##
# Fit a linear regression model with R
model.1 <- lm(SalePrice ~ TotalSqftCalc, data=sample.df)

options(scipen=999)
# Display model summary
summary(model.1)


#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(model.1$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Standardized residuals",datax=FALSE)

qqline(model.1$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

#creating a scatter plot
ggplot(data = sample.df, aes(x = sample.df$TotalSqftCalc, y = model.1$residuals/1000)) + 
  labs(x="Total SQFT", y="Residuals (000)" ) + 
  geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
  ggtitle("Residuals vs Total SQFT") +
  theme_bw() 



##----------------
# MOdel 2
##----------------
# Fit a linear regression model with R
model.2 <- lm(SalePrice ~ TotalBathCalc, data=sample.df)


# Display model summary
summary(model.2)


#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(model.2$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Sample Quantiles",datax=FALSE)

qqline(model.2$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")



#creating a scatter plot
ggplot(data = sample.df, aes(x = sample.df$TotalBathCalc, y = model.2$residuals/1000)) + 
  labs(x="Total Bathroom", y="Residuals (000)" ) + 
  geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
  ggtitle("Residuals vs Total Bathroom") +
  theme_bw() 




#------------------------------------------------------------------
#Requirement #3:  Multiple Linear Regression Models
#------------------------------------------------------------------


# Fit a linear regression model with R
model.3 <- lm(SalePrice ~ TotalSqftCalc + TotalBathCalc, data=sample.df)


# Display model summary
summary(model.3)

# List out components of lm object
names(model.3)


# getting analysis of variance
anova(model.3)

#calculating the coeffcient
confint(model.3)


#Creating 2 Q-Q plots
qqnorm(model.3$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Sample Quantiles",datax=FALSE)

qqline(model.3$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

#creating a scatter plot
mlr.plot1 <- ggplot(data = sample.df, aes(x = sample.df$TotalSqftCalc, y = model.3$residuals/1000)) + 
  labs(x="Total SQFT", y="Residuals (000)" ) + 
  geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
  ggtitle("Residuals vs Total SQFT") +
  theme_bw() 


#creating a scatter plot
mlr.plot2 <- ggplot(data = sample.df, aes(x = sample.df$TotalBathCalc, y = model.3$residuals/1000)) + 
  labs(x="Total Bathroom", y="Residuals (000)" ) + 
  geom_point(colour = "blue", size = 3) + geom_smooth(method = "loess", col = "red")  + 
  ggtitle("Residuals vs Total Bathroom") +
  theme_bw() 


#displaying side-by-side graphs in 1 row
grid.arrange(mlr.plot1, mlr.plot2, nrow=1, ncol=2)



#------------------------------------------------------------------
#Requirement #4: Neighborhood Accuracy
#------------------------------------------------------------------
str(sample.df)

#creating a Box Plot
ggplot(data = sample.df, aes(x = sample.df$Neighborhood, y = model.3$residuals/1000)) + 
  labs(x="Neighborhood", y="Residuals (000)" ) + geom_boxplot() +
  geom_smooth(method = "loess", col = "red")  + 
  ggtitle("Residuals vs Neighborhood") +
  theme_bw() 


# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.3 <- mean(model.3$residuals^2)
mae.3 <- mean(abs(model.3$residuals))


#creating a scatter plot
ggplot(data = sample.df, aes(x = sample.df$SalePrice/sample.df$TotalSqftCalc, y = mae.3)) + 
  labs(x="SalePrice/SQFT", y="MAE" ) + 
  geom_point(colour = "blue", size = 3) +
  ggtitle("MAE by SalePrice/SQFT") +
  theme_bw() 


#calculating the mean price per square foot for each Neighborhood
avgSalePrice.SQFT <- aggregate(sample.df$SalePrice/sample.df$TotalSqftCalc, list(sample.df$Neighborhood), mean)

#rename the column
colnames(avgSalePrice.SQFT) <- c('Neighborhood','AvgSalePricePerSQFT')


#Creating samples and reviewing the
#structure of the sample
set.seed(151)
Neighborhood.Groups <- avgSalePrice.SQFT[sample(nrow(avgSalePrice.SQFT),6),]

Neighborhood.Groups

str(sample.df)
# NoRidge lot indicator - Group 1
# ifelse(condition,valueTrue,valueFalse)
sample.df$groupNoRidge <- ifelse((sample.df$SalePrice/sample.df$TotalSqftCalc > 99) & (sample.df$SalePrice/sample.df$TotalSqftCalc < 100),1,0);

# StoneBr lot indicator - Group 2
# ifelse(condition,valueTrue,valueFalse)
sample.df$groupStoneBr <- ifelse((sample.df$SalePrice/sample.df$TotalSqftCalc > 117) & (sample.df$SalePrice/sample.df$TotalSqftCalc < 118),1,0);

# Sawyer lot indicator - Group 3
# ifelse(condition,valueTrue,valueFalse)
sample.df$groupSawyer <- ifelse((sample.df$SalePrice/sample.df$TotalSqftCalc > 80) & (sample.df$SalePrice/sample.df$TotalSqftCalc < 81),1,0);

# IDOTRR lot indicator - Group 4
# ifelse(condition,valueTrue,valueFalse)
sample.df$groupIDOTRR <- ifelse((sample.df$SalePrice/sample.df$TotalSqftCalc > 75) & (sample.df$SalePrice/sample.df$TotalSqftCalc < 76),1,0);

# Timber lot indicator - Group 5
# ifelse(condition,valueTrue,valueFalse)
sample.df$groupTimber <- ifelse((sample.df$SalePrice/sample.df$TotalSqftCalc > 103) & (sample.df$SalePrice/sample.df$TotalSqftCalc < 104),1,0);

#The base category is: NAmes


str(sample.df)


# Fit a linear regression model with R
model.4 <- lm(SalePrice ~ TotalSqftCalc + TotalBathCalc + groupNoRidge + groupStoneBr + groupSawyer + groupIDOTRR + groupTimber, data=sample.df)


# Display model summary
summary(model.4)

# List out components of lm object
names(model.4)



# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.4 <- mean(model.4$residuals^2)
mae.4 <- mean(abs(model.4$residuals))


cbind ("MLR Model 3" = mae.3, "MLR Model 4" = mae.4)


#------------------------------------------------------------------
#Requirement #5: Regression models for the transformed response log(SalePrice)
#------------------------------------------------------------------

str(sample.df)

#Transforming the SalePrice field
L_SalePrice <- log(sample.df$SalePrice)


## Cotinuous variables - Selected based on Correlation
# - TotalSqftCalc
# - TotalBathCalc
# - GarageArea
# - LotArea

## Discreate variable
# - YearBuilt

## Nominal variable
# - Neighborhood


#model 5
##
# Fit a linear regression model with R
model.5 <- lm(SalePrice ~ TotalSqftCalc + TotalBathCalc + GarageArea + LotArea +  YearBuilt + Neighborhood, data=sample.df)

# Display model summary
summary(model.5)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.5 <- mean(model.5$residuals^2)
mae.5 <- mean(abs(model.5$residuals))



#model 6
##
# Fit a linear regression model with R
model.6 <- lm(L_SalePrice ~ TotalSqftCalc + TotalBathCalc + GarageArea + LotArea +  YearBuilt + Neighborhood, data=sample.df)

# Display model summary
summary(model.6)

#Creating 2 Q-Q plots
qqnorm(model.6$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Sample Quantiles",datax=FALSE)

qqline(model.6$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")



yhat <- exp(model.6$fitted.values)

mae.6 <- mean(abs(yhat - sample.df$SalePrice))



#Using Par to layout the graphs below 3 rows and 3 columns
par(mfrow=c(2,1))

#Creating 2 Q-Q plots
qqnorm(model.5$residuals, main = "Q-Q plot, Rediduals (Model 5)", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Sample Quantiles",datax=FALSE)

qqline(model.5$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")


#Creating 2 Q-Q plots
qqnorm(model.6$residuals, main = "Q-Q plot, Rediduals (Model 6)", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Sample Quantiles",datax=FALSE)

qqline(model.6$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")


cbind ("MLR Model 5" = mae.5, "MLR Model 6" = mae.6)


#------------------------------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#------------------------------------------------------------------


