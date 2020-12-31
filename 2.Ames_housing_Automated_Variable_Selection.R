#------------------------------------------------------------------
# Ames Housing Data Analysis.
# Gurjeet Singh
# Automated Variable Selection, Multicollinearity, and Predictive Modeling
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
#Requirement #2:  The Predictive Modeling Framework
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


# Create train/test split;
train.df <- subset(sample.df, train==1);
test.df <- subset(sample.df, train==0);


# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up? - Yes
dim(sample.df)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

tbl <- cbind(dim(train.df)[1], dim(test.df)[1])

colnames(tbl) <- c("Train_DF","Test_DF")

rownames(tbl) <- "Observation"
tbl

View(tbl)


#------------------------------------------------------------------
#Requirement #3:  Model Identification by Automated Variable Selection 
#------------------------------------------------------------------

#creating a drop list
drop.list <- c('SID','PID','LotConfig','Neighborhood','HouseStyle','YearBuilt','YearRemodel',
              'Exterior1','BsmtFinSF1','BsmtFinSF2','CentralAir','YrSold','MoSold','SaleCondition',
              'u','train','I2010','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath',
              'FireplaceInd1','FireplaceInd2','OverallQual','OverallCond','PoolArea','GrLivArea',
              'I2006','I2007','I2008','I2009','FireplaceAdder2','FireplaceAdder1');

#droping the variables
train.clean <-train.df[,!(names(sample.df) %in% drop.list)];

str(train.clean)

col.Name <-as.matrix(colnames(train.clean))
colnames(col.Name) <- "Field_Names"

View(col.Name)


# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean);
summary(lower.lm)


# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean);
summary(sqft.lm)

options(scipen=999)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)


junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)

#summary output of each of the models
summary(forward.lm)
summary(backward.lm)
summary(stepwise.lm)
summary(junk.lm)

#checking the correlation of the junk model variables
corr.Var <- data.frame(train.df$OverallQual, train.df$OverallCond, train.df$QualityIndex, 
                       train.df$GrLivArea, train.df$TotalSqftCalc)

colnames(corr.Var) <- c('OverallQual','OverallCond', 'QualityIndex',
                        'GrLivArea','TotalSqftCalc');
ggpairs(cor(corr.Var[,1:5]))



# Compute the VIF values
library(car)
forward.VIF <- as.matrix(sort(vif(forward.lm),decreasing=TRUE))
backward.VIF <- as.matrix(sort(vif(backward.lm),decreasing=TRUE))
stepwise.VIF <- as.matrix(sort(vif(stepwise.lm),decreasing=TRUE))
junk.VIF <- as.matrix(sort(vif(junk.lm),decreasing=TRUE))

colnames(forward.VIF) <- "VIF_Values"
colnames(backward.VIF) <- "VIF_Values"
colnames(stepwise.VIF) <- "VIF_Values"
colnames(junk.VIF) <- "VIF_Values"

View(forward.VIF)
View(backward.VIF)
View(stepwise.VIF)
View(junk.VIF)


#-----------------------------------------------------
## - Model Comparison section
#-----------------------------------------------------

#extract the model information from summary output
forward_selection_model <- forward.lm$call
backward_selection_model <- backward.lm$call
stepwise_selection_model <- stepwise.lm$call
junk_selection_model <- junk.lm$call

#Printing the each model.
forward_selection_model

backward_selection_model

stepwise_selection_model

junk_selection_model

#Creating a table to VIF values for 3 models
table.models <- matrix(c(forward.VIF, backward.VIF, stepwise.VIF), ncol = 3)
rownames(table.models) <- row.names(forward.VIF)
colnames(table.models) <- c("forward.VIF","backward.VIF","stepwise.VIF")
View(table.models)

#Creating a table to VIF values of the junk model
table.junk.model <- matrix(junk.VIF)
rownames(table.junk.model) <- row.names(junk.VIF)
colnames(table.junk.model) <- c("junk.VIF")
View(table.junk.model)

#adjusted R-Squared
summary(forward.lm)$adj.r.squared
summary(backward.lm)$adj.r.squared
summary(stepwise.lm)$adj.r.squared
summary(junk.lm)$adj.r.squared

#AIC
AIC(forward.lm)
AIC(backward.lm)
AIC(stepwise.lm)
AIC(junk.lm)

#BIC
BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

##MSE
mse.forward <- mean(forward.lm$residuals^2)
mse.backward <- mean(backward.lm$residuals^2)
mse.stepwise <- mean(stepwise.lm$residuals^2)
mse.junk <- mean(junk.lm$residuals^2)

##MAE
mae.forward <- mean(abs(forward.lm$residuals))
mae.backward <- mean(abs(backward.lm$residuals))
mae.stepwise <- mean(abs(stepwise.lm$residuals))
mae.junk <- mean(abs(junk.lm$residuals))


##Creating a Table to include all the metrics
rsqrd.Mat <- matrix(c(summary(junk.lm)$adj.r.squared, summary(forward.lm)$adj.r.squared, 
                      summary(backward.lm)$adj.r.squared, summary(stepwise.lm)$adj.r.squared
                      ), ncol = 1)
rownames(rsqrd.Mat) <- c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(rsqrd.Mat) <- "Adjusted_R_Squared"


AIC.Mat <- matrix(c(AIC(junk.lm), AIC(forward.lm), AIC(backward.lm), AIC(stepwise.lm)
                      ), ncol = 1)
rownames(AIC.Mat) <- c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(AIC.Mat) <- "AIC_Values"


BIC.Mat <- matrix(c(BIC(junk.lm), BIC(forward.lm), BIC(backward.lm), BIC(stepwise.lm)
                    ), ncol = 1)
rownames(BIC.Mat) <-  c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(BIC.Mat) <- "BIC_Values"


rank <- matrix(c(1,2,2,2), ncol = 1)
rownames(rank) <-  c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(rank) <- "Rank"


MSE.Mat <- matrix(c(mse.junk, mse.forward, mse.backward, mse.stepwise), ncol = 1)
rownames(MSE.Mat) <-  c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(MSE.Mat) <- "MSE_Values"

MAE.Mat <- matrix(c(mae.junk, mae.forward, mae.backward, mae.stepwise), ncol = 1)
rownames(MAE.Mat) <-  c("junk.lm", "Forward.lm", "backward.lm", "stepwise.lm")
colnames(MAE.Mat) <- "MAE_Values"



final.table  <- cbind(rank, rsqrd.Mat, AIC.Mat, BIC.Mat, MSE.Mat, MAE.Mat)

View(final.table)


#------------------------------------------------------------------
#Requirement #4:  Predictive Accuracy 
#------------------------------------------------------------------


forward.test <- predict(forward.lm,newdata=test.df);
backward.test <- predict(backward.lm,newdata=test.df);
stepwise.test <- predict(stepwise.lm,newdata=test.df);
junk.test <- predict(junk.lm,newdata=test.df);


##MSE
mse.forward.test <- mean((test.df$SalePrice - forward.test)^2)
mse.backward.test <- mean((test.df$SalePrice - backward.test)^2)
mse.stepwise.test <- mean((test.df$SalePrice - stepwise.test)^2)
mse.junk.test <- mean((test.df$SalePrice - junk.test)^2)

##MAE
mae.forward.test <- mean(abs(forward.test - test.df$SalePrice))
mae.backward.test <- mean(abs(backward.test - test.df$SalePrice))
mae.stepwise.test <- mean(abs(stepwise.test - test.df$SalePrice))
mae.junk.test <- mean(abs(junk.test - test.df$SalePrice))

##Creating a Table to include all the metrics
MSE.Mat.test <- matrix(c(mse.junk.test, mse.forward.test, mse.backward.test, mse.stepwise.test), ncol = 1)
rownames(MSE.Mat.test) <-  c("junk.lm.test", "Forward.lm.test", "backward.lm.test", "stepwise.lm.test")
colnames(MSE.Mat.test) <- "MSE_Values"


MAE.Mat.test <- matrix(c(mae.junk.test, mae.forward.test, mae.backward.test, mae.stepwise.test), ncol = 1)
rownames(MAE.Mat.test) <-  c("junk.lm.test", "Forward.lm.test", "backward.lm.test", "stepwise.lm.test")
colnames(MAE.Mat.test) <- "MAE_Values"

final.table.test  <- cbind(MSE.Mat.test, MAE.Mat.test )

View(final.table.test)


#------------------------------------------------------------------
#Requirement #5: Operational Validation
#------------------------------------------------------------------

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;

# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                        )
                                  )
forward.trainTable <- table(forward.PredictionGrade)
forward.train.result <- forward.trainTable/sum(forward.trainTable)

# Assign Prediction Grades;
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                        )
                                  )
backward.trainTable <- table(backward.PredictionGrade)
backward.train.result <- backward.trainTable/sum(backward.trainTable)

# Assign Prediction Grades;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0,0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                         )
                                  )
stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.train.result <- stepwise.trainTable/sum(stepwise.trainTable)


# Assign Prediction Grades;
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0,0.10]',
                                   ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )
)
junk.trainTable <- table(junk.PredictionGrade)
junk.train.result <- junk.trainTable/sum(junk.trainTable)



# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;


# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                            )
                                      )
forward.testTable <-table(forward.testPredictionGrade)
forward.test.result <- forward.testTable/sum(forward.testTable)


# Assign Prediction Grades;
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
backward.testTable <-table(backward.testPredictionGrade)
backward.test.result <- backward.testTable/sum(backward.testTable)



# Assign Prediction Grades;
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.test.result <- stepwise.testTable/sum(stepwise.testTable)


# Assign Prediction Grades;
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
junk.testTable <-table(junk.testPredictionGrade)
junk.test.result <- junk.testTable/sum(junk.testTable)


##Creating a Table to include all grades
Model.Predict.com <-matrix(c("forward.train.result" = forward.train.result,"forward.test.result" = forward.test.result,
                             "backward.train.result" = backward.train.result, "backward.test.result" = backward.test.result,
                             "stepwise.train.result" = stepwise.train.result, "stepwise.test.result" = stepwise.test.result,
                             "junk.train.result" = junk.train.result, "junk.test.result" = junk.test.result
                             ), ncol =8)

colnames(Model.Predict.com) = c("forward.train.result", "forward.test.result", 
                                "backward.train.result", "backward.test.result" ,
                                "stepwise.train.result", "stepwise.test.result",
                                "junk.train.result", "junk.test.result" )

rownames(Model.Predict.com) = c(" Grade 1: [0,0.10]", "Grade 2: (0.10,0.15]" ,
                                "Grade 3: (0.15,0.25]" ,"Grade 4: (0.25+]" )

View(Model.Predict.com)


######################################################################################################################

######################################################################################################################

#------------------------------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#------------------------------------------------------------------


