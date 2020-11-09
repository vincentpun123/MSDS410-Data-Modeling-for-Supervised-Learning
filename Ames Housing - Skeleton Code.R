
##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")

str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                  "price_sqft", "SalePrice","LotArea",
                                  "BsmtFinSF1","Neighborhood","HouseStyle",
                                  "LotShape","OverallQual","logSalePrice",
                                  "TotalBsmtSF","HouseStyle"))

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","OverallQual","logSalePrice"))
#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=QualityIndex)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(subdat, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

############################################################
################ model focussed EDA #######################
###########################################################

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(subdat)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

#####################################################################
############# Define the sample data ###########################
##################################################################

subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat2)

ggplot(subdat2, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat2, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###



# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(subdat2)
s3d <-scatterplot3d(TotalFloorSF,QualityIndex,SalePrice,pch=16, 
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit <- lm(SalePrice ~ TotalFloorSF + QualityIndex) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat2)
scatter3d(SalePrice,TotalFloorSF,QualityIndex)

############## fitting a SLR ###################################

SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdatnum)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(1,1))  # visualize four graphs at once


pred <- as.data.frame(predict(SLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
str(subdatnum)
head(subdatnum)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
library(reshape)
subdatnum <- rename(subdatnum, c(fit="fitSLR"))


############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLR"))
subdatnum$res <- subdatnum$SalePrice - subdatnum$fitMLR

head(subdatnum)

###################################################################
##################### Assignment 3  ################################
#################################################################

################  MAE calculation ###################################
MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdat)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

require(ggplot2)
ggplot(subdat, aes(x=OverallQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################################################################
############### Log Transformation #############################

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)

pred <- as.data.frame(predict(MLRLogresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLRLog"))
subdatnum$reslog <- subdatnum$logSalePrice - subdatnum$fitMLRLog
MAE <- mean(abs(subdatnum$reslog))
MAE

head(subdatnum)

library(car)
vif(MLRLogresult)
par(mfrow=c(1,1))
influencePlot(MLRLogresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLRLogresult))
dffitslog <- dffits(MLRLogresult)
subdatnum <- cbind(subdatnum,dffitslog)
str(subdatnum)

ggplot(subdatnum, aes(x=OverallQual, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdatnum$absdf <- abs(subdatnum$dffitslog)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.064),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnuminf)
anova(MLRLogresult)
summary(MLRLogresult)

############## analyze Neighborhood variable #########

require(ggplot2)
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(plyr)
subdat1 <- ddply(subdat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#### Clean up of the Neighborhood varaible  ########

subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=100, "grp1", 
         ifelse(subdat$price_sqft<=120, "grp2",
                ifelse(subdat$price_sqft<=140, "grp3",
                       "grp4"))) 

################ include categoriacl variable in the model #######

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp, data=subdat)
anova(MLRresult)
summary(MLRresult)
pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

################# define dummy variables ###################

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
  ifelse(subdat$NbhdGrp == "grp3", 1, 0)

MLRresult4 = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp1+NbhdGrp2+NbhdGrp3, 
                data=subdat)
anova(MLRresult4)
summary(MLRresult4)



############################################################
############## assignment 5 #############################
#########################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subdat
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

train.clean <- subset(train.df, select=c("TotalFloorSF","HouseAge",
                                         "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                         "TotalBsmtSF","Style1","Style2"))

test.clean <- subset(test.df, select=c("TotalFloorSF","HouseAge",
                                       "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                       "TotalBsmtSF","Style1","Style2"))


# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)


# Define the upper model as the FULL model
upper.lm <- lm(logSalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(logSalePrice ~ 1,data=train.clean);
summary(lower.lm)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(logSalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(logSalePrice ~ OverallQual + LotArea, data=train.clean)
summary(junk.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

forward.test <- predict(forward.lm,newdata=test.clean);
backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.clean);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$logSalePrice-backward.test)/test.df$logSalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$logSalePrice-stepwise.test)/test.df$logSalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$logSalePrice-junk.test)/test.df$logSalePrice;
MAPE <- mean(junk.testPCT)
MAPE


# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
######################################################################
sub <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                               "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                               "TotalBsmtSF","Style1","Style2"))

MLRresult1 = lm(logSalePrice ~ ., data=sub)

sub2 <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                                "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                "TotalBsmtSF"))

MLRresult2 = lm(logSalePrice ~ ., data=sub2)
anova(MLRresult1,MLRresult2)

anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

names(MLRresult)
head(MLRresult$df.residual)

inflm.MLRLog <- influence.measures(MLRresult)
names(inflm.MLRLog)
str(inflm.MLRLog)
inflmetrics <- as.data.frame(inflm.MLRLog$infmat)
dffit_df <- subset(inflmetrics, select= c(dffit))
sub$r1 <- row.names(sub)
dffit_df$r1 <- row.names(dffit_df)
subnew <- merge(sub, dffit_df, all=FALSE)

subnew <- subset(subnew, select= -c(r1))

subnew$absdffit <- abs(subnew$dffit)

subnewinf <- subnew[which(subnew$absdf < 0.064),]



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+
                    LotArea+BsmtFinSF1+TotalBsmtSF+Style1+Style2,data=subnewinf)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)








