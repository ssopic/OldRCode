#Assignment one but mobing towards the modelling stage of the assignment
#Now we already know a lot about how the data looks like so I am adding the linear and other models to it
#Set up##############
setwd("C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models")
############Adding all of the above prices###########
################
#Libraries
library(psych) #for correlation matrices and vizualization
library(readxl) #reading xls
library(Hmisc) #For imputations
library(reshape2) #for melting
library(tsoutliers) #for time series outliers
library(lubridate) #Date functions
library(RCurl) #I don't remember
library(caret) #train/test
clean <- read_excel("clean.xlsx")
#Removing the first empty column
clean <- clean[,-1]
names(clean)
###################LINEAR MODEL############
#Focal_Brand_Sales: #We are Nivea
      #My competitors are Dove, Vogue, Rexona + we think that Axe might have initiated the price decrease which might have initiated a price war. 
clean$WEEK.x <- as.Date(clean$WEEK.x)
#Rexona odjeb
#Dove odjeb
#Vogue nije face valid
#Fa odjeb
#8by4 odjeb
#add the price war?
clean$PriceWar <- NA
clean$PriceWar <- ifelse(clean$Index>75,1,0)
#Adding year quarters
library(zoo)
clean$yq <- as.yearqtr(clean$WEEK.x, format = "%Y-%m-%d")
yq
      lm <- summary(lm(data=clean, clean$NIVEASales~clean$NIVEAPrice*clean$Nivea_Above+clean$NIVEAPriceDummy+clean$SANEXPrice*clean$Sanex_Above+clean$VOutMarket +clean$SANEXSales+clean$`8X4D+F.x`+clean$PriceWar+yq+clean$DOVEPrice))
  lmn <- lm(data=clean, clean$NIVEASales~clean$NIVEAPrice*clean$Nivea_Above+clean$DOVEPrice*clean$PriceWar+clean$WEEK.x*clean$Temperature+clean$`REXONAD+F.x`+clean$PriceWar)    
install.packages("stargazer")
library(stargazer)

stargazer(lmn, type="text")
summary(lmn)
out <- capture.output(lmn)
cat("LM Output", out, file="LMOutput.jpeg", sep="n", append=TRUE)

log <- lm(data=clean, log(clean$NIVEASales)~log(clean$NIVEAPrice)*log(clean$Nivea_Above+1)+log(clean$DOVEPrice)*log(clean$PriceWar+1)+clean$WEEK.x*log(clean$Temperature)+log(clean$`REXONAD+F.x`+1)+log(clean$PriceWar+1))
summary(log)
stargazer(log, type="text")
library(car)
durbinWatsonTest(lmn)
install.packages("lmtest")
library(lmtest)
#https://cran.r-project.org/web/packages/lmtest/lmtest.pdf
#This package could be useful for later
bgtest(lmn)
bptest(lmn)
library(gvlma)
gvlma(lmn)
par(mfrow=c(2,2)) # 4 charts in 1 panel
plot(lmn)
gvlma(log)
bptest(log)
durbinWatsonTest(log)
gqtest(lmn, order.by = fitted(lmn))
gqtest(log)

#Added data on quartiles
#Dodavati varijable dokle god ima smisla i onda ih pokusati izvesti u predvidanje. To znaci podijeliti na test and train. Moram saznati kako to napraviti sa time series podatcima.Za kraj treba odraditi testove da li mi je model heteroskedastican itd. To treba prepisati iz notesa(White test etc.) I onda to ponoviti na log modelu.  I trebam nesto znati o elasticnostima kada zavrsim sa modelom. 
summary(d)
total <- merge(cleantotal, clean, by="Index")
###Measuring out of sample fit
??createTimeSlices
library(caret)
set.seed(123)
split <- createTimeSlices(clean, initialWindow = 50)
trainSlices <- split[[1]]
testSlices <- split[[2]]


lmmod <- train(data=clean[trainSlices[[1]],], clean$NIVEASales~clean$NIVEAPrice*clean$Nivea_Above+clean$DOVEPrice*clean$PriceWar+clean$WEEK.x*clean$Temperature+clean$`REXONAD+F.x`+clean$PriceWar, method="lm")

mytimecontrol <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon=12,
                              fixedWindow = TRUE)
logmod <- train(data=clean[trainSlices[[1]],], log(NIVEASales)~log(NIVEAPrice)*log(Nivea_Above+1)+log(DOVEPrice)*log(PriceWar+1)+WEEK.x*log(Temperature)+log(`REXONAD+F.x`+1)+log(PriceWar+1), method="lm", trControl=mytimecontrol)
logmod
lmmod <- train(data=clean[trainSlices[[1]],], NIVEASales~NIVEAPrice*Nivea_Above+DOVEPrice*PriceWar+WEEK.x*Temperature+`REXONAD+F.x`+PriceWar, method="lm", trControl=mytimecontrol)
lmmod

install.packages("MLmetrics")
MSE()
train <- sample(1:nrow(clean), 90)
test <- (1:nrow(clean))[-train]
plm <- predict(lm, test)
lm
log
library(modelr)
splitData <- resample_partition(clean, c(test=0.3, train=0.7))

mae(model=exp(logmod), data=splitData$test)
mae(model=lmmod, data=splitData$test)

predictedlog <- predict(log, splitData$test)

predictedlog <- exp(predictedlog)
predictedlm <- predict(lmn, splitData$test)
errorlog <- splitData$test$data-predictedlog
errorlm <- splitData$test$data-predictedlm
rmse(errorlog$NIVEASales)
rmse(errorlm$NIVEASales)
mae(errorlog$NIVEASales)
mae(errorlm$NIVEASales)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
      sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
      mean(abs(error))
}
true <- splitData$test
true$data
data <- as.data.frame(true$data) 


#############ASSIGNMENT 2###############
#REPORT THE DEGREES OF FREEDOM FROM PREVIOUS MODEL
#NEED A VARIABLE FOR THE PRICE DROP OF ALL BRANDS
#ROBUSTNESS CHECKS
#TEST DATASET
rm(list=setdiff(ls(), "clean"))

#removal of index X__1
clean <- clean[,-1]
names(clean)
###########Creating lags and leads###############
#From: https://stackoverflow.com/questions/49437319/lag-lead-entire-dataframe-in-r

#Lag 1
lag1 <- rbind(NA, clean[-nrow(clean), ])
#I am not removing the first observation because I need to merge the datasets later on(and I can not have NAs later). Before merging them we need to create a new index
#lag1 <- lag1[-1,]
#Creating lag1 variable names
#Check the lag variable merge and redo everything
names_lag1 <- names(clean)
names_lag1 <- paste("lag1", names_lag1, sep="_")
names_lag1
names(lag1) <- names_lag1
rm(names_lag1)
lag1 <- lag1[,c(-1,-2)]
lag1$Index <- seq(1:124)
#Lag 2
lag2 <- rbind(NA, lag1[-nrow(lag1), ])
names_lag2 <- names(clean)
names_lag2 <- paste("lag2", names_lag2, sep="_")
names_lag2 <- names_lag2[-1]
names_lag2 <- names_lag2[-1]
lag2 <- lag2[,-55]
names(lag2) <- names_lag2
rm(names_lag2)
lag2$Index <- seq(1:124)
#Lag3
lag3 <- rbind(NA, lag2[-nrow(lag2), ])
names_lag3 <- names(clean)
names_lag3 <- paste("lag3", names_lag3, sep="_")
names_lag3 <- names_lag3[-1]
names_lag3 <- names_lag3[-1]
lag3 <- lag3[,-55]
names(lag3) <- names_lag3
rm(names_lag3)
lag3$Index <- seq(1:124)
#Lag4
lag4 <- rbind(NA, lag3[-nrow(lag3), ])
names_lag4 <- names(clean)
names_lag4 <- paste("lag4", names_lag4, sep="_")
names_lag4 <- names_lag4[-1]
names_lag4 <- names_lag4[-1]
lag4 <- lag4[,-55]
names(lag4) <- names_lag4
rm(names_lag4)
lag4$Index <- seq(1:124)
#Lag 5
lag5<- rbind(NA, lag4[-nrow(lag4), ])
names_lag5 <- names(clean)
names_lag5 <- paste("lag5", names_lag5, sep="_")
names_lag5 <- names_lag5[-1]
names_lag5 <- names_lag5[-1]
lag5 <- lag5[,-55]
names(lag5) <- names_lag5
rm(names_lag5)
lag5$Index <- seq(1:124)
#Lag 6
lag6<- rbind(NA, lag5[-nrow(lag5), ])
names_lag6 <- names(clean)
names_lag6 <- paste("lag6", names_lag6, sep="_")
names_lag6 <- names_lag6[-1]
names_lag6 <- names_lag6[-1]
lag6 <- lag6[,-55]
names(lag6) <- names_lag6
rm(names_lag6)
lag6$Index <- seq(1:124)
#Lag 7
lag7<- rbind(NA, lag6[-nrow(lag6), ])
names_lag7 <- names(clean)
names_lag7 <- paste("lag7", names_lag7, sep="_")
names_lag7 <- names_lag7[-1]
names_lag7 <- names_lag7[-1]
lag7 <- lag7[,-55]
names(lag7) <- names_lag7
rm(names_lag7)
lag7$Index <- seq(1:124)
#Lag 8
lag8<- rbind(NA, lag7[-nrow(lag7), ])
names_lag8 <- names(clean)
names_lag8 <- paste("lag8", names_lag8, sep="_")
names_lag8 <- names_lag8[-1]
names_lag8 <- names_lag8[-1]
lag8 <- lag8[,-49]
names(lag8) <- names_lag8
rm(names_lag8)
lag8$Index <- seq(1:124)
#Lead1
#check leads and lags
library(data.table)
lead1 <- shift(clean, n=1, type="lead")
lead1 <- as.data.frame(lead1)
names_lead1 <- names(clean)
names_lead1 <- paste("lead1", names_lead1, sep="_")
names(lead1) <- names_lead1
rm(names_lead1)
lead1 <- lead1[,c(-1,-2)]
lead1$Index <- seq(1:124)
#Lead2
lead2 <- shift(lead1, n=1, type="lead")
lead2 <- as.data.frame(lead2)
lead2 <- lead2[,-55]
names_lead2 <- names(clean[,-c(1,2)])
names_lead2 <- paste("lead2", names_lead2, sep="_")
names(lead2) <- names_lead2
rm(names_lead2)
lead2$Index <- seq(1:124)
#Lead3
lead3 <- shift(lead2, n=1, type="lead")
lead3 <- as.data.frame(lead3)
lead3 <- lead3[,-55]
names_lead3 <- names(clean[,-c(1,2)])
names_lead3 <- paste("lead3", names_lead3, sep="_")
names(lead3) <- names_lead3
rm(names_lead3)
lead3$Index <- seq(1:124)
#Lead4
lead4 <- shift(lead3, n=1, type="lead")
lead4 <- as.data.frame(lead4)
lead4 <- lead4[,-55]
names_lead4 <- names(clean[,-c(1,2)])
names_lead4 <- paste("lead4", names_lead4, sep="_")
names(lead4) <- names_lead4
rm(names_lead4)
lead4$Index <- seq(1:124)
#Lead5
lead5 <- shift(lead4, n=1, type="lead")
lead5 <- as.data.frame(lead5)
lead5 <- lead5[,-55]
names_lead5 <- names(clean[,-c(1,2)])
names_lead5 <- paste("lead5", names_lead5, sep="_")
names(lead5) <- names_lead5
rm(names_lead5)
lead5$Index <- seq(1:124)
#Lead6
lead6 <- shift(lead5, n=1, type="lead")
lead6 <- as.data.frame(lead6)
lead6 <- lead6[,-55]
names_lead6 <- names(clean[,-c(1,2)])
names_lead6 <- paste("lead6", names_lead6, sep="_")
names(lead6) <- names_lead6
rm(names_lead6)
lead6$Index <- seq(1:124)
#Lead7
lead7 <- shift(lead6, n=1, type="lead")
lead7 <- as.data.frame(lead7)
lead7 <- lead7[,-55]
names_lead7 <- names(clean[,-c(1,2)])
names_lead7 <- paste("lead7", names_lead7, sep="_")
names(lead7) <- names_lead7
rm(names_lead7)
lead7$Index <- seq(1:124)
#Lead8
lead8 <- shift(lead7, n=1, type="lead")
lead8 <- as.data.frame(lead8)
lead8 <- lead8[,-55]
names_lead8 <- names(clean[,-c(1,2)])
names_lead8 <- paste("lead7", names_lead8, sep="_")
names(lead8) <- names_lead8
rm(names_lead8)
lead8$Index <- seq(1:124)
clean$Index <- seq(1:124)


#Merging the datasets
totalafter <- Reduce(function(x,y) merge(x,y,by="Index" ),list(clean, lag1, lag2, lag3, lag4, lag5,lag6,lag7,lag8, lead1, lead2, lead3, lead4, lead5, lead6, lead7, lead8 ))

cleantotal <- total

AVPrice <-rowMeans(subset(cleantotal, select = c(NIVEARPrice,REXONARPrice, VOGUERPrice,DOVERPrice)))  
#Dove
cleantotal$Dove_Above <- cleantotal$DOVERPrice - AVPrice      
cleantotal$Dove_Above <- ifelse(cleantotal$Dove_Above>0,1,0)
#nivea
cleantotal$Nivea_Above <- cleantotal$NIVEAPrice - AVPrice      
cleantotal$Nivea_Above <- ifelse(cleantotal$Nivea_Above>0,1,0)
#rexona
cleantotal$Rexona_Above <- cleantotal$REXONARPrice - AVPrice      
cleantotal$Rexona_Above <- ifelse(cleantotal$Rexona_Above>0,1,0)
#vogue
cleantotal$Vogue_Above <- cleantotal$VOGUERPrice - AVPrice      
cleantotal$Vogue_Above <- ifelse(cleantotal$Vogue_Above>0,1,0)
#8*4
cleantotal$Above8 <- cleantotal$`8X4RPrice`-AVPrice
cleantotal$Above8 <- ifelse(cleantotal$Above8>0,1,0)
#Sanex
cleantotal$Sanex_Above <- cleantotal$SANEXRPrice-AVPrice
cleantotal$Sanex_Above <- ifelse(cleantotal$Sanex_Above>0,1,0)
#FA
cleantotal$Fa_Above <- cleantotal$FAPrice-AVPrice
cleantotal$Fa_Above <- ifelse(cleantotal$Fa_Above>0,1,0)
#Axe above
cleantotal$Axe_Above <- cleantotal$AXEPrice-AVPrice
cleantotal$Axe_Above <- ifelse(cleantotal$Axe_Above>0,1,0)

rm(lag1, lag2, lag3, lag4,lag5,lag6,lag7, lag8, lead1, lead2, lead3, lead4, lead5, lead6, lead7, lead8)
#Saving clean total
library(xlsx)
write.csv(cleantotal, "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\lagtotal.csv")
write.xlsx(cleantotal, "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\lagtotal.xlsx")
rm(clean)
rm(total)
rm(AVPrice)
#######################Creating time slices for model test and train####
slices # 88 test and train sets
# [...]
slices <- createTimeSlices(y = dat, initialWindow = 12, horizon = 12,                    skip = 11, fixedWindow = T)
slices 


library(caret)
library(ggplot2)
data(economics)
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = TRUE)

#axe, fa, sanex, vouge, dove, nivea, 8*4, rexona
cleantotal <- na.omit(cleantotal)
lm <- lm(data=cleantotal, NIVEASales~NIVEAPrice*Nivea_Above+NIVEAPriceDummy+SANEXPrice*Sanex_Above+VOutMarket +SANEXSales+`8X4D+F.x`+PriceWar)
summary(lm)
na.omit(cleantotal)
total.after <- na.omit(totalafter)
write.csv(total.after, "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\totalafter.csv")
#I think Ineed to combine total.after and lagtotal
first <- read.csv("totalafter.csv")
second <- lagtotal

fdata <- merge(first, second, by = "Index")
fdata$WEEK.x <- as.Date(fdata$WEEK.x)

################### Define Adstock Rate############
adstock_rate <- 0.9
max_memory   <- 5

# Create Data
advertising <- fdata$NIVEAD.F.x

# Calculate Advertising Adstock
learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), advertising), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]

# Graph Data
plot(seq(1,length(advertising)), advertising, type="h", 
     xlab="Time", ylab="Nivea D+F", 
     ylim=c(0, max(c(advertising, adstocked_advertising))), 
     frame.plot=FALSE)
lines(adstocked_advertising)
fdata$NiveaADS <-  adstocked_advertising
######################






      

#fdataN <- fdata[,c(8,display=31, 47=niveadf, 39=niveafeat)]
#fdataN <- fdata[,c(8,31)]
#names(fdata)
##this is the static model we will use to compare with the dynamic one
final <- lm(data=splitData$train$data, log(NIVEASales.y)~log(NIVEAPrice.y)*log(Nivea_Above+1)+log(DOVEPrice.y)*log(PriceWar+1)+WEEK.x +log(lag1_DOVESales.y )*log(lag1_PriceWar+1)+log(lag1_NIVEASales.y)+log(lead2_NIVEAD.F+1)+log(fdata$NiveaADS+1))
summary(final)
#

#TEST
splitData <- resample_partition(fdata, c(test=0.3, train=0.7))
predictedlog <- predict(final, splitData$test$data)
predictedlog <- exp(predictedlog)
c <- splitData$train$data$NIVEASales.x
errorlog <- c- predictedlog
rmse(errorlog)
mae(errorlog)




#          +log(total.after$lead1_NIVEAFEAT+1)+log(total.after$lead2_NIVEAFEAT+1)+log(total.after$lead3_NIVEAFEAT+1)+log(total.after$lead4_NIVEAFEAT+1)+log(total.after$lead5_NIVEAFEAT+1)+log(total.after$lead6_NIVEAFEAT+1))
summary(final)
#############Model Robustness##############
install.packages("MLmetrics")
MSE()
train <- sample(1:nrow(clean), 90)
test <- (1:nrow(clean))[-train]
plm <- predict(lm, test)
lm
log
library(modelr)
totaltotal <- merge(total.after, cleantotal, by="Index")



splitData <- resample_partition(fdata, c(test=0.3, train=0.7))
predictedlog <- predict(final, splitData$test)
predictedlog <- exp(predictedlog)
c <- splitData$test$data$NIVEASales.x
errorlog <- c- predictedlog
 rmse(errorlog)
 mae(errorlog)
#

mae(model=exp(logmod), data=splitData$test)
mae(model=lmmod, data=splitData$test)
predictedlog <- predict(final, splitData$test)


#Test with new method
smp_size <- floor(0.75*nrow(totaltotal))
set.seed(123)
train_ind <- sample(seq_len(nrow(totaltotal)), size=smp_size)

train <- totaltotal[train_ind]
test <- totaltotal[-train_ind]
#


predictedlog <- exp(predictedlog)
predictedlm <- predict(lmn, splitData$test)
c <- splitData$test$data$NIVEASales.x
errorlog <- c- predictedlog

errorlm <- splitData$test$data-predictedlm
rmse(errorlog)
rmse(errorlm$NIVEASales)
mae(errorlog)
mae(errorlm$NIVEASales)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
      sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
      mean(abs(error))
}
true <- splitData$test
true$data
data <- as.data.frame(true$data) 






#When viewing the lag effects of nivea display advertising we do not notice any significance. 
library(stargazer)

stargazer(log, type="text")
summary(lmn)
out <- capture.output(lmn)
cat("LM Output", out, file="LMOutput.jpeg", sep="n", append=TRUE)

#+log(cleantotal$`lead1_NIVEAD+F.x`+1)+log(cleantotal$`lead2_NIVEAD+F.x`+1)
summary(log)
clean <- subset(DataDeodorant, Chain=="EDAH")

#I am making a new table for lagging display variables
#Usin the code from above

#Lags for nivea price
#Dove D+F advertising does not work with lags
#When observing the data on NiveaFeature and its leads wee notice no statistical significance. When we tried laggin the effect we also notice no statistical significance.
log(NIVEAPrice)*log(Nivea_Above+1) + log(cleantotal$Lag1_Nivea_Above)*log(Lag1_NiveaPrice)
#I need data on price drops. 






##########GRANGER############
library(lmtest)
#Testing on nivea price and dove price
price_average_total_sales$Chain <- as.factor(price_average_total_sales$Chain) 
albert <- subset(price_average_total_sales, DataDeodorant$Chain=="ALBERT HEIJN")
edah <- subset(price_average_total_sales, DataDeodorant$Chain=="EDAH")
super <- subset(price_average_total_sales, DataDeodorant$Chain=="SUPER DE BOER")
c1000 <- subset(price_average_total_sales, DataDeodorant$Chain=="C-1000")
jumbo <- subset(price_average_total_sales, DataDeodorant$Chain=="JUMBO")

price_average_total_sales$WEEK <- seq(1:124)

#CREATE A GRAPH FOR EACH POSSIBILTIY
#SO FAR DOVE AND SANEX INFLUENCED AXE
agris <- diff(log(albert$`PRICE AVERAGE`))
library(ggplot2)
ggplot(price_average_total_sales, aes(WEEK,`SALES TOTAL`, col=Chain))+
      geom_point()+
      stat_smooth()
ggplot(price_average_total_sales, aes(WEEK,`PRICE AVERAGE`, col=Chain))+
      geom_point()+
      stat_smooth()



agrif <- diff(log(jumbo$`PRICE AVERAGE`))
#Jumbo influences albert(0.006)
#Edah has no influence on any retailers
#Super influences jumbo(0.004)
#C1000 has no influence on albert, c1000 influences Jumbo,Edah, and super
#albert does not influence super, edah, C1000, Albert influences jumbo

#Null: agris does not cause agrif
#Agris would be the dependent variable
#Agrif would be the independent variable
grangertest(agrif~agris, order=3)
#Interpretation
#Model one tests agrif on its own lags and the lags of agris
#Model two tests only the lags of agrif
#We basically test which model is better, the one with agris or the one without
#When the p-test is significant we reject the null hypothis
#Do not forget to test the opposite as the direction can be bidirectional meaning that they cause each other. 
#USE AIC TO SELECT NUMBER OF LAGS
#to get the number of lags
#url <- "https://stats.stackexchange.com/questions/160671/estimate-lag-for-granger-causality-test"
#Advertising total comparison
adv <- fdata[,c(30:53)]
names(adv)
library(dplyr)
#First i turn them all into zeroes and ones for when they use advertising
#
advdummy <- with(adv, adv>0)
advdummy <- ifelse(advdummy==FALSE,0,1)
totalTimeAdvertised <- colSums(advdummy)
totalTimeAdvertised <- as.data.frame(totalTimeAdvertised)
write.table(totalTimeAdvertised, "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\totaltimeadvertised.csv")
nrow(subset(advdummy, advdummy$NIVEADISP & advdummy$DOVEDISP))
m <- advdummy
m <- as.matrix(m)
m[m>=1] <- 1
m <- m %*% t(m) 
m[5:10,5:10] 

m <- as.data.frame(m)
m
write.csv(m, "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\sth.csv")
