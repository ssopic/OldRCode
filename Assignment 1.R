######Presetting####################################################
setwd("C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models")
#Libraries
library(psych) #for correlation matrices and vizualization
library(readxl) #reading xls
library(Hmisc) #For imputations
library(reshape2) #for melting
library(tsoutliers) #for time series outliers
library(lubridate) #Date functions
library(RCurl) #I don't remember
library(caret) #train/test
#for some plots we use the code from http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#Getting the data
FullData <- read_xls("C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\DataDeodorant.xls")
##You need to change this to your folder. do not forget the double \
#Subset Edah and removal of full dataset
Edah <- subset(FullData, Chain=="EDAH")
rm(FullData)
#Summation statistics
summary(Edah) #EDAH is ours
#this part removes the name of the store
names(Edah)
Edah <- Edah[,-1]
#Check for outliers
#Outlier detection 

############Turning week into a data variable##################################
#Getting the year(the year is imaginary)
YearVector <- ifelse(grepl("W03", Edah$WEEK)==TRUE,2011,ifelse(grepl("W04", Edah$WEEK)==TRUE,2012,ifelse(grepl("W05", Edah$WEEK)==TRUE,2013,2014)))

#Getting the week
Weeks <- substr(Edah$WEEK, 5, nchar(Edah$WEEK))

#Merging them together
#url="http://r.789695.n4.nabble.com/Convert-201248-to-Year-Week-date-format-in-R-td4654068.html"
dat1 <- read.table(text=paste0(YearVector, Weeks), sep="", header = FALSE)
#Getting it into date format
YearWeekDate<- parse_date_time(paste(dat1$V1,1), "%Y%U %u")
Edah$WEEK <- YearWeekDate
Edah$WEEK <- floor_date(Edah$WEEK, "week")
summary(Edah)
which(is.na(Edah$WEEK))
#When I use the years and the info provided in the dataset one week fails to parse... When I use the year which is considered a pass year(1 extra week, everything is fine). For this reason I decided to use this years in my analysis.
#Week is now in date format!
#Removing variables used in the procedure and leaving Edah only
rm(dat1)
rm(Weeks)
rm(YearVector)
rm(YearWeekDate)

###############Outliers definition###################################
#For a given continuous variable, outliers are those those observations that lie outside 1.5*IQR. Where IQR(the Interquantile range) is the difference between the 75th and 25th quartiles. The code below allows us to find them and show them in a boxplot.
#url="http://r-statistics.co/Outlier-Treatment-With-R.html"
#
###############################Temperature and rain data######
##############http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical###
#Getting the data
url <- "http://sdwebx.worldbank.org/climateportal/DownloadData/pr_1991_2015.xls"
download.file(url, destfile = "./file.xlsx")
rain <- read_xls("Rainfall.xls")
rain$WEEK <- paste(rain$Year, rain$Month,sep="")
rain$WEEK <- parse_date_time(rain$WEEK, "%Y%m")
#longway
Edah$Month <- month(Edah$WEEK)
Edah$Year <- year(Edah$WEEK)
rain$Month <- month(rain$WEEK)
rain$Year <- year(rain$WEEK)
Edah$MY <- paste0(Edah$Month, Edah$Year)
rain$MY <- paste0(rain$Month, rain$Year)
Edah <- merge(Edah, rain, by="MY", all.x = TRUE)
#do not forget to change the collumn names
rm(rain)
rm(url)
Edah <- Edah[,-c(1,51,52,54:59)] #variables from percipation
names(Edah)[1] <- paste("WEEK")
names(Edah)[50] <- paste("Rain")
#Rain Data
temp <- read_xls("Temperature.xls")
temp$WEEK <- paste(temp$Year, temp$Month,sep="")
temp$WEEK <- parse_date_time(temp$WEEK, "%Y%m")
#longway
Edah$Month <- month(Edah$WEEK)
Edah$Year <- year(Edah$WEEK)
temp$Month <- month(temp$WEEK)
temp$Year <- year(temp$WEEK)
Edah$MY <- paste0(Edah$Month, Edah$Year)
temp$MY <- paste0(temp$Month, temp$Year)
Edah <- merge(Edah, temp, by="MY", all.x = TRUE)
#Cleaning the data
rm(temp)
Edah <- Edah[,-c(1,52,53,55:60)]
names(Edah)[1] <- paste("WEEK")
names(Edah)[51] <- paste("Temperature")

#Temperature plot with Nivea Sales
plot <- ggplot(Edah, aes(WEEK, Temperature))+
      geom_line()
plot+geom_line(data=Edah, aes(x=WEEK, y=Edah$NIVEASales, color="Nivea Sales" ))  + 
      scale_color_manual(labels=c("Brand Sales", "Temperature"), values=c("red", "darkblue"))

#Rain plot with Nivea Sales
Edah$totalSales <- rowSums(Edah[2:9])
plot <- ggplot(Edah, aes(WEEK, Temperature*10))+
      geom_line()
plot+geom_line(data=Edah, aes(x=WEEK, y=totalSales, color="Nivea Sales" ))  + 
      scale_color_manual(labels=c("Brand Sales", "Temperature"), values=c("red", "darkblue"))

###############BOXPLOTS###########
#Show all of the boxplots at the same time
SalesOutlier <- Edah[,c(1,2,3,4,5,6,7,8,9)]
names(SalesOutlier) <- c("WEEK", "DOVE", "FA", "NIVEA", "REXONA", "SANEX", "VOGUE", "8x4", "AXE")
SalesOutlier <- melt(SalesOutlier, id.vars = "WEEK")
SalesOutlierPlot<- ggplot(SalesOutlier, aes(x=WEEK, y=value, col=variable))+ 
      geom_boxplot(outlier.shape = 25, outlier.size = 5, alpha=0.5)+
      labs(xlab("Edah Brands")) +
      labs(title="Sales outliers of the Edah brands")+
      labs(ylab("Sales"))+
      theme(axis.ticks.x = element_blank())+
      theme(axis.text.x = element_blank())+
      labs(color="Brands\n")
SalesOutlierPlot
FeaturePlot <- Edah[,c(1,34:41)]
FeaturePlot <- melt(FeaturePlot, id.vars="WEEK")
ggplot(FeaturePlot, aes(x=WEEK, y=value, col=variable))+ 
      geom_boxplot(outlier.shape = 25, outlier.size = 5, alpha=0.5)+
      labs(xlab("Edah Brands")) +
      labs(title="Feature advertising")+
      labs(ylab("Feature"))+
      theme(axis.ticks.x = element_blank())+
      theme(axis.text.x = element_blank())+
      labs(color="Brands\n")




#If I divide the dataset into two, could I get a better form? I mean less outliers
PriceOutlier <- Edah[,c(1,10,11,12,13,14,15,16,17)]
names(PriceOutlier) <- c("WEEK", "DOVE", "FA", "NIVEA", "REXONA", "SANEX", "VOGUE", "8x4", "AXE")
PriceOutlier <- melt(PriceOutlier, id.vars="WEEK")
PriceOutlierPlot <- ggplot(PriceOutlier, aes(x=WEEK, y=value, col=variable)) +
      geom_boxplot(outlier.shape = 25, outlier.size = 5, alpha=0.5)+
      labs(xlab("Edah Brands")) +
      labs(title="Price outliers of the Edah brands")+
      labs(ylab("Price"))+
      theme(axis.ticks.x = element_blank())+
      theme(axis.text.x = element_blank())+
      labs(color="Brands\n")
PriceOutlierPlot
jpeg('PriceOutlierPlot.jpg')
ggplot(PriceOutlier, aes(x=WEEK, y=value, col=variable)) +
      geom_boxplot(outlier.shape = 25, outlier.size = 5, alpha=0.5)+
      labs(xlab("Edah Brands")) +
      labs(title="Price outliers of the Edah brands")+
      labs(ylab("Price"))+
      theme(axis.ticks.x = element_blank())+
      theme(axis.text.x = element_blank())+
      labs(color="Brands\n")
dev.off()
PriceOutlierPlot
ggplot(PriceOutlier, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()
####OUTLIER REMOVAL###############
#We define a dummy vector for outliers. this is done using the code below. Out of all possible outliers we define as definitive issues with the data being the zeroes in vogue. This is the only data we impute after creating a dummy vector for outliers. Other changes will contain dummies for outliers themselves(which will be part of the model).  

#####Outlier management code##########
#Imputing with a specific number
library(Hmisc)
#DOVE prices
#Here I need to define the outlier number for the location of the vertical line below


#Dummies for outliers
#Our variables are Rexona, Dove, Vogue, Nivea 
#Index Creation
Edah$Index <- NA
Edah$Index <- 1:124

DovePriceOutlier <- Edah$DOVEPrice[Edah$DOVEPrice %in%  boxplot.stats(Edah$DOVEPrice)$out]
obs <- match(DovePriceOutlier,Edah$DOVEPrice)
Edah$DOVEPriceDummy <- ifelse(match(Edah$Index, obs),1,0)
Edah$DOVEPriceDummy[is.na(Edah$DOVEPriceDummy)] <- 0
#dove price shows no outliers so we drop the variable

RexonaPriceOutlier <- Edah$REXONAPrice[Edah$REXONAPrice %in% boxplot.stats(Edah$REXONAPrice)$out]
#Rexona prices do not show outliers
NiveaPriceOutlier <- Edah$NIVEAPrice[Edah$NIVEAPrice %in% boxplot.stats(Edah$NIVEAPrice)$out]
obs <- match(NiveaPriceOutlier,Edah$NIVEAPrice)
Edah$NIVEAPriceDummy <- ifelse(match(Edah$Index, obs),1,0)
Edah$NIVEAPriceDummy[is.na(Edah$NIVEAPriceDummy)] <- 0
#Nivea has two outliers
#Cleaning
rm(NiveaPriceOutlier)
rm(obs)
rm(DovePriceOutlier)
rm(RexonaPriceOutlier)
#Vogue has 26 outliers
VoguePriceOutlier <- Edah$VOGUEPrice[Edah$VOGUEPrice %in% boxplot.stats(Edah$VOGUEPrice)$out]
obs <- match(VoguePriceOutlier,Edah$VOGUEPrice)
Edah$VOGUEPriceDummy <- ifelse(match(Edah$Index, obs),1,0)
Edah$VOGUEPriceDummy[is.na(Edah$VOGUEPriceDummy)] <- 0
rm(VoguePriceOutlier)





#Graph removal
rm(PriceOutlierPlot)
rm(SalesOutlier)
rm(SalesOutlierPlot)
rm(DovePriceOutlier)
rm(PriceOutlier)
rm(FeaturePlot)
rm(obs)
#Vogue outlier
#Getting total sales to find out what happens to Vogue in the zeroes
#aggregate sales per week
TotalSales <- Edah[,2:9]
TotalSales <- rowSums(TotalSales)
TotalSales <- as.data.frame(TotalSales)
TotalSales$Week <- NA
TotalSales$Week <- Edah$WEEK
rm(TotalSales)

VoguePriceOutlier <- Edah$VOGUEPrice[Edah$VOGUEPrice %in%  boxplot.stats(Edah$VOGUEPrice)$out]
obs <- match(VoguePriceOutlier,Edah$VOGUEPrice)
Edah$VoguePriceDummy <- ifelse(match(Edah$Index, obs),1,0)
Edah$VoguePriceDummy[is.na(Edah$VoguePriceDummy)] <- 0
Edah$VoguePriceDummy

Edah$VOutMarket <- ifelse(Edah$VOGUESales==0,1,0)
plot(Edah$WEEK, Edah$VOutMarket)
p_heat <- ggplot(Edah,
                 aes(x=Edah$WEEK, y=VOutMarket)) +  
      geom_tile()
#Edah$VOutMarket2 <- ifelse(Edah$VOGUEPrice==0,1,0)
#Here I just need to use one; Later I find out that they are the same
fasf <- ifelse(Edah$VOutMarket==Edah$VOutMarket2, TRUE, FALSE)
box <- boxplot(TotalSales$TotalSales)
box$out
#The outlier bumber in this graph is
box$out
#For vogue, as we got a lot of zeroes we define a vector for zero dummies
#Therefore we need to impute them as they are clearly extreme values. Why they're extreme is because they go against basic economic theory that a company would be giving products for free and not selling any. In this situation we would just like to impute it with the median or some kind of trend variable(dummy or a simple smoothing model?) Can I take the model out oof the graph?
#Imputing with the median for now
#Edah$VOGUESales[Edah$VOGUESales==0] <- NA
#Edah$VOGUESales <- impute(Edah$VOGUESales, median)
#plot(Edah$WEEK,Edah$VOGUESales)
#For imputations
#Edah$DOVESales <- impute(Edah$DOVESales, mean)
rm(obs)
rm(VoguePriceOutlier)
rm(fasf)

#############################################
#########prices or dips in prices + compare with average price of the competitors######
######This needs to be done after adding the temp and rain variables
#For the creation of the average price we use the Rprice as it is clean of promotional effects
#R reports warnings but I think it is connected to rowmean of vector names so no harm here
#We use Rprice here as it should be free of promotional effects so the dummy itself would not be able to cause multicollinearity as we do not use the Rprice in the model itself
#For futher variables we define a dummy showing if the price is below or above the average price in the same market. It functions like this: Dove_Above is the dummy for when Doves price is above the average price. We presume that this variable would be negative as the consumers would want to buy the product when it costs less than the average market price.
AVPrice <-rowMeans(subset(Edah, select = c(NIVEARPrice,REXONARPrice, VOGUERPrice,DOVERPrice)))  
#Dove
Edah$Dove_Above <- Edah$DOVERPrice - AVPrice      
Edah$Dove_Above <- ifelse(Edah$Dove_Above>0,1,0)
#nivea
Edah$Nivea_Above <- Edah$NIVEAPrice - AVPrice      
Edah$Nivea_Above <- ifelse(Edah$Nivea_Above>0,1,0)
#rexona
Edah$Rexona_Above <- Edah$REXONARPrice - AVPrice      
Edah$Rexona_Above <- ifelse(Edah$Rexona_Above>0,1,0)
#vogue
Edah$Vogue_Above <- Edah$VOGUERPrice - AVPrice      
Edah$Vogue_Above <- ifelse(Edah$Vogue_Above>0,1,0)
#8*4
Edah$Above8 <- Edah$`8X4RPrice`-AVPrice
Edah$Above8 <- ifelse(Edah$Above8>0,1,0)
#Sanex
Edah$Sanex_Above <- Edah$SANEXRPrice-AVPrice
Edah$Sanex_Above <- ifelse(Edah$Sanex_Above>0,1,0)
#FA
Edah$Fa_Above <- Edah$FAPrice-AVPrice
Edah$Fa_Above <- ifelse(Edah$Fa_Above>0,1,0)
#Axe above
Edah$Axe_Above <- Edah$AXEPrice-AVPrice
Edah$Axe_Above <- ifelse(Edah$Axe_Above>0,1,0)

#PriceWar
#Started around observation 25
Edah$PriceWar <- NA
Edah$PriceWar <- ifelse(Edah$Index>40,1,0)
Edah$PriceWarAftermath <- ifelse(Edah$Index>70,1,0)

#Finding the smallest observation as it would be negative and result in issues with the log transformation
Test[which.min(Test)]
#We need to increase this variable by some value or just turn it into a dummy(or datamine to find which item fits better, or create an entirely new variable which is defined as one being 1 when there is a price dip and 0 otherwise so we can use both)
rm(AVPrice)
rm(p_heat)
#Also: for some reason I have two same variables in the code
names(Edah)
identical(Edah$VoguePriceDummy,Edah$VOGUEPriceDummy)
Edah <- Edah[,-57]
names(Edah)
#One removed as it was identical
#######Promotion outliers and visualization##########
#normalize data
library(reshape2)
library(ggplot2)
library(tidyr)

DisplayArea <- melt(Edah[,c(1,26,27,28,29,30,31,32,33)],id.vars="WEEK")
ggplot(DisplayArea, aes(WEEK, value, col=variable)) +
      geom_area()
mean(Edah$AXEDISP)
mean(Edah$NIVEADISP)
which.max(Edah$NIVEADISP)
Edah$NIVEADISP[94]

#Graph shows when different brands are advertising using display
ggplotDispAll <- melt(Edah[,c(1,26:33)], id.vars="WEEK")
ggplotDispAll$YN <- ifelse(ggplotDispAll$value>0,1,0)
ggplot(ggplotDispAll, aes(WEEK, YN), ifelse(YN==0, color="red", color="blue"))+
      geom_point()+
      facet_grid(variable~.)

      
#Graph shows when different brands are advertising using Feat
ggplotFeatAll <- melt(Edah[,c(1,34:41)], id.vars="WEEK")
ggplotFeatAll$YN <- ifelse(ggplotFeatAll$value>0,1,0)
ggplot(ggplotFeatAll, aes(WEEK, YN), color="blue")+
      geom_point()+
      facet_grid(variable~.)



dev.off()
Featured <- melt(Edah[,c(1,34:41)], id.vars="WEEK")
ggplot(Featured, aes(WEEK, value, col=variable)) +
      geom_area()

#The total space a store can provide for the use of either feature or display area advertising can not be above 1. Therefore. We normalize the data to get a new collumn. 
Display <- as.data.frame(Edah[,c(1,26,27,28,29,30,31,32,33)])
datDisp <- aggregate(DisplayArea$value, by=list(Category=DisplayArea$WEEK), FUN=sum)
Edah$TotalDispay <- datDisp$x
plot(datDisp)
#Code for normalization taken from 
#https://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
#normalized = (x-min(x))/(max(x)-min(x))
test <- (Edah$DOVEDISP-min(Edah$DOVEDISP))/(max(Edah$DOVEDISP)-min(Edah$DOVEDISP))
par(mfrow=c(1,2))
hist(Edah$DOVEDISP,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(test, breaks=10, xlab="Normalized Data", col="lightblue", main="")

#Normalization failed because I needed to use the long slim format which would look at all of them at the same time
#Moving to the long format for normalization
Display <- melt(Edah[,c(1,26:33)], id.vars="WEEK")
#Normalizing the data
Display$DispNormal <- (Display$value-min(Display$value))/(max(Display$value)-min(Display$value))


#Graph for checking it out
par(mfrow=c(1,2))
hist(Edah$DOVEDISP,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(Display$DispNormal, breaks=10, xlab="Normalized Data", col="lightblue", main="")

#LOOKING AT TOTAL: NOW EVERYTHING SHOULD BE BELOW 1
#Everything is below 1. Now I need to transfer it back to the full table
#Data for display has been normalized
ggplot(Display, aes(variable, Display$DispNormal)) +
      geom_boxplot()

#For feature
Feature <- melt(Edah[,c(1,34:41)], id.vars="WEEK")
Feature$FeatNormal <- (Feature$value-min(Feature$value))/(max(Feature$value)-min(Feature$value))

ggplot(Feature, aes(Feature$variable, Feature$FeatNormal)) +
      geom_boxplot()


#
plot(Display$WEEK, Display$DispNormal)
Display <- Display[,-3]
library(tidyr)
DisplayWide <- spread(Display, variable, DispNormal, drop = TRUE)
plot(DispNormal)
#Feat normalization
#Moving to the long format for normalization
Feat <- melt(Edah[,c(1,34:41)], id.vars="WEEK")
#Normalizing the data
Feat$FeatNormal <- (Feat$value-min(Feat$value))/(max(Feat$value)-min(Feat$value))
Feat <- Feat[,-3]
library(tidyr)
FeatWide <- spread(Feat, variable, FeatNormal, drop = TRUE)
#Feat Wide now contains normalized data

#D+F normalization
#Moving to the long format for normalization
DplusF <- melt(Edah[,c(1,42:49)], id.vars="WEEK")
#Normalizing the data
DplusF$DplusFNormal <- (DplusF$value-min(DplusF$value))/(max(DplusF$value)-min(DplusF$value))
DplusF <- DplusF[,-3]
library(tidyr)
DplusFWide <- spread(DplusF, variable, DplusFNormal, drop = TRUE)
#DplusF Wide now contains normalized data
#Now I need to merge the collumns and then decide on which data I would like to keep
ggplot(Display, aes(WEEK, value, col=variable)) +
      geom_area()


#######Normalization divided############
##Normalization for dove
#Edah$DOVEDISP <- (Edah$DOVEDISP-min(Edah$DOVEDISP))/(max(Edah$DOVEDISP)-min(Edah$DOVEDISP))
#Edah$DOVEFEAT <- (Edah$DOVEFEAT-min(Edah$DOVEFEAT))/(max(Edah$DOVEFEAT)-min(Edah$DOVEFEAT))
#Edah$`DOVED+F` <- (Edah$`DOVED+F`-min(Edah$`DOVED+F`))/(max(Edah$`DOVED+F`)-min(Edah$`DOVED+F`))
#Normalization for Nivea
#Edah$NIVEADISP <- (Edah$NIVEADISP-min(Edah$NIVEADISP))/(max(Edah$NIVEADISP)-min(Edah$NIVEADISP))
#Edah$NIVEAFEAT <- (Edah$NIVEAFEAT-min(Edah$NIVEAFEAT))/(max(Edah$NIVEAFEAT)-min(Edah$NIVEAFEAT))
#Edah$`NIVEAD+F` <- (Edah$`NIVEAD+F`-min(Edah$`NIVEAD+F`))/(max(Edah$`NIVEAD+F`)-min(Edah$`NIVEAD+F`))
#Normalization for vogue
#Edah$VOGUEDISP <- (Edah$VOGUEDISP-min(Edah$VOGUEDISP))/(max(Edah$VOGUEDISP)-min(Edah$VOGUEDISP))
#Edah$VOGUEFEAT <- (Edah$VOGUEFEAT-min(Edah$VOGUEFEAT))/(max(Edah$VOGUEFEAT)-min(Edah$VOGUEFEAT))
#Edah$`VOGUED+F` <- (Edah$`VOGUED+F`-min(Edah$`VOGUED+F`))/(max(Edah$`VOGUED+F`)-min(Edah$`VOGUED+F`))
#Normalization for Rexona
#Edah$REXONADISP <- (Edah$REXONADISP-min(Edah$REXONADISP))/(max(Edah$REXONADISP)-min(Edah$REXONADISP))
#Edah$REXONAFEAT <- (Edah$REXONAFEAT-min(Edah$REXONAFEAT))/(max(Edah$REXONAFEAT)-min(Edah$REXONAFEAT))
#Edah$`REXONAD+F` <- (Edah$`REXONAD+F`-min(Edah$`REXONAD+F`))/(max(Edah$`REXONAD+F`)-min(Edah$`REXONAD+F`))
###############END OF SINGLE NORMALIZATION##################

#This area plot shows what would happen when all of the collective advertising space would be summed up. 
#It seems I have an issue with the data here, it looks like the display function, which should be between 0 and 1 for store level is above 1 in some cases.


#From the boxplots, we can notice that the companies probably use ADPULS: they do not advertize everyday but use pulses. This also makes the usage of boxplots obsolete as everything close to  zero is an outlier(as they do not advertise always). We can either just use a proportion of all of them and accept the mistakes or remove them using the standard definition of an outlier + add the proportion later(we need all of the variables to sum to 1 on a specific week).


#To start we find which observations do not sum up to 1 using the aggregate function.
#Below find the data frame with 
Display <- as.data.frame(Edah[,c(1,26,27,28,29,30,31,32,33)])
datDisp <- aggregate(DisplayArea$value, by=list(Category=DisplayArea$WEEK), FUN=sum)
#The plot below shows 9 variables higher than one(which is bad as they should sum to 1)
par(mfrow=c(1,1))
plot(datDisp)
abline(h=1)


#Moving to the combination variable
#The variables of interest are the normalized versions. The first graph shows the total prior to normalization while the second shows our variables themselves.

Feat <- melt(Edah[,c(1,42,43,44,45,46,47,48,49)], id.vars = "WEEK")
ggplot(FeatDisp, aes(WEEK, value, col=variable)) + 
      geom_area()
Feat <- melt(Edah[,c(1,34:41)], id.vars = "WEEK")
ggplot(Feat, aes(WEEK, value, col=variable)) + 
      geom_area()


#Same thing, how do I remove the outliers??
#Check outliers through the boxplots. Maybe I can remove some error for values that are full(eg. over 1)
datFD <- aggregate(FeatDisp$value, by=list(Category=DisplayArea$WEEK), FUN=sum)
#The plot below shows 9 variables higher than one(which is bad as they should sum to 1)
plot(datFD)
abline(h=1)
#Now we get 10 outliers above 1






#Price plot for all      
#Ovaj plot sprintati sa manjim dimenzijama
Price <- Edah[,c(1,10,11,12,13,14,15,16,17)]
Price <- melt(Price, id.vars = "WEEK")
ggplot(Price, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()




#We still see the outliers in rexona which I do not know how to remove
#############################Total market size through time:#########
plot(y=Edah$WEEK, x=Edah$TotalSales)
#this also needs a dummy variable for outliers to ask managers
boxplot(Edah$TotalSales)

###################Price and sales through weeks#############
library(reshape2)#for melting function
Price <- Edah[,c(1,10,17,14)]
Price <- melt(Price, id.vars = "WEEK")
ggplot(Price, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()

#Saving in pdf
pdf("Price Week.pdf")
priceweek <-  ggplot(Price, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()

print(priceweek)
dev.off()

Sales <- Edah[,c(1,2,3,4,5,6,7,8,9)]
Sales <- melt(Sales, id.vars = "WEEK")
ggplot(Sales, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()

pdf("myplot2.pdf")
myplot <- ggplot(Sales, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()


print(myplot)
dev.off()



#Fancy graph
ggplot(data = Edah, mapping = aes(y=DOVEPrice)) +
      geom_boxplot()

Sales <- Edah[,c(1,2,3,4,5,6,7,8,9)]
Sales <- melt(Sales, id.vars = "WEEK")
ggplot(Sales, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()

###################Correlations####################################
CorrelationSales <- pairs.panels(Edah[,c(2,3,4,5,6,7,8,9)], scale=TRUE)
install.packages("corrr")
Sales <- Edah[,c(1,2,3,4,5,6,7,8,9)]
Sales <- melt(Sales, id.vars = "WEEK")
ggplot(Sales, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()


###############FROM THIS POINT ON WE CONSIDER THE DATA TO BE CLEAN AND NORMALIZED SO WE PRODUCE THE TOTAL FILE#################
Edah$Index
total <- merge(Edah, DisplayWide, by="WEEK")
DisplayWide$Index <- seq(1:124)
total <- merge(DisplayWide, Edah, by="Index")
FeatWide$Index <- seq(1:124)
total <- merge(FeatWide, total, by="Index")
DplusFWide$Index <- seq(1:124)
total <- merge(DplusFWide, total, by="Index") 
#Now we need to get rid of the duplicated variables
names(total)
#We keep the .x variables 
#Index:1
#WEEK.x but also needs to be converted to WEEK :2
#d+f variables with .x: 3:10
#Week.y is removed:11
#The standard feat variables are removed:12:19
#Feat variables with .x are kept: 21:28
#Week.y is removed:29
#Standard sales are kept with 30:37
#Standard prices and R prices are kept with 38:53
#Display variables are not kept: 54:62
#Display and d+f with .y is not kept 62:77
#Rain 78
#Temperature 79
#Dummies till the end
Total <- total[,-c(11:19,29,54:77)]
Total <- Total[,-11]
#Creating the names vector

install.packages("xlsx")
library(xlsx)
file <- "C:\\Users\\silvi\\OneDrive\\Desktop\\Market Response Models\\clean.xlsx"
write.xlsx(Total, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


#############################Need to decide on a brand#############
###############Need to test with lm model##############
#Viewing the prices we have decided to work with Dove, Nivea and Rexona because we are interested in the price war

Prices <- Edah[,c(1,10,12,13,15)]
Prices <- melt(Prices, id.vars = "WEEK")
ggplot(Prices, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()

Sales <- Edah[,c(1,2,4,5,7)]
Sales <- melt(Sales, id.vars = "WEEK")
ggplot(Sales, aes(WEEK,value, col=variable)) + 
      geom_point() + 
      stat_smooth()



###############Prepare tests for multicollinearity etc.############
###################divide the data into train and test######
set.seed(42)
train.index <- createDataPartition(Edah$DOVESales, p = 0.75, list = FALSE)
Edah$train <- Edah[train.index,]
Edah$test  <- Edah[-train.index,]

###################LINEAR MODEL############
      #My competitors are Dove, Vogue, Rexona + we think that Axe might have initiated the price decrease which might have initiated a price war. 
lm <- summary(lm(data=Edah, Edah$DOVESales~Edah$WEEK*Edah$Temperature+DOVEPriceDummy*Edah$DOVEPrice+Edah$NIVEAPrice*Nivea_Above+Edah$Rexona_Above*Edah$REXONAPrice +Edah$Temperature*Edah$DOVEPrice))#Dodavati varijable dokle god ima smisla i onda ih pokusati izvesti u predvidanje. To znaci podijeliti na test and train. Moram saznati kako to napraviti sa time series podatcima.Za kraj treba odraditi testove da li mi je model heteroskedastican itd. To treba prepisati iz notesa(White test etc.) I onda to ponoviti na log modelu.  I trebam nesto znati o elasticnostima kada zavrsim sa modelom. 
lm
