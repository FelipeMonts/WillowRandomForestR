#Felipe Montes
#2014 03 17

#Program to incorporate data from excel and analyze it using the Random Forest Methodology

setwd("C:/Felipe/Willow Project/Willow Random Forest")

#setwd("G:/Willow Random Forest")

#install needed packages
#install.packages("XLConnect")
#install.packages("party")
#install.packages("randomForest")
#install.packages("RColorBrewer")

# Call Packages


library(XLConnect)

library(randomForest)

library(boot)

library(lattice)
library(RColorBrewer)

#Reading the data from the excel file with the data from: C:\Felipe\Willow Project\Willow Random Forest\Biomass Across Sites Master File 2014-02-13.xlsx

Willow.data<-readWorksheetFromFile("Biomass Across Sites Master File 2014-02-13.xlsx", sheet = "Full data", startRow = 0, startCol = 0)

names(Willow.data)

Willow.data<-Willow.data[,1:21]


#Rename the variables
names(Willow.data)<-c("Year","Clone","Site","Repetition","Hemicellulose","Cellulose","Lignin","Ash","Density_g_cm3","Yield_Mg_ha_yr","Moisture","Soil_OrganicMatter","Soil_pH","Soil_Aluminum","Soil_Calcium","Soil_Iron","Soil_Potassium","Soil_Magnesium","Soil_Manganese","Soil_Zinc","Soil_Phosphorous")


Response.variables<-c("Hemicellulose","Cellulose","Lignin","Ash","Density_g_cm3","Yield_Mg_ha_yr","Moisture")
Dependent.variables<-names(Willow.data)[!names(Willow.data) %in% Response.variables]
Factor.variables<-c("Clone","Site","Repetition")

#conditioning the data for processing

# the data has many missing values that are represented as a ".". Therefore it is needed to locate them and extract the rest of the data


#Making variables numeric

Willow.data[,Response.variables]<-sapply(Willow.data[,Response.variables],as.numeric)

Willow.data[,11:21]<-sapply(Willow.data[,11:21],as.numeric)


#convert Qualitative Predictor variables as factors
Willow.data[,"Clone"]<-as.factor(Willow.data[,"Clone"])
Willow.data[,"Site"]<-as.factor(Willow.data[,"Site"])
Willow.data[,"Repetition"]<-as.factor(Willow.data[,"Repetition"])


#sumarize the data for exploration

summary(Willow.data)

summary(Willow.data$Clone)

histogram(Willow.data$Clone)

#The data has many typos that need corrections i.e. "SX61" and "SX61 " are considered different

Willow.data[Willow.data$Clone=="SX61 ","Clone"]<-"SX61"
Willow.data[Willow.data$Clone=="SX64 ","Clone"]<-"SX64"
Willow.data[Willow.data$Clone=="SX67 ","Clone"]<-"SX67"

#remove clone 0X032096 which only have one observation and clone 988224 which only has 4 observations

#Revert the column "Clone" from Factor to character 

Willow.data[,"Clone"]<-as.character(Willow.data[,"Clone"])

#remove clone 0X032096
Willow.data<-Willow.data[!Willow.data$Clone=="0X032096",]

#remove clone 988224
Willow.data<-Willow.data[!Willow.data$Clone=="988224",]

# With the data Typos corrected redo the Factor

Willow.data[,"Clone"]<-as.factor(as.character(Willow.data[,"Clone"]))

#Plot the data for exploration

histogram(Willow.data$Clone)


#first Analysis random Forest

#Yield data without missing values in Yield and the dependent variables
Yield.data<-Willow.data[!is.na(Willow.data$Yield_Mg_ha_yr),]

#impute the missing values usinf rfinpute function in random forests, see package description
Yield.data.imputed<-rfImpute(Yield_Mg_ha_yr~.,data=Yield.data)

#run Random Forest on with yield data as a response variable
RF.Yield<-randomForest(Yield_Mg_ha_yr~.,data=Yield.data.imputed, mtry=5, ntree=500,importance=T, proximity=T)

plot(RF.Yield)

MDSplot(RF.Yield, Yield.data.imputed$Yield_Mg_ha_yr, k=4)


MDSplot(RF.Yield, Yield.data.imputed$Yield_Mg_ha_yr, pch=unclass(Yield.data.imputed$Yield_Mg_ha_yr))



#Varible importance plots

barchart(sort(importance(RF.Yield)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Yield)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")

#plot scatter plot matrix of Response.variables
pairs(Willow.data[,Response.variables],col="BLUE")

##plot scatter plot matrix of Lignin, Cellulose, Ash and Yield 

pairs(Willow.data[,c("Cellulose","Lignin","Ash","Yield_Mg_ha_yr","Moisture")],col="RED")

#Random forests for Yield as the only response varible
RF.Yield.1<-randomForest(Yield_Mg_ha_yr~.,data=Yield.data.imputed[,c("Hemicellulose","Density_g_cm3","Yield_Mg_ha_yr","Moisture",Dependent.variables)], mtry=5, ntree=1000,importance=T, proximity=T)

barchart(sort(importance(RF.Yield.1)[,1],decreasing=F), main="Yield_Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Yield.1)[,2],decreasing=F), main="Yield_Variable importance, IncNodePurity",xlab="IncNodePurity ")

#importance of each class on the predictor variables

partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Site")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Clone")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Moisture")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Soil_Potassium")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Soil_Calcium")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Year")


dim(Willow.data[Willow.data$Year==2009,])
histogram(Willow.data$Year)

##run Random Forest on with Lignin data as a response variable

#Lignin data without missing values in Yield and the dependent variables
Lignin.data<-Willow.data[!is.na(Willow.data$Lignin),]

#impute the missing values usinf rfinpute function in random forests, see package description
Lignin.data.imputed<-rfImpute(Lignin~.,data=Lignin.data)

#run Random Forest on with lignin data as a response variable
RF.Lignin<-randomForest(Lignin~.,data=Lignin.data.imputed, mtry=5, ntree=500,importance=T, proximity=T)

plot(RF.Yield)

#Varible importance plots

barchart(sort(importance(RF.Lignin)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Lignin)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")

#plot scatter plot matrix of Response.variables
pairs(Willow.data[,Response.variables],col="BLUE")

##plot scatter plot matrix of Lignin, Cellulose, Ash and Yield 

pairs(Willow.data[,c("Cellulose","Lignin","Ash","Yield_Mg_ha_yr")],col="RED")

#Random forests for Lignin as the only response varible
RF.Lignin.1<-randomForest(Lignin~.,data=Lignin.data.imputed[,c("Hemicellulose","Density_g_cm3","Lignin","Moisture",Dependent.variables)], mtry=5, ntree=1000,importance=T, proximity=T)


#Varible importance plots

barchart(sort(importance(RF.Lignin.1)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Lignin.1)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")


#importance of each class on the predictor variables

partialPlot(RF.Lignin.1,Lignin.data.imputed,x.var="Site")
partialPlot(RF.Lignin.1,Lignin.data.imputed,x.var="Clone")
partialPlot(RF.Lignin.1,Lignin.data.imputed,x.var="Hemicellulose")
partialPlot(RF.Lignin.1,Lignin.data.imputed,x.var="Soil_Magnesium")
partialPlot(RF.Lignin.1,Lignin.data.imputed,x.var="Year")


##run Random Forest on with Ash data as a response variable

#Ash data without missing values in Yield and the dependent variables
Ash.data<-Willow.data[!is.na(Willow.data$Ash),]

#impute the missing values usinf rfinpute function in random forests, see package description
Ash.data.imputed<-rfImpute(Ash~.,data=Ash.data)

#run Random Forest on with Ash data as a response variable
RF.Ash<-randomForest(Ash~.,data=Ash.data.imputed, mtry=5, ntree=500,importance=T, proximity=T)


#Varible importance plots

barchart(sort(importance(RF.Ash)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Ash)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")


#Random forests for Lignin as the only response varible
RF.Ash.1<-randomForest(Ash~.,data=Ash.data.imputed[,c("Hemicellulose","Density_g_cm3","Ash","Moisture",Dependent.variables)], mtry=5, ntree=1000,importance=T, proximity=T)


#Varible importance plots

barchart(sort(importance(RF.Ash.1)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Ash.1)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")


#importance of each class on the predictor variables

partialPlot(RF.Ash.1,Ash.data.imputed,x.var="Site")
partialPlot(RF.Ash.1,Ash.data.imputed,x.var="Clone")
partialPlot(RF.Ash.1,Ash.data.imputed,x.var="Soil_Calcium")


#Cellulose data without missing values in Cellulose and the dependent variables
Cellulose.data<-Willow.data[!is.na(Willow.data$Cellulose),]

#impute the missing values usinf rfinpute function in random forests, see package description
Cellulose.data.imputed<-rfImpute(Cellulose~.,data=Cellulose.data)

#run Random Forest on with Cellulose data as a response variable
RF.Cellulose<-randomForest(Cellulose~.,data=Cellulose.data.imputed, mtry=5, ntree=500,importance=T, proximity=T)


#Varible importance plots

barchart(sort(importance(RF.Cellulose)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Cellulose)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")


#Random forests for Cellulose as the only response variable
RF.Cellulose.1<-randomForest(Cellulose~.,data=Cellulose.data.imputed[,c("Hemicellulose","Density_g_cm3","Cellulose","Moisture",Dependent.variables)], mtry=5, ntree=1000,importance=T, proximity=T)

barchart(sort(importance(RF.Cellulose.1)[,1],decreasing=F), main="Yield_Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Cellulose.1)[,2],decreasing=F), main="Yield_Variable importance, IncNodePurity",xlab="IncNodePurity ")

#importance of each class on the predictor variables

partialPlot(RF.Cellulose.1,Cellulose.data.imputed,x.var="Site")
partialPlot(RF.Cellulose.1,Cellulose.data.imputed,x.var="Clone")
partialPlot(RF.Cellulose.1,Cellulose.data.imputed,x.var="Moisture")
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Soil_Magnesium")


#Clone data without missing values variables
Clone.data<-Willow.data[!is.na(Willow.data$Clone),]

#impute the missing values usin rfinpute function in random forests, see package description
Clone.data.imputed<-rfImpute(Clone~.,data=Clone.data)

#run Random Forest on with Clone data as a response variable
RF.Clone<-randomForest(Clone~.,data=Clone.data.imputed, mtry=5, ntree=500,importance=T, proximity=T)


#Varible importance plots

barchart(sort(importance(RF.Clone)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE")
barchart(sort(importance(RF.Clone)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ")



#importance of each class on the predictor variables

partialPlot(RF.Clone, Clone.data.imputed,x.var="Yield_Mg_ha_yr")
bwplot(Clone~Yield_Mg_ha_yr, Willow.data,main="summary of Yield By clone", Ylab="Yield_Mg_ha_yr")
panel.abline(v=322,col="RED")


partialPlot(RF.Clone, Clone.data.imputed,x.var="Density_g_cm3")
bwplot(Clone~Density_g_cm3, Willow.data,main="summary of Density By clone", Ylab="Density_g_cm3")
panel.abline(v=270,col="RED")


partialPlot(RF.Clone, Clone.data.imputed,x.var="Moisture")
bwplot(Clone~Moisture, Willow.data,main="summary of Moisture By clone", Ylab="Moisture")
panel.abline(v=363,col="RED")

partialPlot(RF.Clone, Clone.data.imputed,x.var="Cellulose")
bwplot(Clone~Cellulose, Willow.data,main="summary of Cellulose By clone", Ylab="Cellulose")
panel.abline(v=353,col="RED")
panel.abline(v=515,col="RED")

partialPlot(RF.Clone, Clone.data.imputed,x.var="Lignin")
bwplot(Clone~Cellulose, Willow.data,main="summary of lignin By clone", xlab="Lignin")


MDSplot(RF.Clone, Clone.data.imputed$Clone, k=4)


MDSplot(RF.Clone, Clone.data.imputed$Clone, pch=unclass(Clone.data.imputed$Clone))

