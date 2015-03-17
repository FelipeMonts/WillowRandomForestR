#Felipe Montes
#Updated 2015 03 02 to reflect new additions in the original database


#Program to incorporate data from excel and analyze it using the Random Forest Methodology

setwd("C:/Felipe/GitHub/WillowRandomForestR");
#setwd("C:/Felipe/Willow Project/Willow Random Forest")

#setwd("G:/Willow Random Forest")

#install needed packages
#install.packages("XLConnect")
#install.packages("party")
#install.packages("randomForest")
#install.packages("RColorBrewer")

# Call Packages


library(XLConnect);

library(randomForest);

library(boot);

library(lattice);

library(RColorBrewer);

library(gplots);

#Reading the data from the excel file with the data from: C:\Felipe\Willow Project\Willow Random Forest\Biomass Across Sites Master File 2014-02-13.xlsx

Willow.data<-readWorksheetFromFile("WillowGXE2015_02_11_FM.xlsx", sheet = "CombinedDataset", startRow = 0, startCol = 0);

Willow.data.names <-names(Willow.data);

str(Willow.data)

# Willow.data<-Willow.data[,1:21]


#Explore the completness of the data and the apearance of NA values or similar


# #Rename the variables
# names(Willow.data)<-c("Year","Clone","Site","Repetition","Hemicellulose","Cellulose","Lignin","Ash","Density_g_cm3","Yield_Mg_ha_yr","Moisture","Soil_OrganicMatter","Soil_pH","Soil_Aluminum","Soil_Calcium","Soil_Iron","Soil_Potassium","Soil_Magnesium","Soil_Manganese","Soil_Zinc","Soil_Phosphorous")


# Group the variables into predictive variables and Response Variables

Descriptor.variables<-c("Establish.Year","Harvest.Year","Trial.ID","Site..SAS.","Rep","Comments");

Predictor.variables<-c("X..Organic.Matter", "Soil.pH", "X.H..", "Soil.P..mg.kg.", "Soil.K..mg.kg.", "Soil.Ca..mg.kg.","Soil.Mg..mg.kg.", "Soil.Fe..mg.kg.", "Soil.Mn..mg.kg.", "Soil.Zn..mg.kg.","Soil.Al..mg.kg.", "Mean.ann.prcp..mm.", "Mean.ann.GDD..base.10oC.", "Prcp..April.Oct..mm.", "Tmax..April.Oct.oC.", "Annual.Tmin..oC.", "Depth.to.water.table.low.cm.", "Depth.to.Water.Table.high.cm.", "Available.water.capacity..cm.cm."); #, "Height..m.", "Area.per.plot..cm2.");

Predictor.variables.factors<-c("Clone.ID","Epithet","Family","New.Diversity.Group","Clone..SAS.","Ploidy.level","Land.capability.class","LC.subclass");
Response.variables<-c( "Survival....","Surviv.prop","Wet.Yield..Mg.ha.","Biomass...Moisture","Biomass.Moisture.prop","Biomas...dry.matter","Biomass.dry.matter.prop","Dry.Yield..Mg.ha.","Annual.Yield..Mg.ha.yr.","Dry.tons.ac","Dry.tons.ac.yr","X..Hemicellulose","X..Cellulose","X..Lignin", "X..Ash", "Density..g.cm3.", "Hemicellulose.yield", "Cellulose.yield", "Lignin.yield", "Ash.yield");


#conditioning the data for processing

# the data has many missing values that are represented as a ".". Therefore it is needed to locate them and extract the rest of the data


#Making variables numeric or factors depending on the structure of the variable

Willow.data[,Descriptor.variables]<-lapply(Willow.data[,Descriptor.variables],as.factor);

Willow.data[,Predictor.variables]<-sapply(Willow.data[,Predictor.variables],as.numeric);

Willow.data[,Predictor.variables.factors]<-lapply(Willow.data[,Predictor.variables.factors],as.factor);

Willow.data[,Response.variables]<-sapply(Willow.data[,Response.variables],as.numeric);


# After converting the data to numeric and factors check for NA values in the data

# Descriptor variables

Willow.data.NA.Descriptor<-as.data.frame(is.na(Willow.data[,Descriptor.variables])+0);
Willow.data.NA.Descriptor.sum<-sapply(Willow.data.NA.Descriptor,sum);
barplot(Willow.data.NA.Descriptor.sum,names.arg=names(Willow.data.NA.Descriptor.sum),horiz=F,las=2, main="Distribution of NA values");

# Predictor Variables

Willow.data.NA.Predictor<-as.data.frame(is.na(Willow.data[,Predictor.variables])+0);
Willow.data.NA.Predictor.sum<-sapply(Willow.data.NA.Predictor,sum);
barplot(Willow.data.NA.Predictor.sum,names.arg=names(Willow.data.NA.Predictor.sum),horiz=F,las=2, main="Distribution of NA values");

# Predictor Variables factors

Willow.data.NA.Predictor.factors<-as.data.frame(is.na(Willow.data[,Predictor.variables.factors])+0);
Willow.data.NA.Predictor.factors.sum<-sapply(Willow.data.NA.Predictor.factors,sum);
barplot(Willow.data.NA.Predictor.factors.sum,names.arg=names(Willow.data.NA.Predictor.factors.sum),horiz=F,las=2, main="Distribution of NA values");


# Response variables

Willow.data.NA.Response<-as.data.frame(is.na(Willow.data[,Response.variables])+0);
Willow.data.NA.Response.sum<-sapply(Willow.data.NA.Response,sum);
barplot(Willow.data.NA.Response.sum,names.arg=names(Willow.data.NA.Response.sum),horiz=F,las=2, main="Distribution of NA values");




# Based on the bar plot there are several variables with NA values  how to handle them?



# check for typos invariables and factors:

for (i in Descriptor.variables ) {
  print(histogram(Willow.data[,i],xlab=i,type='count'));
}
  

for (i in Predictor.variables.factors ) {
  print(histogram(Willow.data[,i],xlab=i,type='count'));
}
   

# There are a couple of Clone..SAS.  with few entries:"01X265020" "01X266016"
dim(Willow.data[Willow.data$Clone..SAS.=="01X265020",])[1];
dim(Willow.data[Willow.data$Clone..SAS.=="01X266016",])[1];

# New.Diversity.Group 3 has very few entries as well
dim(Willow.data[Willow.data$New.Diversity.Group=="3",])[1];

# There are very limited entries for land capability clases 4 and 5

dim(Willow.data[Willow.data$Land.capability.class=="4",])[1];
dim(Willow.data[Willow.data$Land.capability.class=="5",])[1];

# Since the study tries to analyze Genotype by environment, lest see which Genotypes are in which sites
CloneXSite<-xtabs(formula=~Clone.ID +Site..SAS., data=Willow.data);

heatmap(CloneXSite,scale='none');

# Using the package gplots can improve the heat map

h.palette<-brewer.pal(3,"YlGnBu")
heatmap.2(CloneXSite,scale='none', dendrogram='none',breaks=c(0,3,4,7),col=h.palette);

CloneSasXSite<-xtabs(formula=~Clone..SAS.+Site..SAS., data=Willow.data);

h.palette<-brewer.pal(3,"YlOrRd")
heatmap.2(CloneSasXSite,scale='none', dendrogram='none',breaks=c(0,3,4,7),col=h.palette);


#We can also see which ploidy level is in each site  "Clone.ID","Epithet","Family" "Ploidy.level"

PloidyXsite<-xtabs(formula=~Ploidy.level+Site..SAS., data=Willow.data);
h.palette<-brewer.pal(3,"BuGn")
heatmap.2(PloidyXsite,scale='none', dendrogram='none',breaks=c(0,3,4,7),col=h.palette);


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

