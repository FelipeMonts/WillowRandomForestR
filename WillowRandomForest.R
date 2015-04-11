# Felipe Montes
# Updated 2015 03 02 to reflect new additions in the original database 
# Updated 2015 03 25 to include the latests changes added data to the database



#Program to incorporate data from excel and analyze it using the Random Forest Methodology

setwd("C:/Felipe/Willow Project/Willow Random Forest")

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

library(ggplot2);

# Reading the data from the excel file with the data from: C:\Felipe\Willow Project\Willow Random Forest\Biomass Across Sites Master File 2014-02-13.xlsx

# Reading the data from the excel file: C:\Felipe\GitHub\WillowRandomForestR\WillowGXE2015_02_11_FM.xlsx

# Willow.data<-readWorksheetFromFile("WillowGXE2015_02_11_FM.xlsx", sheet = "CombinedDataset", startRow = 0, startCol = 0);

# Readin the data from the excel file: C:\Felipe\GitHub\WillowRandomForestR\Willow G X E yield & composition database 2015_03_18.xlsx

Willow.data<-readWorksheetFromFile("Willow G X E yield & composition database 2015_03_18.xlsx", sheet = "Combined dataset (2)", startRow = 0, startCol = 0, endCol=60);

Willow.data.names <-names(Willow.data);

str(Willow.data);

# Location is not added separately; Site..SAS, but it has the year associated with. Create a new Variable called Location that only accounts for the location independent of year

# Split the Trial.ID information based on the character "_" i.e: "BellevilleNY_2005_YT" results in a three component list "BellevilleNY" "2005"         "YT". 
Location<-strsplit(as.character(Willow.data$Trial.ID), "_");

# Extracting the first component in the list Location and adding to the data frame

Willow.data$Location<-sapply(Location, "[[", 1);

# Group the variables into predictive variables and Response Variables

Descriptor.variables<-c("Establish.Year","Harvest.Year","Trial.ID","Site..SAS.","Elevation..m.", "Location","Rep","Comments");

Predictor.variables<-c("X..Organic.Matter", "Soil.pH", "X.H..", "Soil.P..mg.kg.", "Soil.K..mg.kg.", "Soil.Ca..mg.kg.","Soil.Mg..mg.kg.", "Soil.Fe..mg.kg.", "Soil.Mn..mg.kg.", "Soil.Zn..mg.kg.","Soil.Al..mg.kg.", "Mean.ann.prcp..mm.", "Mean.ann.GDD..base.10oC.", "Prcp..April.Oct..mm.", "Tmax..April.Oct.oC.", "Annual.Tmin..oC.", "Annual.solar.radiation..MJ.m.1.day.1.", "Solar.radiation..Apr.Oct..MJ.m.1.d.1.","Depth.to.water.table.low.cm.", "Depth.to.Water.Table.high.cm.", "Available.water.capacity..cm.cm."); #, "Height..m.", "Area.per.plot..cm2.");

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

# Change the lower marging to allow the names to be read. par (mar) is a global variable 
par(mar=c(10,4,4,2));

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
  print(histogram(Willow.data[,i],xlab=i,type='count',scales=list(x=list(rot=90))));
}
  

for (i in Predictor.variables.factors ) {
  print(histogram(Willow.data[,i],xlab=i,type='count',labels=TRUE,scales=list(x=list(rot=90))));
  # scales$rot is used to rotate the axis labels 90 degrees
}


# There are very limited entries for land capability clases 4 and 5

dim(Willow.data[Willow.data$Land.capability.class=="4",])[1];
dim(Willow.data[Willow.data$Land.capability.class=="5",])[1];

# There are  Ploidy levels with named "???" ###

dim(Willow.data[Willow.data$Ploidy.level=="???",])[1];

Willow.data[Willow.data$Ploidy.level=="???",];


# There are a couple of Clone..SAS.  with few entries
Clone..SAS.sum<-summary(Willow.data$Clone..SAS.);

Clone..SAS.sum[which(Clone..SAS.sum <= 4)];

# Similarly there are a couple of Clone.ID with few entries

Clone.ID.sum<-summary(Willow.data$Clone.ID);

Clone.fewEntries<-names(Clone.ID.sum[which(Clone.ID.sum<= 4)]);

# A few new.Diversity.Group have very few entries as well
Diversity.sum<-summary(Willow.data$New.Diversity.Group);

Diversity.sum[which(Diversity.sum <= 10)];


# try heat maps to see where the missing values are in the dataset


# Since the study tries to analyze Genotype by environment, lets see which Genotypes are in which sites
CloneXSite<-xtabs(formula=~Clone.ID +Site..SAS., data=Willow.data);

# Using the heatmap2 can improve the heat map


h.palette<-colorRampPalette(c("yellow","blue","green"));

heatmap.2(CloneXSite,scale='none', dendrogram='none',col=h.palette, main="Clone X Site");



CloneSasXLocation<-xtabs(formula=~Clone..SAS.+Location, data=Willow.data);

heatmap.2(CloneSasXLocation,scale='none', dendrogram='none',col=h.palette, main="CloneSAS  X Location");



#We can also see which "Ploidy.level" "Epithet","Family" "Ploidy.level" level is in each site  

PloidyXSite<-xtabs(formula=~Ploidy.level+Site..SAS., data=Willow.data);

heatmap.2(PloidyXSite,scale='none', dendrogram='none',col=h.palette, main="Ploidy X SiteSAS");


PloidyXLocation<-xtabs(formula=~Ploidy.level+Location, data=Willow.data);

heatmap.2(PloidyXLocation,scale='none', dendrogram='none',col=h.palette, main="Ploidy X Location");

EpithetXSite<-xtabs(formula=~Epithet+Site..SAS., data=Willow.data);

heatmap.2(EpithetXSite,scale='none', dendrogram='none',col=h.palette, main="Epithet X SiteSAS");

EpithetXLocation<-xtabs(formula=~Epithet+Location, data=Willow.data);

heatmap.2(EpithetXLocation,scale='none', dendrogram='none',col=h.palette, main="Epithet X Location");

FamilyXLocation<-xtabs(formula=~Family+Location, data=Willow.data);

heatmap.2(FamilyXLocation,scale='none', dendrogram='none',col=h.palette, main="Family X Location");


# Exploring other variables data completness with heat maps

CloneXLocation<-xtabs(formula=~Clone.ID+Location, data=Willow.data);

heatmap.2(CloneXLocation,scale='none', dendrogram='none',col=h.palette, main="Clone X Location");

# We can use is.na to transform survival data into a 1 or 0 data type and the use tables or xtabs to create heat maps

# SURVIVAL DATA

Survival.data<-data.frame(abs(is.na(Willow.data$Survival....)-1),Willow.data$Survival....,Willow.data$Clone.ID,Willow.data$Location);
names(Survival.data)<-c("Is.data", "Survival","Clone.ID","Location");

Survival.tab<-xtabs(Is.data~Clone.ID+Location,Survival.data);
heatmap.2(Survival.tab,scale='none', dendrogram='none',col=h.palette, main="Survival Data: Clone ID x Location");



# WET YIELD DATA

WYield.data<-data.frame(abs(is.na(Willow.data$Wet.Yield..Mg.ha.)-1),Willow.data$Wet.Yield..Mg.ha.,Willow.data$Clone.ID,Willow.data$Location);
names(WYield.data)<-c("Is.data", "Wet Yield","Clone.ID","Location");

WYield.tab<-xtabs(formula=Is.data~Clone.ID+Location,data=WYield.data);
heatmap.2(WYield.tab,scale='none', dendrogram='none',col=h.palette, main="Wet Yield Data: Clone ID x Location");

# DRY YIELD DATA

DYield.data<-data.frame(abs(is.na(Willow.data$Dry.Yield..Mg.ha.)-1),Willow.data$Dry.Yield..Mg.ha.,Willow.data$Clone.ID,Willow.data$Location);
names(DYield.data)<-c("Is.data", "Dry Yield","Clone.ID","Location");

DYield.tab<-xtabs(formula=Is.data~Clone.ID+Location,data=DYield.data);
heatmap.2(DYield.tab,scale='none', dendrogram='none',col=h.palette, main="Dry Yield Data: Clone ID x Location");

# HEMICELLULOSE

HEMICELLULOSE.data<-data.frame(abs(is.na(Willow.data$X..Hemicellulose)-1),Willow.data$Dry.Yield..Mg.ha.,Willow.data$Clone.ID,Willow.data$Location);
names(HEMICELLULOSE.data)<-c("Is.data", "X..Hemicellulose","Clone.ID","Location");

HEMICELLULOSE.tab<-xtabs(formula=Is.data~Clone.ID+Location,data=HEMICELLULOSE.data);
heatmap.2(HEMICELLULOSE.tab,scale='none', dendrogram='none',col=h.palette, main="X HEMICELLULOSE: Clone ID x Location");




# STARTING RANDOM FOREST ANALYSYS

# first Analysis random Forest

# Yield data without missing values in Yield and the dependent variables
Yield.data<-Willow.data[!is.na(Willow.data$Dry.Yield..Mg.ha.),c(Predictor.variables,Response.variables, "Location","Clone.ID","Epithet","Family","New.Diversity.Group","Ploidy.level","Land.capability.class","LC.subclass" )];

# Remove clones with few entries as the algorith cannot handle cathegorical data with more than 53 levels


Yield.data<-Yield.data[!Yield.data$Clone.ID %in% Clone.fewEntries,];

# The line above remove the data that correspond to clones with few entries "Clone.fewEntries", but the levels of the factor Clone.ID still are considered part of the Yield.data$Clone.ID factor, even though they have "0" (zero) entries. To get rid of these levels use the function droplevels which drop any levels that are not used

Yield.data<-droplevels(Yield.data);

# impute the missing values using rfinpute function in random forests, see package description
Yield.data.imputed<-rfImpute(Annual.Yield..Mg.ha.yr.~.,data=Yield.data);

# run Random Forest on with yield data as a response variable
RF.Yield<-randomForest(Annual.Yield..Mg.ha.yr.~.,data=Yield.data.imputed, mtry=5, ntree=500,importance=T, proximity=T);

plot(RF.Yield);

# Varible importance plots

barchart(sort(importance(RF.Yield)[,1],decreasing=F), main="Variable importance, % Increase MSE", xlab="%IncMSE");
barchart(sort(importance(RF.Yield)[,2],decreasing=F), main="Variable importance, IncNodePurity",xlab="IncNodePurity ");

# plot scatter plot matrix of Response.variables
pairs(Willow.data[,names(Yield.data.imputed)[1:20]],col="BLUE");

# The initial random forest results with all variables explaining yield, indicated that obiously correlated variables (wet yield, ash yield, ligning yield) were the most important. This is not unexpected but for sure is of no use. A more useful result would be obtained when these correlated variables are not included in the analaysis. Also, since location determines, soil  pH, and all the other soil varaibles, allthose variables are correlated and should not be at the same time in the analysis

# Use only uncorrelated variables in the random forest analysys

Yield.data.imputed.1<-Yield.data.imputed[,c("Annual.Yield..Mg.ha.yr.","Mean.ann.prcp..mm.","Mean.ann.GDD..base.10oC.","Prcp..April.Oct..mm.", "Tmax..April.Oct.oC.","Annual.Tmin..oC.","Annual.solar.radiation..MJ.m.1.day.1.","Solar.radiation..Apr.Oct..MJ.m.1.d.1.","Depth.to.water.table.low.cm.","Depth.to.Water.Table.high.cm.","Available.water.capacity..cm.cm.","Survival....","Biomass...Moisture","Biomas...dry.matter","X..Hemicellulose","X..Cellulose","X..Lignin","X..Ash","Density..g.cm3.","Location","Clone.ID","Epithet","Family","New.Diversity.Group","Ploidy.level")];



# Random forests for Yield and uncorrelated variables 

RF.Yield.1<-randomForest(Annual.Yield..Mg.ha.yr.~.,data=Yield.data.imputed.1, mtry=5, ntree=1000,importance=T, proximity=T);

barchart(sort(importance(RF.Yield.1)[,1],decreasing=F), main="Yield_Variable importance, % Increase MSE", xlab="%IncMSE");
barchart(sort(importance(RF.Yield.1)[,2],decreasing=F), main="Yield_Variable importance, IncNodePurity",xlab="IncNodePurity ");

# importance of each class on the predictor variables

partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Location");
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="X..Lignin"); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Clone.ID");
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="X..Cellulose"); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Epithet");
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Survival...."); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Tmax..April.Oct.oC."); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Family");
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="X..Ash"); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Annual.solar.radiation..MJ.m.1.day.1."); # Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Solar.radiation..Apr.Oct..MJ.m.1.d.1.");# Worth exploring further
partialPlot(RF.Yield.1,Yield.data.imputed,x.var="Mean.ann.GDD..base.10oC.");# Worth exploring further



# Exploring further the results of the predictor importance results


pairs(Yield.data.imputed[,c("Dry.Yield..Mg.ha.","X..Lignin","X..Cellulose","Survival....","Tmax..April.Oct.oC.","X..Ash","Annual.solar.radiation..MJ.m.1.day.1.","Solar.radiation..Apr.Oct..MJ.m.1.d.1.")],col='BLUE');

dim(Willow.data[Willow.data$Year==2009,])
histogram(Willow.data$Year)

MDSplot(RF.Yield, Yield.data.imputed$Location);

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

