# RandomForest_CGHR
Algorithm for assigning causes of death to symptom data from verbal autopsies. Uses random forest method to classify cases and predict most probably causes

To use: 

library(randomForest)
setwd("RFClassify")	#set the working directory
source("RFClassify_Maxwins_Opt.txt")

#**make sure the RHMEData file is in the working directory**

VAdata<-read.table("RHMEData.txt", header=TRUE, sep = ",")


VAdataNew <- VAdata[1:1200,-1]
VAdataTest <- VAdata[1201:1502,-1]
trained<-200

CSMFest<-RFClassifyMod(VAdataNew, VAdataTest, 143, 32, trained)
