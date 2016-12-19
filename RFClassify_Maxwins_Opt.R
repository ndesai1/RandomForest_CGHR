####Test Data Result Pairwise Coupling Random Forest method for Cause of Death assignment ####

#RF Classify is a function that takes a dataset containing training data (VAdataNew), data we want to predict 
#(VAdataTest) and the number of causes of death documented (tCoD) and outputs a table of the top predicted
#causes for each case, as well two tables used in the IHME ranking. 

#remember to set the working directory to a place where all the necessary data can be read from, and where output files will be stored
install.packages("randomForest")

RFClassifyMod<-function(VAdataNew, VAdataTest, codcol, tCoD, trained)
{
  # open file RFFunctionsMod, which contains all the functions needed to perform cod classification
  source("RFFunctions_Maxwins_Opt.R")
  
  dir.create(path = 'Output', showWarnings = FALSE)
  #Three files are produced each time the code is run. These files are deleted before running the classification again 
  unlink("appearancesUnweeded.txt")
  unlink("appearancesUnweededTrain.txt")
  unlink("appearranksUnweeded.txt")
  unlink("appearranksUnweededTrain.txt")
  
  
  unlink("Output/TopCausesRaw.csv")
  
  library(randomForest)
  set.seed(71)
  
  Cod <- tCoD-1
  #winning is a matrix that will contain the number of trees that voted for a particular cause for each case
  winning1<-matrix(0, nrow = NROW(VAdataTest), ncol=tCoD)
  winning2<-matrix(0, nrow = NROW(VAdataTest), ncol=tCoD)
  win<-list("winning1"=winning1, "winning2"=winning2)
  
  
  #if the VAdataTest data has an assigned cod, need to remove the column containing the CoD information
  if (codcol == 0)
  {
    VAdataTest1<-VAdataTest
  }
  
  if (codcol != 0)
  {
    VAdataTest1<-VAdataTest[,-codcol] 
  }
  
  grown<-1
  
  #the following nested loops are performed for each of the rows (i.e. verbal autopsy cases) in VAdataTest
  for(i in 1:Cod)
  {
    #cod_appear counts the number of times a certain cod appears in the training dataset  	
    
    appearedi <-cod_appear(i, VAdataNew)
    j <- i+1
    
    if (j<=tCoD)
    {
      for(k in j:tCoD)
      {
        appearedj<-cod_appear(k, VAdataNew)		
        print(c(i, k, appearedi, appearedj))	
        #a certain cod has to appear at least once for the Random forest function to be run on it					
        
        if (appearedi >= 1 && appearedj >= 1)
        {
          win<-prediction(i,k,VAdataNew, VAdataTest1, win)
        }			
      }
      
    }
  }
  
  print(win$winning1[1:15,]) 						#print winning to make sure it is in the right format
  print(win$winning2[1:15,])
  for(i in 1:NROW(win$winning1))
  {
    if (i<=trained)
    {
      appearranks<-win$winning1[i,]
      mm<-file("appearranksUnweededTrain.txt", "a")
      write(appearranks, mm, tCoD, append=TRUE)
      close(mm)
      appeartimes<-win$winning2[i,]
      zz<-file("appearancesUnweededTrain.txt", "a")
      write(appeartimes, zz, tCoD, append=TRUE)
      close(zz)
    }
    
    if (i>trained)
    {
      appearranks<-win$winning1[i,]
      nn<-file("appearranksUnweeded.txt", "a")
      write(appearranks, nn, tCoD, append=TRUE)
      close(nn)
      appeartimes<-win$winning2[i,]
      yy<-file("appearancesUnweeded.txt", "a")
      write(appeartimes, yy, tCoD, append=TRUE)
      close(yy)
    }
  }
  
  CSMFest<-detTopCause(win$winning2, tCoD)
  return(CSMFest)
}
