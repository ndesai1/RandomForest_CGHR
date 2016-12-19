#cod_appear is a function that determines how many times
#a cod with number of argument passed appears in the 
#training dataset

cod_appear<-function(CauseNo, VAdata)
{
  #Cause A contains vector whose values are either TRUE or FALSE
  #depending on whether the cod in a particular row is equal to i
  
  CauseA <- VAdata$cod==CauseNo
  limit <- NROW(CauseA)
  appeared <- 0
  
  #The following for loop calculates the number of times a particular cause
  #appears in VAdata
  
  for(check in 1:limit)
  {
    if (CauseA[check]=="TRUE")
    {
      appeared<-appeared+1
    }
  }
  appeared
}

#prediction() takes all the cases in VAdataNew that have cause numbers i and k, and builds 100 decision trees between the 
#two causes, then runs all the VAdataTest cases through the 100 trees to vote for one cause or another
prediction<-function(i,k, VAdataNew, VAdataTest, win)
{
  NewNode <- rbind(VAdataNew[VAdataNew$cod==i,], VAdataNew[VAdataNew$cod==k,])
  NewNode$cod <- as.factor(NewNode$cod)
  growforest.rf <- randomForest(NewNode$cod~., data=NewNode, ntree = 120, mtry=2, replace=TRUE, importance = TRUE)
  
  #for(entry in 1:NROW(VAdataTest))
  #{
  Predicts <- VAdataTest								#can potentially be optimized by vectorization
  predicted<- predict(growforest.rf, Predicts, type="vote")		#vectorize
  for (entry in 1:NROW(predicted))
  {
    if (predicted[entry, 1]>predicted[entry, 2])
    {
      win$winning1[entry, i]<-win$winning1[entry, i]+ 1
    }
    if (predicted[entry, 2]>predicted[entry, 1])
    {
      win$winning1[entry, k]<-win$winning1[entry, k]+ 1
    }
    win$winning2[entry,i]<-win$winning2[entry, i]+predicted[entry, 1]
    win$winning2[entry,k]<-win$winning2[entry, k]+predicted[entry, 2]
  }
  #}
  return(win)
}

#detTopCause() chooses the 20 causes that were voted for by the most number of trees and prints these five causes to a csv file
detTopCause<-function(winning, tCoD)
{
  for (i in 1:NROW(winning))
  {
    appeartimes<-winning[i,]
    TopCause<-matrix(,nrow=20, ncol=2, dimnames=list(NULL,c("Cause","Occurence")))
    for(rank in 1:20)
    {
      TopCause[rank, 2]<- max(appeartimes)
      TopCod<-which.max(appeartimes)
      TopCause[rank, 1]<- TopCod
      
      appeartimes[TopCod]<- 0
    }
    
    yy<-file("Output/TopCausesRaw.csv", "a")
    write.csv(x=rbind(i, t(TopCause)), yy, row.names = TRUE)
    close(yy)
  }
  CSMFest<-array()							#summing up winners might resulr in a lower CSMF accuracy than summing up the number of trees, try summing trees instead
  CauseMortTotal<-colSums (winning)
  MortTotal<-sum(CauseMortTotal)
  for (i in 1:tCoD)
  {
    CSMFest[i]<-CauseMortTotal[i]/MortTotal
  }
  print(CauseMortTotal)
  return(CSMFest)
}

#IHME ranking method
RFRanking<-function(trainrows, tCoD)
{
  training<-read.table("appearranksUnweededTrain.txt")
  testing<-read.table("appearranksUnweeded.txt")
  training1<-read.table("appearancesUnweededTrain.txt")
  testing1<-read.table("appearancesUnweeded.txt")
  training<-training[1:trainrows,]
  CSMFest<-array()
  CauseMortTotal<-colSums(testing)
  MortTotal<-sum(CauseMortTotal)
  for (i in 1:tCoD)
  {
    CSMFest[i]<-CauseMortTotal[i]/MortTotal
  }
  print(CSMFest)
  
  
  unlink("codranking.txt")
  unlink("Output/topcauses.csv")
  write(c(1:tCoD), "codranking.txt", tCoD, append=TRUE, sep = "\t")
  
  for(test in 1:NROW(testing))
  {
    #write a function that takes out a row if the appeartimes is equal to 0
    codranking<-array()
    
    for(trainappear in 1:tCoD)
    {
      placement<-test+1
      n<-2
      case<-testing[test, trainappear]
      trainappeared<-array()
      orders<-NULL
      sorting<-NROW(training)
      trainappeared<-sort(training[1:sorting,trainappear])
      num<-1
      if (sum(training[1:sorting, trainappear])==0)
      {
        orders<-training[1:sorting, trainappear]
      }
      if (sum(training[1:sorting, trainappear])!=0)
      {	
        for(m in 1:30)
        {
          sigh<-length(trainappeared)
          
          if (sigh > 1)
          {	
            for(a in 1:(sigh-1))
            {	
              if(trainappeared[a]<trainappeared[a+1])
              {
                highest=trainappeared[a+1]
              }
              if(trainappeared[a]>=trainappeared[a+1])
              {
                highest=trainappeared[a]
              }	
            }
            orders[num]<-highest
            num<-num+1	
          }
          trainappeared<-trainappeared[trainappeared!=highest]
        }
      }
      print(orders)
      rowrank<-0
      for(n in 1:length(orders))
      {
        if(case==orders[n])
        {
          rowrank<-n+1
        }
        if(case<orders[n])
        {
          rowrank<-n+2
        }
        if(case>orders[1])
        {
          rowrank<-1
        }
      }
      codranking[trainappear]<-rowrank
    }
    
    write(codranking, "codranking.txt", tCoD, append=TRUE, sep = "\t")
    cod<-read.table("codranking.txt", sep="\t")
    mer<-NULL
    
    for (clean in 1:tCoD)
    {
      if(sum(training[2:NROW(training),clean])!=0)
      {
        mernew<-cod[,clean]
        mer<-cbind(mer, mernew)
      }
    }
    
    topcauses<-array()
    causerank<-array()
    chosen<-NCOL(mer)
    
    for(ems in 1:20)
    {
      limits<-NROW(testing)
      for(choosing in 1:chosen)
      {
        if (mer[placement,choosing]<=limits)
        {
          topcause<-mer[1, choosing]
          best<-mer[placement, choosing]
          limits<-best
          deletion<-choosing
        }
      }
      topcauses[ems]<-topcause
      causerank[ems]<-best
      mer[placement, deletion]<-NROW(testing)
    }
    yy<-file("Output/topcauses.csv", "a")
    write.csv(x=rbind(test, topcauses, causerank), yy, row.names=TRUE)
    close(yy)	
  }
  return(CSMFest)
}


#Concordance is a function that checks the agreement between the coded cod and
#the top five cod
RFcsmf<- function(CSMFest, tCoD, CoDAssigned, output)
{
  MortFracSecond<-array(tCoD)
  MortFracSecond[1:tCoD]<-0
  CSMFactual<-NULL
  agreement<-NULL
  CSMFtotal<-0
  
  CSMFdiff<- NULL
  SEdiff<-NULL
  CSMEtotal<-0
  
  for(concur in 1:NROW(CoDAssigned))
  {
    mort2<-CoDAssigned[concur]
    MortFracSecond[mort2]<-MortFracSecond[mort2] + 1
  }
  for (i in 1:tCoD)
  {
    CSMFactual[i]<-MortFracSecond[i]/sum(MortFracSecond[1:tCoD])
  }
  CSMFminimum<-min(CSMFactual)
  
  zz<-file("C:/Documents and Settings/desaini/Desktop/CCVAComparison/RF Test Datasets/RandomForestMethodFinal/RandomForestCSMF.csv", "a")
  write.csv(rbind(CSMFest,CSMFactual), zz, row.names=TRUE)
  close(zz)
  
  for (i in 1:tCoD)
  {
    CSMFdiff[i]<-abs(CSMFactual[i]-CSMFest[i])
    SEdiff[i]<- CSMFdiff[i]^2
    
    CSMFtotal<-CSMFtotal+CSMFdiff[i]
    CSMEtotal<-CSMEtotal+SEdiff[i]
  }
  CSMFerror<-CSMFtotal/(2*(1-CSMFminimum))
  CSMFaccuracy<-1-CSMFerror
  CMSE<-CSMEtotal/tCoD
  RSME<-CMSE^0.5
  print(cbind(CSMFaccuracy, CMSE, RSME))
  if (output==1)
  {
    yy<-file("C:/Documents and Settings/desaini/Desktop/CCVAComparison/RF Test Datasets/RandomForestMethodFinal/RandomForestResults.csv", "a")
    write.csv(rbind(CSMFaccuracy, CMSE, RSME), yy, row.names=TRUE)
    close(yy)
  }
}

RFConcordance<-function(CSMFest, trainrows, topcauses, tCoD, trained, output, replace)
{
  #open file containing Top Causes.csv as a data table, then compare cod in cleaned
  #training dataset with Top Causes
  train<-trained+1
  Total_Cases<-array(tCoD)
  Total_Neg_Cases<-array(tCoD)
  Total_Neg_Cases[1:tCoD]<-Total_Cases[1:tCoD]<-0
  CCC<-array(tCoD)
  
  if(trainrows!=0)
  {
    CoDAssigned<-VAdataTest$cod[train:NROW(VAdataTest)]
  }
  if(trainrows==0)
  {
    CoDAssigned<-VAdataTest$cod
  }
  
  print(t(CoDAssigned))
  n<-3
  TP1<-second<-third<-fourth<-fifth<-array(tCoD)
  TP1[1:tCoD]<-0
  second[1:tCoD]<-0
  third[1:tCoD]<-0
  fourth[1:tCoD]<-0
  fifth[1:tCoD]<-0
  TN1<-TN2<-TN3<-TN4<-TN5<-array(tCoD)
  TN1[1:tCoD]<-TN2[1:tCoD]<-TN3[1:tCoD]<-TN4[1:tCoD]<-TN5[1:tCoD]<-0
  
  for (i in 1:(NROW(topcauses)/4))
  {
    CodedCause<-CoDAssigned[i]
    for(z in 1:tCoD)
    {
      
      if(CodedCause==z)
      {		
        
        Total_Cases[z]<-Total_Cases[z]+1
        if(CodedCause==as.integer(topcauses[n, 2]))
        {
          TP1[z]<-TP1[z]+1
        }
        if(CodedCause==as.integer(topcauses[n, 3]))
        {
          second[z]<-second[z]+1
        }
        if(CodedCause==as.integer(topcauses[n, 4]))
        {
          third[z]<-third[z]+1
        }
        if(CodedCause==as.integer(topcauses[n, 5]))
        {
          fourth[z]<-fourth[z]+1
        }
        if(CodedCause==as.integer(topcauses[n, 6]))
        {
          fifth[z]<-fifth[z]+1
        }
      }
      
      if (z!=CodedCause)
      {
        Total_Neg_Cases[z]<-Total_Neg_Cases[z]+1
        
        if(z!=as.integer(topcauses[n, 2]))
        {
          TN1[z]<-TN1[z]+1
        }
        if(z!=as.integer(topcauses[n, 2]) & z!=as.integer(topcauses[n, 3]))
        {
          TN2[z]<-TN2[z]+1
        }
        if(z!=as.integer(topcauses[n, 2]) & z!=as.integer(topcauses[n, 3]) & z!=as.integer(topcauses[n, 4]))
        {
          TN3[z]<-TN3[z]+1
        }
        if(z!=as.integer(topcauses[n, 2]) & z!=as.integer(topcauses[n, 3]) & z!=as.integer(topcauses[n, 4]) & z!=as.integer(topcauses[n, 5]))
        {
          TN4[z]<-TN4[z]+1
        }
        if(z!=as.integer(topcauses[n, 2]) & z!=as.integer(topcauses[n, 3]) & z!=as.integer(topcauses[n, 4]) & z!=as.integer(topcauses[n, 5])&z!=as.integer(topcauses[n, 6]))
        {
          TN5[z]<-TN5[z]+1
        }
        
      }
      
      
    }
    n<-n+4
  }	
  print(cbind(i,sum(Total_Cases)))
  prob1<-sum(TP1[1:tCoD])
  prob2<-prob1+sum(second[1:tCoD])
  prob3<-prob2+sum(third[1:tCoD])
  prob4<-prob3+sum(fourth[1:tCoD])
  prob5<-prob4+sum(fifth[1:tCoD])
  PPV1<-prob1/i
  PPV2<-prob2/i
  PPV3<-prob3/i
  PPV4<-prob4/i
  PPV5<-prob5/i
  
  sensitivity1<-TP1/Total_Cases
  sensitivity2<-second/Total_Cases
  sensitivity2<-sensitivity1+sensitivity2
  sensitivity3<-third/Total_Cases
  sensitivity3<-sensitivity2+sensitivity3
  sensitivity4<-fourth/Total_Cases
  sensitivity4<-sensitivity3+sensitivity4
  sensitivity5<-fifth/Total_Cases
  sensitivity5<-sensitivity4+sensitivity5
  
  specificity1<-TN1/Total_Neg_Cases
  specificity2<-TN2/Total_Neg_Cases
  specificity3<-TN3/Total_Neg_Cases
  specificity4<-TN4/Total_Neg_Cases
  specificity5<-TN5/Total_Neg_Cases
  
  chance<-1/tCoD
  chance2<-1/tCoD
  CCC<-(sensitivity5/chance2)-1
  PCCC1<-((prob1/i)-chance)/(1-chance)
  PCCC2<-((prob2/i)-chance)/(1-chance)
  PCCC3<-((prob3/i)-chance)/(1-chance)
  PCCC4<-((prob4/i)-chance)/(1-chance)
  PCCC5<-((prob5/i)-chance)/(1-chance)
  
  print("Positive Predictive Value")
  print(c(prob1/i, prob2/i, prob3/i, prob4/i, prob5/i))
  print("Partial Chance Corrected Concordance")
  print(cbind(PCCC1, PCCC2, PCCC3, PCCC4, PCCC5))
  print("CCC")
  print(CCC)
  print("sensitivity")
  print(sensitivity5)
  print("specificiy")
  print(rbind(specificity1,specificity2,specificity3,specificity4,specificity5))	
  if(output==1)
  {
    if (replace==1)
    {
      unlink("RandomForestResults.csv")
    }
    yy<-file("C:/Documents and Settings/desaini/Desktop/CCVAComparison/RF Test Datasets/RandomForestMethodFinal/RandomForestResults.csv", "a")
    write.csv(rbind(trainrows, tCoD, i), yy, row.names=TRUE)
    write.csv(cbind(PPV1, PPV2, PPV3, PPV4, PPV5), yy, row.names="Positive Predictive Values")
    write.csv(cbind(PCCC1, PCCC2, PCCC3, PCCC4, PCCC5), yy, row.names="Partial Chance Corrected Concordance")
    write.csv(rbind(sensitivity5, specificity1,specificity2,specificity3,specificity4,specificity5), yy, row.names=TRUE)
    close(yy)
  }
  
  RFcsmf(CSMFest, tCoD, CoDAssigned, output)	
}
