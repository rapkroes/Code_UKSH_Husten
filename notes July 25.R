# notes July 25th


#no. 12
multiple.entry.analysis<- function(df){
  #like contains.multiple.entries, but returns instead diagnostics how many entries are somewhat duplicate
  im<- as.factor(paste0(df$uniPatID, df$TG_DateNum))
  v<- c(length(levels(im)),nrow(df),nrow(df)-length(levels(im)),1-length(levels(im))/nrow(df))
  names(v)<- c("unique name/date combinations", "number of rows in data set", "difference","relative difference")
  return(v)
}



im<- paste(stamm.up$uniPatID, stamm.up$TG_DateNum)
im_2<- matrix(c(stamm.up$uniPatID, stamm.up$TG_DateNum),ncol = 2)[duplicated(im),]
colnames(im_2)<- c("uniPatID","TG_DateNum")
stamm.analysis<- function(i){
  pat<- im_2[i,1]
  date<- im_2[i,2]
  data<- Stamm3|>
    filter(uniPatID==pat,TG_DateNum==date)
  return(data)
}
stamm.analysis(1)
stamm.analysis(2)
stamm.analysis(3)

data.repair<- function(problemdata){
  # For data sets which contain multiple entries for identical patient/ date combinations. Selects the newest entry of the multiple cases and removes the other one(s).
  pdv<- paste(problemdata$uniPatID,problemdata$TG_DateNum)
  pat.date.matrix<- matrix(c(problemdata$uniPatID,problemdata$TG_DateNum), ncol = 2)[duplicated(pdv),]
  browser()
  n<- length(pdv)
  selector<- seq(n,1)[!duplicated(pdv)]
  selector<- selector[length(selector):1]
  
  fixed.data<- problemdata[selector,]
  return(fixed.data)
}

nrow(stamm.up)

abc<- data.repair(stamm.up)
nrow(abc)
nrow(stamm.up)-nrow(abc)



contains.multiple.entries<- function(df){
  #checks whether a data frame or matrix contains multiple entries for uniPatID/TG_DateNum combinations.
  im<- unique(paste0(df$uniPatID, df$TG_DateNum))
  length(im)!=nrow(df)
}

data.repair<- function(problemdata){
  # For data sets which contain multiple entries for identical patient/ date combinations. Selects the newest entry of the multiple cases and removes the other one(s).
  n<- nrow(problemdata)
  pdv<- paste(problemdata$uniPatID,problemdata$TG_DateNum)
  selector<-seq(n,1)[!duplicated(pdv)]
  return(problemdata[selector,])
}
nrow(stamm.up)
nrow(data.repair(stamm.up))
contains.multiple.entries(data.repair(stamm.up))
stamm.up<- data.repair(stamm.up)


data.repair<- function(problemdata){
  n<- nrow(problemdata)
  v<- numeric(n)
  selector<- rep(1,n)
  for (i in seq(n,1)) {
    if(!(paste0(problemdata$uniPatID[i],problemdata$TG_DateNum[i])%in% v)){
      v[i]<- paste0(problemdata$uniPatID[i],problemdata$TG_DateNum[i])
    }else{
      selector[i]<- 0
    }
  }
  selector<- selector[length(selector):1]
  fixed.data<- problemdata[as.logical(selector),]
}


