



patient.file_2<- function(dl, no.patients="all", weeks=8){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  # no.patients is the number of patients that the function analyzes. If set to "all," it will loop through all patients. Otherwise it has to be a vector with a start number and an end number (e.g. c(5,10) to analyze patient visits 5 to 10)
  # weeks is an integer that specifies how many weeks make up an incident
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  pat.time.df<- as.data.frame(cbind(pat.vec,date.vec))
  distinct.pat.time.df<- distinct(pat.time.df)
  colnames(distinct.pat.time.df)<- c("uniPatID","TG_DateNum")
  
  col.names.vec<- colnames(dl[[1]])
  for(i in seq(2,l.dl)){
    col.names.vec<- c(col.names.vec,colnames(dl[[i]]))
  }
  col.names.vec<- levels(as.factor(col.names.vec))
  
  if(no.patients=="all"){
    beginning<- 1
    end<- dim(distinct.pat.time.df)[1]
  }else{
    beginning<- no.patients[1]
    end<- no.patients[2]
  }
  
  output<- matrix(0,nrow = dim(distinct.pat.time.df)[1], ncol = length(col.names.vec))
  colnames(output)<- col.names.vec
  output<- as.data.frame(output)
  output$uniPatID<- distinct.pat.time.df[,1]
  output$TG_DateNum<- distinct.pat.time.df[,2]
  
  # rows: distinct cases, columns: how many entries per case in each datataset
  number.of.entries<- matrix(0,nrow = dim(distinct.pat.time.df)[1], ncol = l.dl+2)
  number.of.entries[,1:2]<- as.matrix(distinct.pat.time.df[,1:2])
  
  ticker<- 1
  multiple.entries<- list()
  # add progress indicator
  for(i in seq(beginning,end)){
    visit<- as.vector(distinct.pat.time.df[i,])
    patient<- visit[1]
    date<- visit[2]
    for (j in seq(1,l.dl)) {
      file<- filter(dl[[j]],uniPatID==patient,TG_DateNum==date)
      no.entries<- dim(file)[1]
      number.of.entries[i,(2+j)]<- no.entries
      if(no.entries==1){
        matching.cols<- intersect(colnames(file),colnames(output))
        output[i,matching.cols]<- file[1,matching.cols] #check whether the df/matrix combination is a problem, if error occurs
      }else if(no.entries>1){
        multiple.entries[[ticker]]<- file
        ticker<- ticker+1
        # add at the right place AND add columns, if necessary
        # better: save table to list. or create a specific function for each dataset, what to do with the output.
        # Alt. method: merge datasets by patient! loop over each patient, merge all datasets, then combine the result! parallelize process if necessary. save result with save.image
      }
    }
  }
  return(list(output,number.of.entries,multiple.entries))
}

# fake.result<-patient.file_2(fake.dl)
# fake.dl<- list(faked.stamm,faked.diag,faked.icp)
# 
# # order of datasets: stamm, icp, diag
# fake.dl_2<- list(faked.stamm,faked.icp,faked.diag, fake.diab, fake.cov, fake.impf, fake.lab)
# tic()
# fake.result_2<-patient.file_2(fake.dl_2)
# toc()

#### list:
#### Stamm, Diag, icp2, Labor, PZN, Ueberweis, LU
#### 
#### Labor Spalte "Untersuchung" suche nach "KREA"; add dummies for whether it is within the norm
#### 
#### Überweisung nach: Pneumo, Radio, KH [Krankenhaus]
#### Bei Stamm: PKV/ GKV hinzufügen mithilfe von weiterer Tabelle
#### LU: Wurde LUFU gemacht?

labor.up<- Labor3|>select(uniPatID,TG_DateNum,Untersuchung,Wertnum,NormLB,NormUB) |> filter(Untersuchung=="KREA")
labor.up$KREA_unter<- as.numeric(labor.up$Wertnum<labor.up$NormLB)
labor.up$KREA_ueber<- as.numeric(labor.up$Wertnum>labor.up$NormUB)
labor.up<- labor.up|> select(uniPatID,TG_DateNum,KREA_unter,KREA_ueber)

Ueberweis.up<- Ueberweis3|> select(uniPatID,TG_DateNum,Uberw_Pneumo,Uberw_Radiol,Uberw_KH) |>
  filter(Uberw_Pneumo>0|Uberw_Radiol>0|Uberw_KH>0)

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  return(out)
}

stamm.up<- Stamm3
stamm.up$PKV<- IK2PKV(stamm.up$IK)
stamm.up<- stamm.up|> select(-PatID,-index_i,-IK,-Kasse)

ipc.up<- IPC23 |>select(uniPatID,TG_DateNum,AnamnTyp,ipc2)

diag.up<- Diag3 |> select(uniPatID,TG_DateNum,DiagTyp,icd10)

lu.up<- LU3|>select(uniPatID,TG_DateNum)

impf.up<- Impf3|> select(uniPatID,TG_DateNum,Influenza,starts_with("Cov"))

pzn.up<- PZN3 |> select(uniPatID,TG_DateNum,PZN) |> filter(TG_DateNum>0)

real.list<- list(stamm.up,ipc.up,diag.up,labor.up,Ueberweis.up,pzn.up,lu.up,impf.up)

#
real.results<- patient.file_2(real.list)
#
for (i in seq_along(real.list)) {
  if(i==1){
    pat.date.df<- real.list[[1]] |> select(uniPatID,TG_DateNum)
  }else{
    pat.date.df<- rbind(pat.date.df, real.list[[i]] |> select(uniPatID,TG_DateNum))
  }
}

dim(pat.date.df)
dim(distinct(pat.date.df))

################ for fake data

labor.up<- fake.lab|>select(uniPatID,TG_DateNum,Untersuchung,Wertnum,NormLB,NormUB) |> filter(Untersuchung=="KREA")
labor.up$KREA_unter<- as.numeric(labor.up$Wertnum<labor.up$NormLB)
labor.up$KREA_ueber<- as.numeric(labor.up$Wertnum>labor.up$NormUB)
labor.up<- labor.up|> select(uniPatID,TG_DateNum,KREA_unter,KREA_ueber)

Ueberweis.up<- faked.ueber|> select(uniPatID,TG_DateNum,Uberw_Pneumo,Uberw_Radiol,Uberw_KH) |>
  filter(Uberw_Pneumo>0|Uberw_Radiol>0|Uberw_KH>0)

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  return(out)
}

stamm.up<- faked.stamm
stamm.up$PKV<- IK2PKV(stamm.up$IK)
stamm.up<- stamm.up|> select(-PatID,-index_i,-IK,-Kasse)

ipc.up<- faked.icp |>select(uniPatID,TG_DateNum,AnamnTyp,ipc2)

diag.up<- faked.diag |> select(uniPatID,TG_DateNum,DiagTyp,icd10)

lu.up<- fake.lab|>select(uniPatID,TG_DateNum)

impf.up<- fake.impf|> select(uniPatID,TG_DateNum,Influenza,starts_with("Cov"))

pzn.up<- faked.pzn |> select(uniPatID,TG_DateNum,PZN) |> filter(TG_DateNum>0)

real.list<- list(a=stamm.up,b=ipc.up,c=diag.up,d=labor.up,e=Ueberweis.up,f=pzn.up,g=lu.up,h=impf.up)
listnames<- c("stamm.up","ipc.up","diag.up","labor.up","Ueberweis.up","pzn.up","lu.up","impf.up")
names(real.list)<- listnames
######################


list.collapse<- function(result.list){
  goal<- result.list[[1]]
  merge.basis<- as.vector(result.list[[2]][,-c(1,2)])
  df.list<- result.list[[3]]
  
  # create assistant matrizes to store the result
  ass.mat.icp<- matrix(ncol = 5)
  colnames(ass.mat.icp)<- c("ICP_1","ICP_2","ICP_3","ICP_4","ICP_5",)
  
  ass.mat.diag<- 
    
    ticker<- 1
  for (i in seq_along(merge.basis)) {
    if (merge.basis[i]>1) {
      coords<- vector.pos2matrix.coords(i, dim(merge.basis))
      if (coords[2]==1) {
        stop("Duplicate entry in the Stammdaten!")
      }else{
        df<- df.list[[ticker]]
        ticker<- ticker+1
        check.matchability<- isTRUE(all.equal(df[,"uniPatID"])) & isTRUE(all.equal(df[,"PatID"])) & isTRUE(all.equal(df[,"TG_DateNum"])) & isTRUE(all.equal(df[,"PraxisID"]))
        if(!check.machability){
          stop(paste("The uniPatID, PatID or PraxisID of the entries to be merged do not correspond. Check result.list[[3]][[", ticker-1,"]] to find the problem."))
        }
        if(coords[2]==2){
          
          if(merge.basis[i]>5) stop("The number of symptoms exceeds 5 for result.list[[3]][[", ticker-1,"]]. Change the number of columns in ass.mat.icp. Optionally, reevaluate whether the number of symptoms is correct.")
          new.row<- numeric(5)
          replacement<- as.vector(df[,"ipc2"])
          new.row[1:length(replacement)]<- replacement
          ass.mat.icp<- rbind(ass.mat.icp,new.row)
          
          #make some entry on goal such that it is clear that the result must be searched for in ass.mat.icp. Alternatively, make the reference already in the patient.file function.
          
        }
        
        if(coords[2]==3){
          
        }
        
      }
    }
  }
}

# patient.merge.function<- function(dl){
#   # dl is a list of dataframes that pertain only to a single patient
#   # order of dfs of dl: stammdaten, icp, diag
#   
#   l.dl<- length(dl)
#   dates.vec<- as.numeric(levels(as.factor(dl[[1]]$TG_DateNum)))
#   for(i in seq(2,l.dl)){
#     dates<- union(dates.vec,dl[[i]]$TG_DateNum)
#   }
#   dates.vec<- sort(dates.vec)
#   
# }





# Create an empty output data frame
output_df <- data.frame(d = character(),
                        u = character(),
                        stringsAsFactors = FALSE)


for (i in 1:nrow(im)) {
  d_value <- im$TG_DateNum[i]
  u_value <- im$uniPatID[i]
  
  # Filter the list of data frames ('dl') based on the current 'd' and 'u' combination
  filtered_data <- lapply(dl, function(df) {
    subset(df, d == d_value & u == u_value)
  })
  
  # Combine the filtered data frames into a single data frame
  combined_df <- do.call(rbind, filtered_data)
  
  # Rename duplicate variables with a suffix
  duplicate_cols <- duplicated(names(combined_df))
  duplicate_counts <- tabulate(match(names(combined_df), names(combined_df[duplicate_cols])))
  duplicate_suffix <- ifelse(duplicate_counts > 1, duplicate_counts, "")
  names(combined_df)[duplicate_cols] <- paste(names(combined_df[duplicate_cols]), duplicate_suffix, sep = ".")
  
  # Add the 'd' and 'u' combination to the combined data frame
  combined_df$d <- d_value
  combined_df$u <- u_value
  
  # Append the combined data frame to the output data frame
  output_df <- rbind(output_df, combined_df)
}

# Reset row names of the output data frame
rownames(output_df) <- NULL

# Print the resulting output data frame
print(output_df)





a<- 1:5
b<- 2:11
c<- 5:9
d<- list(a,b,c)

do.call(rbind, d)

g<- c("var1","var2","var3")
f<- list()
for(i in 1:3){
  for(j in 1:3){
    h<- paste0("f$",)
    f[[i]]$var1
  }
}


###############################from dektop pc
###############################






patient.file_2<- function(dl, no.patients="all", weeks=8){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  # no.patients is the number of patients that the function analyzes. If set to "all," it will loop through all patients. Otherwise it has to be a vector with a start number and an end number (e.g. c(5,10) to analyze patient visits 5 to 10)
  # weeks is an integer that specifies how many weeks make up an incident
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  pat.time.df<- as.data.frame(cbind(pat.vec,date.vec))
  distinct.pat.time.df<- distinct(pat.time.df)
  colnames(distinct.pat.time.df)<- c("uniPatID","TG_DateNum")
  
  col.names.vec<- colnames(dl[[1]])
  for(i in seq(2,l.dl)){
    col.names.vec<- c(col.names.vec,colnames(dl[[i]]))
  }
  col.names.vec<- levels(as.factor(col.names.vec))
  
  if(no.patients=="all"){
    beginning<- 1
    end<- dim(distinct.pat.time.df)[1]
  }else{
    beginning<- no.patients[1]
    end<- no.patients[2]
  }
  
  output<- matrix(0,nrow = dim(distinct.pat.time.df)[1], ncol = length(col.names.vec))
  colnames(output)<- col.names.vec
  output<- as.data.frame(output)
  output$uniPatID<- distinct.pat.time.df[,1]
  output$TG_DateNum<- distinct.pat.time.df[,2]
  
  # rows: distinct cases, columns: how many entries per case in each datataset
  number.of.entries<- matrix(0,nrow = dim(distinct.pat.time.df)[1], ncol = l.dl+2)
  number.of.entries[,1:2]<- as.matrix(distinct.pat.time.df[,1:2])
  
  ticker<- 1
  multiple.entries<- list()
  # add progress indicator
  for(i in seq(beginning,end)){
    visit<- as.vector(distinct.pat.time.df[i,])
    patient<- visit[1]
    date<- visit[2]
    for (j in seq(1,l.dl)) {
      file<- filter(dl[[j]],uniPatID==patient,TG_DateNum==date)
      no.entries<- dim(file)[1]
      number.of.entries[i,(2+j)]<- no.entries
      if(no.entries==1){
        matching.cols<- intersect(colnames(file),colnames(output))
        output[i,matching.cols]<- file[1,matching.cols] #check whether the df/matrix combination is a problem, if error occurs
      }else if(no.entries>1){
        multiple.entries[[ticker]]<- file
        ticker<- ticker+1
        # add at the right place AND add columns, if necessary
        # better: save table to list. or create a specific function for each dataset, what to do with the output.
        # Alt. method: merge datasets by patient! loop over each patient, merge all datasets, then combine the result! parallelize process if necessary. save result with save.image
      }
    }
  }
  return(list(output,number.of.entries,multiple.entries))
}

# fake.result<-patient.file_2(fake.dl)
# fake.dl<- list(faked.stamm,faked.diag,faked.icp)
# 
# # order of datasets: stamm, icp, diag
# fake.dl_2<- list(faked.stamm,faked.icp,faked.diag, fake.diab, fake.cov, fake.impf, fake.lab)
# tic()
# fake.result_2<-patient.file_2(fake.dl_2)
# toc()

#### list:
#### Stamm, Diag, icp2, Labor, PZN, Ueberweis, LU
#### 
#### Labor Spalte "Untersuchung" suche nach "KREA"; add dummies for whether it is within the norm
#### 
#### Überweisung nach: Pneumo, Radio, KH [Krankenhaus]
#### Bei Stamm: PKV/ GKV hinzufügen mithilfe von weiterer Tabelle
#### LU: Wurde LUFU gemacht?

labor.up<- Labor3|>select(uniPatID,TG_DateNum,Untersuchung,Wertnum,NormLB,NormUB) |> filter(Untersuchung=="KREA")
labor.up$KREA_unter<- as.numeric(labor.up$Wertnum<labor.up$NormLB)
labor.up$KREA_ueber<- as.numeric(labor.up$Wertnum>labor.up$NormUB)
labor.up<- labor.up|> select(uniPatID,TG_DateNum,KREA_unter,KREA_ueber)

Ueberweis.up<- Ueberweis3|> select(uniPatID,TG_DateNum,Uberw_Pneumo,Uberw_Radiol,Uberw_KH) |>
  filter(Uberw_Pneumo>0|Uberw_Radiol>0|Uberw_KH>0)

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  return(out)
}

stamm.up<- Stamm3
stamm.up$PKV<- IK2PKV(stamm.up$IK)
stamm.up<- stamm.up|> select(-PatID,-index_i,-IK,-Kasse)

ipc.up<- IPC23 |>select(uniPatID,TG_DateNum,AnamnTyp,ipc2)

diag.up<- Diag3 |> select(uniPatID,TG_DateNum,DiagTyp,icd10)

lu.up<- LU3|>select(uniPatID,TG_DateNum)

impf.up<- Impf3|> select(uniPatID,TG_DateNum,Influenza,starts_with("Cov"))

pzn.up<- PZN3 |> select(uniPatID,TG_DateNum,PZN) |> filter(TG_DateNum>0)

real.list<- list(stamm.up,ipc.up,diag.up,labor.up,Ueberweis.up,pzn.up,lu.up,impf.up)

#
real.results<- patient.file_2(real.list)
#
for (i in seq_along(real.list)) {
  if(i==1){
    pat.date.df<- real.list[[1]] |> select(uniPatID,TG_DateNum)
  }else{
    pat.date.df<- rbind(pat.date.df, real.list[[i]] |> select(uniPatID,TG_DateNum))
  }
}

dim(pat.date.df)
dim(distinct(pat.date.df))

################ for fake data

# read in fake data on home desktop
standardwd<- getwd()
setwd("C:/Users/Raphael (limited)/Downloads/RS_DB_v02")

names.vec<- c("Cov3","Diab3","Diag3","DMPChron3","EBM3","EKG3","Impf3","IPC23","Konsul3","Kuerzel3","Labor3","LU3","PZN3","Stamm3","Time3","Ueberweis3","Unters3")


for (i in seq(1,length(names.vec))) {
  file.name<- paste0("RS_","DB_",names.vec[i],"_v02",".csv")
  im.file<- read.csv(file = file.name, header = TRUE, sep = ";")
  
  ass.name<- paste0("fake.",names.vec[i])
  assign(ass.name,im.file)
  
}
setwd(standardwd)

#adapted for home use
labor.up<- fake.Labor3|>select(uniPatID,TG_DateNum,Untersuchung,Wertnum,NormLB,NormUB) |> filter(Untersuchung=="KREA")
labor.up$KREA_unter<- as.numeric(labor.up$Wertnum<labor.up$NormLB)
labor.up$KREA_ueber<- as.numeric(labor.up$Wertnum>labor.up$NormUB)
labor.up<- labor.up|> select(uniPatID,TG_DateNum,KREA_unter,KREA_ueber)

Ueberweis.up<- fake.Ueberweis3|> select(uniPatID,TG_DateNum,Uberw_Pneumo,Uberw_Radiol,Uberw_KH) |>
  filter(Uberw_Pneumo>0|Uberw_Radiol>0|Uberw_KH>0)

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  return(out)
}

stamm.up<- fake.Stamm3
stamm.up$PKV<- IK2PKV(stamm.up$IK)
stamm.up<- stamm.up|> select(-PatID,-index_i,-IK,-Kasse)

ipc.up<- fake.IPC23 |>select(uniPatID,TG_DateNum,AnamnTyp,ipc2)

diag.up<- fake.Diag3 |> select(uniPatID,TG_DateNum,DiagTyp,icd10)

lu.up<- fake.LU3|>select(uniPatID,TG_DateNum)

impf.up<- fake.Impf3|> select(uniPatID,TG_DateNum,Influenza,starts_with("Cov"))

pzn.up<- fake.PZN3 |> select(uniPatID,TG_DateNum,PZN) |> filter(TG_DateNum>0)

real.list<- list(a=stamm.up,b=ipc.up,c=diag.up,d=labor.up,e=Ueberweis.up,f=pzn.up,g=lu.up,h=impf.up)
listnames<- c("stamm.up","ipc.up","diag.up","labor.up","Ueberweis.up","pzn.up","lu.up","impf.up")
names(real.list)<- listnames
######################


list.collapse<- function(result.list){
  goal<- result.list[[1]]
  merge.basis<- as.vector(result.list[[2]][,-c(1,2)])
  df.list<- result.list[[3]]
  
  # create assistant matrizes to store the result
  ass.mat.icp<- matrix(ncol = 5)
  colnames(ass.mat.icp)<- c("ICP_1","ICP_2","ICP_3","ICP_4","ICP_5",)
  
  ass.mat.diag<- 
    
    ticker<- 1
  for (i in seq_along(merge.basis)) {
    if (merge.basis[i]>1) {
      coords<- vector.pos2matrix.coords(i, dim(merge.basis))
      if (coords[2]==1) {
        stop("Duplicate entry in the Stammdaten!")
      }else{
        df<- df.list[[ticker]]
        ticker<- ticker+1
        check.matchability<- isTRUE(all.equal(df[,"uniPatID"])) & isTRUE(all.equal(df[,"PatID"])) & isTRUE(all.equal(df[,"TG_DateNum"])) & isTRUE(all.equal(df[,"PraxisID"]))
        if(!check.machability){
          stop(paste("The uniPatID, PatID or PraxisID of the entries to be merged do not correspond. Check result.list[[3]][[", ticker-1,"]] to find the problem."))
        }
        if(coords[2]==2){
          
          if(merge.basis[i]>5) stop("The number of symptoms exceeds 5 for result.list[[3]][[", ticker-1,"]]. Change the number of columns in ass.mat.icp. Optionally, reevaluate whether the number of symptoms is correct.")
          new.row<- numeric(5)
          replacement<- as.vector(df[,"ipc2"])
          new.row[1:length(replacement)]<- replacement
          ass.mat.icp<- rbind(ass.mat.icp,new.row)
          
          #make some entry on goal such that it is clear that the result must be searched for in ass.mat.icp. Alternatively, make the reference already in the patient.file function.
          
        }
        
        if(coords[2]==3){
          
        }
        
      }
    }
  }
}










# remove all unnecessary nonsense from the environment before running this code. For coding convenience I export the work environment to all cores in the parallelisation process. 
par.patient.file<- function(dl){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  pat.time.df<- as.data.frame(cbind(pat.vec,date.vec))
  distinct.pat.time.df<- distinct(pat.time.df)
  colnames(distinct.pat.time.df)<- c("uniPatID","TG_DateNum")
  
  
  browser()
  
  # # parallel processing
  # no.cores<- detectCores()-1
  # merger.cluster<- makeCluster(no.cores)
  # clusterExport(cl = merger.cluster, varlist = "data_list")
  # result.list<- parSapply(cl= merger.cluster, seq_along(dl), patient.file.internal)
  # stopCluster(merger.cluster)
  # 
  # # putting the results together
  # im<- sapply(result.list, cbind)
  # out<- as.data.frame(cbind(distinct.pat.time.df,im))
  # 
  # return(out)
}

par.patient.file<- function(dl){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  pat.time.df<- as.data.frame(cbind(pat.vec,date.vec))
  distinct.pat.time.df<- distinct(pat.time.df)
  colnames(distinct.pat.time.df)<- c("uniPatID","TG_DateNum")
  
  
  # parallel processing
  
  result.list<- sapply(seq_along(dl), patient.file.internal)
  
  
  # no.cores<- detectCores()-1
  # merger.cluster<- makeCluster(no.cores)
  # distinct_env <- environment()  # Change this to the correct environment
  # clusterExport(cl = merger.cluster, varlist = c("distinct.pat.time.df", "dl"), envir = distinct_env)
  # result.list<- parSapply(cl= merger.cluster, seq_along(dl), patient.file.internal)
  # stopCluster(merger.cluster)
  
  # putting the results together
  im<- sapply(result.list, cbind)
  out<- as.data.frame(cbind(distinct.pat.time.df,im))
  
  return(out)
}


patient.file.internal<- function(i){
  df <- dl[[i]]
  n_rows <- nrow(distinct.pat.time.df)
  n_cols <- ncol(df)
  modified.df <- matrix(NA, nrow = n_rows, ncol = n_cols)
  
  lookup <- with(df, interaction(uniPatID, TG_DateNum, drop = TRUE))
  
  for (j in seq_len(n_rows)) {
    u <- distinct.pat.time.df$uniPatID[j]
    t <- distinct.pat.time.df$TG_DateNum[j]
    
    match.index <- match(interaction(u, t, drop = TRUE), lookup)
    
    if (!is.na(match.index)) {
      modified.df[j, ] <- df[match.index, ]
    }
  }
  
  return(modified.df)
}

#attempt with parallelized loop
merged.list<- par.patient.file(real.list)



modifyDataFramesParallel <- function(A, L) {
  library(parallel)
  
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("A", "L"))
  
  result <- parSapply(cl, seq_along(L), function(i) {
    df <- L[[i]]
    n_rows <- nrow(A)
    n_cols <- ncol(df)
    modified_df <- matrix(NA, nrow = n_rows, ncol = n_cols)
    
    lookup <- with(df, interaction(U, T, drop = TRUE))
    
    for (j in seq_len(n_rows)) {
      u <- A$U[j]
      t <- A$T[j]
      
      match_idx <- match(interaction(u, t, drop = TRUE), lookup)
      
      if (!is.na(match_idx)) {
        modified_df[j, ] <- df[match_idx, ]
      }
    }
    
    as.data.frame(modified_df)
  })
  
  stopCluster(cl)
  
  return(result)
}


