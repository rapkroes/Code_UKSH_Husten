#################### functions #########################
#### contained functions:
#no.1 merge.datasets: merges datasets by patient ID (uniPatID) and date of consultation (TG_DateNum) [largely obsolete]
#no.2 query.data: selects all relevant incidents with a certain ICD10 code and possibly other restrictions [largely obsolete]
#no.3 icd10.to.chapter: maps a vector of ICD10 codes to the chapter each code corresponds to. Output is a numeric vector of the chapter number.
#no.4 code2num: converts a vector of ICD10 codes to numbers that correspond to subchapters. code2num is a necessary accessory function for no.5 icd10.to.subchapter
#no.5 icd10.to.subchapter: converts a vector of raw ICD10 codes to their respective subchapters
#no.6 new.plotter_2: Saves a plot to the working directory. The plot that is saved depends on ICD10 codes and may be specified in greater detail.
#no.7 par.patient.file: A parallelised, memory efficient loop that merges data sets from a list of datasets.
#no.8 IK2PKV: Takes a vector of IK codes (code individual to a health insurance company in Germany) and returns a dummy vector which specifies whether the individual helath insurance is a private health insurer.
#no.9 contains.multiple.entries: Checks whether a data frame or matrix contains multiple entries for uniPatID/TG_DateNum combinations. If it returns TRUE, there are multiple entries for at least one combination in that dataset.
#no.10 
#no.11 TG_DateNum2date: takes a vector of TG_DateNum dates and converts them to readable dates in a year-month-day fashion
#no. 12 multiple.entry.analysis:Like contains.multiple.entries, but returns instead diagnostics how many entries are somewhat duplicate
#no.13 data.repair: For data sets which contain multiple entries for identical patient/ date combinations. Selects the newest entry of the multiple episodes and removes the other one(s).
#no. 15 chunk.data: Splits a data frame into several data frames. They are returned as a list. The data is split by the uniPatID variable.
# no. 16 chunk.data_date: Splits a data frame into several data frames. They are returned as a list. The data is split by the TG_DateNum variable.
# no. 17 episode.fun: Uses an ipc data frame to create a data frame of episodes.
# no. 18 episodes.dr1: Applies decision rule 1 to a raw (i.e. containing overlapping episodes) episode data frame.
# no. 19 episodes.dr2: Applies decision rule 2 to a raw (i.e. containing overlapping episodes) episode data frame.
# no. 20 episodes.dr3: Applies decision rule 3 to a raw (i.e. containing overlapping episodes) episode data frame.
# no. 21 episodes.dr4: Applies decision rule 4 to a raw (i.e. containing overlapping episodes) episode data frame.
# no. 22 episodes.dr5: Applies decision rule 5 to a raw (i.e. containing overlapping episodes) episode data frame.
# no. 23 add.stamm: adds stamm data to the episode data frame by matching entries.
# no. 24 add.dataset: matches the entries from any suitable dataset to the ones from an episode data frame.
# no. 25 date2TG_DateNum: transforms a date (yyyy-mm-dd) into a Matlab Serial time code (like TG_DateNum)
# no. 26 age.weights: Uses data from Genesis data base to create a vector of points of mass from a probability density function of the live age distribution in Germany
# # no. 27 episodes.overlap.finder: finds overlapping intervals in episode data frames. Used as a debugging function.

#no.1
merge.datasets <- function(dfs, na.rm=TRUE) {
  # dfs is a list (!) of data frames which are to be merged
  # na.rm determines whether incomplete rows are to be omitted in the output data frame (by default they are omitted)
  # depends on libraries dplyr and purrr
  
  intermediate<- reduce(dfs, full_join, by = c("uniPatID", "TG_DateNum"))|>
    select(uniPatID,PatID.x,TG_DateNum,index_i.x,PraxisID.x,DiagTyp,icd10,AnamnTyp,ipc2,ipc2txt)
  colnames(intermediate)<- c("uniPatID",
                             "PatID",
                             "TG_DateNum",
                             "index_i",
                             "PraxisID",
                             "DiagTyp",
                             "icd10",
                             "AnamnTyp",
                             "ipc2",
                             "ipc2txt")
  if(na.rm==TRUE){
    final<- intermediate[complete.episodes(intermediate),]
  }else{
    final<- intermediate
  }
  
  return(final)
}

# no. 2
query.data<- function(data, ipc2.code,
                      date.range=FALSE, 
                      PraxisID=FALSE, 
                      AnamnTyp=FALSE, 
                      DiagTyp=FALSE){
  # data: the data frame with all observations
  # ipc2.code: the code of the anamnese
  # date.range: a two element vector with the first and the last date to be considered
  # AnamnTyp: a specified anamnese code
  # DiagTyp: a specified code for type of diagnosis
  
  data.out<- filter(data, ipc2==ipc2.code)
  if(date.range!=FALSE) data.out<- filter(data.out,TG_DateNum>=date.range[1],TG_DateNum<=date.range[2])
  if(PraxisID!=FALSE) data.out<- filter (data.out,PraxisID==PraxisID)
  if(AnamnTyp!=FALSE) data.out<- filter(data.out,AnamnTyp==AnamnTyp)
  if(DiagTyp!=FALSE) data.out<- filter(data.out, DiagTyp==DiagTyp)
  return(data.out)
}

# no.3
icd10.to.chapter<- function(vector){
  #maps a vector of icd10 codes to their respective chapters
  output<- rep(0,length(vector))
  int.fun_1<- function(a) substr(a,1,1)
  int.fun_2<- function(a) substr(a,2,2)
  int.fun_3<- function(a) substr(a,3,3)
  first<- sapply(vector,FUN=int.fun_1)
  names(first)<- NULL
  second<- as.numeric(sapply(vector,FUN=int.fun_2))
  third<- as.numeric(sapply(vector,FUN=int.fun_3))
  
  for(i in 1:length(vector)){
    # browser()
    if(first[i]=="A"|first[i]=="B"){
      output[i]<- 1
    }else if(first[i]=="C"|first[i]=="D"){
      if(first[i]=="D"& second[i]>=5){
        output[i]<- 3
      }else{
        output[i]<- 2
      }
    }else if(first[i]=="E"){
      output[i]<-4
    }else if(first[i]=="F"){
      output[i]<- 5
    }else if(first[i]=="G"){
      output[i]<-6
    }else if(first[i]=="H"){
      if(second[i]>=6){
        output[i]<- 8
      }else{
        output[i]<- 7
      }
    }else if(first[i]=="I"){
      output[i]<-9
    }else if(first[i]=="J"){
      output[i]<- 10
    }else if(first[i]=="K"){
      output[i]<- 11
    }else if(first[i]=="L"){
      output[i]<- 12
    }else if(first[i]=="M"){
      output[i]<- 13
    }else if(first[i]=="N"){
      output[i]<- 14 
    }else if(first[i]=="O"){
      output[i]<-15
    }else if(first[i]=="P"){
      output[i]<- 16
    }else if(first[i]=="Q"){
      output[i]<- 17
    }else if(first[i]=="R"){
      output[i]<- 18
    }else if(first[i]=="S"|first[i]=="T"){
      output[i]<- 19
    }else if(first[i]=="V"|first[i]=="W"|first[i]=="X"|first[i]=="Y"){
      output[i]<- 20
    }else if(first[i]=="Z"){
      output[i]<-21
    }else if(first[i]=="U"){
      output[i]<- 22
    }else{
      stop(paste("The function is not able to map entry no.", i,"=",vector[i], "to a chapter."))
    }
  }
  out<- cbind(vector,output)
  return(out)
}

# no. 4
code2num<- function(dv){
  n<- length(dv)
  output<- numeric(n)
  for(i in seq(1,n)){
    l<- which(substr(dv[i],1,1)==LETTERS)
    output[i]<- 100*l+as.numeric(substr(dv[i],2,3))
  }
  return(output)
}

# no. 5 DYSFUNCT
icd10.to.subchapter<- function(vector){
  # converts a vector of raw ICD10 codes to their respective subchapters
  # depends on the self-written function 'code2num,' and also on the file icd_blocks.csv (named without .csv)
  
  n<- length(vector)
  im_1<- code2num(vector)
  im_2<- numeric(n)
  im_3<- numeric(n)
  for(i in seq(1,n)){
    im_2[i]<- sum(Table_ICD_Group$ICD_Group_begNum<im_1[i])
    im_3[i]<- Table_ICD_Group$ICD_Group_Title[im_2[i]]
  }
  output<- cbind(im_2,im_3)
  colnames(output)<- c("group_number","group_title")
  return(output)
  
}
# no. 5 b
# icd10.to.block<- function(vector){
#   n<- length(vector)
#   no<- numeric(n)
#   title<- numeric(n)
#   vector.num<- code2num(vector)
#   startcode<- code2num(icd_blocks$startcode)
#   endcode<- code2num(icd_blocks$endcode)
#   for(i in seq_along(no)){
#     loc<- which(vector.num[i]>=startcode & vector.num[i]<=endcode)
#     no[i]<- loc
#     title[i]<- icd_blocks$description[loc]
#   }
#   return(cbind(no,title))
# }
# no. 5 c
icd10.to.block<- function(vector, debug=FALSE){
  n<- length(vector)
  number<- numeric(n)
  title<- numeric(n)
  if(debug==TRUE) error<- numeric(n); errorno<- 1
  for(i in seq(1,n)){
    im<- vector[i]
    im_2<-substr(im,1,1)
    im_3<- as.numeric(substr(im,2,3))
    if(im_2 %in% c("V","W","X","Y")){
      if(im_2=="V"|im_2=="W"){
        loc<- 223
        number[i]<- loc
        title[i]<- icd10_blocks$description[loc]
      }else if(im_2=="X"){
        if(im_3<=59){
          loc<- 223
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else if(im_3>59 & im_3<=84){
          loc<- 224
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else{
          loc<- 225
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }
      }else if(im_2=="Y"){
        if(im_3<=9){
          loc<- 225
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else{
          im_4<- im_2==icd10_blocks$start_code_letter
          im_5<- im_3>=icd10_blocks$start_code_no & im_3<=icd10_blocks$end_code_no
          loc<- which(im_4 & im_5)
          if(length(loc)==0){
            stop(paste("No corresponding row found for",im))
            error[errorno]<- im
            errorno<- errorno+1
          }else if(length(loc)>1){
            stop(paste("No corresponding row found for",im, ". The rows found for this code are", loc))
            error[errorno]<- im
            errorno<- errorno+1
          }
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }
      }
    }else{
      im_4<- im_2==icd10_blocks$start_code_letter
      im_5<- im_3>=icd10_blocks$start_code_no & im_3<=icd10_blocks$end_code_no
      loc<- which(im_4 & im_5)
      if(length(loc)>0){
        number[i]<- loc
        title[i]<- icd10_blocks$description[loc]
      }
      
    }
  }
  out<- data.frame(number=number,title=title)
  if(debug==TRUE) out$error<- error
  return(out)
}
# no. 6
new.plotter_2<- function(data, plot.type){
  # data is the data frame which yields the data. It needs a column entitled 'icd10'
  # plot.type determines what data are analyzed. It may be either by code segment (e.g. R05), chapter (i.e. distribution across chapters) or subchapters. For subchapters, set the plot.type argument equal to the chapter number to be analyzed.
  # Note that this function depends on the ggplot2 library and the self-written function 'icd10.to.chapter'
  
  sc.insert<- "" #for later naming of the plot
  if(plot.type=="chapter"){
    dv<- as.numeric(icd10.to.chapter(data$icd10)[,2])
    
  }else if(is.character(plot.type) & plot.type!="chapter"){
    # part for code segments, e.g. R05
    n<- dim(data)[1]
    l<- nchar(plot.type)
    a<- 0
    if(l==3) a<- 1
    im_1<- numeric(n)
    im_2<- im_1
    for(i in seq(1,n)){
      im_1[i]<- substr(data$icd10[i],1,(l+1+a))
      im_2[i]<- substr(data$icd10[i],1,l)
    }
    dv<- as.factor(im_1[im_2==plot.type])
    
  }else if(is.numeric(plot.type)){
    # insert subchapter here
    im_1<- as.numeric(icd10.to.chapter(data$icd10)[,2])
    im_2<- data$icd10[im_1==plot.type]
    dv<- as.factor(icd10.to.subchapter(im_2)[,1])
    
    sc.insert<- "subgroups of chapter "
    
  }else{
    stop("The plot.type argument is misspecified. It is neither chapter, chapter number nor an ICD10 code segment.")
  }
  
  dt<- as.data.frame(table(dv))
  colnames(dt)<- c("code.segment","Freq")
  
  plot <- ggplot(dt, aes(x = reorder(code.segment, -Freq), y = Freq/sum(Freq))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Distribution of Diagnosis by",sc.insert,plot.type), x = "ICD10 category", y = "Probability") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + # Add units to margin values
    scale_y_continuous(labels = scales::percent_format(1, scale = 100)) + # Scale y-axis to represent density
    geom_text(aes(label = ""), vjust = -1) # Remove letters above each bar
  
  ggsave(paste0("distribution_",as.character(substitute(data)),"_icd10_",plot.type,".png"), plot, width = 10, height = 6)
}

# no.7
par.patient.file<- function(dl){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  # Depends on dplyr and parallel libraries
  
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq_along(dl)){
    pat.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,ticker+dimensions[i,1]-1)]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  pat.time.df<- as.data.frame(cbind(pat.vec,date.vec))
  distinct.pat.time.df<- distinct(pat.time.df)
  colnames(distinct.pat.time.df)<- c("uniPatID","TG_DateNum")
  
  # parallel processing
  no.cores<- detectCores()-1
  merger.cluster<- makeCluster(no.cores)
  distinct_env <- environment()  # Change this to the correct environment
  clusterExport(cl = merger.cluster, varlist = c("distinct.pat.time.df", "dl"), envir = distinct_env)
  result.list<- parSapply(cl= merger.cluster, seq_along(dl), function(i){
    df <- dl[[i]]
    n_rows <- nrow(distinct.pat.time.df)
    n_cols <- ncol(df)
    modified.df <- matrix(NA, nrow = n_rows, ncol = n_cols)
    colnames(modified.df)<- colnames(df)
    
    # merge the information content of uniPatID and TG_DateNum to one vector
    lookup <- with(df, interaction(uniPatID, TG_DateNum, drop = TRUE))
    
    for (j in seq_len(n_rows)) {
      u <- distinct.pat.time.df$uniPatID[j]
      t <- distinct.pat.time.df$TG_DateNum[j]
      
      match.index <- match(interaction(u, t, drop = TRUE), lookup)
      
      if (!is.na(match.index)) {
        modified.df[j, ] <- as.matrix(df[match.index, ])
      }
    }
    
    # Drop uniPatID and TG_DateNum columns
    modified.df<- modified.df[,colnames(modified.df)!="uniPatID"&colnames(modified.df)!="TG_DateNum"]
    
    return(modified.df)
  })
  stopCluster(merger.cluster)
  
  
  
  
  # putting the results together
  im<- sapply(result.list, cbind)
  out<- as.data.frame(cbind(distinct.pat.time.df,im))
  # colnames(out)<- c("uniPatID","TG_DateNum",col.names.vec)
  
  return(out)
}

# no.8
IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes; 0=publich health insurance, 1=private health insurance or no health insurance
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  out[is.na(out)]<- 1
  return(out)
}

# no.9
contains.multiple.entries<- function(df){
  #checks whether a data frame or matrix contains multiple entries for uniPatID/TG_DateNum combinations.
  length(levels(as.factor(paste(df$uniPatID, df$TG_DateNum))))!=nrow(df)
}

# no.10
ipc.filter <- function(ipcdata) {
  # create factor 'index' to find distinct episodes and their frequency of occurrence
  index<- as.factor(paste0(ipcdata$uniPatID,ipcdata$TG_DateNum))
  loop.row.selector<- !duplicated(index)
  
  count<- numeric(nrow(ipcdata))
  for(i in seq(1,nrow(ipcdata))){
    count[i]<- sum(index==index[i])
  }
  
  out1<- ipcdata
  
  red.index<- levels(index)
  n_loop<- length(red.index)
  
  
  # create out2
  n <- max(count)  # Highest number of occurrences
  p <- ncol(ipcdata)  # Number of columns in ipcdata
  y<- sum(loop.row.selector & count>1)
  out2 <- matrix(NA, nrow = y, ncol = 1 + p * n)
  colnames(out2) <- c("ipcindex", paste0(rep(names(ipcdata), each = n), rep(1:n, p)))
  
  ticker<- 1
  for(i in seq_along(count)){
    if(loop.row.selector[i]){
      if(count[i]>1){
        id<- ipcdata$uniPatID[i]
        date<- ipcdata$TG_DateNum[i]
        
        rows<- ipcdata |>
          filter(uniPatID==id,TG_DateNum==date)
        
        for(j in seq(1,nrow(rows))){
          out2[ticker, seq(2+(j-1)*p,1+j*p)] <- unlist(rows[j, ])
        }
        
        out2[ticker,1]<- ticker
        
        # change data in out1 to reference out2
        row.selector<- ipcdata$uniPatID==id & ipcdata$TG_DateNum==date
        column.selector<- names(out1)!="uniPatID" & names(out1)!="TG_DateNum"
        out1[row.selector,column.selector]<- ticker
        
        
        ticker<- ticker+1
      }
    }
  }
  
  # add dummy for multiple anameses and remove duplicate rows(= several entries for one visit)
  out1$`multiple rows`<- count>1
  out1<- distinct(out1)
  
  return(list(out1 = out1, out2 = out2))
  
}

# no.11
TG_DateNum2date<- function(TG_DateNum) as.Date(TG_DateNum, origin = "0000-01-01")

# no. 12
multiple.entry.analysis<- function(df){
  #like contains.multiple.entries, but returns instead diagnostics how many entries are somewhat duplicate
  im<- unique(paste0(df$uniPatID, df$TG_DateNum))
  v<- c(length(im),nrow(df),nrow(df)-length(levels(im)),1-length(im)/nrow(df))
  names(v)<- c("unique name/date combinations", "number of rows in data set", "difference","relative difference")
  return(v)
}

# no.13
data.repair<- function(problemdata){
  # For data sets which contain multiple entries for identical patient/ date combinations. Selects the newest entry of the multiple episodes and removes the other one(s).
  out<- problemdata[nrow(problemdata):1,]
  b<- paste0(out$uniPatID,out$TG_DateNum)
  c<- duplicated(b)
  out<- out[!c,]
  return(out)
}

# no.14
aligner<- function(df,x, debugging=FALSE){
  #finds rows with non-unique uniPatID/ TG_DateNum combinations and writes them all into one row. x specifies the maximum number of rows from df that are framed into a single row in the output data frame. If more than x rows are found in the data set, the first x rows are kept and the other ones are discarded.
  raw.combinations<- paste0(df$uniPatID,df$TG_DateNum)
  combinations<- levels(as.factor(raw.combinations))
  l<- length(combinations)
  memory<- data.frame(a=combinations, b= NA, c=NA)
  colnames(memory)<- c("combination","no. seen before","last seen in row")
  
  out<- as.data.frame(matrix(0, nrow = l, ncol = 2+x*(ncol(df)-2)))
  
  ticker<- 1 #answer to "Onto which row in out do we write right now?"
  
  partial.row<- colnames(df)!= "uniPatID" & colnames(df)!= "TG_DateNum"
  colnames(out)<- c(colnames(df),paste(rep(colnames(df)[partial.row],x-1),sort(rep(1:(x-1),ncol(df)-2)),sep = "_"))
  
  progress.bar<- txtProgressBar(min = 0, max = nrow(df), initial = 0) 
  
  for(i in seq(1,nrow(df))){
    setTxtProgressBar(progress.bar,i)
    case<- raw.combinations[i]
    location<- which(combinations==case)
    
    if(is.na(memory[location,2])){
      out[ticker,1:ncol(df)]<- df[i,]
      memory[location,2]<- 1
      memory[location,3]<- ticker
      ticker<- ticker+1
    }else{
      no.seen<- memory[location,2]
      if(no.seen<x){
        last.seen<- memory[location,3]
        sel.col<-seq(1,ncol(df)-2)+no.seen*(ncol(df)-2)+2
        sel.data<- df[i,partial.row]
        #browser()
        out[last.seen,sel.col]<- sel.data
        memory[location,2]<- memory[location,2]+1
      }
      
    }
  }
  close(progress.bar)
  if(debugging==TRUE){
    return(list(as.data.frame(out),memory))
  }else{
    return(as.data.frame(out))
  }
}
# no. 14 b
aligner.parallel.new<- function(df,x,no.splits,no.cores){
  dl<- chunk.data(df, no.splits)
  local.aligner<- function(df,x, debugging=FALSE){
    #finds rows with non-unique uniPatID/ TG_DateNum combinations and writes them all into one row. x specifies the maximum number of rows from df that are framed into a single row in the output data frame. If more than x rows are found in the data set, the first x rows are kept and the other ones are discarded.
    raw.combinations<- paste0(df$uniPatID,df$TG_DateNum)
    combinations<- levels(as.factor(raw.combinations))
    l<- length(combinations)
    memory<- data.frame(a=combinations, b= NA, c=NA)
    colnames(memory)<- c("combination","no. seen before","last seen in row")
    
    out<- as.data.frame(matrix(0, nrow = l, ncol = 2+x*(ncol(df)-2)))
    
    ticker<- 1 #answer to "Onto which row in out do we write right now?"
    
    partial.row<- colnames(df)!= "uniPatID" & colnames(df)!= "TG_DateNum"
    colnames(out)<- c(colnames(df),paste(rep(colnames(df)[partial.row],x-1),sort(rep(1:(x-1),ncol(df)-2)),sep = "_"))
    
    
    for(i in seq(1,nrow(df))){
      case<- raw.combinations[i]
      location<- which(combinations==case)
      
      if(is.na(memory[location,2])){
        out[ticker,1:ncol(df)]<- df[i,]
        memory[location,2]<- 1
        memory[location,3]<- ticker
        ticker<- ticker+1
      }else{
        no.seen<- memory[location,2]
        if(no.seen<x){
          last.seen<- memory[location,3]
          sel.col<-seq(1,ncol(df)-2)+no.seen*(ncol(df)-2)+2
          sel.data<- df[i,partial.row]
          out[last.seen,sel.col]<- sel.data
          memory[location,2]<- memory[location,2]+1
        }
        
      }
    }
    if(debugging==TRUE){
      return(list(as.data.frame(out),memory))
    }else{
      return(as.data.frame(out))
    }
  }
  
  local.x<- x
  align.cluster<- makeCluster(no.cores)
  distinct.environment<- environment()
  clusterExport(align.cluster, varlist = c("dl","local.aligner","local.x"), envir = distinct.environment)
  result.list<- parSapply(align.cluster,1:no.splits,function(i){local.aligner(dl[[i]],local.x)})
  stopCluster(align.cluster)
  nc<- 2+x*(ncol(df)-2)
  for(i in seq(1,no.splits)){
    im<- result.list[[1+(i-1)*nc]]
    for(j in seq(2,nc)){
      im<- cbind(im, result.list[[j+(i-1)*nc]])
    }
    im<- as.data.frame(im)
    if(i==1){
      out<- im
    }else{
      out<- rbind(out,im)
    }
  }
  for(i in seq(1,ncol(out))){
    if(is.numeric(result.list[[i]])){
      out[,i]<- as.numeric(out[,i])
    }else{
      out[,i]<- as.character(out[,i])
    }
  }
  colnames(out)<- c(colnames(df),paste(rep(colnames(df)[colnames(df)!= "uniPatID" & colnames(df)!= "TG_DateNum"],x-1),sort(rep(1:(x-1),ncol(df)-2)),sep = "_"))
  
  
  return(out)
}

# no. 15
chunk.data<- function(df,  no.splits){
  # splits a data frame into several data frames. They are returned as a list. The data is split by the uniPatID variable.
  # no.splits is the number in how many separate data frames the original data frame df is split.
  v<- as.factor(df$uniPatID)
  v_2<- levels(v)
  l<- length(v_2)
  
  f<- ceiling(l/no.splits)
  allocation<- rep(seq(1,no.splits),f)[1:l]
  
  out<- list()
  for (i in seq(1,no.splits)) {
    out[[i]]<- df[v %in% v_2[allocation==i],]
  }
  return(out)
}

# no. 16
chunk.data_date<- function(df,  no.splits){
  # splits a data frame into several data frames. They are returned as a list. The data is split by the TG_DateNum variable.
  # no.splits is the number in how many separate data frames the original data frame df is split.
  v<- as.factor(df$TG_DateNum)
  v_2<- levels(v)
  l<- length(v_2)
  
  f<- ceiling(l/no.splits)
  allocation<- rep(seq(1,no.splits),f)[1:l]
  
  out<- list()
  for (i in seq(1,no.splits)) {
    out[[i]]<- df[v %in% v_2[allocation==i],]
  }
  return(out)
}

# no. 17
# episode.fun<- function(ipc, length.of.episode){
#   #Uses an ipc data frame to create a data frame of episodes.
#   #length.of.episode is a a parameter how many days after the coughing was reported the diagnoses are still considered part of it.
#   patients<- levels(as.factor(as.character(ipc$uniPatID)))
#   out<- matrix(0,nrow = nrow(ipc), ncol = 3)
#   ticker<- 1
#   ipc.cols<- grepl("ipc2", colnames(ipc))
#   ipc$all_ipc<- ipc[,which(ipc.cols)[1]]
#   for(i in seq(2,sum(ipc.cols))){
#     ipc$all_ipc<- paste(ipc$all_ipc,ipc[,which(ipc.cols)[i]], sep = ";")
#   }
#   
#   progress.bar<- txtProgressBar(min = 0, max = length(patients), initial = 0) 
#   for (l in seq(1,length(patients))) {
#     setTxtProgressBar(progress.bar,l)
#     par.data<- ipc |>
#       filter(uniPatID==patients[l]) |>
#       arrange(TG_DateNum)
#     for (m in seq(1,nrow(par.data))) {
#       if(grepl("R05",par.data$all_ipc[m])){
#         out[ticker,]<- c(patients[l], par.data$TG_DateNum[m], par.data$TG_DateNum[m]+length.of.episode)
#         ticker<- ticker+1
#       }
#     }
#   }
#   close(progress.bar)
#   out<- out[1:(ticker-1),]
#   #out<- data.frame(out[,1], as.numeric(out[,2]), as.numeric(out[,3]))
#   colnames(out)<- c("uniPatID","start_date","end_date")
#   out$uniPatID<- as.factor(out$uniPatID)
#   out$start_date<- as.numeric(out$start_date)
#   out$end_date<- as.numeric(out$end_date)
#   return(out)
# }
episode.fun.new<- function(df, length.of.episode){
  ipc.cols<- which(grepl("ipc2", colnames(df)))
  im<- df[,ipc.cols]
  for(i in seq(1,ncol(im))){
    im[,i]<- grepl("R05",im[,i])
  }
  im<- rowSums(im)>0
  out<- data.frame(uniPatID=df$uniPatID[im],
                   start_date=df$TG_DateNum[im],
                   end_date=df$TG_DateNum[im]+length.of.episode)
  return(out)
}

episode.fun.alt<- function(df, length.of.episode){
  ipc.cols<- which(grepl("ipc2", colnames(df)))
  im<- as.vector(df[,ipc.cols])
  selector<- grepl("R05", im)
  start.dates<- df$TG_DateNum[selector]
  out<- data.frame(uniPatID=df$uniPatID[selector],
                   start_date=start.dates,
                   end_date=start.dates+length.of.episode)
}

# no. 18
episodes.dr1<- function(episodedf){
  # Applies decision rule 1 to a raw (i.e. containing overlapping episodes) episode data frame.
  length.of.episode<- episodedf$end_date[1]-episodedf$start_date[1]
  patients<- levels(as.factor(as.character(episodedf$uniPatID)))
  out.list<- list()
  for(i in seq_along(patients)){
    pat_i<- as.numeric(patients[i])
    dates<- sort(episodedf$start_date[episodedf$uniPatID==pat_i])
    l<- length(dates)
    if(l==1){
      out.list[[i]]<- matrix(c(pat_i,dates,dates+length.of.episode),ncol=3)
    }else if(l>1){
      merge.choice<- diff(dates)<length.of.episode
      episode.matrix<- matrix(NA,nrow = 1+sum(!merge.choice), ncol = 2)
      episode.matrix[1,1]<- dates[1]
      episode.matrix[1,2]<- dates[1]+length.of.episode
      ticker<- 1
      for(j in seq_along(merge.choice)){
        if(merge.choice[j]){
          episode.matrix[ticker,2]<- dates[1+j]+length.of.episode
        }else{
          ticker<- ticker+1
          episode.matrix[ticker,1]<- dates[1+j]
          episode.matrix[ticker,2]<- dates[1+j]+length.of.episode
        }
      }
      out.list[[i]]<- cbind(rep(pat_i,nrow(episode.matrix)),episode.matrix)
    }else{
      stop(paste("For patient",pat_i,"there is a mistake: The number of recorded dates is supposedly",l))
    }
  }
  out<- as.data.frame(do.call(rbind,out.list))
  colnames(out)<- c("uniPatID","start_date","end_date")
  return(out)
}

# no. 19
episodes.dr2<- function(episodedf){
  # Applies decision rule 2 to a raw (i.e. containing overlapping episodes) episode data frame.
  n<- nrow(episodedf)
  out<- episodedf
  for(i in seq(2,n)){
    if(episodedf$uniPatID[i]==episodedf$uniPatID[i-1]){
      if(episodedf$end_date[i-1]>episodedf$start_date[i]){
        out$end_date[i-1]<- out$start_date[i]-1
      }
    }
  }
  return(out)
}

# no. 20
episodes.dr3<- function(episodedf,ipc, debug=FALSE){
  # Applies decision rule 3 to a raw (i.e. containing overlapping episodes) episode data frame.
  patients<- levels(as.factor(episodedf$uniPatID))
  next.incident<- numeric(nrow(episodedf))
  for(i in seq(1,length(patients))){
    pat_i<- patients[i]
    rows.episodes<- which(episodedf$uniPatID==pat_i)
    start.dates.episodes<- episodedf$start_date[rows.episodes]
    end.dates.episodes<- episodedf$end_date[rows.episodes]
    incident.dates<- ipc$TG_DateNum[ipc$uniPatID==pat_i]
    store<- numeric(length(start.dates.episodes))
    for(j in seq(1, length(start.dates.episodes))){
      indices<- which(incident.dates> start.dates.episodes[j] & incident.dates<end.dates.episodes[j])
      if(length(indices)>0){
        store[j]<- min(incident.dates[indices])
      }else{
        store[j]<- end.dates.episodes[j]
      }
    }
    next.incident[rows.episodes]<- store
  }
  
  if(debug==TRUE){
    out<- cbind(episodedf,next.incident)
  }else{
    out<- episodedf
    out$end_date<- next.incident
  }
  return(out)
}

# no. 21
episodes.dr4<- function(episodedf){
  # Applies decision rule 4 to a raw (i.e. containing overlapping episodes) episode data frame.
  earliest.TG_Date<- min(episodedf$start_date)
  latest.TG_Date<- max(episodedf$start_date)
  earliest.year.POSIXlt<- year(TG_DateNum2date(earliest.TG_Date))
  latest.year.POSIXlt<- year(TG_DateNum2date(latest.TG_Date))
  no.years<- latest.year.POSIXlt-earliest.year.POSIXlt
  threshold.dates<- numeric(4*no.years)
  for(i in seq(1,no.years)){
    threshold.dates[1+(i-1)*4]<- paste0(seq(earliest.year.POSIXlt, latest.year.POSIXlt)[i], "-01-01")
    threshold.dates[2+(i-1)*4]<- paste0(seq(earliest.year.POSIXlt, latest.year.POSIXlt)[i], "-04-01")
    threshold.dates[3+(i-1)*4]<- paste0(seq(earliest.year.POSIXlt, latest.year.POSIXlt)[i], "-07-01")
    threshold.dates[4+(i-1)*4]<- paste0(seq(earliest.year.POSIXlt, latest.year.POSIXlt)[i], "-10-01")
  }
  threshold.dates_num<- as.numeric(as.Date(threshold.dates, origin= "0000-01-01"))
  #if(all(as.Date(threshold.dates_num!=as.Date(threshold.dates)))) stop("Conversion error in the threshold date!")
  
  n<- nrow(episodedf)
  out<- episodedf
  for(i in seq(2,n)){
    if(episodedf$uniPatID[i]==episodedf$uniPatID[i-1]){
      if(out$start_date[i]<out$end_date[i-1]){
        if(sum(out$start_date[i]>threshold.dates_num & out$end_date[i-1]<threshold.dates_num)==1){#condition: one threshold numeric date lies between these two
          outf$start_date[i]<- out$start_date[i-1]
          out$end_date[i]<- out$end_date[i-1]
        }else if(sum(out$start_date[i]>threshold.dates_num & out$end_date[i-1]<threshold.dates_num)>1){
          warning(paste("Between the coughing incidents, lies more than one quarter. To check out the episode, have a look at lines", (i-1),"&", i, "in the original dataset (episodedf, not the output dataset)."))
          outf$start_date[i]<- out$start_date[i-1]
          out$end_date[i]<- out$end_date[i-1]
        }else{
          out$end_date[i-1]<- out$start_date[i]-1
        }
      }
    }
    
  }
  out<- distinct(out)
  return(out)
}
# no. 22

# no. 23
add.stamm<- function(episodedf, stamm){
  #adds stamm data to the episode data frame by matching entries.
  #episodedf is a data frame created through one of the decision rules episodes.drx
  #stamm is a cleaned and prepared stamm data frame (e.g. stamm.up_2)
  #The function returns an episode data frame with the Stamm data matched to the episodes. There is also a variable called stamm_is_there, which is 1 if a Stamm entry was able to be matched, and 0 if no Stamm entry was able to be matched to that patient/ episode.
  n<- nrow(episodedf)
  p<- ncol(episodedf)
  p_out<- p+ncol(stamm)
  out<- as.data.frame(matrix(NA, nrow = n, ncol = p_out))
  stamm$uniPatID<- as.character(stamm$uniPatID)
  stamm.is.there<- numeric(n)
  for(i in seq(1,n)){
    pat_i<- as.character(episodedf$uniPatID[i])
    pat.rows<- which(stamm$uniPatID==pat_i)
    if(length(pat.rows)>0){
      dates<- stamm$TG_DateNum[pat.rows]
      im<- dates<=episodedf$start_date[i]
      if(all(!im)){
        #if no Stamm data has been recorded prior to the first coughing (i.e. first visit of clinic), this might be due to the patient being at the clinic for the first time and the doctor back dated the start of coughing (i.e. first health incident, which occurred before the visit visit at the clinic).
        loc<- pat.rows[1]
      }else{
        loc<- pat.rows[sum(im)]
      }
      out[i,seq(p+1,p_out)]<- stamm[loc,]
      stamm.is.there[i]<- 1
    }
  }
  unipat.col<- p+which(colnames(stamm)=="uniPatID")
  #remove duplicate uniPatID column
  out<- out[,-(unipat.col)]
  out[,1:p]<- episodedf
  out<- cbind(out,stamm.is.there)
  colnames(out)<- c(colnames(episodedf),colnames(stamm),"stamm_is_there")[-unipat.col]
  stamm_date.col<- which(colnames(out)=="TG_DateNum")
  colnames(out)[stamm_date.col]<- "Stamm_date"
  return(as.data.frame(out))
}

add.stamm.new<- function(episodedf, stamm){
  n<- nrow(episodedf)
  im<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(stamm)))
  stamm.is.there<- numeric(n)
  for(i in seq(1,n)){
    pat<- episodedf$uniPatID[i]
    start_date<- episodedf$start_date[i]
    patient.stamm<- stamm|>
      filter(uniPatID==pat)|>
      arrange(TG_DateNum)
    if(nrow(patient.stamm)==1){
      stamm.is.there[i]<- 1
      im[i,]<- patient.stamm
    }else if(nrow(patient.stamm)>1){
      stamm.is.there[i]<- 1
      before.episode.start<- patient.stamm$TG_DateNum<=start_date
      if(all(before.episode.start)){
        im[i,]<- patient.stamm[nrow(patient.stamm),]
      }else if(!all(before.episode.start)){
        im[i,]<- patient.stamm[1,]
      }else{
        im[i,]<- patient.stamm[max(which(before.episode.start)),]
      }
    }
  }
  out<- cbind(episodedf,im,stamm.is.there)
  colnames(out)<- c(colnames(episodedf),colnames(stamm),"stamm.is.there")
  out<- out[,-4]
  return(out)
}

# no. 24
add.dataset<- function(episodedf, adddata,name.dataset){
  #matches the entries from any suitable dataset to the ones from an episode data frame.
  #episodedf is the episode data frame, as produced by episodes.drx
  #adddata is any of the cleaned and prepared datasets except for Stammdaten
  #name.dataset is a character string giving the name of the dataset. It is used to name the 'number of matches' variable.
  #The function returns an episode data frame with the first and last matches of the added dataset, and the number of entries in adddata that are a match for this episode.
  patients<- levels(as.factor(as.character(episodedf$uniPatID)))
  adddata$uniPatID<- as.character(adddata$uniPatID)
  n<- nrow(episodedf)
  no.matches<- numeric(n)
  preselector<- as.character(adddata$uniPatID) %in% patients
  adddata<- adddata[preselector,]
  
  adddata<- transform.data_character(adddata)
  
  first.match<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(adddata)))
  last.match<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(adddata)))
  for(j in seq_along(patients)){
    pat_i<- patients[j]
    rows_episode<- which(episodedf$uniPatID==pat_i)
    match.data<- adddata|>
      filter(uniPatID==pat_i)|>
      arrange(TG_DateNum)
    if(nrow(match.data)>0){
      for(i in seq_along(rows_episode)){
        sd<- episodedf$start_date[rows_episode[i]]
        ed<- episodedf$end_date[rows_episode[i]]
        matches<- which(match.data$TG_DateNum>=sd & match.data$TG_DateNum<=ed)
        if(length(matches)>0){
          no.matches[rows_episode[i]]<- length(matches)
          first.match[rows_episode[i],]<- match.data[min(matches),]
          last.match[rows_episode[i],]<- match.data[max(matches),]
        }
      }
    }
  }
  colnames(first.match)<- paste0("first_",colnames(adddata))
  colnames(first.match)[colnames(first.match)=="first_TG_DateNum"]<- paste0("first_TG_DateNum",name.dataset)
  colnames(last.match)<- paste0("last_",colnames(adddata))
  colnames(last.match)[colnames(last.match)=="last_TG_DateNum"]<- paste0("last_TG_DateNum",name.dataset)
  col.selector<- colnames(adddata)!= "uniPatID"
  out<- cbind(episodedf, first.match[,col.selector],last.match[,col.selector],no.matches)
  colnames(out)[ncol(out)]<- paste0("no.matches_",name.dataset)
  
  out<- transform.data_factor(out)
  
  return(out)
}

# no. 25
date2TG_DateNum<- function(date) as.numeric(as.Date(date)) +719528
# transforms a date (yyyy-mm-dd) into a Matlab Serial time code (like TG_DateNum)

# no. 26
age.weights<- function(startage,endage){
  # Uses data from Genesis data base to create a vector of points of mass from a probability density function of the live age distribution in Germany
  if(startage<1) stop("By default, the start age has to be at least one year.")
  if(startage>85) stop("All ages are dropped due to the start age being too high. Please select a lower start age.")
  if(endage<1) stop("The end age is too low. It has to be no lower than one year.")
  if(endage>84) warning("Please note that the data provided by the statistical office is placed into a single bin from age 85 on. All ages from that bin are therefore treated as equally distributed across the range from 85 to the end date.")
  vector<- altersstruktur_deutschland$`Bevölkerungsstand (Anzahl)`
  if(endage<=84){
    out<- vector[seq(startage+1,endage+1)]
  }else{
    l<- endage-84
    im<- vector[seq(startage+1,85)]
    out<- c(im,rep(vector[86]/l,l))
  }
  out<- out/sum(out)
  return(out)
}

# no. 27
episodes.overlap.finder<- function(episodedf, date=FALSE){
  #finds overlapping intervals in episode data frames. Used as a debugging function.
  n<- nrow(episodedf)
  overlap<- numeric(n)
  for(i in seq(2,n)){
    if(episodedf$uniPatID[i]==episodedf$uniPatID[i-1]){
      if(episodedf$start_date[i]<episodedf$end_date[i-1]){
        overlap[i]<- 1
      }
    }
  }
  out<- episodedf
  out$overlap<- overlap
  if(date==TRUE){
    out$start_date<- TG_DateNum2date(out$start_date)
    out$end_date<- TG_DateNum2date(out$end_date)
  }
  return(out)
}

# no. 28
block.no2chapter.no<- function(dv){
  #transforms a vector of block numbers to the corresponding chapter number
  n<- length(dv)
  out<- numeric(n)
  boundaries<- c(0,21,39,45,53,64,75,86,90,100,110,120,128,143,154,163,173,184,197,221,227,234,241)
  for(i in seq_along(out)){
    out[i]<- sum(dv[i]>boundaries)
  }
}

# no. 29
block.no2chapter.name<- function(dv){
  #transforms a vector of block numbers to the corresponding chapter name
  n<- length(dv)
  out<- numeric(n)
  boundaries<- c(0,21,39,45,53,64,75,86,90,100,110,120,128,143,154,163,173,184,197,221,227,234,241)
  chapter.names<- c("Bestimmte infektiöse und parasitäre Krankheiten","Neubildungen", "Krankheiten des Blutes und der blutbildenden Organe sowie bestimmte Störungen mit Beteiligung des Immunsystems", "Endokrine, Ernährungs- und Stoffwechselkrankheiten","Psychische und Verhaltensstörungen","Krankheiten des Nervensystems","Krankheiten des Auges und der Augenanhangsgebilde","Krankheiten des Ohres und des Warzenfortsatzes","Krankheiten des Kreislaufsystems","Krankheiten des Atmungssystems","Krankheiten des Verdauungssystems","Krankheiten der Haut und der Unterhaut","Krankheiten des Muskel-Skelett-Systems und des Bindegewebes","Krankheiten des Urogenitalsystems","Schwangerschaft, Geburt und Wochenbett","Bestimmte Zustände, die ihren Ursprung in der Perinatalperiode haben","Angeborene Fehlbildungen, Deformitäten und Chromosomenanomalien","Symptome und abnorme klinische und Laborbefunde, die anderenorts nicht klassifiziert sind","Verletzungen, Vergiftungen und bestimmte andere Folgen äußerer Ursachen","Äußere Ursachen von Morbidität und Mortalität","Faktoren, die den Gesundheitszustand beeinflussen und zur Inanspruchnahme des Gesundheitswesens führen","Schlüsselnummern für besondere Zwecke")
  for(i in seq_along(out)){
    out[i]<- chapter.names[sum(dv[i]>boundaries)]
  }
  return(out)
}

# no. 30
transform.data_factor <- function(df) {
  # This ChatGPT 3.5 created function takes a data frame and turns its columns into numeric columns if they contain only numbers and into factors if a column contains not only numbers.
  # Step 1: Transform every entry in df to character strings
  df <- as.data.frame(lapply(df, as.character))
  
  # Step 2: Column-wise check for numbers and transform columns
  for (col in names(df)) {
    # Check if only numbers are contained in the column
    if (all(grepl("^\\d+\\.?\\d*$", df[[col]], ignore.case = TRUE))) {
      # Transform the column to numeric if only numbers are present
      df[[col]] <- as.numeric(df[[col]])
    } else {
      # Otherwise, transform the column to a factor
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  # Step 3: Return the transformed data frame
  return(df)
}

# no. 31
transform.data_character <- function(df) {
  # This ChatGPT 3.5 created function takes a data frame and turns its columns into numeric columns if they contain only numbers and into character strings if a column contains not only numbers.
  # Step 1: Transform every entry in df to character strings
  df <- as.data.frame(lapply(df, as.character))
  
  # Step 2: Column-wise check for numbers and transform columns
  for (col in names(df)) {
    # Check if only numbers are contained in the column
    if (all(grepl("^\\d+\\.?\\d*$", df[[col]], ignore.case = TRUE))) {
      # Transform the column to numeric if only numbers are present
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  # Step 3: Return the transformed data frame
  return(df)
}

# no. 32
discovery.tool<- function(decision.rule, pandemic="none", dependent.variable, research.question){
  # Tool to create plots and tables for specific data requests.
  # decision.rule is an integer between 1 and 5 giving the decision rule for how to deal with overlaps in the data set.
  # pandemic: if specified, set it to TRUE if only pandemic data is to be analysed, and to false if only data before the pandemic is to be included.
  # dependent.variable specifies the dependent variable to be looked at.
  # constraint.list is a list of constraints what cases are to be selected.
  # research.question is the question that the plot/ table aims to answer. It is used as label for the plot and is used to name the generated files. Therefore, it must not contain any kind of punctuation (only underline permitted)! The function adds a question mark automatically at the end of each label with the research question!
  
  
  # 1. setup of parameters and data
  if(decision.rule==1){
    episodedf<- full.data.episodes_r1
  }else if(decision.rule==2){
    episodedf<- full.data.episodes_r2
  }else if(decision.rule==3){
    episodedf<- full.data.episodes_r3
  }else if(decision.rule==4){
    episodedf<- full.data.episodes_r4
  }else if(decision.rule==5){
    episodedf<- full.data.episodes_r5
  }else{
    stop("The decision rule must be an integer between 1 and 5!")
  }
  
  earliest.TG_Date<- min(episodedf$start_date)
  latest.TG_Date<- max(episodedf$start_date)
  earliest.year.POSIXlt<- year(TG_DateNum2date(earliest.TG_Date))
  latest.year.POSIXlt<- year(TG_DateNum2date(latest.TG_Date))
  no.years<- latest.year.POSIXlt-earliest.year.POSIXlt
  pandemic.start<- date2TG_DateNum("2020-01-01")
  pandemic.end<- date2TG_DateNum("2023-01-01")
  length.pandemic<- 3
  
  if(pandemic==TRUE){
    episodedf<- episodedf[episodedf$start_date>=pandemic.start & episodedf$start_date<pandemic.end,]
  }else if(pandemic==FALSE){
    episodedf<- episodedf[episodedf$start_date<pandemic.start,]
  }else if(pandemic=="none"){
    #nothing happens
  }else{
    stop("pandemic must be specified as TRUE, FALSE, or be non-specified.")
  }
  
  #creating plots and tables
  
  #####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag#####diag
  if(dependent.variable=="diag"){
    if(diag.parameters[["initial.or.last"]]=="last"){
      all.diagnoses<- as.vector(as.matrix(episodedf[,grepl("last_icd10", colnames(episodedf))]))
    }else if(diag.parameters[["initial.or.last"]]=="initial"){
      all.diagnoses<- as.vector(as.matrix(episodedf[,grepl("first_icd10", colnames(episodedf))]))
    }else{
      stop("initial.or.last in diag.parameters is incorrectly specified")
    }
    
    all.diagnoses<- all.diagnoses[!is.na(all.diagnoses)]
    all.diagnoses<- all.diagnoses[all.diagnoses!="0"]
    
    chapters<- icd10.to.chapter(all.diagnoses)[,2]
    im<- table(chapters)
    chapter.no<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(22)
    prob[chapter.no]<- im
    prob<- prob/sum(prob)
    d.chapter<- data.frame(x=1:22,prob=prob)
    
    
    blocks<- icd10.to.block(all.diagnoses)
    im<- table(blocks[,1])
    blocks.no<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(241)
    prob[blocks.no]<- im
    prob<- prob/sum(prob)
    d.block_num<- data.frame(x=1:241,prob=prob)
    d.block_description<- data.frame(x=icd10_blocks$description, prob=prob)
    
    p<- ggplot(d.chapter, aes(x=x, y=prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"),x = "ICD10 chapter", y= "percentage")
    ggsave(paste0(research.question,"_chapter.png"), plot = p, path = getwd(), device = "png")
    
    p<- ggplot(d.block_num, aes(x=x, y=prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"),x = "ICD10 block", y= "percentage")
    ggsave(paste0(research.question,"_block_num.png"), plot = p, path = getwd(), device = "png")
    
    p<- ggplot(d.block_description, aes(x=x, y=prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"),x = "ICD10 block", y= "percentage")
    ggsave(paste0(research.question,"_block_description.png"), plot = p, path = getwd(), device = "png")
    
    write_xlsx(data.frame(no=1:241,description=icd10_blocks$description, probability=prob),paste0(research.question,".xlsx"))
    
    #####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age#####age
    
  }else if(dependent.variable=="age"){
    
    x<- as.numeric(year(TG_DateNum2date(episodedf$start_date)))-episodedf$Geburtsjahr
    if(sum(is.na(x))>0){
      warning(paste("There are",sum(is.na(x)),"entries where no birth date is available. This corresponds to",round(sum(is.na(x))/length(x),2)*100), "% of all entries. The NA results are dropped in the following.")
      x<- x[!is.na(x)]
    }
    max.age<- max(c(100,max(x)))
    im<- table(x)
    ages<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(max.age)
    prob[ages]<- im
    prob<- prob/sum(prob)
    d<- data.frame(x=seq(1,max.age),prob=prob)
    
    d$y<- age.weights(1,max.age)
    
    p<- ggplot(d, aes(x = x)) +
      geom_bar(aes(y = prob, fill = "Patient age"), stat = "identity") +
      geom_line(aes(y = y, color = "Age distribution of Germany")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"),
           x = "Age in years",
           y = "Percentage") +
      scale_fill_manual(values = c("skyblue"), labels = c("Patient age")) +
      scale_color_manual(values = c("red"), labels = c("Age distribution of Germany")) +
      theme(legend.position = "bottom")
    
    ggsave(paste0(research.question,".png"), plot = p, path = getwd(), device = "png")
    
    #####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation#####escalation
    
  }else if(dependent.variable=="escalation"){
    
    if(escalation.parameters[["type"]]==1){
      #blood test
      x<- episodedf$first_TG_DateNumlab-episodedf$start_date
      x<- x[!is.na(x)]
      warning(paste("There are", length(x), "patients whose blood was tested for Kreatinin levels."))
    }else if(escalation.parameters[["type"]]==2){
      #referral to specialist care
      x<- episodedf$first_TG_DateNumueberweis-episodedf$start_date
      x<- x[!is.na(x)]
      warning(paste("There are", length(x), "patients who were referred to a specialist."))
    }else if(escalation.parameters[["type"]]==3){
      #referral pulmonologist
      x<- episodedf$first_TG_DateNumueberweis-episodedf$start_date
      x<- x[episodedf$first_Uberw_Pneumo==1]
      x<- x[!is.na(x)]
      warning(paste("There are", length(x), "patients who were referred to a pulmonologist."))
    }else if(escalation.parameters[["type"]]==4){
      #referral radiologist
      x<- episodedf$first_TG_DateNumueberweis-episodedf$start_date
      x<- x[episodedf$first_Uberw_Radiol==1]
      x<- x[!is.na(x)]
      warning(paste("There are", length(x), "patients who were referred to a radiologist."))
    }else if(escalation.parameters[["type"]]==5){
      #referral hospital
      x<- episodedf$first_TG_DateNumueberweis-episodedf$start_date
      x<- x[episodedf$first_Uberw_KH==1]
      x<- x[!is.na(x)]
      warning(paste("There are", length(x), "patients who were referred to a hospital."))
    }
    
    im<- table(x)
    allocation<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(max(allocation)+1)
    prob[allocation+1]<- im
    prob<- prob/sum(prob)
    d<- data.frame(x=seq(0,max(allocation)),prob=prob)
    
    
    p<- ggplot(d, aes(x=x, y=prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"),
           x = "number of days since initial cough", 
           y= "percentage",
           subtitle = paste("based on the observations of", length(x), "patients"))
    ggsave(paste0(research.question,".png"), plot = p, path = getwd(), device = "png")
  }
}

#no. 33
chunk.adddata<- function(chunkeddl, adddata){
  #Takes the result of the chunk.data-function and another dataset and chunks the other dataset in the same way as the chunked dataset.
  l<- length(chunkeddl)
  adddata$uniPatID<- as.character(adddata$uniPatID)
  chunked.adddata<- list()
  for(i in seq(1,l)){
    ids<- as.character(chunkeddl[[i]]$uniPatID)
    chunked.adddata[[i]]<- adddata|>
      filter(uniPatID %in% ids)
  }
  return(chunked.adddata)
}

#no. 34
add.stamm.new.par<- function(edf,sdf, no.splits,no.workers){
  #parallelised version of add.stamm.new
  dl_e<- chunk.data(edf,no.splits)
  dl_stamm<- chunk.adddata(dl_e,sdf)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("dl_e","dl_stamm","filter","arrange"), envir = dist.env)
  
  result<- parLapply(par.cl,1:no.splits,function(k){
    episodedf<- dl_e[[k]]
    stamm<- dl_stamm[[k]]
    n<- nrow(episodedf)
    im<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(stamm)))
    colnames(im)<- colnames(stamm)
    stamm.is.there<- numeric(n)
    for(i in seq(1,n)){
      pat<- episodedf$uniPatID[i]
      start_date<- episodedf$start_date[i]
      patient.stamm<- stamm|>
        filter(uniPatID==pat)|>
        arrange(TG_DateNum)
      if(nrow(patient.stamm)==1){
        stamm.is.there[i]<- 1
        im[i,]<- patient.stamm
      }else if(nrow(patient.stamm)>1){
        stamm.is.there[i]<- 1
        before.episode.start<- patient.stamm$TG_DateNum<=start_date
        if(all(before.episode.start)){
          im[i,]<- patient.stamm[nrow(patient.stamm),]
        }else if(!all(before.episode.start)){
          im[i,]<- patient.stamm[1,]
        }else{
          im[i,]<- patient.stamm[max(which(before.episode.start)),]
        }
      }
    }
    col.selector<- colnames(im)!="uniPatID" & colnames(im)!="TG_DateNum"
    im<- im[,col.selector]
    out<- cbind(episodedf,im,stamm.is.there)
    colnames(out)<- c(colnames(episodedf),colnames(im),"stamm.is.there")
    
    return(out)
  })
  out<- as.data.frame(matrix(NA, nrow = nrow(edf), ncol = length(colnames(result[[1]]))))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}

#no. 35
add.lab.par<- function(episodedf, lab, no.splits, no.workers){
  dl_e<- chunk.data(episodedf,no.splits)
  dl_lab<- chunk.adddata(dl_e,lab)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("dl_e","dl_lab","filter","arrange"), envir = dist.env)
  
  result<- parLapply(cl=par.cl, 1:no.splits,function(k){
    edf<- dl_e[[k]] |>
      arrange(uniPatID)
    labdf<- dl_lab[[k]]
    out<- matrix(NA,nrow = nrow(edf), ncol = 7)
    
    pat<- edf$uniPatID[1]
    im_lab<- labdf|>
      filter(uniPatID==pat)|>
      arrange(TG_DateNum)
    if(nrow(im_lab)>0){
      for(i in seq(1, nrow(im_lab))){
        if (im_lab$TG_DateNum[i]>=edf$start_date[1] && im_lab$TG_DateNum[i]<=edf$end_date[1]) {
          selector<- max(which(im_lab[i,]==1))-4 #max because we want to select the last column
          out[1,selector]<- im_lab$TG_DateNum[i]
        }
      }
    }
    
    for(j in seq(2,nrow(edf))){
      pat<- edf$uniPatID[j]
      if(edf$uniPatID[j-1]!=pat){
        im_lab<- labdf|>
          filter(uniPatID==pat)|>
          arrange(TG_DateNum)
      }
      if(nrow(im_lab)>0){
        for(i in seq(1, nrow(im_lab))){
          if (im_lab$TG_DateNum[i]>=edf$start_date[j] && im_lab$TG_DateNum[i]<=edf$end_date[j]) {
            selector<- max(which(im_lab[i,]==1))-4 #max because we want to select the last column
            out[j,selector]<- im_lab$TG_DateNum[i]
          }
        }
      }
    }
    out<- as.data.frame(cbind(edf,out))
    colnames(out)<- c(colnames(edf),colnames(labdf)[-(1:4)])
    return(out)
  })
  out<- as.data.frame(matrix(NA,nrow = nrow(episodedf), ncol = ncol(episodedf)+7))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}

#no. 36
add.ueberweis.par<- function(episodedf, ueberweis, no.splits, no.workers){
  dl_e<- chunk.data(episodedf,no.splits)
  dl_ueberweis<- chunk.adddata(dl_e,ueberweis)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("dl_e","dl_ueberweis","filter","arrange"), envir = dist.env)
  
  result<- parLapply(cl=par.cl, 1:no.splits,function(k){
    edf<- dl_e[[k]] |>
      arrange(uniPatID)
    ueberweisdf<- dl_ueberweis[[k]]
    out<- matrix(NA,nrow = nrow(edf), ncol = 3)
    
    pat<- edf$uniPatID[1]
    im_ueberweis<- ueberweisdf|>
      filter(uniPatID==pat)|>
      arrange(TG_DateNum)
    if(nrow(im_ueberweis)>0){
      for(i in seq(1, nrow(im_ueberweis))){
        if (im_ueberweis$TG_DateNum[i]>=edf$start_date[1] && im_ueberweis$TG_DateNum[i]<=edf$end_date[1]) {
          selector<- max(which(im_ueberweis[i,]==1))-2 #max because we want to select the last column
          out[1,selector]<- im_ueberweis$TG_DateNum[i]
        }
      }
    }
    
    for(j in seq(2,nrow(edf))){
      pat<- edf$uniPatID[j]
      if(edf$uniPatID[j-1]!=pat){
        im_ueberweis<- ueberweisdf|>
          filter(uniPatID==pat)|>
          arrange(TG_DateNum)
      }
      if(nrow(im_ueberweis)>0){
        for(i in seq(1, nrow(im_ueberweis))){
          if (im_ueberweis$TG_DateNum[i]>=edf$start_date[j] && im_ueberweis$TG_DateNum[i]<=edf$end_date[j]) {
            selector<- max(which(im_ueberweis[i,]==1))-4 #max because we want to select the last column
            out[j,selector]<- im_ueberweis$TG_DateNum[i]
          }
        }
      }
    }
    out<- as.data.frame(cbind(edf,out))
    colnames(out)<- c(colnames(edf),colnames(ueberweisdf)[-(1:2)])
    return(out)
  })
  out<- as.data.frame(matrix(NA,nrow = nrow(episodedf), ncol = ncol(episodedf)+3))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}

# add.ueberweis.par<- function(episodedf, ueberweis, no.splits, no.workers){
#   dl_e<- chunk.data(episodedf,no.splits)
#   dl_ueberweis<- chunk.adddata(dl_e,ueberweis)
#   par.cl<- makeCluster(no.workers)
#   dist.env<- environment()
#   clusterExport(par.cl,varlist = c("dl_e","dl_ueberweis","filter","arrange"), envir = dist.env)
#   
#   result<- parLapply(cl=par.cl, 1:no.splits,function(k){
#     edf<- dl_e[[k]]|>
#       arrange(uniPatID)
#     udf<- dl_ueberweis[[k]]
#     
#     out<- matrix(NA,nrow = nrow(edf), ncol = 3)
#     
#     im_u<- udf|>
#       filter(uniPatID==edf$uniPatID[1])
#     im_u2<- im_u|>
#       filter(TG_DateNum<=edf$end_date[1],TG_DateNum>=edf$start_date[1])
#     if(nrow(im_u2)>0){
#       for(j in 1:3){
#         if(colSums(im_u2[,1+j], na.rm = TRUE)>0){
#           out[1,j]<- min(im_u2$TG_DateNum[im_u2[,1+j]==1])
#         }
#       }
#     }
#     
#     for(i in seq(2,nrow(edf))){
#       if(edf$uniPatID[i-1]!=edf$uniPatID[i]){
#         im_u<- udf|>
#           filter(uniPatID==edf$uniPatID[i])
#       }
#       im_u2<- im_u|>
#         filter(TG_DateNum<=edf$end_date[i],TG_DateNum>=edf$start_date[i])
#       if(nrow(im_u2)>0){
#         for(j in 1:3){
#           if(colSums(im_u2[,1+j], na.rm = TRUE)>0){
#             out[i,j]<- min(im_u2$TG_DateNum[im_u2[,1+j]==1])
#           }
#         }
#       }
#     }
#     return(out)
#   })
#   out<- as.data.frame(matrix(NA,nrow = nrow(episodedf), ncol = ncol(episodedf)+3))
#   colnames(out)<- colnames(result[[1]])
#   ticker<- 1
#   for(j in seq(1,length(result))){
#     out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
#     ticker<- ticker+nrow(result[[j]])
#   }
#   return(out)
# }

#no. 37
add.diag.par<- function(episodedf, adddata, no.splits, no.workers){
  edl<- chunk.data(episodedf, no.splits)
  adddl<- chunk.adddata(edl,adddata)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("edl","adddl","arrange","filter","select"), envir = dist.env)
  
  result<- parLapply(par.cl,1:no.splits, function(k){
    edf<- edl[[k]]|>
      arrange(uniPatID)
    diag<- adddl[[k]]
    out<- as.data.frame(matrix(NA,nrow = nrow(edf),ncol = 2*ncol(diag)-4))
    no.matches_diag<- numeric(nrow(edf))
    
    pat<- edf$uniPatID[1]
    im_diag<- diag|>
      filter(uniPatID==pat)|>
      arrange(TG_DateNum)
    im_diag_2<- im_diag|>
      select(-uniPatID,-TG_DateNum)
    no.matches<- 0
    if(nrow(im_diag)>0){
      for(i in seq(1,nrow(im_diag))){
        if(im_diag$TG_DateNum[i]>=edf$start_date[1] && im_diag$TG_DateNum[i]<=edf$end_date[1]){
          if(no.matches==0){
            out[1,]<-rep(im_diag_2[i,],2)
            no.matches<- 1
          }else{
            out[1,seq(ncol(diag)+1,ncol(out))]<- im_diag_2[i,]
            no.matches<- no.matches+1
          }
        }
      }
    }
    no.matches_diag[1]<- no.matches
    
    for(j in seq(2, nrow(edf))){
      pat<- edf$uniPatID[j]
      if(edf$uniPatID[j-1]!=pat){
        im_diag<- diag|>
          filter(uniPatID==pat)|>
          arrange(TG_DateNum)
        im_diag_2<- im_diag|>
          select(-uniPatID,-TG_DateNum)
      }
      no.matches<- 0
      if(nrow(im_diag)>0){
        for(i in seq(1,nrow(im_diag))){
          if(im_diag$TG_DateNum[i]>=edf$start_date[j] && im_diag$TG_DateNum[i]<=edf$end_date[j]){
            if(no.matches==0){
              out[j,]<-rep(im_diag_2[i,],2)
              no.matches<- 1
            }else{
              out[1,seq(ncol(diag)+1,ncol(out))]<- im_diag_2[i,]
              no.matches<- no.matches+1
            }
          }
        }
      }
      no.matches_diag[j]<- no.matches
    }
    out<- cbind(edf,out,no.matches_diag)
    colnames(out)<- c(colnames(edf),paste0("first_",colnames(im_diag_2)),paste0("last_",colnames(im_diag_2)),"no.matches_diag")
    return(out)
  })
  out<- as.data.frame(matrix(NA,nrow = nrow(episodedf),ncol = ncol(result[[1]])))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}


data.repair.new<- function(df){
  #browser()
  out<- df|>
    arrange(uniPatID,desc(TG_DateNum))
  selector<- !duplicated(out$uniPatID)
  out<- out[selector,]
  return(out)
}
