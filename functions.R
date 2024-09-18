#################### functions #########################

icd10.to.chapter<- function(vector){
  #maps a vector of icd10 codes to their respective chapters
  output<- rep(0,length(vector))
  int.fun_1<- function(a) substr(a, 1, 1)
  int.fun_2<- function(a) substr(a, 2, 2)
  int.fun_3<- function(a) substr(a, 3, 3)
  first<- sapply(vector, FUN = int.fun_1)
  names(first)<- NULL
  second<- as.numeric(sapply(vector, FUN = int.fun_2))
  third<- as.numeric(sapply(vector, FUN = int.fun_3))
  
  for(i in 1:length(vector)){
    if(first[i] == "A" | first[i] == "B"){
      output[i]<- 1
    }else if(first[i] == "C" | first[i] == "D"){
      if(first[i] == "D" & second[i] >= 5){
        output[i]<- 3
      }else{
        output[i]<- 2
      }
    }else if(first[i] == "E"){
      output[i]<-4
    }else if(first[i] == "F"){
      output[i]<- 5
    }else if(first[i] == "G"){
      output[i]<-6
    }else if(first[i] == "H"){
      if(second[i] >= 6){
        output[i]<- 8
      }else{
        output[i]<- 7
      }
    }else if(first[i] == "I"){
      output[i]<-9
    }else if(first[i] == "J"){
      output[i]<- 10
    }else if(first[i] == "K"){
      output[i]<- 11
    }else if(first[i] == "L"){
      output[i]<- 12
    }else if(first[i] == "M"){
      output[i]<- 13
    }else if(first[i] == "N"){
      output[i]<- 14 
    }else if(first[i] == "O"){
      output[i]<-15
    }else if(first[i] == "P"){
      output[i]<- 16
    }else if(first[i] == "Q"){
      output[i]<- 17
    }else if(first[i] == "R"){
      output[i]<- 18
    }else if(first[i] == "S" | first[i] == "T"){
      output[i]<- 19
    }else if(first[i] == "V" | first[i]== "W" |first[i] == "X" | 
             first[i] == "Y"){
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

icd10.to.block<- function(vector, debug=FALSE){
  n<- length(vector)
  number<- numeric(n)
  title<- numeric(n)
  if(debug==TRUE){
    error<- numeric(n)
    errorno<- 1
  } 
  for(i in seq(1,n)){
    im<- vector[i]
    im_2<-substr(im, 1, 1)
    im_3<- as.numeric(substr(im, 2, 3))
    if(im_2 %in% c("V", "W", "X", "Y")){
      if(im_2 == "V" | im_2 == "W"){
        loc<- 223
        number[i]<- loc
        title[i]<- icd10_blocks$description[loc]
      }else if(im_2 == "X"){
        if(im_3 <= 59){
          loc<- 223
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else if(im_3 > 59 & im_3 <= 84){
          loc<- 224
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else{
          loc<- 225
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }
      }else if(im_2 == "Y"){
        if(im_3<=9){
          loc<- 225
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }else{
          im_4<- im_2==icd10_blocks$start_code_letter
          im_5<- im_3>=icd10_blocks$start_code_no & im_3<=icd10_blocks$end_code_no
          loc<- which(im_4 & im_5)
          if(length(loc) == 0){
            stop(paste("No corresponding row found for",im))
            error[errorno]<- im
            errorno<- errorno + 1
          }else if(length(loc) > 1){
            stop(paste("No corresponding row found for",im, ". The rows found for this code are", loc))
            error[errorno]<- im
            errorno<- errorno + 1
          }
          number[i]<- loc
          title[i]<- icd10_blocks$description[loc]
        }
      }
    }else{
      im_4<- im_2 == icd10_blocks$start_code_letter
      im_5<- im_3 >= icd10_blocks$start_code_no & im_3 <= icd10_blocks$end_code_no
      loc<- which(im_4 & im_5)
      if(length(loc) > 0){
        number[i]<- loc
        title[i]<- icd10_blocks$description[loc]
      }
      
    }
  }
  out<- data.frame(number = number, title = title)
  if(debug==TRUE){
    out$error<- error
  } 
  return(out)
}

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes; 0=public health insurance, 1=private health insurance or no health insurance
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  out[is.na(out)]<- 1
  out[out == 1]<- 1
  return(out)
}

TG_DateNum2date<- function(TG_DateNum) as.Date(TG_DateNum, origin = "0000-01-01")

multiple.entry.analysis<- function(df){
  #like contains.multiple.entries, but returns instead diagnostics how many entries are somewhat duplicate
  im<- unique(paste0(df$uniPatID, df$TG_DateNum))
  v<- c(length(im),nrow(df),nrow(df)-length(levels(im)),1-length(im)/nrow(df))
  names(v)<- c("unique name/date combinations", "number of rows in data set", "difference","relative difference")
  return(v)
}

aligner<- function(inputdf, cutoff, var.name){
  unipat_date.counts<- table(inputdf$unipat_date)|>
    as.data.frame()
  colnames(unipat_date.counts)<- c("unipat_date", "count")
  df<- left_join(inputdf, unipat_date.counts, by = "unipat_date")|>
    filter(count <= cutoff)
  data.list<- list()
  im<- df|>
    filter(count == 1)|>
    select(uniPatID, TG_DateNum, unipat_date, icd10)
  im_3<- cbind(im, matrix(NA, nrow = nrow(im), ncol = cutoff - 1))
  colnames(im_3)<- c("uniPatID", "TG_DateNum", "unipat_date", 
                     paste0(var.name, "_", seq(1, cutoff)))
  data.list[[1]]<- im_3
  for(i in seq(2, cutoff)){
    im<- df|>
      filter(count == i)|>
      select(-count)|>
      arrange(unipat_date)
    im_2<- matrix(im$icd10, ncol = i)
    im<- im[seq(1, nrow(im), by = i),]|>
      select(uniPatID, TG_DateNum, unipat_date)
    im_3<- cbind(im, im_2, matrix(NA, nrow = nrow(im), ncol = cutoff - i))
    colnames(im_3)<- c("uniPatID", "TG_DateNum", "unipat_date", 
                       paste0(var.name, "_", seq(1, cutoff)))
    data.list[[i]]<- im_3
  }
  out<- bind_rows(data.list)
  return(out)
}

chunk.data<- function(df, no.splits){
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

episodes.dr1<- function(episodedf){
  # Applies decision rule 1 to a raw (i.e. containing overlapping episodes) episode data frame.
  length.of.episode<- episodedf$end_date[1] - episodedf$start_date[1]
  patients<- levels(as.factor(as.character(episodedf$uniPatID)))
  out.list<- list()
  for(i in seq_along(patients)){
    pat_i<- as.numeric(patients[i])
    dates<- sort(episodedf$start_date[episodedf$uniPatID == pat_i])
    l<- length(dates)
    if(l==1){
      out.list[[i]]<- matrix(c(pat_i, dates, dates + length.of.episode), ncol=3)
    }else if(l>1){
      merge.choice<- diff(dates) < length.of.episode
      episode.matrix<- matrix(NA,nrow = 1 + sum(!merge.choice), ncol = 2)
      episode.matrix[1, 1]<- dates[1]
      episode.matrix[1, 2]<- dates[1] + length.of.episode
      ticker<- 1
      for(j in seq_along(merge.choice)){
        if(merge.choice[j]){
          episode.matrix[ticker, 2]<- dates[1 + j] + length.of.episode
        }else{
          ticker<- ticker + 1
          episode.matrix[ticker, 1]<- dates[1 + j]
          episode.matrix[ticker, 2]<- dates[1 + j] + length.of.episode
        }
      }
      out.list[[i]]<- cbind(rep(pat_i, nrow(episode.matrix)), episode.matrix)
    }else{
      stop(paste("For patient", pat_i,
                 "there is a mistake: The number of recorded dates is supposedly",
                 l))
    }
  }
  out<- as.data.frame(do.call(rbind, out.list))
  colnames(out)<- c("uniPatID", "start_date", "end_date")
  return(out)
}

episodes.dr2<- function(episodedf){
  # Applies decision rule 2 to a raw (i.e. containing overlapping episodes) episode data frame.
  n<- nrow(episodedf)
  out<- episodedf
  for(i in seq(2, n)){
    if(episodedf$uniPatID[i] == episodedf$uniPatID[i - 1]){
      if(episodedf$end_date[i - 1] > episodedf$start_date[i]){
        out$end_date[i - 1]<- out$start_date[i] - 1
      }
    }
  }
  return(out)
}

episodes.dr3<- function(episodedf,ipc, debug=FALSE){
  # Applies decision rule 3 to a raw (i.e. containing overlapping episodes) episode data frame.
  patients<- levels(as.factor(episodedf$uniPatID))
  next.incident<- numeric(nrow(episodedf))
  for(i in seq(1, length(patients))){
    pat_i<- patients[i]
    rows.episodes<- which(episodedf$uniPatID == pat_i)
    start.dates.episodes<- episodedf$start_date[rows.episodes]
    end.dates.episodes<- episodedf$end_date[rows.episodes]
    incident.dates<- ipc$TG_DateNum[ipc$uniPatID == pat_i]
    store<- numeric(length(start.dates.episodes))
    for(j in seq(1, length(start.dates.episodes))){
      indices<- which(incident.dates > start.dates.episodes[j] & 
                      incident.dates<end.dates.episodes[j])
      if(length(indices) > 0){
        store[j]<- min(incident.dates[indices])
      }else{
        store[j]<- end.dates.episodes[j]
      }
    }
    next.incident[rows.episodes]<- store
  }
  
  if(debug==TRUE){
    out<- cbind(episodedf, next.incident)
  }else{
    out<- episodedf
    out$end_date<- next.incident
  }
  return(out)
}

episodes.dr4<- function(episodedf){
  # Applies decision rule 4 to a raw (i.e. containing overlapping episodes) episode data frame.
  earliest.TG_Date<- min(episodedf$start_date)
  latest.TG_Date<- max(episodedf$start_date)
  earliest.year.POSIXlt<- year(TG_DateNum2date(earliest.TG_Date))
  latest.year.POSIXlt<- year(TG_DateNum2date(latest.TG_Date))
  no.years<- latest.year.POSIXlt-earliest.year.POSIXlt
  threshold.dates<- numeric(4 * no.years)
  for(i in seq(1, no.years)){
    threshold.dates[1 +(i - 1) * 4]<- paste0(seq(earliest.year.POSIXlt, 
                                                 latest.year.POSIXlt)[i], 
                                             "-01-01")
    threshold.dates[2 +(i - 1) * 4]<- paste0(seq(earliest.year.POSIXlt, 
                                                 latest.year.POSIXlt)[i], 
                                             "-04-01")
    threshold.dates[3 +(i - 1) * 4]<- paste0(seq(earliest.year.POSIXlt, 
                                                 latest.year.POSIXlt)[i], 
                                             "-07-01")
    threshold.dates[4 +(i - 1) * 4]<- paste0(seq(earliest.year.POSIXlt, 
                                                 latest.year.POSIXlt)[i], 
                                             "-10-01")
  }
  threshold.dates_num<- as.Date(threshold.dates, origin= "0000-01-01")|>
    as.numeric()
  #if(all(as.Date(threshold.dates_num!=as.Date(threshold.dates)))) stop("Conversion error in the threshold date!")
  
  n<- nrow(episodedf)
  out<- episodedf
  for(i in seq(2, n)){
    if(episodedf$uniPatID[i] == episodedf$uniPatID[i - 1]){
      if(out$start_date[i] < out$end_date[i - 1]){
        if(sum(out$start_date[i] > threshold.dates_num & 
               out$end_date[i-1]<threshold.dates_num)
           == 1){#condition: one threshold numeric date lies between these two
          outf$start_date[i]<- out$start_date[i-1]
          out$end_date[i]<- out$end_date[i-1]
        }else if(sum(out$start_date[i] > threshold.dates_num & 
                     out$end_date[i-1] < threshold.dates_num)
                 > 1){
          warning(paste(
            "Between the coughing incidents, lies more than one quarter. To check out the episode, have a look at lines", 
            (i - 1),"&", i, 
            "in the original dataset (episodedf, not the output dataset)."
            ))
          outf$start_date[i]<- out$start_date[i - 1]
          out$end_date[i]<- out$end_date[i - 1]
        }else{
          out$end_date[i - 1]<- out$start_date[i] - 1
        }
      }
    }
    
  }
  out<- distinct(out)
  return(out)
}

date2TG_DateNum<- function(date){
  # transforms a date (yyyy-mm-dd) into a Matlab Serial time code (like TG_DateNum)
  as.numeric(as.Date(date)) + 719528
}

age.weights<- function(startage, endage){
  # Uses data from Genesis data base to create a vector of points of mass from a probability density function of the live age distribution in Germany
  if(startage < 1) stop("By default, the start age has to be at least one year.")
  if(startage > 85) stop("All ages are dropped due to the start age being too high. Please select a lower start age.")
  if(endage < 1) stop("The end age is too low. It has to be no lower than one year.")
  if(endage > 84) warning("Please note that the data provided by the statistical office is placed into a single bin from age 85 on. All ages from that bin are therefore treated as equally distributed across the range from 85 to the end date.")
  vector<- altersstruktur_deutschland$bevoelkerungsstand
  if(endage <= 84){
    out<- vector[seq(startage + 1, endage + 1)]
  }else{
    l<- endage - 84
    im<- vector[seq(startage + 1, 85)]
    out<- c(im, rep(vector[86]/l,l))
  }
  out<- out / sum(out)
  return(out)
}

episodes.overlap.finder<- function(episodedf, date = FALSE){
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

block.no2chapter.no<- function(dv){
  #transforms a vector of block numbers to the corresponding chapter number
  n<- length(dv)
  out<- numeric(n)
  boundaries<- c(0, 21, 39, 45, 53, 64, 75, 86, 90, 100, 110, 120, 128, 143, 
                 154, 163, 173, 184, 197, 221, 227, 234, 241)
  for(i in seq_along(out)){
    out[i]<- sum(dv[i] > boundaries)
  }
}

block.no2chapter.name<- function(dv){
  #transforms a vector of block numbers to the corresponding chapter name
  n<- length(dv)
  out<- numeric(n)
  boundaries<- c(0, 21, 39, 45, 53, 64, 75, 86, 90, 100, 110, 120, 128, 143, 
                 154, 163, 173, 184, 197, 221, 227, 234, 241)
  chapter.names<- c("Bestimmte infektiöse und parasitäre Krankheiten", "Neubildungen", "Krankheiten des Blutes und der blutbildenden Organe sowie bestimmte Störungen mit Beteiligung des Immunsystems", "Endokrine, Ernährungs- und Stoffwechselkrankheiten", "Psychische und Verhaltensstörungen", "Krankheiten des Nervensystems", "Krankheiten des Auges und der Augenanhangsgebilde", "Krankheiten des Ohres und des Warzenfortsatzes", "Krankheiten des Kreislaufsystems", "Krankheiten des Atmungssystems", "Krankheiten des Verdauungssystems", "Krankheiten der Haut und der Unterhaut", "Krankheiten des Muskel-Skelett-Systems und des Bindegewebes", "Krankheiten des Urogenitalsystems", "Schwangerschaft, Geburt und Wochenbett", "Bestimmte Zustände, die ihren Ursprung in der Perinatalperiode haben", "Angeborene Fehlbildungen, Deformitäten und Chromosomenanomalien", "Symptome und abnorme klinische und Laborbefunde, die anderenorts nicht klassifiziert sind", "Verletzungen, Vergiftungen und bestimmte andere Folgen äußerer Ursachen", "Äußere Ursachen von Morbidität und Mortalität", "Faktoren, die den Gesundheitszustand beeinflussen und zur Inanspruchnahme des Gesundheitswesens führen", "Schlüsselnummern für besondere Zwecke")
  for(i in seq_along(out)){
    out[i]<- chapter.names[sum(dv[i] > boundaries)]
  }
  return(out)
}

chunk.adddata<- function(chunkeddl, adddata){
  #Takes the result of the chunk.data-function and another dataset and chunks the other dataset in the same way as the chunked dataset.
  l<- length(chunkeddl)
  adddata$uniPatID<- as.character(adddata$uniPatID)
  chunked.adddata<- list()
  for(i in seq(1, l)){
    ids<- as.character(chunkeddl[[i]]$uniPatID)
    chunked.adddata[[i]]<- adddata|>
      filter(uniPatID %in% ids)
  }
  return(chunked.adddata)
}

add.stamm<- function(edf,sdf, no.splits,no.workers){
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

add.lab<- function(episodedf, lab.df){
  sel<- colnames(episodedf)[colnames(episodedf) != "episode.ID"]
  ndf<- full_join(episodedf, lab.df, by = "uniPatID")
  with.lab<- ndf|>
    filter(start_date <= TG_DateNum & end_date >= TG_DateNum)|>
    select(-sel, -unipat_date)
  date.cols<- colnames(with.lab) %in% c("labtest", "something", "blood_normal",
                                        "blood_large_test", "CRP", "ery",
                                        "leuko", "procalcitonin")
  with.lab[,date.cols]<- with.lab$TG_DateNum * with.lab[,date.cols]
  out<- left_join(episodedf, with.lab, by = "episode.ID")|>
    select(-TG_DateNum)
  return(out)
}

add.ueberweis<- function(episodedf, ueberweis.df){
  ueberweis.new<- ueberweis.df
  ueberweis.new$FR_Pneumo<- ueberweis.df$FR_Pneumo * ueberweis.df$TG_DateNum
  ueberweis.new$FR_Radiol<- ueberweis.df$FR_Radiol * ueberweis.df$TG_DateNum
  ueberweis.new$FR_KH<- ueberweis.df$FR_KH * ueberweis.df$TG_DateNum
  sel<- colnames(episodedf)[colnames(episodedf) != "episode.ID"]
  ndf<- full_join(episodedf, ueberweis.new, by = "uniPatID")
  with.ueberweis<- ndf|>
    filter(start_date <= TG_DateNum & end_date >= TG_DateNum)|>
    select(-sel, -unipat_date)
  out<- left_join(episodedf, with.ueberweis, by = "episode.ID")|>
    select(-TG_DateNum)
  return(out)
}

add.diag<- function(episodedf, adddata, no.splits, no.workers){
  edl<- chunk.data(episodedf, no.splits)
  adddl<- chunk.adddata(edl,adddata)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl, varlist = c("edl", "adddl", "arrange", "filter", 
                                    "select"), envir = dist.env)
  
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
  out<- df|>
    arrange(uniPatID,desc(TG_DateNum))
  selector<- !duplicated(out$uniPatID)
  out<- out[selector,]
  return(out)
}

icd10.to.3St<- function(icd10vec){
  codes<- substr(icd10vec, 1, 3)
  description<- ICD3St$text[match(codes, ICD3St$code)]
  return(data.frame(codes = codes, description = description))
}

summary.stats<- function(episodedf){
  no.patients<- length(levels(as.factor(episodedf$uniPatID)))
  no.episodes<- nrow(episodedf)
  age<- episodedf$age
  length.of.episode<- episodedf$length_of_episode
  
  numbers<- list(
    no.episodes= nrow(episodedf),
    no.patients= length(levels(as.factor(episodedf$uniPatID))),
    no.stammdata.there= sum(episodedf$stamm.is.there, na.rm = TRUE),
    earliest.recorded.episode.start= min(episodedf$start_date)|>
      TG_DateNum2date()|>
      as.character(),
    latest.recorded.episode.start= max(episodedf$start_date)|>
      TG_DateNum2date()|>
      as.character(),
    no.males=sum(episodedf$Maennl, na.rm = TRUE),
    no.females=sum(episodedf$Weibl, na.rm = TRUE),
    no.transgender=sum(episodedf$Transgen, na.rm = TRUE),
    no.undefined.sex=sum(episodedf$Geschlechtundef, na.rm = TRUE),
    
    percentage.chronically.diseased.patients= mean(as.character(levels(as.factor(episodedf$uniPatID))) %in% chronic.patients$chronic.patients),
    percentage.episodes.of.chronically.diseased= mean(episodedf$uniPatID %in% chronic.patients$chronic.patients),
    percentage.copd.patients= mean(as.character(levels(as.factor(episodedf$uniPatID))) %in% copd.vec),
    percentage.episodes.of.copd.patients= mean(episodedf$uniPatID %in% copd.vec),
    min.age= min(age, na.rm = TRUE),
    mean.age= mean(age, na.rm= TRUE),
    median.age= median(age, na.rm = TRUE),
    max.age= max(age, na.rm = TRUE),
    percentage.episodes.minors= mean(age<18, na.rm=TRUE),
    percentage.patients.minors= mean((age<18)[!duplicated(episodedf$uniPatID)], na.rm=TRUE),
    min.length.of.episode= min(length.of.episode),
    mean.length.of.episode= mean(length.of.episode),
    max.length.of.episode= max(length.of.episode),
    
    average.no.visit.per.patient=nrow(episodedf)/no.patients,
    percentage.stammdata.there= sum(episodedf$stamm_is_there, na.rm = TRUE)/no.episodes,
    percentage.males.episodes=mean(episodedf$Maennl, na.rm = TRUE),
    percentage.females.episodes=mean(episodedf$Weibl, na.rm = TRUE),
    percentage.males.patients=mean(episodedf$Maennl[!duplicated(episodedf$uniPatID)], na.rm = TRUE),
    percentage.females.patients=mean(episodedf$Weibl[!duplicated(episodedf$uniPatID)], na.rm = TRUE),
    no.private.insurance= sum(episodedf$PKV, na.rm= TRUE),
    percentage.private.insurance= mean(episodedf$PKV, na.rm= TRUE),
    percentage.retired.patients= mean(episodedf$Status_R[!duplicated(episodedf$uniPatID)], na.rm= TRUE),
    percentage.retired.episodes= mean(episodedf$Status_R, na.rm= TRUE),
    no.AUBs = sum(!is.na(episodedf$time_until_AUB)),
    percentage.AUBs.episodes = mean(!is.na(episodedf$time_until_AUB))
  )
  
  numbers<- unlist(numbers)|>
    as.data.frame()
  colnames(numbers)<- NULL
  return(numbers)
}

discovery.tool<- function(decision.rule, agelims = c(1,150), 
                          insurance = c("all", "GKV", "PKV")[1], 
                          remove.copd = FALSE, lims.chronic.diseases = c(0, 100), 
                          pandemic = "none", cutoff_diag = cutoff_diag,
                          preliminary.diag = c(NA, TRUE, FALSE)[1], 
                          AUB=c(NA, TRUE, FALSE)[1], quarter.selection = 1:4, 
                          month.selection=1:12, selected.years = "all", 
                          dependent.variable, research.question,
                          diag_initial_last = c("first", "last")[1],
                          diag_no_diagnoses = FALSE,
                          escalation_lab = c("any", "blood", "diff_blood", 
                                             "CRP", "erythocytes", "leucocytes", 
                                             "procalcitonin")[1]
                          ){
  # Tool to create plots and tables for specific data requests.
  # decision.rule is an integer between 1 and 5 giving the decision rule for how to deal with overlaps in the data set. Can currently only be set to 1 or 2.
  # insurance can be selected as "GKV" or "PKV" in order to restrict the patients to being insured by a public/ private insurer
  # remove.copd can be set to TRUE to remove all patients that were registered with COPD within the time of the data
  # lims.chronic.diseases is a two element vector with the lowest and highest numbers of chronic diseases that are to be considered. By default, patients with 0 to 100 different chronic diseases are considered. Example: only patients with three chronic diseases are to be considered-> set lims.chronic.diseases=c(3,3); only people with two or fewer chronic diseases are to be considered-> set lims.chronic.diseases=c(0,2). Note that the people for which no data is recorded are kept by default in the analysis.
  # pandemic: if specified, set it to TRUE if only pandemic data is to be analysed, and to false if only data before the pandemic is to be included.
  # selected.years is "all" for no limitation of the years, and a vector c(start year, end year) if the years are to be limited.
  # dependent.variable specifies the dependent variable to be looked at. Current options are: diag, age, escalation_lab, escalation_ueberweis.
  # constraint.list is a list of constraints what cases are to be selected.
  # research.question is the question that the plot/ table aims to answer. It is used as label for the plot and is used to name the generated files. Therefore, it must not contain any kind of punctuation (only underline permitted)! The function adds a question mark automatically at the end of each label with the research question!
  #browser()
  
  # 1. setup of parameters and data
  # if(decision.rule==1){
  #   episodedf<- full_r1
  # }else if(decision.rule==2){
  #   episodedf<- full_r2
  # }else if(decision.rule==3){
  #   episodedf<- full_r3
  # }else if(decision.rule==4){
  #   episodedf<- full_r4
  # }else if(decision.rule==5){
  #   episodedf<- full_r5
  # }else{
  #   stop("The decision rule must be an integer between 1 and 5!")
  # }
  if(decision.rule %in% seq(1, 5)){
    episodedf<- get(paste0("full_r", decision.rule))
  }else{
    stop("The decision rule must be an integer between 1 and 5!")
  }
  
  #age
  age.vec<- episodedf$age
  age.selector<- agelims[1]<= age.vec & agelims[2]>= age.vec
  age.selector[is.na(age.selector)]<- TRUE
  episodedf<- episodedf[age.selector,]
  
  #insurance
  if(insurance=="PKV"){
    episodedf<- episodedf[episodedf$PKV == 1,]
  }else if(insurance=="GKV"){
    episodedf<- episodedf[episodedf$PKV == 0,]
  }
  
  #copd
  if(remove.copd){
    keep.vec<- numeric(nrow(episodedf))
    for(i in seq(1,nrow(episodedf))){
      if(episodedf$uniPatID[i] %in% copd.vec){
        keep.vec[i]<- FALSE
      }else{
        keep.vec[i]<- TRUE
      }
    }
    episodedf<- episodedf[keep.vec,]
  }
  
  #number of chronic diseases
  selector<- episodedf$no.chronic.diseases >= lims.chronic.diseases[1] &
    episodedf$no.chronic.diseases <= lims.chronic.diseases[2]
  selector[is.na(selector)]<- TRUE
  episodedf<- episodedf[selector,]
  
  earliest.TG_Date<- min(episodedf$start_date)
  latest.TG_Date<- max(episodedf$start_date)
  earliest.year.POSIXlt<- year(TG_DateNum2date(earliest.TG_Date))
  latest.year.POSIXlt<- year(TG_DateNum2date(latest.TG_Date))
  no.years<- latest.year.POSIXlt-earliest.year.POSIXlt
  pandemic.start<- date2TG_DateNum("2020-01-01")
  pandemic.end<- date2TG_DateNum("2023-01-01")
  length.pandemic<- 3
  
  if(pandemic == TRUE){
    episodedf<- episodedf[episodedf$start_date >= pandemic.start & 
                            episodedf$start_date < pandemic.end,]
  }else if(pandemic == FALSE){
    episodedf<- episodedf[episodedf$start_date < pandemic.start,]
  }else if(pandemic == "none"){
    #nothing happens
  }else{
    stop("pandemic must be specified as TRUE, FALSE, or not be specified.")
  }
  
  if(!is.na(preliminary.diag)){
    if(preliminary.diag==TRUE){
      episodedf<- episodedf[episodedf$first_diag_V_present,]
    }else if(preliminary.diag==FALSE){
      episodedf<- episodedf[!episodedf$first_diag_V_present,]
    }
  }
  if(nrow(episodedf)==0){
    stop("Due to the parameter configuration there are no episodes left after accounting for preliminary.diag")
  }
  
  if(!is.na(AUB)){
    if(AUB==TRUE){
      episodedf<- episodedf[!is.na(episodedf$TG_DateNum_AUB),]
    }else if(AUB==FALSE){
      episodedf<- episodedf[is.na(episodedf$TG_DateNum_AUB),]
    }
  }
  if(nrow(episodedf)==0){
    stop("Due to the parameter configuration there are no episodes left after accounting for AUB")
  }
  
  episodedf<- episodedf|>
    filter(start_quarter %in% quarter.selection)|>
    filter(start_month %in% month.selection)
  if(nrow(episodedf)==0){
    stop("Due to the parameter configuration there are no episodes left after accounting for quarter.selection and month.selection")
  }
  
  if(length(selected.years) != 1){
    if(length(selected.years) != 2 | !is.numeric(selected.years)){
      warning("The selection of years is likely misspecified.")
    }
    constr<- date2TG_DateNum(paste0(selected.years, "-01-01"))
    episodedf<- filter(episodedf, start_date >= constr[1] & end_date <= constr[2])
  }
  if(nrow(episodedf)==0){
    stop("Due to the parameter configuration there are no episodes left after the year selection.")
  }
  
  #creating plots and tables
  if(dependent.variable=="diag"){
    dt_int_diag(research.question, episodedf, diag_initial_last, diag_no_diagnoses)
  }else if(dependent.variable=="age"){
    dt_int_age(research.question, episodedf, agelims)
  }else if(dependent.variable=="escalation_lab"){
    dt_int_escalation_lab(research.question, episodedf, 
                          escalation.parameters_lab)
  }else if(dependent.variable=="escalation_antibiotics"){
    dt_int_escalation_antibiotics(research.question, episodedf)
  }else if(dependent.variable=="escalation_ueberweis"){
    
  }
  return(TRUE)
}

dt_int_diag<- function(research.question, episodedf, diag_initial_last, diag_no_diagnoses){
  if(isFALSE(diag_no_diagnoses)){
    if(diag_initial_last == "last"){
      all.diagnoses<- episodedf[,grepl("last_diag", colnames(episodedf))]|>
        as.matrix()|>
        as.vector()
    }else if(diag_initial_last == "initial"){
      all.diagnoses<- episodedf[paste0("first_diag_", seq(1, cutoff_diag))]|>
        unlist()
    }else{
      stop("initial.or.last in diag.parameters is incorrectly specified")
    }
    
    all.diagnoses<- all.diagnoses[!is.na(all.diagnoses)]
    all.diagnoses<- all.diagnoses[all.diagnoses != "0"]
    
    chapters<- icd10.to.chapter(all.diagnoses)[, 2]
    im<- table(chapters)
    chapter.no<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(22)
    prob[chapter.no]<- im
    prob<- prob / sum(prob)
    d.chapter<- data.frame(x = 1:22, prob = prob)
    
    
    blocks<- icd10.to.block(all.diagnoses)
    blocks<- blocks[blocks[, 1] != 0,]
    im<- table(blocks[, 1])
    blocks.no<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(241)
    prob[blocks.no]<- im
    prob<- prob/sum(prob)
    d.block_num<- data.frame(x = 1:241, prob = prob)
    d.block_description<- data.frame(x = icd10_blocks$description, prob = prob)
    
    three.St<- icd10.to.3St(all.diagnoses)
    d.3St<- paste0(three.St$codes, "- ", three.St$description)|>
      table()|>
      as.data.frame()
    colnames(d.3St)<- c("description", "frequency")
    d.3St$icd10code<- as.data.frame(table(three.St$codes))
    d.3St$prob<- d.3St$frequency / sum(d.3St$frequency)
    
    p<- ggplot(d.chapter, aes(x = x, y = prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      ylim(0, 0.6) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6)) +
      labs(title = paste0(research.question,"?"), x = "ICD10 chapter", 
           y= "percentage")
    ggsave(paste0(research.question,"_chapter.png"), plot = p, path = getwd(), 
           device = "png")
    
    p<- ggplot(d.block_num, aes(x = x, y = prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      ylim(0, 0.5) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6)) +
      labs(title = paste0(research.question, "?"), x = "ICD10 block", 
           y= "percentage")
    ggsave(paste0(research.question, "_block_num.png"), plot = p, path = getwd(), 
           device = "png")
    
    p<- ggplot(d.block_description, aes(x = x, y = prob)) +
      geom_bar(stat = "identity", fill="skyblue") +
      ylim(0, 0.5) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6)) +
      labs(title = paste0(research.question,"?"),x = "ICD10 block", 
           y= "percentage")
    ggsave(paste0(research.question, "_block_description.png"), plot = p, 
           path = getwd(), device = "png")
    
    write_xlsx(data.frame(no=1:241, description = icd10_blocks$description, 
                          probability = prob), 
               paste0(research.question,".xlsx"))
    
    write_xlsx(d.3St, paste0("icd10_3St_", research.question, ".xlsx"))
    
    #distribution of number of diagnoses
  }else if(isTRUE(diag_no_diagnoses)){
    x<- episodedf$no.matches_diag
    x<- x[!is.na(x)]
    
    im<- table(x)
    allocation<- as.numeric(names(im))
    names(im)<- NULL
    prob<- numeric(max(allocation) + 1)
    prob[allocation + 1]<- im
    prob<- prob / sum(prob)
    distr.df<- data.frame(no.diagnoses = seq(0, max(allocation)),
                          no.occurrences = prob)
    
    p<- ggplot(distr.df, aes(x = no.diagnoses, y = no.occurrences)) +
      geom_bar(stat = "identity", fill="skyblue") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(research.question,"?"), x = "number of occurrences", 
           y= "percentage")
    ggsave(paste0(research.question, "_distr_matches.png"), plot = p, 
           path = getwd(), device = "png")
  }
}

dt_int_age<- function(research.question, episodedf, agelims){
  x<- episodedf$age
  if(sum(is.na(x)) > 0){
    warning(paste("There are", sum(is.na(x)), "entries where no birth date is available. This corresponds to",
                  round(sum(is.na(x)) / length(x), 2) * 100), "% of all entries. The NA results are dropped in the following.")
    x<- x[!is.na(x)]
  }
  
  im<- table(x)
  ages<- as.numeric(names(im))
  names(im)<- NULL
  prob<- numeric(length = agelims[2])
  prob[ages]<- im
  if(agelims[1]>=2){
    prob<- prob[-seq(1, agelims[1] - 1)]
  }
  prob<- prob / sum(prob)
  d<- data.frame(x  = seq(agelims[1], agelims[2]), prob=prob)|>
    filter(x >= agelims[1])
  
  d$y<- age.weights(agelims[1],agelims[2])
  
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
  write_xlsx(d, paste0(research.question, ".xlsx"))
}

dt_int_escalation_lab<- function(research.question, episodedf, escalation_lab){
  if(escalation_lab == "any"){
    #lab test, generally
    x<- episodedf$labtest - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "blood"){
    #normal blood
    x<- episodedf$blood_normal - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "diff_blood"){
    #differential blood test
    x<- episodedf$blood_large_test - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "CRP"){
    #CRP
    x<- episodedf$CRP - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "erythocytes"){
    #erythocytes
    x<- episodedf$ery - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "leucocytes"){
    #leucocytes
    x<- episodedf$leuko - episodedf$start_date
    x<- x[!is.na(x)]
  }else if(escalation_lab == "procalcitonin"){
    #procalcitonin
    x<- episodedf$procalcitonin - episodedf$start_date
    x<- x[!is.na(x)]
  }
  
  im<- table(x)
  allocation<- as.numeric(names(im))
  names(im)<- NULL
  prob<- numeric(max(allocation) + 1)
  prob[allocation + 1]<- im
  prob<- prob / sum(prob)
  d<- data.frame(x = seq(0, max(allocation)), prob = prob)
  
  
  p<- ggplot(d, aes(x = x, y = prob)) +
    geom_bar(stat = "identity", fill="skyblue") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste0(research.question, "?"),
         x = "number of days since initial cough", 
         y= "percentage",
         subtitle = paste("based on", length(x), "episodes"))
  ggsave(paste0(research.question,".png"), plot = p, path = getwd(), 
         device = "png")
  
  write_xlsx(d, paste0(research.question, ".xlsx"))
}

dt_int_escalation_antibiotics<- function(research.question, episodedf){
  x<- episodedf$time_until_antibiotic
  x<- x[!is.na(x)]
  
  im<- table(x)
  allocation<- as.numeric(names(im))
  names(im)<- NULL
  prob<- numeric(max(allocation) + 1)
  prob[allocation + 1]<- im
  prob<- prob / sum(prob)
  d<- data.frame(x = seq(0, max(allocation)), prob = prob)
  
  p<- ggplot(d, aes(x = x, y = prob)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste0(research.question,"?"),
         x = "number of days since initial cough", 
         y= "percentage",
         subtitle = paste("based on", length(x), "episodes"))
  ggsave(paste0(research.question, ".png"), plot = p, path = getwd(), 
         device = "png")
  
  write_xlsx(d, paste0(research.question, ".xlsx"))
}

dt_int_escalation_ueberweis<- function(research.question, episodedf){
  if(escalation.parameters_ueberweis[["type"]] == 1){
    #referral to specialist care
    x<- cbind(episodedf$FR_Pneumo - episodedf$start_date, 
              episodedf$FR_Radiol - episodedf$start_date, 
              episodedf$FR_KH - episodedf$start_date)
    x<- apply(x, 1, function(row) {
      if (all(is.na(row))) {
        return(NA)
      } else {
        return(min(row, na.rm = TRUE))
      }
    })
    x<- x[!is.na(x)]
    warning(paste("There were", length(x), 
                  "episodes when patients were referred to a specialist."))
  }else if(escalation.parameters_ueberweis[["type"]] == 2){
    #referral pulmonologist
    x<- episodedf$FR_Pneumo - episodedf$start_date
    x<- x[!is.na(x)]
    warning(paste("There were", length(x), 
                  "episodes when patients were referred to a pulmonologist."))
  }else if(escalation.parameters_ueberweis[["type"]] == 3){
    #referral radiologist
    x<- episodedf$FR_Radiol - episodedf$start_date
    x<- x[!is.na(x)]
    warning(paste("There were", length(x), 
                  "episodes when patients were referred to a radiologist."))
  }else if(escalation.parameters_ueberweis[["type"]] == 4){
    #referral hospital
    x<- episodedf$FR_KH - episodedf$start_date
    x<- x[!is.na(x)]
    warning(paste("There were", length(x), 
                  "episodes when patients were referred to a hospital."))
  }
  
  im<- table(x)
  allocation<- as.numeric(names(im))
  names(im)<- NULL
  prob<- numeric(max(allocation) + 1)
  prob[allocation + 1]<- im
  prob<- prob / sum(prob)
  d<- data.frame(x=seq(0, max(allocation)), prob = prob)
  
  
  p<- ggplot(d, aes(x = x, y = prob)) +
    geom_bar(stat = "identity", fill="skyblue") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste0(research.question, "?"),
         x = "number of days since initial cough", 
         y= "percentage",
         subtitle = paste("based on", length(x), "episodes"))
  ggsave(paste0(research.question, ".png"), plot = p, path = getwd(), 
         device = "png")
  
  write_xlsx(d, paste0(research.question, ".xlsx"))
}
