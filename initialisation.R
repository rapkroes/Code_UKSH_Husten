# initialisation

### read in data from data base, alt. generate data for debugging
if(real.data){
  setwd("N:/StudentischeHilfskraefte/_Kroes (Christoph)/Routinedaten/01_Husten/R/DBnew2")
  names.vec<- c("Diag","ICPC","Labor","Stamm","Ueberweis", "PZN")
  
  for (i in seq(1,length(names.vec))) {
    file.name<- paste0("DB_",names.vec[i],"1_v03",".csv")
    im.file<- read.csv(file = file.name, header = TRUE, sep = ";")
    assign(names.vec[i],im.file)
  }
  rm(file.name,im.file)
  
  # Read in supplementary data
  ListeKrankenkassen<- read.csv("https://github.com/rapkroes/Code_UKSH_Husten/raw/main/ListeKrankenkassen.csv", sep = ";")
  altersstruktur_deutschland<- read.csv("https://github.com/rapkroes/Code_UKSH_Husten/raw/main/altersstruktur%20deutschland.csv", sep = ";")
  colnames(altersstruktur_deutschland)<- c("altersjahre","bevoelkerungsstand")
  icd10_blocks<- read.csv("https://github.com/rapkroes/Code_UKSH_Husten/raw/main/icd10_blocks.csv", sep = ";")
  icd10_description<- read.csv("https://github.com/rapkroes/Code_UKSH_Husten/blob/288b25aa745f178d2705d990e5106ad6db320a4d/icd10_description.csv")
  selected.lab.descriptions<- read.csv("https://github.com/rapkroes/Code_UKSH_Husten/raw/main/freitextanalyse.csv", sep = ";")
  ICD3St<- read.csv("https://raw.githubusercontent.com/rapkroes/Code_UKSH_Husten/main/Table_ICD_3St.csv", sep = ";")
  source("//fileserver1.uni-luebeck.de/ifa_homes/raphaelkroes/Documents/GitHub/Code_UKSH_Husten/functions.R")
  
}else{
  source("fakedgp_work.R")
}


# cleaning of DB entries
diag.up<- Diag |> 
  select(uniPatID, TG_DateNum, Typ, icd10)|>
  distinct()
diag.up$chronic<- 0
diag.up$chronic[diag.up$chronic=="DD"]<- 1
diag.up$unipat_date<- paste(diag.up$uniPatID, diag.up$TG_DateNum, sep = "_")

#find patients with Chronic Pulmonary Obstructive Disease (COPD)
copd.vec<- unique(diag.up$uniPatID[grepl("J44", diag.up$icd10) | 
                                     grepl("J 44", diag.up$icd10)])

# find all other chronically diseased people
im<- diag.up|>
  select(uniPatID, Typ, icd10)|>
  distinct()
chronic.patients<- im$uniPatID[im$Typ=="DD"]
chronic.patients<- as.data.frame(table(chronic.patients))
rm(im)
diag.up$chronic<- NULL

icpc.up<- ICPC |>
  select(uniPatID, TG_DateNum, TGTyp, icpc2)
icpc.up$unipat_date<- paste(icpc.up$uniPatID, icpc.up$TG_DateNum, sep = "_")

labor.up<- Labor
labor.up$labtest<- 1
labor.up<- labor.up|>
  select(uniPatID, TG_DateNum, Untersuchung, Unterstext, labtest)|>
  distinct()
colnames(selected.lab.descriptions)[1]<- "Unterstext"
labor.up<- left_join(labor.up, selected.lab.descriptions, by = "Unterstext")
labor.up$unipat_date<- paste(labor.up$uniPatID, labor.up$TG_DateNum, sep = "_")

stamm.up<- Stamm
stamm.up$PKV<- IK2PKV(stamm.up$IK)
stamm.up<- stamm.up|> 
  select(-PatID, -index_i, -IK, -Kasse, -PLZ)|>
  distinct()|>
  arrange(uniPatID,desc(TG_DateNum))
stamm.up$unipat_date<- paste(stamm.up$uniPatID, stamm.up$TG_DateNum, sep = "_")

ueberweis.up<- Ueberweis|>
  select(uniPatID, TG_DateNum, FR_Pneumo, FR_Radiol, FR_KH) |>
  filter(FR_Pneumo > 0 | FR_Radiol > 0 | FR_KH > 0) |>
  distinct()
ueberweis.up$unipat_date<- paste(ueberweis.up$uniPatID, ueberweis.up$TG_DateNum, sep = "_")

aub<- Diag|>
  filter(Typ == "AUB")|>
  select(uniPatID, TG_DateNum)
colnames(aub)[2]<- "TG_DateNum_AUB"

pzn.up<- PZN[substr(PZN$ATC, 1, 3) == "J01",]|>
  filter(TG == "ME")|>
  select(uniPatID, TG_DateNum)
colnames(pzn.up)[2]<- "TG_DateNum_antibiotic"


### cleaning: one row per patient/ date combination
if(length(unique(labor.up$unipat_date)) != nrow(labor.up)){
  labor.up<- data.repair.new(labor.up)
  if(length(unique(labor.up$unipat_date)) != nrow(labor.up)){
    warning("Cleaning of labor.up (1 row per patient/ date combination) has failed.")
  }
}

if(length(unique(stamm.up$unipat_date)) != nrow(stamm.up)){
  stamm.up<- data.repair.new(stamm.up)
  if(length(unique(stamm.up$unipat_date)) != nrow(stamm.up)){
    warning("Cleaning of stamm.up (1 row per patient/ date combination) has failed.")
  }
}

if(length(unique(ueberweis.up$unipat_date)) != nrow(ueberweis.up)){
  ueberweis.up<- data.repair.new(ueberweis.up)
  if(length(unique(ueberweis.up$unipat_date)) != nrow(ueberweis.up)){
    warning("Cleaning of ueberweis.up (1 row per patient/ date combination) has failed.")
  }
}

cutoff_diag<- quantile(table(diag.up$unipat_date), probs = cutoff_quantile)
if(length(unique(diag.up_2$unipat_date)) != nrow(diag.up_2)){
  diag.up_2<- aligner(diag.up, cutoff = cutoff_diag, var.name = "diag")
  if(length(unique(diag.up_2$unipat_date)) != nrow(diag.up_2)){
    warning("Cleaning of diag.up (1 row per patient/ date combination) has failed.")
  }
}

### create baseline episode data frame
episodes<- icpc.up|>
  filter(icpc2 == "R05")|>
  select(uniPatID, TG_DateNum)
colnames(episodes)[2]<- "start_date"
episodes$end_date<- episodes$start_date + selected.max.length.of.episode

### create non-overlapping episodes (decision rules 1 and 2), add episodeIDs
# decision rule 1: If two episodes overlap, they are merged into one longer episode.
# decision rule 2: If two episodes overlap, they earlier episode is cut short: Its end date is set to the day before the second episode starts.
# In consequence, there are no overlapping episodes.
episodes_r1<- episodes.dr1(episodes)
episodes_r1$episode.ID<- paste0(episodes_r1$uniPatID, episodes_r1$start_date)
episodes_r2<- episodes.dr2(episodes)
episodes_r2$episode.ID<- paste0(episodes_r2$uniPatID, episodes_r2$start_date)

if(sum(episodes.overlap.finder(episodes_r1)$overlap) > 0){
  warning("episodes_r1 appears to contain overlapping episodes.")
}
if(sum(episodes.overlap.finder(episodes_r2)$overlap) > 0){
  warning("episodes_r2 appears to contain overlapping episodes.")
}

### complete full episode data frames
# decision rule 1
episodes.stamm_r1<- add.stamm(episodes_r1, stamm.up, 100, 10)
episodes.diag_r1<- add.diag(episodes_r1, diag.up_2, 100, 10)
episodes.labor_r1<- add.lab(episodes_r1,labor.up)
episodes.ueberweis_1<- add.ueberweis(episodes_r1, ueberweis.up)
full_r1<- cbind(arrange(episodes.stamm_r1, uniPatID, start_date),
                arrange(episodes.diag_r1, uniPatID, start_date),
                arrange(episodes.labor_r1, uniPatID, start_date),
                arrange(episodes.ueberweis_1, uniPatID, start_date))
full_r1<- full_r1[,!duplicated(colnames(full_r1))]
full_r1$age<- year(TG_DateNum2date(full_r1$start_date)) - full_r1$Geburtsjahr
full_r1<- filter(full_r1, age <= 100)
full_r1$length_of_episode<- full_r1$end_date - full_r1$start_date

# decision rule 2
episodes.stamm_r2<- add.stamm(episodes_r2, stamm.up, 100, 10)
episodes.diag_r2<- add.diag(episodes_r2, diag.up_2, 100, 10)
episodes.labor_r2<- add.lab(episodes_r2,labor.up)
episodes.ueberweis_2<- add.ueberweis(episodes_r2, ueberweis.up)
full_r2<- cbind(arrange(episodes.stamm_r2, uniPatID, start_date),
                arrange(episodes.diag_r2, uniPatID, start_date),
                arrange(episodes.labor_r2, uniPatID, start_date),
                arrange(episodes.ueberweis_2, uniPatID, start_date))
full_r2<- full_r2[,!duplicated(colnames(full_r2))]
full_r2$age<- year(TG_DateNum2date(full_r2$start_date)) - full_r2$Geburtsjahr
full_r2<- filter(full_r2, age <= 100)
full_r2$length_of_episode<- full_r2$end_date - full_r2$start_date

#no.chronic diseases 
colnames(chronic.patients)<- c("uniPatID", "no.chronic.diseases")
chronic.patients$uniPatID<- chronic.patients$uniPatID|>
  as.character()|>
  as.numeric()
full_r1<- left_join(full_r1, chronic.patients, by = "uniPatID")
full_r2<- left_join(full_r2, chronic.patients, by = "uniPatID")

for(k in 1:2){
  im<- get(paste0("full_r", k))
  
  #find cases where at least one initial diagnosis was preliminary
  first.cols<- grep("first_diag", colnames(im))
  last.cols<- grep("last_diag", colnames(im))
  
  im_first<- apply(im[,first.cols], 2, FUN = function(dv){
    im_dv<- substr(dv,nchar(dv),nchar(dv))
    return(grepl("V",im_dv))
  })
  im_last<- apply(im[,last.cols],2,FUN = function(dv){
    im_dv<- substr(dv,nchar(dv),nchar(dv))
    return(grepl("V",im_dv))
  })
  
  im$first_diag_V_present<- rowSums(im_first)>0
  im$last_diag_V_present<- rowSums(im_last)>0
  
  #add start month/ quarter
  im$start_month<- month(TG_DateNum2date(im$start_date))
  im$start_quarter<- quarter(TG_DateNum2date(im$start_date))
  
  assign(paste0("full_r",k),im)
  rm(im)
}

full_r1$FR_KH[full_r1$FR_KH == 0]<- NA
full_r1$FR_Pneumo[full_r1$FR_Pneumo == 0]<- NA
full_r1$FR_Radiol[full_r1$FR_Radiol == 0]<- NA

full_r2$FR_KH[full_r2$FR_KH == 0]<- NA
full_r2$FR_Pneumo[full_r2$FR_Pneumo == 0]<- NA
full_r2$FR_Radiol[full_r2$FR_Radiol == 0]<- NA

full_r1<- left_join(full_r1, aub, by = "uniPatID")
has.AUB<- full_r1$start_date <= full_r1$TG_DateNum_AUB & 
  full_r1$end_date >= full_r1$TG_DateNum_AUB
full_r1$TG_DateNum_AUB[!has.AUB]<- NA
full_r1<- arrange(full_r1, episode.ID, TG_DateNum_AUB)
episode.duplicate<- duplicated(full_r1$episode.ID)
full_r1<- full_r1[!episode.duplicate,]
full_r1$time_until_AUB<- full_r1$TG_DateNum_AUB - full_r1$start_date

full_r2<- left_join(full_r2, aub, by = "uniPatID")
has.AUB<- full_r2$start_date <= full_r2$TG_DateNum_AUB & 
  full_r2$end_date >= full_r2$TG_DateNum_AUB
full_r2$TG_DateNum_AUB[!has.AUB]<- NA
full_r2<- arrange(full_r2, episode.ID, TG_DateNum_AUB)
episode.duplicate<- duplicated(full_r2$episode.ID)
full_r2<- full_r2[!episode.duplicate,]
full_r2$time_until_AUB<- full_r2$TG_DateNum_AUB - full_r2$start_date

full_r1<- left_join(full_r1, pzn.up, by = "uniPatID")
has.PZN<- full_r1$start_date <= full_r1$TG_DateNum_antibiotic & 
  full_r1$end_date >= full_r1$TG_DateNum_antibiotic
full_r1$TG_DateNum_antibiotic[!has.PZN]<- NA
full_r1<- arrange(full_r1, episode.ID, TG_DateNum_antibiotic)
episode.duplicate<- duplicated(full_r1$episode.ID)
full_r1<- full_r1[!episode.duplicate,]
full_r1$time_until_antibiotic<- full_r1$TG_DateNum_antibiotic - 
  full_r1$start_date

full_r2<- left_join(full_r2, pzn.up, by = "uniPatID")
has.PZN<- full_r2$start_date <= full_r2$TG_DateNum_antibiotic & 
  full_r2$end_date >= full_r2$TG_DateNum_antibiotic
full_r2$TG_DateNum_antibiotic[!has.PZN]<- NA
full_r2<- arrange(full_r2, episode.ID, TG_DateNum_antibiotic)
episode.duplicate<- duplicated(full_r2$episode.ID)
full_r2<- full_r2[!episode.duplicate,]
full_r2$time_until_antibiotic<- full_r2$TG_DateNum_antibiotic - 
  full_r2$start_date

message("Both episode data frames, r1 and r2, have been successfully generated.")
