

# date.patient.overlap<- function(...){
#   # collect all the different data frames
#   tables<- list(...)
#   no.tables<- length(tables)
#   
#   # pull all unique patient id-date combinations
#   unique.dates.patient.dfs<- list()
#   
#   for(i in 1:no.tables){
#     #for each data frame extract unique patient-date combinations
#     unique.dates.patient.dfs[[i]]<- unique(cbind(tables[[i]]$uniPatID,tables[[i]]$TG_DateNum))
#     
#   }
#   
#   all.unique.days.patients<- matrix(unlist(unique.dates.patient.dfs),ncol = 2)
#   
#   
# }

a<- matrix(c(1:10), ncol = 2,byrow = TRUE)
a
unique(a)

a<- as.data.frame(a)
b<-10+a
c<-10+b
d<- list(a,b,c)
sapply(d, rbind)
unlist(d)
matrix(unlist(d), ncol = 2)

library(purrr)


library(dplyr)
library(purrr)

merge_datasets <- function(dfs) {
  reduce(dfs, full_join, by = c("PatID", "TG_DateNum")) |>
    filter(!is.na(PatID) & !is.na(TG_DateNum))
}



mf<- function(){
  a<- "icd10_rough_I"
  return(filtered.data|> count(as.factor("icd10_rough_I")) |> arrange(desc(n)))
}

filtered.data|> count(as.factor("icd10_rough_I"))


output<- numeric(10)
first<- LETTERS[1:10]
second<- rep(1:5,2)
for(i in 1:10){
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
    outpt[i]<- 5
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
    stop("The function is not able to map at least one entry to its chapter.")
  }
}

output


for(k in 1:length(vector)){
  
}





get_icd10_chapter <- function(icd10_codes) {
  # Create a data frame to store the mapping of ICD10 codes to chapters
  icd10_mapping <- data.frame(
    icd10_code = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"),
    icd10_chapter = c("Certain infectious and parasitic diseases", "Neoplasms", "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism", "Endocrine, nutritional and metabolic diseases", "Mental and behavioral disorders", "Diseases of the nervous system", "Diseases of the eye and adnexa", "Diseases of the ear and mastoid process", "Diseases of the circulatory system", "Diseases of the respiratory system", "Diseases of the digestive system", "Diseases of the skin and subcutaneous tissue", "Diseases of the musculoskeletal system and connective tissue", "Diseases of the genitourinary system", "Pregnancy, childbirth and the puerperium", "Certain conditions originating in the perinatal period", "Congenital malformations, deformations and chromosomal abnormalities", "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", "Injury, poisoning and certain other consequences of external causes", "External causes of morbidity and mortality", "Factors influencing health status and contact with health services", "Codes for special purposes", "Supplementary factors related to causes of morbidity and mortality classified elsewhere", "Codes for special purposes", "Codes for special purposes", "Codes for special purposes", "Codes for special purposes")
  )
  
  # Extract the first character or first two characters if they are a letter from each ICD10 code to get the chapter
  icd10_chapter <- substr(icd10_codes, 1, ifelse(substr(icd10_codes, 2, 2) == ".", 2, 1))
  
  # Replace the chapter codes with the corresponding chapter names
  icd10_chapter <- factor(icd10_chapter, levels = icd10_mapping$icd10_code, labels = icd10_mapping$icd10_chapter)
  
  return(icd10_chapter)
}








# Step 1: Create a vector of letters and random integer numbers
set.seed(1234) # Set seed for reproducibility
letters <- letters[1:10] # Create a vector of letters from a to j
counts <- sample(0:100, 10, replace = TRUE) # Generate random integer numbers between 0 and 100
df <- data.frame(letters = factor(letters), counts = counts) # Combine letters and counts into a data frame

# Step 2: Load ggplot2 package and create bar plot
library(ggplot2) # Load ggplot2 package

# Create bar plot with title, axis labels, and sorted bars
plot <- ggplot(df, aes(x = reorder(letters, -counts), y = counts/sum(counts))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Random Data Distribution", x = "Letters", y = "Probability") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + # Add units to margin values
  scale_y_continuous(labels = scales::percent_format(1, scale = 100)) + # Scale y-axis to represent density
  geom_text(aes(label = ""), vjust = -1) # Remove letters above each bar

# Print data frame
print("Data Frame:")
print(df)

# Display bar plot
print("Bar Plot:")
print(plot)

# Save plot to working directory as PNG image file
ggsave("random_data_distribution.png", plot, width = 10, height = 6)




# Plot a bar plot with highest class first
# function to sort according to highest bar
library(dplyr)
bar.plot.fct<- function(data,variable){
  #data.vec is the data frame used as input
  # variable is column which is to be sorted. Has to be set in "" quotation marks.
  df<- data|> count(as.factor(variable)) |> arrange(desc(n))
  browser()
  ggplot(df,aes(as.factor(variable)))+
    geom_bar()
}

library(ggplot2)

#table with code/ frequency
as.data.frame(table(as.factor(filtered.data$icd10_rough_I)))

t<- filtered.data$icd10_rough_I=="J"
d<- as.factor(filtered.data$icd10_rough_II[t])
hist(d)
ggplot(as.data.frame(d),aes(x=d)) +
  geom_bar()

ggplot(as.data.frame(filtered.data),aes(x=icd10_rough_I)) +
  geom_bar()

# data visualization:
# roughness: 1=1st letter of ICD10 code; 2=1st letter of ICD10 code plus first digit; 3=1st letter of ICD10 code plus first two digits
# class: if roughness>1, give the desired sub-code part: e.g. roughness=2, class=L; or roughness=3, class=L5
new.plotter<- function(roughness,class=FALSE){
  if(roughness==1){
    dt<- as.data.frame(table(as.factor(filtered.data$icd10_rough_I)))
  }else if(roughness==2){
    t<- numeric(dim(filtered.data)[1])
    for(i in 1:dim(filtered.data)[1]){
      t[i]<- class==filtered.data$icd10_rough_I[i]
    }
    t<- as.logical(t)
    im<- as.factor(filtered.data$icd10_rough_II[t])
    dt<- as.data.frame(table(im))
    colnames(dt)<- c("code.segment","Freq")
  }else{
    t<- numeric(dim(filtered.data)[1])
    for(i in 1:dim(filtered.data)[1]){
      t[i]<- class==filtered.data$icd10_rough_II[i]
    }
    t<- as.logical(t)
    im<- as.factor(filtered.data$icd10_rough_III[t])
    dt<- as.data.frame(table(im))
    colnames(dt)<- c("code.segment","Freq")
  }
  plot <- ggplot(dt, aes(x = reorder(code.segment, -Freq), y = Freq/sum(Freq))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Distribution of Cough Diagnosis by ICD10 code", x = "ICD10 code", y = "Probability") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + # Add units to margin values
    scale_y_continuous(labels = scales::percent_format(1, scale = 100)) + # Scale y-axis to represent density
    geom_text(aes(label = ""), vjust = -1) # Remove letters above each bar
  
  if(class==FALSE){
    class.insert<-""
  }else{
    class.insert<- as.character(class)
  }
  
  ggsave(paste0("distribution_cough_icd10_",class.insert,".png"), plot, width = 10, height = 6)
  
  
}


new.plotter(2,class = "J")



t<- numeric(dim(filtered.data)[1])
for(i in 1:dim(filtered.data)[1]){
  t[i]<- "L0"==filtered.data$icd10_rough_II[i]
}
im<- as.factor(filtered.data$icd10_rough_II[t])
dt<- as.data.frame(table(im))
colnames(dt)<- c("var1","Freq")




###################### done at home
###matching assessment

# assessment of overlap between the 
assessment.date.range<- function(data.sets, var_1, centre=FALSE){
  #data.sets is a list of data sets to be matched. The data sets must be data frames.
  #var_1 and var_2 are the variables by which the data sets are to be merged. 
  #If the centre arguments are set to be TRUE, the smallest value of the output matrix is subtracted from it. This is useful for TG_Date, since the numbers become much smaller and thus better to read and compare.
  
  n<- length(data.sets)
  mat<- matrix(0,nrow=n,ncol = 2)
  
  
  for(i in 1:n){
    mat[i,]<- range(data.sets[[i]][var_1])
  }
  if(center==TRUE){
    mat<- mat-min(mat)
  }
  
  return(mat)
}

# calculates share of overlap between the different uniPatID levels. It looks at all available patients and asks, Is that patient in each respective data set? To answer this question, it gives the share of patients in each individual data set to all available patients. 
assessment.unipat.overlap<- function(data.sets){
  # data.sets is a list of data frames which contain the uniPatID as factors (as.factor()).
  
  n<- length(data.sets)
  pat.vec<- data.sets[[1]]["uniPatID"]
  for(i in 2:n){
    pat.vec<- c(pat.vec,data.sets[[i]]["uniPatID"])
  }
  l<- length(levels(as.factor(pat.vec)))
  percentages<- numeric(n)
  for(i in 1:n){
    percentages[i]<- round(length(levels(as.factor(data.sets[[i]]["uniPatID"])))/l,2)
  }
  
  return(percentages)
}

# assessment.killer.rows finds the number of NAs in each column of the data set. It is to be used with an object from merge.datasets where na.rm was set to be FALSE. As the default in na.rm, incomplete entries are removed from the final data set. By identifying which columns are responsible for incomplete entries, the dataset(s) which match poorly with the others can be identified. 
assessment.killer.rows<- function(matched.dataset){
  TF<- is.na(matched.dataset)
  TF.vec<- colMeans(TF)
  names(TF.vec)<- colnames(matched.dataset)
  return(TF.vec)
}

### visualize the plots

#roughness==1
new.plotter(1)

#roughness==2
for(i in LETTERS){
  new.plotter(2,class=i)
}

#roughness==3. Check whether this works.
for(i in LETTERS){
  for(j in 0:9){
    class.name<- paste0(i,j)
    new.plotter(3,class=class.name)
  }
}

assign("asdf.vec",a,envir = .GlobalEnv)



##################16.05.



# extract viable patients to pats
pats<- c()

for(i in seq(1,length(pats))){
  current.pat<- pats[i]
  
}

patient.file<- function(dl){
  # depends on a data list dl which contains datasets of patients with uniPatID
  # depends on the dplyr package
  
  #procedure: find for a patient all dates of interest. loop over all patients.
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  im<- as.data.frame(cbind(pat.vec,date.vec))
  im<- distinct(im)
  col.names.vec<- colnames(dl[[1]])
  for(i in seq(2,l.dl)){
    col.names.vec<- c(col.names.vec,colnames(dl[[i]]))
  }
  col.names.vec<- levels(as.factor(col.names.vec))
  output<- matrix(0,nrow = dim(im)[1], ncol = length(col.names.vec))
  colnames(output)<- col.names.vec
  output<- as.data.frame(output)
  output$uniPatID<- im[,1]
  output$TG_DateNum<- im[,2]
  
  for(i in seq(1,dim(im)[1])){
    selected.pat<- im[i,1]
    selected.date<- im[i,2]
    
  }
  
  
}

patient.file_2<- function(dl){
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  im<- as.data.frame(cbind(pat.vec,date.vec))
  im<- distinct(im)
  col.names.vec<- colnames(dl[[1]])
  for(i in seq(2,l.dl)){
    col.names.vec<- c(col.names.vec,colnames(dl[[i]]))
  }
  col.names.vec<- levels(as.factor(col.names.vec))
  
  
}





##############


patient.file_2<- function(dl, no.patients="all"){
  # dl is a list of data frames containing all the data to be merged. Order of datasets (strictly stick to it!): Stamm, Diag.
  # no.patients is the number of patients that the function analyzes. If set to "all," it will loop through all patients. Otherwise it has to be a vector with a start number and an end number (e.g. c(5,10) to analyze patient visits 5 to 10)
  l.dl<- length(dl)
  dimensions<- matrix(0,nrow = l.dl, ncol = 2)
  for(i in seq(1,l.dl)){
    dimensions[i,]<- dim(dl[[i]])
  }
  pat.vec<- numeric(sum(dimensions[,1]))
  date.vec<- pat.vec
  
  ticker<- 1
  for(i in seq(1,l.dl)){
    pat.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$uniPatID
    date.vec[seq(ticker,dimensions[i,1])]<- dl[[i]]$TG_DateNum
    ticker<- ticker+ dimensions[i,1]
  }
  im<- as.data.frame(cbind(pat.vec,date.vec))
  im<- distinct(im)
  col.names.vec<- colnames(dl[[1]])
  for(i in seq(2,l.dl)){
    col.names.vec<- c(col.names.vec,colnames(dl[[i]]))
  }
  col.names.vec<- levels(as.factor(col.names.vec))
  
  if(no.patients=="all"){
    beginning<- 1
    end<- sum(im[,1])
  }else{
    end<- no.patients
  }
  
  output<- matrix(0,nrow = dim(im)[1], ncol = length(col.names.vec))
  colnames(output)<- col.names.vec
  output<- as.data.frame(output)
  output$uniPatID<- im[,1]
  output$TG_DateNum<- im[,2]
  
  for(i in seq(beginning,end)){
    visit<- as.vector(im[i])
    patient<- visit[1]
    date<- visit[2]
    for (j in seq(1,l.dl)) {
      file<- filter(dl[[j]],uniPatID==patient,TG_DateNum==date)
      if(dim(file)[1]==1){
        # add entry at the right place
      }else if(dim(file)[1]>1){
        # add at the right place AND add columns, if necessary
      }
    }
  }
}


####Notes for data preparation



#IPC data

ipc.filter<- function(ipcdata){
  pat.date.cols<- distinct(data.frame(ipcdata$uniPatID,ipcdata$TG_DateNum))
  n_1<- dim(cleaned.df)[1]
  p_1<- dim(ipcdata)[2]
  cleaned.df<- as.data.frame(matrix(0,nrow = n_1, ncol = p_1))
  cleaned.df[,1:2]<- pat.date.cols
  
  lookup <- with(ipcdata, interaction(uniPatID, TG_DateNum, drop = TRUE))
  
}

# cdouble check whether lookup in the parallelized function par.patient.file really finds the right row!!!




library(dplyr)

ipc.filter <- function(ipcdata) {
  # Step 1: Find cases of multiple entries
  ipcdata <- ipcdata %>%
    group_by(uniPatID, TG_DateNum) %>%
    mutate(index = row_number()) %>%
    ungroup()
  
  # Step 2: Create the first data frame
  df1 <- ipcdata
  
  # Step 3: Create the second data frame
  df2 <- ipcdata %>%
    filter(!duplicated(uniPatID, TG_DateNum))
  
  # Step 4: Return both data frames as a list
  list(df1 = df1, df2 = df2)
}

# new version 05.07.23
ipc.filter <- function(ipcdata) {
  # create factor 'index' to find distinct cases and their frequency of occurrence
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




###chatbot suggestion
cases_r1<- cases
for(i in seq(2,nrow(cases))){
  if(cases$uniPatID[i]==cases$uniPatID[i-1]){
    if(cases$end_date[i-1]>cases$start_date[i]){
      #create duplicate cases, then remove the duplicates
      cases_r1$start_date[i]<- cases_r1$start_date[i-1]
      cases_r1$end_date[i-1]<- cases_r1$end_date[i]
    }
  }
}
cases_r1<- distinct(cases_r1)

all.equal(cases_r1, cases_r1f)

cases_r1 <- cases
for (i in seq(2, nrow(cases))) {
  if (cases$uniPatID[i] == cases$uniPatID[i - 1]) {
    if (cases$end_date[i - 1] > cases$start_date[i]) {
      # Create duplicate cases, then remove the duplicates
      cases_r1$start_date[i] <- cases_r1$start_date[i - 1]
      cases_r1$end_date[i - 1] <- cases_r1$end_date[i]
    }
  }
}
cases_r1 <- distinct(cases_r1)

# Check for equality
all.equal(cases_r1, cases_r1f)








