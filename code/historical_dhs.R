####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#Taken from https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/2013_tab_data2.R

if(Sys.info()[["user"]]=="alex"){
  wd <- "~/git/p20_indicator_time_trends"
  wd2 <- "~/git/p20_private_data/project_data/DHS auto"
}else if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends"
  wd2 <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends/data/DHSauto"
}else{
  wd <- "D:/git/p20_indicator_time_trends"
  wd2 <- "D:/DHSauto"
}

setwd(wd)

'%!in%' <- function(x,y)!('%in%'(x,y))
povcalcuts <- fread("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
# povcalcuts<- fread("D:/git/poverty_trends/data/P20incometrends.csv")
dhsmeta<- fread("data/dhs_meta_data20190524.csv")
dhsmeta$WealthIndex[which(dhsmeta$Country.=="Burkina Faso" & dhsmeta$dhs_recode_code==62)]=1
dhsmeta<- subset(dhsmeta, Recode.Structure.!="DHS-I" & WealthIndex == 1)

dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")]<-"Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")]<-"Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")]<-"Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")]<-"Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")]<-"Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")]<-"Yemen, Republic of"
#Afghanistan, Cambodia, Equatorial Guinea and Eritrea have had DHS surveys but don't have PovcalNet data
names(dhsmeta)[which(names(dhsmeta)=="Country.")] <- "CountryName"

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename")])

variables <- c("birth.registration")
grid = as.data.table(expand.grid(filename=unique(dhsmeta2$filename), variable = variables))

dhsmeta2 <- merge(grid, dhsmeta2, all=T)

isos = povcalcuts[,c("CountryName","CountryCode")]
names(isos)[which(names(isos)=="CountryCode")] <- "iso3"
isos=unique(isos)
af=data.frame(CountryName="Afghanistan",iso3="AFG")
isos=rbind(isos,af)
povcalcuts <- join(dhsmeta2,isos,by=c("CountryName"))

keep <- c("iso3","surveyyr","filename","variable")
povcalcuts <- povcalcuts[,keep, with=F]


povcalcuts = povcalcuts[order(povcalcuts$filename,povcalcuts$surveyyr),]
povcalcuts=subset(povcalcuts, filename!="SNHR7IDT")

pop = WDI(country="all",indicator="SP.POP.TOTL",start=2000,end=2000,extra=T)
regions = unique(pop[,c("iso3c","region")])
names(regions) = c("iso3","region")

povcalcuts = merge(povcalcuts,regions,by="iso3")
# povcalcuts = subset(povcalcuts, region %in% c("Sub-Saharan Africa", "Middle East & North Africa"))

not.dhs = c(
  "CIHR50DT",
  "GHHR7ADT",
  "MZHR51DT",
  "NGHR72DT",
  "ZAHR33DT",
  "TZHR4ADT",
  "TZHR51DT",
  "TZHR6ADT",
  "UGHR6ADT"
)
povcalcuts = subset(povcalcuts,filename %!in% not.dhs)
povcalcuts$filename[which(povcalcuts$filename=="GNHR52DT")] = "GNHR53DT"

povcalcuts = subset(povcalcuts,filename!="IAHR74DT")

rm(grid,dhsmeta,dhsmeta2)
gc()

####Run function####
setwd(wd2)

dataList <- list()
dataIndex <- 1

pb = txtProgressBar(max=nrow(povcalcuts),style=3)
# Loop through every povcalcut
for(i in 1:nrow(povcalcuts)){
  povcal_subset = povcalcuts[i,]
  setTxtProgressBar(pb, i)
  # Pull some coded info out of the dir name
  country <- tolower(substr(povcal_subset$filename,1,2))
  recode <- tolower(substr(povcal_subset$filename,3,4))
  phase <- tolower(substr(povcal_subset$filename,5,6))
  subphase <- substr(povcal_subset$filename,5,5)
  rdata_name = paste0(country,recode,phase,"fl")
  variable <- tolower(povcal_subset$variable)
  if(exists("pr")){rm(pr)}
  pr_patha <- paste0(country,"pr",phase)
  pr_path <- paste0(tolower(pr_patha),"fl.RData")
  load(pr_path)
  pr <- as.data.table(data)
  remove(data)
  keep <- c("hvidx","hhid","hv001","hv002","hv005","hv025","hv219","hv220","hv271","hv104","hv105","hv109","hv112","hv140","hc70","v106")
  pr <- subset(pr, select= (colnames(pr) %in% keep))
  gc()
  names(pr)[which(names(pr)=="hv001")] <- "cluster"
  names(pr)[which(names(pr)=="hv002")] <- "household"
  names(pr)[which(names(pr)=="hvidx")] <- "line"
  
  #Rename sample.weights var
  names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
  pr$weights <- pr$sample.weights/1000000
  
  # Sex
  names(pr)[which(names(pr)=="hv104")] <- "sex"

  # ID vars
  names(pr)[which(names(pr)=="hv001")] <- "cluster"
  names(pr)[which(names(pr)=="hv002")] <- "household"
  names(pr)[which(names(pr)=="hvidx")] <- "line"
  names(pr)[which(names(pr)=="hv112")] <- "mother.line"
  pr$mother.line[which(pr$mother.line==99)] <- NA
  
  names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
  #0 - neither certificate or registered
  #1 - has certificate
  #2 - registered, no certificate
  #3 - registered, no certificate
  #6 - other
  #8 - dk
  pr$birth.reg = NA
  pr$birth.reg[which(pr$birth.cert %in% c(0,6,8,9))] = 0
  pr$birth.reg[which(pr$birth.cert %in% c(1,2,3))] = 1
  
  dsn = svydesign(
    data=pr
    ,ids=~1
    ,weights=~weights
  )
  
  reg.tab = svytable(~birth.reg+sex,dsn)
  
  if("1" %in% rownames(reg.tab)){
    reg.m = reg.tab["1","1"]
    reg.f = reg.tab["1","2"]
  }else{
    reg.m = NA
    reg.f = NA
  }
  if("0" %in% rownames(reg.tab)){
    non.reg.m = reg.tab["0","1"]
    non.reg.f = reg.tab["0","2"]
  }else{
    non.reg.m = NA
    non.reg.f = NA
  }
  reg.m.numerator = reg.m
  reg.m.denominator = sum(reg.m,non.reg.m,na.rm=T)
  reg.m.stat = reg.m.numerator/reg.m.denominator
  reg.f.numerator = reg.f
  reg.f.denominator = sum(reg.f,non.reg.f,na.rm=T)
  reg.f.stat = reg.f.numerator/reg.f.denominator
  
  reg.total.stat = sum(reg.m.numerator,reg.f.numerator,na.rm=T) / sum(reg.m.denominator,reg.f.denominator,na.rm=T)

 
  dat = data.frame(
    male.u5.birth.registration = reg.m.stat,
    female.u5.birth.registration = reg.f.stat,
    total.u5.birth.registration = reg.total.stat
  )
  
  
  dat$iso3 = povcal_subset$iso3
  dat$survey_year = povcal_subset$surveyyr
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1

}


close(pb)
data.total <- rbindlist(dataList)
setwd(wd)
# save(data.total,file="data/dhs_crvs.RData")

data.total = merge(data.total,regions,by="iso3")

fwrite(data.total,"data/dhs_crvs.csv")
