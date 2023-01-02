pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot, np, estimatr, skedastic, car)

setwd(dirname(getActiveDocumentContext()$path)); getwd()

data_repo <- paste0(getwd(),"/data")

morgdata <- fread(paste0(data_repo,"/morg-2014-emp.csv"))

summary(morgdata)

#Create a sample indicator (not subsetting!) for  1 = Market research analysts and marketing specialists; 2 =  Computer and Mathematical Occupations
morgdata[ , sample := ifelse(occ2012==0735,1, ifelse(morgdata$occ2012>=1005 & morgdata$occ2012<=1240,2,0))]

dt <- morgdata

dt[ ,female:=as.numeric(sex==2)][ 
  ,w:=earnwke/uhours ][ 
    ,lnw:=log(w)][ 
      ,agesq:=age^2]

#Further variable definitions (dummy)
dt[,white:= as.numeric(race==1)][
  , afram := as.numeric(race==2)][ 
    , asam := as.numeric(4)][
      , hisp := !is.na(ethnic)][
        , other.nonw:=as.numeric(white==0&afram==0&asam ==0&hisp==0)]

dt[ , married := as.numeric(marital ==1 | marital ==2)][
  , divorced := as.numeric(marital == 3 | marital == 5 | marital == 6)][
    , widowed := as.numeric(marital == 4)][
      , nevermar := as.numeric(marital == 7)]

dt[ ,ed_MA := as.numeric(grade92==44) ][ 
  ,ed_Profess := as.numeric(grade92 == 45)][ 
    ,ed_PhD := as.numeric(grade92 ==46)]

# dt1 Market research analysts and marketing specialists 
# dt2 Computer and Mathematical Occupations
dt1 <- dt[sample == 1, ]
dt2 <- dt[sample == 2, ]
