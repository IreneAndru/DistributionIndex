###Reworking of Dan Ricard's code (https://github.com/dfo-gulf-science/Maritimes-SUMMER-Atlas) to specifically apply to Georges Bank Species.
#####NOTE: This pulls everything in MANAGREA 523 and 524. If your species is different (yellowtail?), it will need modification.

#Options: test
#Do you want this limited to just EGB (strata 5Z1-5Z4, management areas 523-524 for cod/haddock, and Strata 5Z1-5Z4 for yt)? Assign Y or N appropriately to your decision:
EGB_assessment_strata<-'Y_EGB'
#EGB_assessment_strata<-'Y_GB'
#EGB_assessment_strata<-'N'

#Data Source
data_source<-'NMFS'



##
library(RODBC, ROracle)
##
#chan <- odbcConnect("PTRAN",AwesomeUser,AwesomePwd).
chan<-ROracle::dbConnect(DBI::dbDriver("Oracle"), username=AwesomeUser, password=AwesomePwd, "PTRAN")
rm(AwesomePwd, AwesomeUser)

##Setting up function for DFO Pull
#################
if(data_source=='NMFS'){
  qu <- paste("
  SELECT
cruise6 as mission,
tow as setno,
stratum as strat,
begin_est_towdate as sdate,
est_year as YEAR,
est_month as MONTH,
est_day as DAY,
mindepth as dmin,
maxdepth as dmax,
bottemp as bottom_temperature,
botsalin as bottom_salinity,
dopdistb as dist,
svgear as gear,
AREA as ManArea,
DECDEG_BEGLON SLO,
DECDEG_BEGLAT SLA
FROM usnefsc.uss_station
where
SHG<=136 AND
est_year >= 1978
order by YEAR, mission, setno
", sep="")
  tows.df<-ROracle::dbGetQuery(chan, qu)
}


tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

## GEORGES survey, strata
if (data_source=='NMFS'){
  if (EGB_assessment_strata=="Y_EGB"){
    georges.tows.df <- subset(tows.df, STRAT%in%c('01160','01170','01180','01190','01200','01210','01220')&MANAREA%in%c(551,552,561,562))
  }
  if (EGB_assessment_strata=="Y_GB"){
    georges.tows.df <- subset(tows.df, STRAT%in%c('01130','01230','01160','01170','01180','01190','01200','01210','01220')&MANAREA%in%c(551,552,561,562,522,525))
  }


  georges.tows.df <- subset(tows.df, STRAT%in%c('01160','01170','01180','01190','01120','01210','01220'))#Keep adding strata
  # Added AREA selection. Several of the strata (i.e. 5Z3, 5Z4, 5Z8) extend past the EGB Management unit, so the spatial selection of EGB cod requires limiting data to the intersect of Strata XX, and Areas 523 and 524 for cod and haddock, but not for yellowtail. Species-specific areaxmanarea intersects are done later. I also added month 4, as the survey has gone into April in recent years. Finally, added some brackets because it was applying the | with respect to the month selection.
}



#For non EGB Species
if(EGB_assessment_strata=='Y_EGB'){

  qu <- paste("select * from usnefsc.nmfs5zjm", sep="")

  strat.stats.df <-ROracle::dbGetQuery(chan, qu)

  strat.stats.df<-subset(strat.stats.df, STRAT%in%c('01160','01170','01180','01190','01200','01210','01220'))

}

if(EGB_assessment_strata=='Y_GB'){
  qu <- paste("select * from usnefsc.nmfs5zjm", sep="")

  strat.stats.df <-ROracle::dbGetQuery(chan, qu)
  strat.stats.df<-subset(strat.stats.df, STRAT%in%c('01160','01170','01180','01190','01200','01210','01220'))

  qu <- paste("select * from usnefsc.nmfs5zghno", sep="")

  strat.stats.df2 <-ROracle::dbGetQuery(chan, qu)
  strat.stats.df2<-subset(strat.stats.df2, STRAT%in%c('01130','01140','01150','01160','01170','01180','01190','01200','01210','01220','01230','01240','01250'))
  strat.stats.df<-rbind(strat.stats.df, strat.stats.df2)

  strat.stats.df<-subset(strat.stats.df, STRAT%in%c('5Z1','5Z2','5Z3','5Z4'))

}

georges.strata.stats.df <-strat.stats.df

## The stratum areas in the table below are for the whole stratum. Since five of them are bisected by the management area, I pulled the re-calculated areas for just the portion of the stratum falling into the Management Area:
#strataegb.stats.df <- sqlQuery(chan, paste("select * from usnefsc.dfo5zjm"))
#strataegb.stats.df <-ROracle::dbGetQuery(chan, "select * from usnefsc.dfo5zjm")

#For those who do not have access to usnefsc:
#write.csv(strataegb.stats.df, "strataegb_stats_df.csv")
#strataegb.stats.df<-read.csv("data/strataegb_stats_df.csv")
#names(strataegb.stats.df)[which(colnames(strataegb.stats.df)=="AREA" )]<-'AREA_CROP'

#Replacing the full stratum area value (nm), with the cropped stratum area value (nm) associated with the EGB management area.
#temp<-merge(georges.strata.stats.df, strataegb.stats.df, all.x=TRUE)
#temp$AREA<-with(temp, ifelse(!is.na(AREA_CROP), AREA_CROP, AREA))
#georges.strata.stats.df<-temp[,-(which(colnames(temp)=="AREA_CROP"))]

georges.strata.tt <- georges.strata.stats.df[order(georges.strata.stats.df$STRAT),]
georges.strat <- georges.strata.tt

#odbcClose(chan)

georges.df <- merge(georges.tows.df, georges.strat, by="STRAT")
georges.agg.df <- aggregate(SETNO~STRAT+AREA+YEAR, data=georges.df, length)
t.df<-georges.agg.df[georges.agg.df$SETNO>=2,]
surveyed.df <- aggregate(AREA~YEAR, data=t.df, sum)
## area surveyed, each stratum is counted in if at least 2 successful tows were done in a year

table(t.df$STRAT, t.df$YEAR)

plot(AREA~YEAR, data=surveyed.df, type='b')
#############
##


#Writing General function for a given species:
#################
extract.catch.fct <- function(spec.num) {
  # survey data
  ## bring back a data frame with tows and their associated environmental covariates
  qu <- paste("
  SELECT
cruise6 as mission,
tow as setno,
stratum as strat,
begin_est_towdate as sdate,
est_year as YEAR,
est_month as MONTH,
est_day as DAY,
mindepth as dmin,
maxdepth as dmax,
bottemp as bottom_temperature,
botsalin as bottom_salinity,
dopdistb as dist,
svgear as gear,
AREA as ManArea,
DECDEG_BEGLON SLO,
DECDEG_BEGLAT SLA
FROM usnefsc.uss_station
where
SHG<=136 AND
est_year >= 1978
order by YEAR, mission, setno
", sep="")

  #tows.df <- sqlQuery(chan, qu)
  tows.df<-ROracle::dbGetQuery(chan, qu)
  tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
  tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
  tows.df$MONTH<-as.numeric(tows.df$MONTH)
  tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

  ## GEORGES survey, strata
  georges.tows.df <- tows.df


  ## table(georges.tows.df$YEAR)






  qu <- paste("
  SELECT
cruise6 as mission,
tow as setno,
stratum as strat,
begin_est_towdate as sdate,
est_year as YEAR,
est_month as MONTH,
est_day as DAY,
mindepth as dmin,
maxdepth as dmax,
bottemp as bottom_temperature,
botsalin as bottom_salinity,
dopdistb as dist,
svgear as gear,
AREA as ManArea,
DECDEG_BEGLON SLO,
DECDEG_BEGLAT SLA
FROM usnefsc.uss_station
where
SHG<=136 AND
est_year >= 1978
order by YEAR, mission, setno
", sep="")



  qu <- paste("
SELECT
i.cruise6 as mission,
i.tow as setno,
i.stratum as strat,
i.begin_est_towdate as sdate,
i.est_year as YEAR,
i.est_month as MONTH,
i.est_day as DAY,
c.svspp as spec,
s.comname as comm,
c.expcatchnum as totno,
c.expcatchwt as totwgt,
i.mindepth as dmin,
i.maxdepth as dmax,
i.bottemp as bottom_temperature,
i.botsalin as bottom_salinity,
i.dopdistb as dist,
i.svgear as gear,
c.expcatchnum * (1.75/i.dopdistb) as totnocorr,
c.expcatchwt * (1.75/i.dopdistb) as totwgtcorr
FROM
usnefsc.uss_station i,
usnefsc.uss_catch c,
usnefsc.uss_species_codes s
where
SHG<136 and
i.cruise6 = c.cruise6 AND
i.tow = c.tow AND
s.svspp=c.svspp and
s.svspp='",spec.num,"'
order by YEAR, i.cruise6, i.tow", sep="")

  #spec.df <- sqlQuery(chan, qu)
  spec.df<-ROracle::dbGetQuery(chan, qu)
  spec.df$MONTH<-as.numeric(spec.df$MONTH)
  merged.df <- merge(spec.df, georges.tows.df, all.y=TRUE) #Removing the merge id column names precludes the need to then remove the duplicated variables.
  merged.df[is.na(merged.df$SPEC),]$SPEC <- spec.num

  merged.df[is.na(merged.df$COMM),]$COMM <- unique(merged.df[!is.na(merged.df$COMM),]$COMM)
  merged.df[is.na(merged.df$SCIEN),]$SCIEN <- unique(merged.df[!is.na(merged.df$SCIEN),]$SCIEN)

  merged.df[is.na(merged.df$TOTNO),]$TOTNO <- 0
  merged.df[is.na(merged.df$TOTWGT),]$TOTWGT <- 0
  merged.df[is.na(merged.df$TOTNOCORR),]$TOTNOCORR <- 0
  merged.df[is.na(merged.df$TOTWGTCORR),]$TOTWGTCORR <- 0

  #names(merged.df) <- c("mission","setno","spec","scien","comm","totno","totwgt","totno.corr","totwgt.corr","Strata","YEAR","month","day","dmin","dmax","temperature","salinity","lon","lat","DEPTH","decade","dist")
  names(merged.df)<-tolower(names(merged.df))

  tt <- droplevels(merged.df)
  merged.df <- tt
  merged.df$unique.id <- paste0(merged.df$mission, "-", merged.df$setno)

  #Sorting out sets with two size classes:
  tot_wgt_sets<-aggregate(totwgtcorr~unique.id, merged.df, FUN = "sum")
  tot_no_sets<-aggregate(totnocorr~unique.id, merged.df, FUN = "sum")
  merged.df<-unique(merged.df[,-((which(colnames(merged.df)=="totno")):(which(colnames(merged.df)=="totwgtcorr")))])
  merged.df<-merge(merged.df, tot_wgt_sets, all.x=TRUE)
  merged.df<-merge(merged.df, tot_no_sets, all.x=TRUE)

  return(merged.df)

} ## end function definition
############

#Application:
############
cod.df <- extract.catch.fct(10)
haddock.df <- extract.catch.fct(11)
ylt.df <- extract.catch.fct(42)
##############

if(EGB_assessment_strata=='Y_EGB'){
  cod.df<-subset(cod.df, strat %in% c("5Z1","5Z2","5Z3","5Z4")&manarea%in%c(523, 524))
  haddock.df<-subset(haddock.df, strat %in% c("5Z1","5Z2","5Z3","5Z4")&manarea%in%c(523,524))
}

if(EGB_assessment_strata=='Y_GB'){
  ylt.df<-subset(ylt.df, strat %in% c("5Z1","5Z2","5Z3","5Z4"))
}




