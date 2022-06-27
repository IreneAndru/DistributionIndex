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
#rm(AwesomePwd, AwesomeUser)

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
georges.df$SEASON<-with(georges.df,ifelse(MONTH<6, 'SPRING','FALL'))
georges.agg.df <- aggregate(SETNO~STRAT+AREA+YEAR+SEASON, data=georges.df, length)
t.df<-georges.agg.df[georges.agg.df$SETNO>=2,]
surveyed.df <- aggregate(AREA~YEAR+SEASON, data=t.df, sum)
## area surveyed, each stratum is counted in if at least 2 successful tows were done in a year

table(t.df$STRAT, t.df$YEAR)


ggplot(t.df, aes(as.numeric(YEAR), STRAT))+geom_tile(aes(fill=SETNO))+facet_wrap(~SEASON, ncol=1)+theme_bw()+xlab("YEAR")

ggplot(surveyed.df, aes(as.numeric(YEAR), AREA))+geom_point()+theme_bw()+xlab("YEAR")+facet_wrap(~SEASON, ncol=1)+ylim(0,5500)

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
  #For non EGB Species
  if(EGB_assessment_strata=='Y_EGB'){

    georges.tows.df<-subset(tows.df, STRAT%in%c('01160','01170','01180','01190','01200','01210','01220')&MANAREA%in%c(551,552,561,562))

  }

  if(EGB_assessment_strata=='Y_GB'){

    georges.tows.df<-subset(strat.stats.df, STRAT%in%c('01130','01140','01150','01160','01170','01180','01190','01200','01210','01220','01230','01240','01250')&MANAREA%in%c(551,552,561,562,522,525))
      }


  ## table(georges.tows.df$YEAR)

  qu <- paste("
SELECT
c.cruise6 as mission,
c.svvessel as vessel,
c.tow as setno,
c.stratum as strat,
c.begin_est_towdate as sdate,
c.est_year as YEAR,
c.est_month as MONTH,
c.est_day as DAY,
d.station as station,
d.svspp as spec,
d.expcatchnum as totno,
d.expcatchwt as totwgt,
c.mindepth as dmin,
c.maxdepth as dmax,
c.bottemp as bottom_temperature,
c.botsalin as bottom_salinity,
c.dopdistb as dist,
c.svgear as gear,
d.expcatchnum as totnocorr,
d.expcatchwt as totwgtcorr
FROM
usnefsc.uss_station c,
usnefsc.uss_catch d
where
SHG<136 and
c.cruise6 = d.cruise6 AND
c.tow = d.tow AND
d.station=c.station AND
c.stratum=d.stratum AND
c.status_code=d.status_code AND
c.id=d.id and
d.svspp='",paste(0,spec.num,sep=""),"'
order by YEAR, c.cruise6, c.tow", sep="")

  #c.expcatchnum * (0.991/i.dopdistb) as totnocorr,
  #c.expcatchwt * (0.991/i.dopdistb) as totwgtcorr
  #spec.df <- sqlQuery(chan, qu)
  spec.df<-ROracle::dbGetQuery(chan, qu)
  spec.df$MONTH<-as.numeric(spec.df$MONTH)
  merged.df <- merge(spec.df, georges.tows.df, all.y=TRUE) #Removing the merge id column names precludes the need to then remove the duplicated variables.

  #Quick and dirty standardization
  ggplot(merged.df, aes(DIST))+geom_histogram()+facet_wrap(~VESSEL)
  vessels2<-aggregate(DIST~VESSEL, FUN="mean", merged.df)
  names(vessels2)[2]<-'STD_DIST'
  merged.df<-merge(merged.df, vessels2, all.x=TRUE)#Note there are some records in the early time series where DIST is NA. THose records will be put in uncorrected.
  merged.df$TOTNOCORR<-with(merged.df, TOTNO*DIST/STD_DIST)
  merged.df$TOTNOCORR<-with(merged.df, ifelse(is.na(TOTNOCORR), TOTNO, TOTNOCORR))
  merged.df$TOTWGTCORR<-with(merged.df, TOTWGT*DIST/STD_DIST)
  merged.df$TOTWGTCORR<-with(merged.df, ifelse(is.na(TOTWGTCORR), TOTNO, TOTWGTCORR))

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
cod.df <- extract.catch.fct(73)
cod.df.spring<-subset(cod.df,as.numeric(month)<6)
write.csv(cod.df.spring, "cod.nmfs.spring.csv")
cod.df.fall<-subset(cod.df,as.numeric(month)>6)
write.csv(cod.df.fall, "cod.nmfs.fall.csv")
haddock.df <- extract.catch.fct(74)
haddock.df.spring<-subset(haddock.df,as.numeric(month)<6)
write.csv(haddock.df.spring, "haddock.nmfs.spring.csv")
haddock.df.fall<-subset(haddock.df,as.numeric(month)>6)
write.csv(haddock.df.fall, "haddock.nmfs.fall.csv")

ylt.df <- extract.catch.fct(105)#Won't work right now. Need to for the query (paste in 0, species name)

##############





