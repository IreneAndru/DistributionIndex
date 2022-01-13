##
library(RODBC, ROracle)
##

#chan <- odbcConnect("PTRAN",AwesomeUser,AwesomePwd).
chan<-ROracle::dbConnect(DBI::dbDriver("Oracle"), username=AwesomeUser, password=AwesomePwd, "PTRAN")
rm(AwesomePwd, AwesomeUser)

##Setting up function
#################
qu <- paste("
SELECT
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear,
area as ManArea,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type=1 AND
gear in (3,9) AND
extract(year from sdate) >= 1987
order by YEAR, mission, setno
", sep="") #Added area column. Needed for selection of EGB unit. To avoid confusion with the spatial coverage (nm) area merged later, named the Management Area columnas ManArea.

#tows.df <- sqlQuery(chan, qu)
tows.df<-ROracle::dbGetQuery(chan, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

## GEORGES survey, strata
georges.tows.df <- subset(
tows.df,
((STRAT == '5Z1' | STRAT == '5Z2' | STRAT == '5Z3' | STRAT == '5Z4' | STRAT == '5Z5' | STRAT == '5Z6' | STRAT == '5Z7' | STRAT == '5Z8' | STRAT == '5Z9') & (MONTH == 2 | MONTH == 3 | MONTH == 4) & MANAREA%in%c(523,524)
)
) # Added AREA selection. Several of the strata (i.e. 5Z3, 5Z4, 5Z8) extend past the EGB Management unit, so the spatial selection of EGB cod requires limiting data to the intersect of Strata XX, and Areas 523 and 524. I also added month 4, as the survey has gone into April in recent years. Finally, added some brackets because it was applying the | with respect to the month selection.

## strata statistics
qu <- paste("
select
*
from
groundfish.gsstratum
", sep="")

#strata.stats.df <- sqlQuery(chan, qu)
strata.stats.df<-ROracle::dbGetQuery(chan, qu)

georges.strata.stats.df <- subset(
strata.stats.df,
(STRAT == '5Z1' | STRAT == '5Z2' | STRAT == '5Z3' | STRAT == '5Z4' | STRAT == '5Z5' | STRAT == '5Z6' | STRAT == '5Z7' | STRAT == '5Z8' | STRAT == '5Z9')
)

## The stratum areas in the table below are for the whole stratum. Since five of them are bisected by the management area, I pulled the re-calculated areas for just the portion of the stratum falling into the Management Area:
#strataegb.stats.df <- sqlQuery(chan, paste("select * from usnefsc.dfo5zjm"))
strataegb.stats.df <-ROracle::dbGetQuery(chan, "select * from usnefsc.dfo5zjm")

#For those who do not have access to usnefsc:
#write.csv(strataegb.stats.df, "strataegb_stats_df.csv")
#strataegb.stats.df<-read.csv("data/strataegb_stats_df.csv")
names(strataegb.stats.df)[which(colnames(strataegb.stats.df)=="AREA" )]<-'AREA_CROP'

#Replacing the full stratum area value (nm), with the cropped stratum area value (nm) associated with the EGB management area.
temp<-merge(georges.strata.stats.df, strataegb.stats.df, all.x=TRUE)
temp$AREA<-with(temp, ifelse(!is.na(AREA_CROP), AREA_CROP, AREA))
georges.strata.stats.df<-temp[,-(which(colnames(temp)=="AREA_CROP"))]

georges.strata.tt <- georges.strata.stats.df[order(georges.strata.stats.df$STRAT),]
georges.strat <- georges.strata.tt

#odbcClose(chan)

georges.df <- merge(georges.tows.df, georges.strat, by="STRAT")
georges.agg.df <- aggregate(SETNO~STRAT+AREA+YEAR, data=georges.df, length)
t.df<-georges.agg.df[georges.agg.df$SETNO>=2,]
surveyed.df <- aggregate(AREA~YEAR, data=t.df[t.df$STRAT %in% c("5Z1","5Z2","5Z3","5Z4"),], sum)
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
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear,
area as MANAREA,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type=1 AND
gear in (3,9)
order by YEAR, mission, setno
", sep="")

#tows.df <- sqlQuery(chan, qu)
tows.df<-ROracle::dbGetQuery(chan, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

## GEORGES survey, strata
georges.tows.df <- subset(
  tows.df,
  ((STRAT == '5Z1' | STRAT == '5Z2' | STRAT == '5Z3' | STRAT == '5Z4' | STRAT == '5Z5' | STRAT == '5Z6' | STRAT == '5Z7' | STRAT == '5Z8' | STRAT == '5Z9') & (MONTH == 2 | MONTH == 3 | MONTH == 4) & MANAREA%in%c(523,524)
  )
)

## table(georges.tows.df$YEAR)

qu <- paste("
SELECT
i.mission,
i.setno,
i.strat,
i.sdate,
TO_CHAR(i.sdate,'yyyy') YEAR,
TO_CHAR(i.sdate,'mm') MONTH,
TO_CHAR(i.sdate,'dd') DAY,
c.spec,
s.CODE SCIEN,
s.comm,
c.totno,
c.totwgt,
i.dmin,
i.dmax,
i.bottom_temperature,
i.bottom_salinity,
i.dist,
i.gear,
c.totno * (1.75/i.dist) as totnocorr,
c.totwgt * (1.75/i.dist) as totwgtcorr
FROM
groundfish.gsinf i,
groundfish.gscat c,
groundfish.GSSPECIES s
where
i.type=1 and
i.gear in (3,9) AND
i.mission = c.mission AND
i.setno = c.setno AND
s.CODE=c.spec and
s.CODE='",spec.num,"'
order by YEAR, i.mission, i.setno
", sep="")

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
ytf.df <- extract.catch.fct(42)
##############

## keep only the tows from the Georges survey
keep.tows.df <- georges.df[georges.df$STRAT %in% c("5Z1","5Z2","5Z3","5Z4"),]
keep.tows.df$unique.id <- paste0(keep.tows.df$MISSION, "-", keep.tows.df$SETNO)
cod.keep.df <- merge(keep.tows.df, cod.df, by.x=c("MISSION","SETNO"), by.y=c("mission","setno"), all.x=TRUE)


