###Reworking of Dan Ricard's code (https://github.com/dfo-gulf-science/Maritimes-SUMMER-Atlas) to apply to various pollock spatial units.

#Options:
#Which spatial area do you want this dataset to apply to? Options: WC, 4X5Y, 4X5, 5Z, 4Xmn, EC, 4XVW5, 4VW

Assessment_strata<-'WC'

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
type=1
order by YEAR, mission, setno
", sep="")

#If you want a year restriction, add the following into the query above, before 'order' command
#AND extract(year from sdate) >= 1983

#tows.df <- sqlQuery(chan, qu)
tows.df<-ROracle::dbGetQuery(chan, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df<-subset(tows.df, MONTH%in%c(6,7,8))
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

## strata statistics
qu <- paste("
select
*
from
groundfish.gsstratum
", sep="")

#strata.stats.df <- sqlQuery(chan, qu)
strata.stats.df<-ROracle::dbGetQuery(chan, qu)

all.df <- merge(tows.df, strata.stats.df, by="STRAT")
agg.all.df <- aggregate(SETNO~STRAT+AREA+YEAR, data=all.df, length)
t.df<-agg.all.df[agg.all.df$SETNO>=2,]
surveyed.df <- aggregate(AREA~YEAR, data=t.df, sum)
## area surveyed, each stratum is counted in if at least 2 successful tows were done in a year

table(t.df$STRAT, t.df$YEAR)

plot(AREA~YEAR, data=surveyed.df, type='b', ylim=c(0, max(surveyed.df$AREA)))
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
type=1
order by YEAR, mission, setno
", sep="")

#tows.df <- sqlQuery(chan, qu)
tows.df<-ROracle::dbGetQuery(chan, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df<-subset(tows.df, MONTH%in%c(6,7,8))
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)


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
i.mission = c.mission AND
i.setno = c.setno AND
s.CODE=c.spec and
s.CODE='",spec.num,"'
order by YEAR, i.mission, i.setno
", sep="")

#spec.df <- sqlQuery(chan, qu)
spec.df<-ROracle::dbGetQuery(chan, qu)
spec.df$MONTH<-as.numeric(spec.df$MONTH)
merged.df <- merge(spec.df, tows.df, all.y=TRUE) #Removing the merge id column names precludes the need to then remove the duplicated variables.
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
pollock.df<-extract.catch.fct(16)
##############

#Which spatial area do you want this dataset to apply to? Options: WC, 4X5Y, 4X5, 5Z, EC, 4XVW5, 4VW
if(Assessment_strata=='4XVW5'){pollock.df<-pollock.df} #includes everything
if(Assessment_strata=='WC'){pollock.df<-subset(pollock.df, strat %in% c('474','476','480','481','482','483','484','485','490','491','492','493','494','495','5Z1','5Z2','5Z9'))} #excludes 4Xmn, includes EGB
if(Assessment_strata=='4X5'){pollock.df<-subset(pollock.df, strat %in% c('470','471','472','473','474','475','476','480','481','482','483','484','485','490','491','492','493','494','495','5Z1','5Z2','5Z9'))} #incudes 4Xmn and EGB
if(Assessment_strata=='4X5Y'){pollock.df<-subset(pollock.df, strat %in% c('470','471','472','473','474','475','476','480','481','482','483','484','485','490','491','492','493','494','495'))} #incudes 4Xmn but not EGB
if(Assessment_strata=='5Z'){pollock.df<-subset(pollock.df, strat %in% c('5Z1','5Z2','5Z9'))} #Only EGB
if(Assessment_strata=='4Xmn'){pollock.df<-subset(pollock.df, strat %in% c('470','471','472','473','475','477','478'))}#only 4Xmn
if(Assessment_strata=='EC'){pollock.df<-subset(pollock.df, strat %in% c('558','559','440','441','442','443','444','445','446','447','448','449','450','451','452','453','454','455','456','457','458','459','460','461','462','463','464','465','466','470','471','472','473','475','477','478'))} #incudes 4Xmn
if(Assessment_strata=='4VW'){pollock.df<-subset(pollock.df, strat %in% c('558','559','440','441','442','443','444','445','446','447','448','449','450','451','452','453','454','455','456','457','458','459','460','461','462','463','464','465','466'))} #incudes 4Xmn





