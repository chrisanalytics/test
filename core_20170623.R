
getwd()
setwd("/Users/Chris.Valle/Desktop/DRAW_R/")


library(openxlsx)
library(dplyr)
library(dplyr)
library(reshape2)
library(ggplot2)
############################

dfCore = read.csv("CSV_Core_DRAW_Performance_B9363R86033J10524899_20160101_20170628.csv", header = TRUE, stringsAsFactors = TRUE)
str(dfCore)
dfCore$Media.Buy.Key = as.factor(dfCore$Media.Buy.Key)
dfCore$Day2 = as.Date(dfCore$Day, format = "%m/%d/%Y") #use caps Y to avoid distorting year

#Create Call Group
# -1, +1
#Grp1 = Apr 13 - May 11
#Grp2 = Apr 27 - May 25
#Grp3 = May 11 - Jun 8
#Grp4 = May 25 - Jun22
dfCore$CallGrp[which(dfCore$Day2 < "2017-04-13")] = "Ungrpd"
dfCore$CallGrp[which(dfCore$Day2 > "2017-04-12" & dfCore$Day2 < "2017-05-12")] = "Grp1"
dfCore$CallGrp[which(dfCore$Day2 > "2017-04-26" & dfCore$Day2 < "2017-05-26")] = "Grp2"
dfCore$CallGrp[which(dfCore$Day2 > "2017-05-10" & dfCore$Day2 < "2017-06-09")] = "Grp3"
dfCore$CallGrp[which(dfCore$Day2 > "2017-05-24" & dfCore$Day2 < "2017-06-23")] = "Grp4"


# creative_type = RM
# creative_type = STB
# HTML5 Pushdown Banner = RM
# HTML5 Single Expandable = RM
# HTML5 Standard Banner = STB
# Tracking Pixel = TP
# In-Stream Tracking = TP

unique(dfCore$Creative.Format, na.rm=TRUE)
str(dfCore$Creative.Format)

dfCore$CreativeType[which(dfCore$Creative.Format == "HTML5 Pushdown")] = "RM"
dfCore$CreativeType[which(dfCore$Creative.Format == "HTML5 Single Expandable")] = "RM"
dfCore$CreativeType[which(dfCore$Creative.Format == "HTML5 Standard Banner")] = "STB"
dfCore$CreativeType[which(dfCore$Creative.Format == "In-Stream Video")] = "Video"

dfCore$CreativeType[which(dfCore$Creative.Format == "Tracking Pixel")] ="TP"
dfCore$CreativeType[which(dfCore$Creative.Format == "In-Stream Tracking")] ="TP"

unique(dfCore$CreativeType)

Campaign = dfCore %>%
  tbl_df() %>%
  select(CallGrp,Impressions,Attributable.Conversions,Clicks)%>%
  group_by(CallGrp)%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)

write.csv(Campaign, file = "Core_Campaign.csv")


# VCR =sum(Video.Fully.Played, na.rm=TRUE)/sum(Video.Started, na.rm=TRUE) - getting object not found error

# by Site Partners
Site = dfCore%>%
  tbl_df() %>%
  select(CallGrp, Clean.Site.Name, Impressions, Attributable.Conversions,Clicks)%>%
  group_by(CallGrp,Clean.Site.Name)%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)

dcastSiteImp = dcast(Site,Clean.Site.Name ~CallGrp, value.var = 'total_imp', fun.aggregate = sum)
dcastSiteLPR = dcast(Site,Clean.Site.Name ~CallGrp, value.var = 'LPR', fun.aggregate = sum)

write.csv(dcastSiteImp, file = "Core_SiteImp.csv")
write.csv(dcastSiteLPR, file = "Core_SiteLPR.csv")


# by Site by Tactic 
Tactics = dfCore%>%
  tbl_df()%>%
  select(CallGrp,Clean.Site.Name,Tactic, Impressions,Attributable.Conversions,Clicks)%>%
  group_by(CallGrp,Clean.Site.Name,Tactic)%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)
str(Tactics)


dcastTactics = dcast(Tactics,Clean.Site.Name,  ~CallGrp, value.var = 'total_imp', fun.aggregate = sum)

dcastTactics2 = dcast(Tactics,Tactic ~CallGrp, value.var = 'total_imp', fun.aggregate = sum)
dcastTactics2LPR = dcast(Tactics,Tactic ~CallGrp, value.var = 'LPR', fun.aggregate = sum)


# by Site by tactic by creative message

CGrp = dfCore%>%
  tbl_df()%>%
  select(CallGrp,Clean.Site.Name,Tactic,Attributable.Conversions,Impressions,Clicks, Creative.Group)%>%
  group_by(CallGrp,Clean.Site.Name,Creative.Group)%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)

dcastCGrpLPR = dcast(CGrp,Creative.Group ~CallGrp, value.var = 'LPR', fun.aggregate = sum)


# by Site by device
Device = dfCore%>%
  tbl_df()%>%
  select(CallGrp,Clean.Site.Name,Tactic,Device,Impressions,Attributable.Conversions,Clicks)%>%
  group_by(CallGrp,Clean.Site.Name,Device)%>%
  filter(CallGrp =="Grp4")%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)

# Statistics: Mean Impressions per day
summary(dfCore$Impressions)
summary(dfCore$Attributable.Conversions)


