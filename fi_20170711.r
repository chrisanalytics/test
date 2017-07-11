setwd("/Users/chris/Desktop/DRAW_R/")

library(xlsx)
library(dplyr)
library(reshape2)
library(lubridate)



df = read.csv("CSV_FI_DRAW_Performance_20170709.csv", header = TRUE, skip = 6, stringsAsFactors = FALSE)

str(df$Day)

df$Day2 = ymd(df$Day)


summary(df$Impressions)
summary(df$Total.Conversions)
summary(df$Attributable.Conversions)

# create Week column
# week0 = > 5/20
# week1 = 5/3 - 5/7
# week2 = 5/8 - 5/14
# week3 = 5/15 - 5/21
# week4 = 5/22 - 5/28
# week5 = 5/29 - 6/4
# week6 = 6/5 - 6/11
# week7 = 6/12 - 6/18
# week8 = 6/19 - 6/25
# week9 = 6/26 - 7/02
# week10 = 7/03 - 7/09



#created Week groups
df$WeekGrp[which(df$Day2 < "2017-05-03")] = "week0"
df$WeekGrp[which(df$Day2 > "2017-05-02" & df$Day2 < "2017-05-08")] = "week1"
df$WeekGrp[which(df$Day2 > "2017-05-7" & df$Day2 < "2017-05-15")] = "week2"
df$WeekGrp[which(df$Day2 > "2017-05-14" & df$Day2 < "2017-05-22")] = "week3"
df$WeekGrp[which(df$Day2 > "2017-05-21" & df$Day2 < "2017-05-29")] = "week4"
df$WeekGrp[which(df$Day2 > "2017-05-28" & df$Day2 < "2017-06-5")] = "week5"
df$WeekGrp[which(df$Day2 > "2017-06-4" & df$Day2 < "2017-06-12")] = "week6"
df$WeekGrp[which(df$Day2 > "2017-06-11" & df$Day2 < "2017-06-19")] = "week7"
df$WeekGrp[which(df$Day2 > "2017-06-18" & df$Day2 < "2017-06-26")] = "week8"
df$WeekGrp[which(df$Day2 > "2017-06-25" & df$Day2 < "2017-07-03")] = "week9"
df$WeekGrp[which(df$Day2 > "2017-07-02" & df$Day2 < "2017-07-10")] = "week10"

unique(df$WeekGrp)

NAcheck = df%>%
  tbl_df()%>%
  select(Day2, WeekGrp)%>%
  group_by(WeekGrp)%>%
  filter(is.na(WeekGrp))

# creative_type = RM
# creative_type = STB
# HTML5 Pushdown Banner = RM
# HTML5 Single Expandable = RM
# HTML5 Standard Banner = STB
# Tracking Pixel = TP


unique(df$Creative.Format, na.rm=TRUE)
unique(df$Creative.Format)

df$CreativeType[which(df$Creative.Format == "HTML5 Pushdown Banner")] = "RM"
df$CreativeType[which(df$Creative.Format == "HTML5 Single Expandable")] = "RM"
df$CreativeType[which(df$Creative.Format == "HTML5 Standard Banner")] = "STB"
df$CreativeType[which(df$Creative.Format == "Tracking Pixel")] = "TP"

unique(df$CreativeType)

NAcheck2 = df%>%
  tbl_df()%>%
  select(Creative.Format, CreativeType,Impressions, Clean.Site.Name, WeekGrp, Unit.Size)%>%
  group_by(CreativeType)%>%
  filter(is.na(CreativeType), Impressions>0)
  

NAcheck2B = df%>%
  tbl_df()%>%
  select(Creative.Format, CreativeType,Impressions, Clean.Site.Name, WeekGrp, Unit.Size)%>%
  group_by(CreativeType)%>%
  filter(is.na(CreativeType))%>%
  summarise(total_imp=sum(Impressions, na.rm=TRUE))
#advisor no creative format, unit size = others but with impressions, for week1 and week2

dfDashboard = df%>%
  tbl_df()%>%
  select(WeekGrp,Impressions,Attributable.Conversions,Clicks)%>%
  group_by(WeekGrp)%>%
  summarise(total_conv = sum(Attributable.Conversions, na.rm=TRUE), total_imp=sum(Impressions, na.rm=TRUE), 
            lpr = (total_conv/total_imp), total_clicks= sum(Clicks, na.rm=TRUE),
            ctr = sum(total_clicks/total_imp))

#filter(WeekGrp=="week10")%>%
  
dfCreative = df%>%
  tbl_df()%>%
  select(WeekGrp,Impressions,Attributable.Conversions,Clicks, CreativeType)%>%
  group_by(WeekGrp, CreativeType)%>%
  filter(WeekGrp=="week10")%>%
  summarise(total_conv = sum(Attributable.Conversions, na.rm=TRUE), total_imp=sum(Impressions, na.rm=TRUE), 
            lpr = (total_conv/total_imp), total_clicks= sum(Clicks, na.rm=TRUE),
            ctr = sum(total_clicks/total_imp))  

write.csv(dfCreative,file = 'week10Creative.csv')
write.csv(dfDashboard, file = "week10Dashboard.csv")
