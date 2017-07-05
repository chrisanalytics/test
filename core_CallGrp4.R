getwd()
setwd("/Users/chris/Desktop/DRAW_R/")


library(openxlsx)
library(dplyr)
library(reshape2)
library(ggplot2)

coreGrp4 = read.csv("Core_CallGrp4_20170630.csv", header = TRUE, skip =6,  stringsAsFactors = FALSE)
str(coreGrp4)
#coreGrp4$Day = as.Date(coreGrp4$Day, "%m/%dd/%yyyy")
coreGrp4[,"Day"]= as.POSIXct(coreGrp4[,"Day"]) #convert Day as date object
coreGrp4$Day2 = as.Date(coreGrp4$Day, format = "%m/%d/%Y") #use caps Y to avoid distorting year
colnames(coreGrp4)
colsfactor = c(1:19,21:23)


#call groups
# Grp1 = Apr 13 - May 11
# Grp2 = Apr 27 - May 25
# Grp3 = May 11 - Jun 8
# Grp4 = May 25 - Jun 22

coreGrp4$Grp[which(coreGrp4$Day2 < "2017-04-13")] = "Ungrouped"
coreGrp4$Grp[which(coreGrp4$Day2 > "2017-04-12" & coreGrp4 < "2017-05-12")] = "Grp1"
coreGrp4$Grp[which(coreGrp4$Day2 > "2017-04-26" & coreGrp4 < "2017-05-26")] = "Grp2"
coreGrp4$Grp[which(coreGrp4$Day2 > "2017-05-10" & coreGrp4 < "2017-05-09")] = "Grp3"
coreGrp4$Grp[which(coreGrp4$Day2 > "2017-05-24" & coreGrp4 < "2017-05-23")] = "Grp4"

#regular things I see on the dashboard

##### 1. Top Level
#impressions
Campaign = coreGrp4 %>%
  tbl_df() %>%
  select(Grp,Impressions,Attributable.Conversions,Clicks)%>%
  group_by(Grp)%>%
  summarise(total_imp = sum(Impressions, na.rm=TRUE),total_clicks = sum(Clicks, na.rm=TRUE), 
            total_conv = sum(Attributable.Conversions, na.rm=TRUE), LPR = total_conv/total_imp,
            CTR = total_clicks/total_imp)


#clicks
#conversions (bonus. not on dashboard)
#CTR
#LPR
#VCR
#Planned Spend
#Actual Spend




##### 2. Per Site Partners
# Impressions
# LPR
# CTR
#Unique Reach and Frequency

##### 3. Per Tactical Execution

##### 4. Per Creative Message
# Impressions
# LPR
# CTR


##### 5. Per Media Type- Standard vs Rich Media

##### 6. Per Device Type

##### 7. Per Day of Week

