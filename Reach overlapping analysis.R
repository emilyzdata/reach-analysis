library(dplyr)
library(stringr)
library(lubridate)
library(readxl)

setwd("C:/Users/Emily.Zhao/OneDrive - Ipsos/Documents/Regeneron/Libtayo Report/Libtayo reach report June/Q2/pld data")

june_data<- read.delim("Libtayo_PLD_06202023_07192023.txt")
may_data <- read.delim("Libtayo_PLD_05202023_06192023.txt")
apr_data <- read.delim("Libtayo_PLD_04202023_05192023.txt")

may_data$Activity_DateTime<-strtrim(may_data$Activity_DateTime, 2)
may_data<-may_data%>%filter(Activity_DateTime=="05")
apr_data$Activity_DateTime<-strtrim(apr_data$Activity_DateTime, 2)
apr_data<-apr_data%>%filter(Activity_DateTime=="04")
june_data$Activity_DateTime<-strtrim(june_data$Activity_DateTime, 2)
june_data<-june_data%>%filter(Activity_DateTime=="06")
Skin_target<-read_excel("Libtayo_April_skin_target_file.xlsx",sheet="NPP") 
Lung_target<-read_excel("Libtayo_April_Lung_target_file.xlsx",sheet="NPP") 
data_final<-rbind(may_data,apr_data,june_data)


data_final<-data_final%>%select(CMI_PlacementID,Placement_Description,Supplier,Vehicle_Name)
data_final<-data_final[!duplicated(data_final$Placement_Description),]
write_csv(data_final, "placement_final.csv")



###skin###
#HMP Omnimedia
HMP_Skin<-data_final%>%filter(Placement_Description=="Skin Cancer Topic Center | 4/2023-6/2023"|Placement_Description=="Skin Cancer Topic Center | eNewsletters | 3 drops per month | 4/2023-6/2023"|Placement_Description=="Monograph Hosting | 4/2023-12/2023"|Placement_Description=="ACMS Conference KOL Insights | 5/2023-6/2023"|Placement_Description=="AAD Conference KOL Insights | 4/2023")
HMP_Skin<-HMP_Skin%>%select(NPI_ID)
HMP_Skin<-inner_join(HMP_Skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(HMP_Skin$NPI_ID))

#Pulsepoint
Pulsepoint_Skin<-data_final%>%filter(Placement_Description=="Libtayo HCP | Target List Display | Multi-Indication | Budget $55,200 | 4/3/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List Display | Skin | Budget $56,200 | 4/3/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List | Endemic | Skin | $35,000 | 5/22/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List | Endemic | Both | $35,000 | 5/22/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Competitive | HCPs prescribing Erivedge & Odomzo | Skin | Budget $1,500 | 5/22/2023-12/31/2023")
Pulsepoint_Skin<-Pulsepoint_Skin%>%select(NPI_ID)
Pulsepoint_Skin<-inner_join(Pulsepoint_Skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(Pulsepoint_Skin$NPI_ID))

#CHECK THIS #WebMD
WebMD_Skin<-data_final%>%filter(Placement_Description=="Erivedge and Odomzo Drug Monographs | 4/2023-12/2023"|Placement_Description=="Key Accounts Retargeted Media w/ ULD | 4/2023-12/2023"|Placement_Description=="Key Accounts Retargeted Media w/ ULD | 5/2023-12/2023"|Placement_Description=="LIBTAYO Drug Monograph | 783 Visits | 4/2023-12/2023"|Placement_Description=="NMSC Target List w/ ULD | 4/2023-6/2023"|Placement_Description=="NMSC Target List w/ ULD | 5/2023-7/2023"|Placement_Description=="VALUE ADD | NMSC Target List w/ ULD | 4/2023-6/2023"|Placement_Description=="VALUE ADD | NMSC Target List w/ ULD | 5/2023-7/2023")
WebMD_Skin<-WebMD_Skin%>%select(NPI_ID)
WebMD_Skin<-inner_join(WebMD_Skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(WebMD_Skin$NPI_ID))

#Peerdirect
Peerdirect_Skin<-data_final%>%filter(Placement_Description=="RX Roundup | 900 engagements | 5/2023-7/2023"|Placement_Description=="PV Challenges | Microsite | 2,950 engagements | 5/2023-7/2023"|Placement_Description=="PV Challenges | eMails | 5/2023-7/2023")
Peerdirect_Skin<-Peerdirect_Skin%>%select(NPI_ID)
Peerdirect_Skin<-inner_join(Peerdirect_Skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(Peerdirect_Skin$NPI_ID))

#rxnt
rxnt_Skin<-data_final%>%filter(Placement_Description=="VALUE ADD | Libtayo NMSC | Target List Matched HCP (Overall List) | 4/2023-6/2023"|Placement_Description=="Libtayo NMSC | Target List Matched HCP (Overall List) | 4/2023-6/2023")
rxnt_Skin<-rxnt_Skin%>%select(NPI_ID)
rxnt_Skin<-inner_join(rxnt_Skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(rxnt_Skin$NPI_ID))

#Dermatology Times
Dermatology_Times_skin<-data_final%>%filter(Placement_Description=="Skin Cancer Topic Resource Center | 4/2023-12/2023"|Placement_Description=="INTERNAL TAGGING PURPOSES | Front Line Forum AAD eNewsletters | 3/2023-9/2023")
Dermatology_Times_skin<-Dermatology_Times_skin%>%select(NPI_ID)
Dermatology_Times_skin<-inner_join(Dermatology_Times_skin,Skin_target,by=c("NPI_ID"="NPI"))
length(unique(Dermatology_Times_skin$NPI_ID))


#overlay
HMP_Pulsepoint_skin<-inner_join(HMP_Skin,Pulsepoint_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(HMP_Pulsepoint_skin$NPI_ID))

Webmd_Pulsepoint_skin<-inner_join(WebMD_Skin,Pulsepoint_Skin,by=c("NPI_ID"="NPI_ID"))
Webmd_Pulsepoint_skin<-Webmd_Pulsepoint_skin%>%select(NPI_ID)
Webmd_Pulsepoint_skin<-Webmd_Pulsepoint_skin %>% distinct()

length(unique(Webmd_Pulsepoint_skin$NPI_ID))

Peerdirect_Pulsepoint_skin<-inner_join(Peerdirect_Skin,Pulsepoint_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(Peerdirect_Pulsepoint_skin$NPI_ID))

Peerdirect_HMPmedia_Skin<-inner_join(Peerdirect_Skin,HMP_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(Peerdirect_HMPmedia_Skin$NPI_ID))

WebMD_HMPmedia_skin<-inner_join(WebMD_Skin,HMP_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(WebMD_HMPmedia_skin$NPI_ID))

Peerdirect_WebMD_Skin<-inner_join(Peerdirect_Skin,WebMD_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(Peerdirect_WebMD_Skin$NPI_ID))

RXNT_HMPmedia_Skin<-inner_join(rxnt_Skin,HMP_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_HMPmedia_Skin$NPI_ID))

RXNT_WebMD_Skin<-inner_join(rxnt_Skin,WebMD_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_WebMD_Skin$NPI_ID))

RXNT_Peerdirect_Skin<-inner_join(rxnt_Skin,Peerdirect_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_Peerdirect_Skin$NPI_ID))

webMD_DT_Skin<-inner_join(WebMD_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(webMD_DT_Skin$NPI_ID))

RXNT_pulsepoint_Skin<-inner_join(rxnt_Skin,Pulsepoint_Skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_pulsepoint_Skin$NPI_ID))

Pulsepoint_DT_Skin<-inner_join(Pulsepoint_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(Pulsepoint_DT_Skin$NPI_ID))

HMP_DT_Skin<-inner_join(HMP_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(HMP_DT_Skin$NPI_ID))

RXNT_DT_Skin<-inner_join(rxnt_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_DT_Skin$NPI_ID))

RXNT_DT_Skin<-inner_join(Peerdirect_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_DT_Skin$NPI_ID))

Peerdirect_DT_Skin<-inner_join(Peerdirect_Skin,Dermatology_Times_skin,by=c("NPI_ID"="NPI_ID"))
length(unique(Peerdirect_DT_Skin$NPI_ID))

#unique reach individual

#pulsepoint unique reach

pulsepoint_bind<-rbind(WebMD_Skin,HMP_Skin,rxnt_Skin,Peerdirect_Skin,Dermatology_Times_skin)
Pulsepoint_bind_unique<-inner_join(Pulsepoint_Skin,pulsepoint_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(Pulsepoint_bind_unique$NPI_ID))

#HMP unique reach

hmp_bind<-rbind(WebMD_Skin,rxnt_Skin,Pulsepoint_Skin,Peerdirect_Skin,Dermatology_Times_skin)
hmp_bind_unique<-inner_join(HMP_Skin,hmp_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(hmp_bind_unique$NPI_ID))

#WebMD unique reach
WebMD_bind<-rbind(rxnt_Skin,Pulsepoint_Skin,Peerdirect_Skin,HMP_Skin,Dermatology_Times_skin)
WebMD_bind_unique<-inner_join(WebMD_Skin,WebMD_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(WebMD_bind_unique$NPI_ID))

#Peerdirect unique reach
Peerdirect_bind<-rbind(WebMD_Skin,HMP_Skin,Pulsepoint_Skin,rxnt_Skin,Dermatology_Times_skin)
Peerdirect_bind_unique<-inner_join(Peerdirect_Skin,Peerdirect_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(Peerdirect_bind_unique$NPI_ID))

#rxnt unique reach
rxnt_bind<-rbind(WebMD_Skin,HMP_Skin,Pulsepoint_Skin,Peerdirect_Skin,Dermatology_Times_skin)
rxnt_bind_unique<-inner_join(rxnt_Skin,rxnt_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(rxnt_bind_unique$NPI_ID))

#DT unique reach
DT_bind<-rbind(WebMD_Skin,HMP_Skin,Pulsepoint_Skin,Peerdirect_Skin,rxnt_Skin)
DT_bind_unique<-inner_join(Dermatology_Times_skin,DT_bind,by=c("NPI_ID"="NPI_ID"))
length(unique(DT_bind_unique$NPI_ID))


#Lung

#CHECK THIS#WebMD
WebMD_Lung<-data_final%>%filter(Placement_Description=="LIBTAYO Drug Monograph | 783 Visits | 4/2023-12/2023"|Placement_Description=="Lung Cancer/NSCLC Condition Article Exclusivity | 825,000 impressions | 4/1/2023-12/31/2023"|Placement_Description=="Lung Cancer/NSCLC Condition Article Exclusivity | 825,000 impressions | 4/14/2023-12/31/2023"|Placement_Description=="NSCLC Target List w/ ULD | 4/2023-6/2023"|Placement_Description=="NSCLC Target List w/ ULD | 5/2023-7/2023"|Placement_Description=="VALUE ADD | NSCLC Target List w/ ULD | 4/2023-6/2023"|Placement_Description=="VALUE ADD | NSCLC Target List w/ ULD | 5/2023-7/2023"|Placement_Description=="Key Accounts Custom Target List w/ ULD | 4/2023-12/2023"|Placement_Description=="Key Accounts Custom Target List w/ ULD | 5/2023-12/2023"|Placement_Description=="Advanced Non-Small Cell Lung Cancer (â€œASCOâ€) Conference ReCAP IEE | 384 IEE visits | 6/2023-9/2023")
WebMD_Lung<-WebMD_Lung%>%select(NPI_ID)
WebMD_Lung<-inner_join(WebMD_Lung,Lung_target,by=c("NPI_ID"="NPI"))
length(unique(WebMD_Lung$NPI_ID))

#Patient point
Patientpoint_Lung<-data_final%>%filter(Supplier=="PatientPoint Network Solutions, LLC")
Patientpoint_Lung<-Patientpoint_Lung%>%select(NPI_ID)
Patientpoint_Lung<-inner_join(Patientpoint_Lung,Lung_target,by=c("NPI_ID"="NPI"))
length(unique(Patientpoint_Lung$NPI_ID))

#RXNT
RXNT_Lung<-data_final%>%filter(Placement_Description=="Libtayo NSCLC | Target List Marched HCPs (Overall List ) | 4/2023-6/2023"|Placement_Description=="Libtayo NSCLC | Landing Page: Target List Marched HCPs (Overall List) | 4/2023-6/2023"|Placement_Description=="Libtayo Key Accounts | Target List Marched HCPs (Overall List) | 4/2023-6/2023"|Placement_Description=="VALUE ADD | Libtayo NSCLC | Target List Marched HCPs (Overall List ) | 4/2023-6/2023")
RXNT_Lung<-RXNT_Lung%>%select(NPI_ID)
RXNT_Lung<-inner_join(RXNT_Lung,Lung_target,by=c("NPI_ID"="NPI"))
length(unique(RXNT_Lung$NPI_ID))

#Pulsepoint
Pulsepoint_Lung<-data_final%>%filter(Placement_Description=="Libtayo HCP | Target List Display | Lung | Budget $50,120 | 4/3/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List Display | Multi-Indication | Budget $55,200 | 4/3/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List OLV | Lung | Budget $34,000 | 5/15/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Competitive | HCPs prescribing Keytruda | Lung | Budget $18,500 | 5/22/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List | Endemic | Both | $35,000 | 5/22/2023-12/31/2023"|Placement_Description=="Libtayo HCP | Target List | Endemic | Lung | $20,000 | 5/22/2023-12/31/2023")
Pulsepoint_Lung<-Pulsepoint_Lung%>%select(NPI_ID)
Pulsepoint_Lung<-inner_join(Pulsepoint_Lung,Lung_target,by=c("NPI_ID"="NPI"))
length(unique(Pulsepoint_Lung$NPI_ID))

#overlapping

Webmd_Pulsepoint_Lung<-inner_join(WebMD_Lung,Pulsepoint_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(Webmd_Pulsepoint_Lung$NPI_ID))

RXNT_WebMD_Lung<-inner_join(RXNT_Lung,WebMD_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_WebMD_Lung$NPI_ID))

RXNT_pulsepoint_Lung<-inner_join(RXNT_Lung,Pulsepoint_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_pulsepoint_Lung$NPI_ID))

Patientpoint_Pulsepoint_Lung<-inner_join(Patientpoint_Lung,Pulsepoint_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(Patientpoint_Pulsepoint_Lung$NPI_ID))

WebMD_Patientpoint_Lung<-inner_join(WebMD_Lung,Patientpoint_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(WebMD_Patientpoint_Lung$NPI_ID))

RXNT_Patientpoint_Lung<-inner_join(RXNT_Lung,Patientpoint_Lung,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_Patientpoint_Lung$NPI_ID))

#unique individual

#Pulsepoint
pulsepoint_bind2<-rbind(WebMD_Lung,RXNT_Lung,Patientpoint_Lung)
Pulsepoint_bind_unique2<-inner_join(Pulsepoint_Lung,pulsepoint_bind2,by=c("NPI_ID"="NPI_ID"))
length(unique(Pulsepoint_bind_unique2$NPI_ID))

#WebMD
WebMD_bind2<-rbind(Pulsepoint_Lung,RXNT_Lung,Patientpoint_Lung)
WebMD_bind_unique2<-inner_join(WebMD_Lung,WebMD_bind2,by=c("NPI_ID"="NPI_ID"))
length(unique(WebMD_bind_unique2$NPI_ID))

#RXNT
RXNT_bind2<-rbind(WebMD_Lung,Pulsepoint_Lung,Patientpoint_Lung)
RXNT_bind_unique2<-inner_join(RXNT_Lung,RXNT_bind2,by=c("NPI_ID"="NPI_ID"))
length(unique(RXNT_bind_unique2$NPI_ID))

#Patientpoint
Patientpoint_bind2<-rbind(Pulsepoint_Lung,RXNT_Lung,WebMD_Lung)
Patientpoint_bind_unique2<-inner_join(Patientpoint_Lung,Patientpoint_bind2,by=c("NPI_ID"="NPI_ID"))
length(unique(Patientpoint_bind_unique2$NPI_ID))












