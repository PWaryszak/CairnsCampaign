#R-Packages========
#Install these R-packages if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment:
library("readxl")
library("tidyverse")

#DATA:========
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Cairns_Campaign/CairnsCampaign")

#upload CN data:
age_carbon <- read.csv( "CN_NewDATA.csv") %>% #created in CN_DATA.R
              filter(SOC_AGE =="AGE") #We filter out AGE to be double sure we look at AGE sediment slices only
View(age_carbon )

#Upload MAR data:
#MAR = Mass Accretion Rate (Created based on Pere's Age-dating Report)
MAR <- read_xlsx("MASTER_Field_SOC_DataSheet_Cairns2022.xlsx",sheet="Age4R") #Sheet=1 is where SOC data are
MAR

#Join Pere's MAR data with CN data by CoreID:
age_carbon_mar <-  left_join(age_carbon, MAR, by = "CoreID")
View(age_carbon_mar )

#RESULTS=======
#We assume here who core (100cm long) worked well with Pb210 isotopes
results <- age_carbon_core %>%
  
  #Compute CAR = Carbon Accretion Rate:
  mutate(CAR_gcm2y = C.percent/100 * MAR)%>% #Convert % into fraction by "/100"

  group_by(CoreID) %>% #Group All OC% values by core and estimated the weighted mean of OC%
  
  #get the weighted mean of %OC:
  summarise(AV = weighted.mean(CAR_gcm2y*100, na.rm = T), #1 g/cm2 = 100 tonnes per hectare
            SD = sd(CAR_gcm2y*100, na.rm = T), #SD = Standard Deviation
            N  = n(),                          #N  = number of replicates
            SE = SD / sqrt(N))                 #SE = Standard Error

results

#Get on mean CAR value result:
overall_result <- age_carbon_core %>%
  
  #Compute CAR = Carbon Accretion Rate:
  mutate(CAR_gcm2y = C.percent/100 * MAR)%>% #Convert % into fraction by "/100"
  mutate( habitat = "Mangrrove") %>%
  
  group_by(habitat)%>%
  
  #get the weighted mean of one CAR:
  summarise(AV = weighted.mean(CAR_gcm2y*100, na.rm = T), #1 g/cm2 = 100 tonnes per hectare
            SD = sd(CAR_gcm2y*100, na.rm = T), #SD = Standard Deviation
            N  = n(),                          #N  = number of replicates
            SE = SD / sqrt(N))                 #SE = Standard Error


overall_result
#       AV        SD  N        SE
# 1.838084 0.8550702 60 0.1103891
round(1.838084,2) #1.84

#PLOT RESULTS (by CoreID)==========
ggplot(results, aes(CoreID, AV))  +
  geom_point(aes(color = CoreID, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.2)+
  labs(x= "", y = bquote('Carbon Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+ #Mg = tonne
  
  ggtitle("Cairns Airport campaign (Year 2022)")+
  
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        strip.background =  element_rect(fill = "white"))


#Plot overall_result:
ggplot(overall_result, aes(habitat, AV))  +
  geom_point(aes( size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.2)+
  labs(x= "", y = bquote('Carbon Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+ #Mg = tonne
  
  ggtitle("Cairns Airport campaign")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        strip.background =  element_rect(fill = "white"))

