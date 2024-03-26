#Install 2 R-packages we need if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")



#Merge new CN data with your MASTER file:
MASTER_DATA <- read_xlsx("MASTER_Field_SOC_DataSheet_Cairns2022.xlsx",sheet="CN_NEWDATA") #Sheet=1 is where SOC data are
names(MASTER_DATA)
dim(MASTER_DATA) # 210  27

MASTER_DATA$habitat <- "Mangroves"
Carbonates <- filter(MASTER_DATA, Presence_of_Carbonates =="Yes") #11 syringe core subsamples had carbonates in them
Carbonates[,2] #See the syringe core  subsamples

CairnsData <- MASTER_DATA
names(CairnsData)

#View(CairnsData)
CairnsData$dry_bulk_density.gcm3 <- CairnsData$DryWeightTotal_g / CairnsData$SedVolume_ml
CairnsData$CarbonDensity.gcm3    <- CairnsData$dry_bulk_density.gcm3 * CairnsData$C.percent/100


#CairnsData$SliceLength_cm <- 3  #3cm length of each slice = internal diameter
CairnsData$CarbonStock.Mgha      <- (((CairnsData$CarbonDensity.gcm3  / 1000000 ) *100000000) * CairnsData$SliceLength_cm)   
CairnsData$Core_in.cm            <- CairnsData$PipeLenght_cm  - CairnsData$Compaction_In_cm  #Compaction in cm
CairnsData$Pipe_in.cm            <- CairnsData$PipeLenght_cm - CairnsData$Compaction_Out_cm
CairnsData$Compaction_Correction_Value<- CairnsData$Core_in.cm / CairnsData$Pipe_in.cm
CairnsData$CarbonStock.Mgha_CORRECTED <- CairnsData$CarbonStock.Mgha * CairnsData$Compaction_Correction_Value
CairnsData$dry_bulk_density.gcm3_CORRECTED    <- CairnsData$dry_bulk_density.gcm3 *  CairnsData$Compaction_Correction_Value


DepthValues <- unique(CairnsData$DepthTo_cm, na.rm=T)
DepthValues <- na.omit(DepthValues)
DepthValues <- as.vector(DepthValues)
DepthValues# "NA" "1"  "5"  "9"  "15" "22" "30" "42" "50" "62" "74"

#PLOT SOC=====
ggplot(CairnsData, aes(x = SOC_AGE, y = C.percent, color=SOC_AGE)) +
  geom_boxplot() +
  facet_grid(.~habitat)+ geom_jitter(alpha = 0.4)+
  ylab("Organic Carbon (%)")  +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))

#PLOT OC%========
ggplot(CairnsData[CairnsData$SOC_AGE =="SOC",], aes(y = SliceSection, x = C.percent)) +
  geom_boxplot(outlier.shape = NA) +
  #facet_grid(.~SliceSection)+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() + geom_jitter(alpha = 0.4)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))


#PLOT DBD corrected=======
ggplot(CairnsData[CairnsData$SOC_AGE =="SOC",], aes(y = SliceSection, x =dry_bulk_density.gcm3)) +
  geom_boxplot(outlier.shape = NA) +
  #facet_grid(.~SliceSection)+
   xlab(bquote("Dry bulk density "  (g*~cm^-3))) +
  ylab("Soil depth interval") +
  theme_bw() + geom_jitter(alpha = 0.4)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),

        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))


ggplot(CairnsData[CairnsData$SOC_AGE =="SOC",], aes(y = SliceSection, x =dry_bulk_density.gcm3_CORRECTED)) +
  geom_boxplot(outlier.shape = NA) +
  #facet_grid(.~SliceSection)+
   xlab(bquote("Dry bulk density "  (g*~cm^-3))) +
  ylab("Soil depth interval") +
  theme_bw() + geom_jitter(alpha = 0.4)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),

        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))


ggsave( dpi=300, width = 7, height = 5,       filename = "PLOT_DBD_Corrected.png")


#Plot By Distance to River:=======
river <-  CairnsData %>%
  filter(SOC_AGE == "SOC") %>%
  filter ( SiteID == 1 | SiteID == 6 | SiteID == 7) %>%
  mutate(Distance = ifelse(SiteID == 1, "Far", ifelse(SiteID==6, "Middle", "Close")))

river$Distance <- factor(river$Distance, levels = c( "Close","Middle", "Far"))

#BC stock:
ggplot(river, aes(y = Distance, x = CarbonStock.Mgha_CORRECTED, fill=Distance)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(shape=SliceSection),size=3)+
  xlab(bquote("Carbon stock "  (Mg*~ha^-1))) +
  ylab("Distance to river") +
  labs (shape = "Slice Section:")+
  guides(fill= "none")+
  
  scale_fill_manual(values = c('#deebf7','#9ecae1','#3182bd'))+
  
  theme_classic() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),

        legend.position = c(.85, .55),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))

ggsave( dpi=300, width = 7, height = 5,       filename = "PLOT_River_BC_Corrected.png")


#C.percent:
ggplot(river, aes(y = Distance, x = C.percent, fill=Distance)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(shape=SliceSection),size=3)+
  xlab(bquote("Carbon stock "  (Mg*~ha^-1))) +
  ylab("Distance to river") +
  labs (shape = "Slice Section:")+
  guides(fill= "none")+
  
  scale_fill_manual(values = c('#deebf7','#9ecae1','#3182bd'))+
  
  theme_classic() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),

        legend.position = c(.85, .55),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))

