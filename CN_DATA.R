#Install 2 R-packages we need if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")

#IMPORTANT:
#YOUR SAMPLE_ID (I named it CNCode) should be consistent across all your spreadsheets.

#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Cairns_Campaign/DATA/CN_Data/CN_RAW",
                    pattern = "*.xls", full.names = T)

files#Check file names if all there


#Create function to export data from sheet = 1 (Sample Table)
ReadCN_smp <- function(x) read_xlsx (path = x,
                                     sheet = 1,
                                     skip = 7,
                                     range = cell_cols("C:E"))

#Export "Sample Table" data from all files in your folder:
tbl1 <- sapply(files, ReadCN_smp, simplify=FALSE) %>%
  bind_rows(.id = "id1")


#Create function to export data from sheet = 2 (Element%)
ReadCN <- function(x) read_xlsx (path = x,sheet = 2,skip = 7, range = cell_cols("C:D"))

#Export "Element%" data from all files in your folder using sapply:
tbl2 <- sapply(files, 
               ReadCN, simplify=FALSE) %>% 
  bind_rows(.id = "id2")

#Bind sample (tbl1) and CN (tbl2) data together
CN_DATA <- cbind(tbl1,tbl2)#bind smp and CN data

#Double check if data alligns well:
all.equal(tbl1$id1,tbl2$id2) #should be TRUE!

#Clean up the file to keep data you need in a form you/R likes (no special signs):
CN_DATA_clean <- CN_DATA %>%
  filter(Type == "Smp") %>%
  select("id1", "Sample Name","Weight", "(N) %", "(C) %" ) %>%
  rename(file = "id1", CNCode = "Sample Name", Weight.mg = "Weight",
         N.percent = "(N) %", C.percent = "(C) %")

View(CN_DATA_clean)

#Check for duplicates:
anyDuplicated(CN_DATA_clean$CNCode)#Should be 0!!!
#If not 0, some samples are dupliacted,
#You have to decide what to do with duplicates (e.g., average them, remove them)

#Merge new CN data with your MASTER file:
#See where R is looking now:
getwd() # "C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Cairns_Campaign/CairnsCampaign"

#Set new Directory where to find MASTER file:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Cairns_Campaign/DATA")

MASTER_DATA <- read_xlsx("MASTER_Field_SOC_DataSheet_Cairns2022.xlsx",sheet=1) #Sheet=1 is where SOC data are


#Merge CN_DATA_clean and  MASTER_DATA together:
NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")

dim(MASTER_DATA)#150 rows   20 cols
dim(NewDATA)# 150  24
View(NewDATA)
#write.csv(NewDATA, file = "CN_NewDATA.csv")

#Plot======
NewDATA <- NewDATA [ !is.na(NewDATA$Site),] #remove NA-s if any
NewDATA$SiteID <-  as.factor(NewDATA$SiteID)
NewDATA$SiteID <-  factor(NewDATA$SiteID, levels = c("1","2","3","4", "5", "6", "7", "8", "9","10"))

#PLOT CN-DATA=====
#Plot By ecosystem type:
ggplot(NewDATA, aes(x = SiteID, y = C.percent)) +
  geom_boxplot() +
  #facet_grid(.~habitat)+
  geom_jitter(alpha = 0.4)+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))



#Identify the outlier:
outlier <- NewDATA[which.max(NewDATA$C.percent), "SampleID"]
outlier
