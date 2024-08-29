#~~~~~Data for ACSIS 2021 Report
# source("W:/Users/Shared_Files/STAT/00Material-for-R/Functions_STAT_less_packages.R")
# source("C:/Users/talco/Documents/Statistics/ACSIS_21_PDF/Functions_STAT_less_packages.R")
# source("C:/Users/talco/Documents/Statistics/ACSIS_21_PDF/Functions_STAT_less_packages.R")
source('C:/Users/ido/sheba stats/ACSIS/Booklet/R scripts data prep/Functions_STAT_less_packages.R')



#data updated in 10.3.22 (Version 8 of the booklet):
# dat0 <- read.csv("W:/Users/Shared_Files/datamng/s2021/workfiles/ACSIS24_2.csv", header = TRUE, 
#                  na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), stringsAsFactors = T) #n=1851

#In Sheba's computer I opened the file with notepad, and saved with encoding UTF-8
# dat0 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_21_PDF/ACSIS24_3_save_utf_updated.csv", header = TRUE, 
#                  na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), stringsAsFactors = T) #n=1851
# dat0 <- read.csv('C:/Users/ido/sheba stats/ACSIS/data/ACSIS24_0.csv', header = TRUE, 
#                 na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), stringsAsFactors = T) 
# dat0 <- read.csv('C:/Users/ido/sheba stats/ACSIS/data/ACSIS24_0.csv', header = TRUE, 
#                  na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), 
#                  stringsAsFactors = TRUE, fileEncoding = "UTF-8")
dat0 <- read.csv('C:/Users/ido/sheba stats/ACSIS/data/ACSIS24_1.csv', header = TRUE, 
                 na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), 
                 stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")

#24.7.22 - file for HMO:
# dat_to_send <- dat0[dat0$S24INFORM_CONS=="Yes" ,c( "HAKORG","S24BIRTH_YEAR", "S24SEX")] #n=1629
# write.csv(dat_to_send, file = "W:/Users/Shared_Files/STAT/2021/ACSIS 2021 booklet/File_for_HMO_24072022.csv")

dat <- droplevels(subset(dat0,S24DISDIA %in% c("NSTEMI","STE MI","UAP",
                                               "Microvascular", "MINOCA - VASOSPASTIC,", "Thromboembolic"  
                                               ,NA
                                               ))) #n=1750
dat$age5080 <- cut(dat$AGE, c(min(dat$AGE, na.rm = T),50,65,80, max(dat$AGE, na.rm = T)), right = F, include.lowest = T)


# fillna0 <- function(X) { X[is.na(X)] <- "No"; X}
# 
# #Chronic medical treatment
# vars_chr_treat0 <- c("S18C1","S18C2","S18C3","S18C4","S18C6","S18C7","S18C8","S18C9","S18C10","S18C11","S18C13","S18C14","S18C15",
#                     "S18C17","S18C19","S18C22","S18C23","S18C33","S18C34","S18C35","S18C36","S18C37","S18C38","S18C39","S18C40",
#                     "S18C41","S18C42","S18C43","S18C44","S18C45","S18C46","S18C47","S18C48","S18C49","S18C50","S18C51")
# for (i in vars_chr_treat0) {
#   dat[,i] <- fillna0(dat[,i])
# }
dat$chr_ACEI_ARB <- as.factor(ifelse(dat$S24C14=="YES" | dat$S24C15=="YES", "Yes", "No"))
dat$chr_STAT <- as.factor(ifelse(dat$S24C48=="YES" | dat$S24C49=="YES" | dat$S24C50=="YES" | dat$S24C51=="YES", "Yes", "No"))
#In all of the following variables, there is NA for the non-diabetic (S24PDIAB=="No")
dat$antihyperglycemic <- as.factor(ifelse(dat$S24C34=="YES" | dat$S24C35=="YES" | dat$S24C36=="YES" | dat$S24C37=="YES" |
                                  dat$S24C38=="YES" | dat$S24C39=="YES" | dat$S24C40=="YES" | dat$S24C41=="YES" |
                                  dat$S24C42=="YES" | dat$S24C43=="YES" | dat$S24C44=="YES" | dat$S24C45=="YES" |
                                  dat$S24C46=="YES" | dat$S24C47=="YES", "YES", "No"))
dat$antihyperglycemic[is.na(dat$antihyperglycemic)] <- "No" 



dat$NOAC_CHR <- factor(ifelse(dat$S24C7=="YES" | dat$S24C8=="YES"| dat$S24C9=="YES", "Yes","No"))
dat$chr_oral_anticoag <- factor(ifelse(dat$S24C6=="YES" | dat$S24C7=="YES" | dat$S24C8=="YES"| dat$S24C9=="YES", "Yes","No"))

#Outliers:
outlier_trim_func <- function(x, trim_up_at=0.99, trim_low_at=0) {x <- as.numeric(ifelse(x>quantile(x, probs = trim_up_at, na.rm = T), quantile(x, probs = trim_up_at, na.rm = T), 
                                              ifelse(x<trim_low_at, trim_low_at, as.character(x))))}
dat$HEFT1_trim <- outlier_trim_func(dat$HEFT1)
dat$HEFT2_trim <- outlier_trim_func(dat$HEFT2)
dat$HEFT4_trim <- outlier_trim_func(dat$HEFT4)
dat$HEFT9_trim <- outlier_trim_func(dat$HEFT9)
dat$ARR_TLX_trim <- outlier_trim_func(dat$ARR_TLX)
dat$ARR_PPCI_trim <- outlier_trim_func(dat$ARR_PPCI)

# ECG:
dat$S24ECGRHY <- factor(dat$S24ECGRHY, levels =c( "NSR", "AF","S. tachy"  ,"S. brady","VT/VF" ,
                                                  "2-3 degree AV block", "Asystole", "Other") )

#In hospital medical treatment
vars_inhosp_treat0 <- c("S18H1","S18H2","S18H3","S18H4","S18H5","S18H6","S18H7","S18H8","S18H9","S18H10","S18H11","S18H12","S18H13",
                       "S18H14","S18H15","S18H16","S18H17","S18H18","S18H19","S18H20","S18H21","S18H22","S18H23","S18H24","S18H25",
                       "S18H26","S18H27","S18H28","S18H29","S18H30","S18H31","S18H32","S18H33","S18H34","S18H35","S18H36","S18H37",
                       "S18H38","S18H39","S18H40","S18H41","S18H42","S18H43","S18H44","S18H45","S18H46","S18H47","S18H48","S18H49",
                       "S18H50","S18H51")
# for (i in vars_inhosp_treat0) {
#   dat[,i] <- fillna0(dat[,i])
# }

dat$hosp_ACEI_ARB <- as.factor(ifelse(dat$S24H14=="YES" | dat$S24H15=="YES","Yes","No"))
dat$hosp_STAT <- as.factor(ifelse(dat$S24H48=="YES" | dat$S24H49=="YES" | dat$S24H50=="YES" | dat$S24H51=="YES", "YES", "No"))
#in the antihyperglycemic drugs, if the patient is not Diabetic, than these drugs will be "NA" (according to Lizie):
dat$hosp_antihyperglycemic <- as.factor(ifelse(dat$S24H34=="YES" | dat$S24H35=="YES" | dat$S24H36=="YES" | dat$S24H37=="YES" |
                                            dat$S24H38=="YES" | dat$S24H39=="YES" | dat$S24H40=="YES" | dat$S24H41=="YES" |
                                            dat$S24H42=="YES" | dat$S24H43=="YES" | dat$S24H44=="YES" | dat$S24H45=="YES" |
                                            dat$S24H46=="YES" | dat$S24H47=="YES", "Yes", "No"))
dat$oral_anticoag_hosp <- factor(ifelse(dat$S24H6=="YES" | dat$S24H7=="YES" | dat$S24H8=="YES" | dat$S24H9=="YES", "Yes", "No"))

#Medical treatment at discharge
vars_disch_treat0 <- c("S18D1","S18D2","S18D3","S18D4","S18D5","S18D6","S18D7","S18D8","S18D9","S18D10","S18D11","S18D12","S18D13",
                        "S18D14","S18D15","S18D16","S18D17","S18D18","S18D19","S18D20","S18D21","S18D22","S18D23","S18D24","S18D25",
                        "S18D26","S18D27","S18D28","S18D29","S18D30","S18D31","S18D32","S18D33","S18D34","S18D35","S18D36","S18D37",
                        "S18D38","S18D39","S18D40","S18D41","S18D42","S18D43","S18D44","S18D45","S18D46","S18D47","S18D48","S18D49",
                        "S18D50","S18D51")
# for (i in vars_disch_treat0) {
#   dat[,i] <- fillna0(dat[,i])
# }

dat$disch_ACEI_ARB <- as.factor(ifelse(dat$S24D14=="YES" | dat$S24D15=="YES","Yes","No"))
dat$disch_STAT <- as.factor(ifelse(dat$S24D48=="YES" | dat$S24D49=="YES" | dat$S24D50=="YES" | dat$S24D51=="YES", "Yes", "No"))
dat$disch_antihyperglycemic <- as.factor(ifelse(dat$S24D34=="YES" | dat$S24D35=="YES" | dat$S24D36=="YES" | dat$S24D37=="YES" |
                                                 dat$S24D38=="YES" | dat$S24D39=="YES" | dat$S24D40=="YES" | dat$S24D41=="YES" |
                                                 dat$S24D42=="YES" | dat$S24D43=="YES" | dat$S24D44=="YES" | dat$S24D45=="YES" |
                                                 dat$S24D46=="YES" | dat$S24D47=="YES", "Yes", "No"))

#disch GLP1 and SGL2:
# S24D44	Discharged with: Dapagliflozine (Forxiga)
# S24D56	Discharged with: Empagliflozine (Jardiance)
dat$disch_SGLT2 <- as.factor(ifelse(dat$S24D44=="YES" | dat$S24D56=="YES", "Yes","No"))

# S24D43	Discharged with: Liraglutide (Victoza)
# S24D57	Discharged with: Semaglutide (Ozempic)
# S24D42	Discharged with: Exenatide (Byetta, Budyreon)
# no dulag at disch
dat$disch_GLP1 <- as.factor(ifelse(dat$S24D43=="YES" | dat$S24D57=="YES" | dat$S24D42=="YES", "Yes","No"))

dat$oral_anticoag_disch <- factor(ifelse(dat$S24D6=="YES" | dat$S24D7=="YES" | dat$S24D8=="YES" | dat$S24D9=="YES", "Yes", "No"))

#P2Y12 on discharge corrections

dat$S18D3_corrected <- dat$S18D3
dat$S18D4_corrected <- dat$S18D4
dat[dat$S18D3=="Yes" & !is.na(dat$S18D3) & dat$S18D4=="Yes" & !is.na(dat$S18D4), c("S18D3_corrected","S18D4_corrected")] <- NA

dat$DP2Y12_corrected <- dat$DP2Y12
dat[is.na(dat$S18D3_corrected) & is.na(dat$S18D4_corrected) & (is.na(dat$S18D2)| dat$S18D2=="No"), "DP2Y12_corrected"] <- NA

# dat$FU_STAT <- as.factor(ifelse(dat$S24F48=="YES" | dat$S24F49=="YES" | dat$S24F50=="YES" | dat$S24F51=="YES", "Yes", "No"))

#~~~~~~~~~~~~~~
dat$pciasp <- as.factor(ifelse(dat$S24PCIASSTART %in% c("before",NA), "No","Yes"))
dat$pciclp <- as.factor(ifelse(dat$S24PCICLSTART %in% c("before",NA), "No","Yes"))
dat$pcipra <- as.factor(ifelse(dat$S24PCIPRSTART %in% c("before",NA), "No","Yes"))
dat$pcitic <- as.factor(ifelse(dat$S24PCITISTART %in% c("before",NA), "No","Yes"))
dat$pci2b <- fillnax(dat$S24PCI2B, "No")
dat$pciac3 <- fillnax(dat$S24PCIAC3, "No")
dat$pcipd <- fillnax(dat$S24PCIPD, "No")

# vars_drugs_pci_yn <- c("S18PCIAS","S18PCICL","S18PCIPR","S18PCITI")
# for (i in vars_drugs_pci_yn) {
#   dat[,i] <- fillna0(dat[,i])
# }

dat$S24HOSEFR1 <- factor(dat$EF_CLASS, levels = c("Severe (< 30%)","Moderate (30-39%)","Mild (40-49%)","Preserved (50-54%)",  "Normal (55-65%)"))
dat$S24HOSEFR <- factor(dat$EF_CLASS, levels = c("Normal (55-65%)", "Preserved (50-54%)", "Mild (40-49%)","Moderate (30-39%)","Severe (< 30%)"))

dat$vf <- factor(ifelse(dat$S24COMP14=="Yes" | dat$S24COMP15=="Yes", "Yes", "No"))
  
dat$st_only_angio <- as.factor(ifelse(dat$ANYCABG=="NO" & dat$ANYPCI=="NO", "Yes","No"))
dat$nst_only_angio <- as.factor(ifelse(dat$S24HCABG=="No" & dat$S24HPCI=="No", "Yes","No"))

dat$S24SEX <- as.factor(dat$S24SEX)
dat$sex <- relevel(dat$S24SEX, "Male")

#~~~ Aspirin in-hospital correction
dat$S24H1[dat$S24D1=="YES"] <- "YES"
# dat$S18D1[dat$S18H1=="Yes"] <- "Yes"

#~~~ Completing in-hospital treatment by discharge treatment of Clopidogrel, Tigacrelor and Prasugrel
# dat$S18H2[dat$S18D2=="Yes"] <- "Yes"
# dat$S18H3[dat$S18D3=="Yes"] <- "Yes"
# dat$S18H4[dat$S18D4=="Yes"] <- "Yes"


# dat$S18D2[dat$S18H2=="Yes"] <- "Yes"
# dat$S18D3[dat$S18H3=="Yes"] <- "Yes"
# dat$S18D4[dat$S18H4=="Yes"] <- "Yes"

#~~~ correcting beta-blockers
# dat$S18H17[dat$S18D17=="Yes"] <- "Yes"
# dat$S18D17[dat$S18H17=="Yes"] <- "Yes"

#~~~ correcting ACE/ARB
# dat$hosp_ACEI_ARB[dat$disch_ACEI_ARB=="Yes"] <- "Yes"
# dat$disch_ACEI_ARB[dat$hosp_ACEI_ARB=="Yes"] <- "Yes"

#~~~ Completing "Yes" values for P2Y12
# dat$HP2Y12[dat$DP2Y12=="YES"] <- "YES"
# dat$DP2Y12[dat$HP2Y12=="Yes"] <- "Yes"
levels(dat$HP2Y12) <- c("NO"="No", "YES" = "Yes")

#~~~ Completing "Yes" values for statins
# dat$hosp_STAT[dat$disch_STAT=="Yes"] <- "Yes"
# dat$disch_STAT[dat$hosp_STAT=="Yes"] <- "Yes"
#~~~~~~~~~~~~~


#~~~~~Prasugrel -> Tigacrelor -> Clopidogrel

#In-hospital
# dat$S18H4[dat$S18H4=="Yes" & dat$S18H3=="Yes"] <- "No"
# dat$S18H2[dat$S18H2=="Yes" & (dat$S18H4=="Yes" | dat$S18H3=="Yes")] <- "No"

# #At dishcarge
# dat$S18D4[dat$S18D4=="Yes" & dat$S18D3=="Yes"] <- "No"
# dat$S18D2[dat$S18D2=="Yes" & (dat$S18D4=="Yes" | dat$S18D3=="Yes")] <- "No"

#~~~~~~~~~~~~~Calculating P2Y12
# dat$cp2y12 <- as.factor(ifelse(dat$S18C2=="Yes" | dat$S18C3=="Yes" | dat$S18C4=="Yes", "Yes", "No"))
# dat$CP2Y12 <- as.factor(ifelse((is.na(dat$cp2y12) & dat$S18H2=="No") | dat$cp2y12=="No", "No", "Yes"))

# dat$dp2y12 <- as.factor(ifelse(dat$S18D2=="Yes" | dat$S18D3=="Yes" | dat$S18D4=="Yes", "Yes", "No"))
# dat$DP2Y12 <- as.factor(ifelse((is.na(dat$dp2y12) & dat$S18D2=="No") | dat$dp2y12=="No", "No", "Yes"))

# dat$S18NOREP8 <- factor(dat$S18NOREP8, levels = c("No","Yes"))
# dat$S18NOREP9 <- factor(dat$S18NOREP9, levels = c("No","Yes"))
# dat$S18COMP10 <- factor(dat$S18COMP10, levels = c("No","Yes"))

#till 2018 we stratified by admission diagnosis:
dat$ST_ELEV_adm <- factor(ifelse(dat$ECGGRP=="ST","Yes","No"))
#from 2021 Beigel said to stratify by disch diagnosis:
dat$ST_ELEV_0 <- factor(ifelse(dat$S24DISDIA=="STE MI","Yes","No"))
#if missing in S24DISDIA then take from ARR_DIAG:
dat$ST_ELEV <- factor(ifelse(is.na(dat$S24DISDIA) & dat$ARR_DIAG=="STE MI","Yes",
                           ifelse(is.na(dat$S24DISDIA) & dat$ARR_DIAG!="STE MI","No",
                                  ifelse(is.na(dat$S24DISDIA) & is.na(dat$ARR_DIAG) ,NA,
                                         ifelse(dat$S24DISDIA  %in% c("STE MI"),"Yes","No")))))


# dat$disto <- dat$S18DISTO
# dat$disto[dat$disto %in% c("Convalescence facility/unit (hotel)","Home")] <- "Home"

#STEMI patients with missing data of primary reperfusion can be addressed as NO according to Beigel, 
#because their admission ECG is not ST elevation (dat[dat$ST_ELEV=="Yes" & is.na(dat$S24REP), "S24ADMECG"])
dat$S24NOREP_other <- factor(ifelse(dat$ST_ELEV=="Yes" & is.na(dat$S24REP), "YES","NO"))
#if all other reasons are "no", include it in "other":
dat$S24NOREP_other <- factor(ifelse(dat$ST_ELEV=="Yes" & dat$S24REP%in%"No" & 
                                      rowSums(as.data.frame(lapply(dat[,c(paste0("S24NOREP",c(1:6,8)))], function(x) {x<-as.numeric(x)-1})))==0,
                                    "YES", as.character(dat$S24NOREP_other)))
dat$S24REP <- factor(ifelse(dat$ST_ELEV=="Yes" & is.na(dat$S24REP), "No", as.character(dat$S24REP)))
#1 patients with both NOREP1 & NOREP5: make it to 5 (in NOREP5T it says "other"):
dat$S24NOREP1 <- factor(ifelse(dat$S24NOREP1=="YES" & dat$S24NOREP5=="YES", "NO", as.character(dat$S24NOREP1)))
#20.03.22 (version 9 of the booklet):
#Beigel said that if the patient has S24REP==YES and REP=NA so make REP=PRIMARY PCI:
dat$REP <- as.factor(ifelse(is.na(dat$REP) & dat$S24REP=="Yes", "PRIMARY PCI", as.character(dat$REP)))

dat <- droplevels(dat)

dat21 <- dat

dat_st <- subset(dat, ST_ELEV=="Yes")
dat_st_rep <- subset(dat, ST_ELEV=="Yes" & S24REP=="Yes")
dat_st_rep_ppci <- subset(dat, ST_ELEV=="Yes" & S24REP=="Yes" & REP=="PRIMARY PCI")

dat_st_nrep <- subset(dat, S24REP=="No" & !is.na(S24REP) & ST_ELEV=="Yes" & !is.na(ST_ELEV))


dat_surv <- subset(dat, S24DISDIE=="Alive")
# dat_rehos <- subset(dat, S24FREHOS=="Yes")

#Follow up
# dat_fu <- subset(dat, !is.na(FU_FILLING_IN_BY)) #n=1469

