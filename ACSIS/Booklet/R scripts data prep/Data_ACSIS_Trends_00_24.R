# ~~~~~~~~~Data for Booklet ACSIS 2024 - Temporal Trends

#Data was updated in 10.3.22 (version 8 of the booklet):
# dat0 <- read.csv("W:/Users/Shared_Files/datamng/acs00_02/csvfiles/R00_21_3.csv", 
#                  header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"), 
#                  stringsAsFactors = T) #n=18739
dat0 <- read.csv('C:/Users/ido/sheba stats/ACSIS/data/ACSIS24_1_trend.csv', 
                 header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"), 
                 stringsAsFactors = T) #n=20494

dat <- droplevels(subset(dat0, 
                           SOURCE %in% c("S2010","S2013","S2016","S2018", "S2021", "S2024"))) #n=8983


# Yes -> YES and No -> No correction

# vars_y_n <- c("HTICL","HPRAS","HTICGR","HECHO","HIOB","HANT","HDIGIT","HDIUR","HNIT","DDIGIT","DDIUR","DNIT")
# for (i in vars_y_n){
#   dat[,i] <- factor(ifelse(dat[,i] %in% c("YES", "Yes"), "YES", ifelse(dat[,i] %in% c("NO", "No"), "NO", NA)))
# }


dat$age5075 <- cut(dat$AGE, c(min(dat$AGE, na.rm = T),50,75, max(dat$AGE, na.rm = T)), right = T, include.lowest = T)
dat$year <- as.numeric(substr(dat$SOURCE,2,5))
dat$admecg <- factor(ifelse(dat$ADMECG=="ST","ST elevation","Non ST elevation"), levels = c("ST elevation","Non ST elevation"))
dat$ADMKLP <- as.factor(dat$ADMKLP)
dat$rept <- as.factor(ifelse(dat$REPT=="TLX","TLX","PCI"))
dat$door2b90 <- factor(ifelse(dat$HEFT7<=90, "<=90",">90"), levels = c(">90","<=90"))
dat$heft4 <- ifelse(dat$HEFT4==0,1,dat$HEFT4)
dat$heft6 <- ifelse(dat$HEFT6==0,1,dat$HEFT6)
dat$heft7 <- ifelse(dat$HEFT7==0,1,dat$HEFT7)
dat$ons_rep <- ifelse(dat$ONS_REP==0,1,dat$ONS_REP)

#Medical treatment correction - if "YES" at discharge - then "YES" in-hospital
dat$HASA[dat$DASA=="YES"] <- "YES"
# dat$DASA[dat$HASA=="YES"] <- "YES"



#~~~ Correcting beta-blockers and ACE/ARB

dat$HBB[dat$DBB=="YES"] <- "YES"
# dat$DBB[dat$HBB=="YES"] <- "YES"

dat$HACEARB[dat$DACEARB=="YES"] <- "YES"
# dat$DACEARB[dat$HACEARB=="YES"] <- "YES"

#~~~ Completing "Yes" values for P2Y12
# dat$HP2Y12[dat$DP2Y12=="YES"] <- "YES" #old version
# #dat$DP2Y12[dat$HP2Y12=="YES"] <- "YES"


#~~~ Completing "Yes" values for statins
dat$HSTAT[dat$DSTAT=="YES"] <- "YES"
# dat$DSTAT[dat$HSTAT=="YES"] <- "YES"
#~~~~~~~~~~~~~

#~~~ Completing in-hospital treatment by discharge treatment of Clopidogrel, Tigacrelor and Prasugrel
# updated by Tal in v7: if NA in hosp, take from discharge, if NA in disch, take from hosp

#v7:
dat$HTICL[is.na(dat$HTICL) & !is.na(dat$DTICL)] <- dat$DTICL[is.na(dat$HTICL) & !is.na(dat$DTICL)]
dat$DTICL[is.na(dat$DTICL) & !is.na(dat$HTICL)] <- dat$HTICL[is.na(dat$DTICL) & !is.na(dat$HTICL)]

dat$HPRAS[is.na(dat$HPRAS) & !is.na(dat$DPRAS)] <- dat$DPRAS[is.na(dat$HPRAS) & !is.na(dat$DPRAS)]
dat$DPRAS[is.na(dat$DPRAS) & !is.na(dat$HPRAS)] <- dat$HPRAS[is.na(dat$DPRAS) & !is.na(dat$HPRAS)]

dat$HTICGR[is.na(dat$HTICGR) & !is.na(dat$DTICGR)] <- dat$DTICGR[is.na(dat$HTICGR) & !is.na(dat$DTICGR)]
dat$DTICGR[is.na(dat$DTICGR) & !is.na(dat$HTICGR)] <- dat$HTICGR[is.na(dat$DTICGR) & !is.na(dat$HTICGR)]

#taken from old version: if yes in disch, then yes in hosp:
dat$HTICL[dat$DTICL=="YES"] <- "YES" #old version
#dat$DTICL[dat$HTICL=="YES"] <- "YES" #old version
dat$HPRAS[dat$DPRAS=="YES"] <- "YES" #old version
#dat$DPRAS[dat$HPRAS=="YES"] <- "YES" #old version
dat$HTICGR[dat$DTICGR=="YES"] <- "YES" #old version
#dat$DTICGR[dat$HTICGR=="YES"] <- "YES" #old version

#P2Y12 on discharge corrections
# correction 1: no patients with more than 1 medication
dat$DTICL_corrected <- dat$DTICL
dat[(dat$DTICL=="YES" & !is.na(dat$DTICL)) & ((dat$DPRAS=="YES" & !is.na(dat$DPRAS)) | (dat$DTICGR=="YES" & !is.na(dat$DTICGR))), "DTICL_corrected"] <- "NO"

dat$DPRAS_corrected <- dat$DPRAS
dat$DTICGR_corrected <- dat$DTICGR
dat[(dat$DPRAS=="YES" & !is.na(dat$DPRAS)) & (dat$DTICGR=="YES" & !is.na(dat$DTICGR)), c("DPRAS_corrected","DTICGR_corrected")] <- NA

#correction 2: if "yes" in one medication, then "no" in the others: (added by Tal in version 7 of the PDF)
dat$DTICL_corrected[is.na(dat$DTICL_corrected) & (dat$DPRAS_corrected=="YES" | dat$DTICGR_corrected=="YES")] <- "NO"
dat$DTICGR_corrected[is.na(dat$DTICGR_corrected) & (dat$DPRAS_corrected=="YES" | dat$DTICL_corrected=="YES")] <- "NO"
dat$DPRAS_corrected[is.na(dat$DPRAS_corrected) & (dat$DTICL_corrected=="YES" | dat$DTICGR_corrected=="YES")] <- "NO"

#correction 3: fill "no", except the patients who have NA in all three medications:
dat$missing_all_meds <- as.factor(ifelse(is.na(dat$DTICL_corrected) & is.na(dat$DTICGR_corrected) & is.na(dat$DPRAS_corrected), "missing", "not missing"))
dat$DTICL_corrected[is.na(dat$DTICL_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$DTICL_corrected[is.na(dat$DTICL_corrected) & dat$missing_all_meds=="missing"] <- NA
dat$DTICGR_corrected[is.na(dat$DTICGR_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$DTICGR_corrected[is.na(dat$DTICGR_corrected) & dat$missing_all_meds=="missing"] <- NA
dat$DPRAS_corrected[is.na(dat$DPRAS_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$DPRAS_corrected[is.na(dat$DPRAS_corrected) & dat$missing_all_meds=="missing"] <- NA

#added by Tal in version 7 of the PDF:
dat$DP2Y12_corrected <- as.factor(ifelse(dat$missing_all_meds=="missing", NA,
                                         ifelse(dat$DTICL_corrected=="YES"|
                                          dat$DTICGR_corrected=="YES"|
                                           dat$DPRAS_corrected=="YES",
                                         "YES", "NO")))

# dat$DP2Y12_corrected <- dat$DP2Y12 #silenced by Tal in version 7 of the PDF
# dat[is.na(dat$DPRAS_corrected) & is.na(dat$DTICGR_corrected) & (is.na(dat$DTICL_corrected)| dat$DTICL_corrected=="NO"), "DP2Y12_corrected"] <- NA #this line is from version 6

#P2Y12 in hosp corrections
# correction 1: no patients with more than 1 medication
dat$HTICL_corrected <- dat$HTICL
dat[(dat$HTICL=="YES" & !is.na(dat$HTICL)) & ((dat$HPRAS=="YES" & !is.na(dat$HPRAS)) | (dat$HTICGR=="YES" & !is.na(dat$HTICGR))), "HTICL_corrected"] <- "NO"

dat$HPRAS_corrected <- dat$HPRAS
dat$HTICGR_corrected <- dat$HTICGR
dat[(dat$HPRAS=="YES" & !is.na(dat$HPRAS)) & (dat$HTICGR=="YES" & !is.na(dat$HTICGR)), c("HPRAS_corrected","HTICGR_corrected")] <- NA

#correction 2: if "yes" in one medication, then "no" in the others: (added by Tal in version 7 of the PDF)
dat$HTICL_corrected[is.na(dat$HTICL_corrected) & (dat$HPRAS_corrected=="YES" | dat$HTICGR_corrected=="YES")] <- "NO"
dat$HTICGR_corrected[is.na(dat$HTICGR_corrected) & (dat$HPRAS_corrected=="YES" | dat$HTICL_corrected=="YES")] <- "NO"
dat$HPRAS_corrected[is.na(dat$HPRAS_corrected) & (dat$HTICL_corrected=="YES" | dat$HTICGR_corrected=="YES")] <- "NO"

#correction 3: fill "no", except the patients who have NA in all three medications:
dat$missing_all_meds <- as.factor(ifelse(is.na(dat$HTICL_corrected) & is.na(dat$HTICGR_corrected) & is.na(dat$HPRAS_corrected), "missing", "not missing"))
dat$HTICL_corrected[is.na(dat$HTICL_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$HTICL_corrected[is.na(dat$HTICL_corrected) & dat$missing_all_meds=="missing"] <- NA
dat$HTICGR_corrected[is.na(dat$HTICGR_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$HTICGR_corrected[is.na(dat$HTICGR_corrected) & dat$missing_all_meds=="missing"] <- NA
dat$HPRAS_corrected[is.na(dat$HPRAS_corrected) & dat$missing_all_meds=="not missing"] <- "NO"
dat$HPRAS_corrected[is.na(dat$HPRAS_corrected) & dat$missing_all_meds=="missing"] <- NA

#added by Tal in version 7 of the PDF:
dat$HP2Y12_corrected <- as.factor(ifelse(dat$missing_all_meds=="missing", NA,
                                         ifelse(dat$HTICL_corrected=="YES"|
                                                  dat$HTICGR_corrected=="YES"|
                                                  dat$HPRAS_corrected=="YES",
                                                "YES", "NO")))


# The next section was silenced till version 6 (include 6) of the PDF
#========#
# #Clopidogrel -> Tigacrelor -> Prasugrel
# dat$HTICGR[dat$HTICGR=="YES" & dat$HTICL=="YES"] <- "NO"
# dat$HPRAS[dat$HPRAS=="YES" & (dat$HTICGR=="YES" | dat$HTICL=="YES")] <- "NO"
# 
# dat$DTICGR[dat$DTICGR=="YES" & dat$DTICL=="YES"] <- "NO"
# dat$DPRAS[dat$DPRAS=="YES" & (dat$DTICGR=="YES" | dat$HTICL=="YES")] <- "NO"
# 
# 
# dat$hp2y12 <- as.factor(ifelse(dat$HTICL=="YES" | dat$HPRAS=="YES" | dat$HTICGR=="YES", "YES", "NO"))
# dat$HP2Y12 <- as.factor(ifelse((is.na(dat$hp2y12) & dat$HTICL=="NO") | dat$hp2y12=="NO", "NO", "YES"))
# 
# dat$dp2y12 <- as.factor(ifelse(dat$DTICL=="YES" | dat$DPRAS=="YES" | dat$DTICGR=="YES", "YES", "NO"))
# dat$DP2Y12 <- as.factor(ifelse((is.na(dat$dp2y12) & dat$DTICL=="NO") | dat$dp2y12=="NO", "NO", "YES"))
# 
# #Completing "YES" values for P2Y12
# dat$HP2Y12[dat$year==2016 & dat$DP2Y12=="YES"] <- "YES" #completing in-hospital by discharge
# dat$DP2Y12[dat$year==2016 & dat$HP2Y12=="YES"] <- "YES" #completing discharge by in-hospital
# dat$DP2Y12[dat$year==2013 & is.na(dat$HP2Y12) & (dat$HTICGR=="YES"| dat$HPRAS=="YES")] <- "YES"
#========#

dat$EF10 <- factor(ifelse(!is.na(dat$EF_CLASS),"YES","NO"))
dat$HECHO <- factor(ifelse((is.na(dat$HECHO) | dat$HECHO=="NO") & dat$EF10=="YES" | dat$HECHO=="YES", "YES", "NO"))

levels(dat$HANGIO) <- list("NO"=c("NO","No"), "YES"=c("YES","Yes"))

#19.1.22: define groups with disch diag, and if missing then take from admission diag:
dat$group <- factor(ifelse(is.na(dat$DISDINEW) & dat$ARR_DIAG=="STE MI","STE-ACS",
                           ifelse(is.na(dat$DISDINEW) & dat$ARR_DIAG!="STE MI","NSTE-ACS",
                                  ifelse(is.na(dat$DISDINEW) & is.na(dat$ARR_DIAG) ,NA,
                                         ifelse(dat$DISDINEW  %in% c("STE MI"),"STE-ACS","NSTE-ACS")))))


#20.03.22 (version 9 of the booklet):
#Beigel said that if the patient has REP==YES and REPT=NA so make REPT=PRIMARY PCI:
# dat$REPT <- as.factor(ifelse(is.na(dat$REPT) & dat$REP=="YES", "PRIMARY PCI", as.character(dat$REPT)))
# 
# #Anticoagulants: read files of previous years because it is not yet in the unified acsis:
# dat_10 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/previous years acsis files/REG10_8.csv",
#                  header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"),
#                  stringsAsFactors = T)
# dat_10$SOURCE <- c("S2010")
# dat_10 <- dat_10[,c("HAKZAA","SOURCE", "S10H2", "S10D2" #warfarin
#                     )]
# 
# dat_13 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/previous years acsis files/REG13_4_save_utf.csv",
#                    header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"),
#                    stringsAsFactors = T)
# dat_13$SOURCE <- c("S2013")
# dat_13 <- dat_13[,c("HAKZAA","SOURCE", "S13H5", "S13D5" #warfarin
#                     ,"S13H10", "S13D10" #dabigatran
#                     ,"S13H11", "S13D11" #rivaroxaban
# 
# )]
# 
# dat_16 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/previous years acsis files/REG16_5_save_utf.csv",
#                    header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"),
#                    stringsAsFactors = T)
# dat_16$SOURCE <- c("S2016")
# dat_16 <- dat_16[,c("HAKZAA","SOURCE", "S16H6", "S16D6" #warfarin
#                     ,"S16H7", "S16D7" #dabigatran
#                     ,"S16H8","S16D8" #rivaroxaban
#                     ,"S16H9" ,"S16D9" #apixaban
# 
# )]
# 
# dat_18 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/previous years acsis files/ACSIS18_5_save_utf.csv",
#                    header = TRUE, na.strings = c(""," ","NA","UNKNOWN","UNK","N/A","A","N"),
#                    stringsAsFactors = T)
# dat_18$SOURCE <- c("S2018")
# dat_18 <- dat_18[,c("HAKZAA","SOURCE", "S18H6", "S18D6" #warfarin
#                     ,"S18H7","S18D7" #dabigatran
#                     ,"S18H8" ,"S18D8" #rivaroxaban
#                     ,"S18H9" ,"S18D9" #apixaban
# )]
# 
# dat_21 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/ACSIS21_3_save_utf_updated.csv", header = TRUE,
#                  na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), stringsAsFactors = T) #n=1851
# dat_21$SOURCE <- c("S2021")
# dat_21 <- dat_21[,c("HAKZAA","SOURCE", "S21H6", "S21D6" #warfarin
#                     ,"S21H7" ,"S21D7" #dabigatran
#                     ,"S21H8","S21D8" #rivaroxaban
#                     ,"S21H9","S21D9" #apixaban
# )]
# 
# dat_24 <- read.csv("C:/Users/talco/Documents/Statistics/ACSIS_24_PDF/ACSIS24_3_save_utf_updated.csv", header = TRUE,
#                    na.strings = c(""," ","NA","UNKNoWN","UNK","N/A","nan"), stringsAsFactors = T) #n=1851
# dat_24$SOURCE <- c("S2024")
# dat_21 <- dat_21[,c("HAKZAA","SOURCE", "S24H6", "S24D6" #warfarin
#                     ,"S24H7" ,"S24D7" #dabigatran
#                     ,"S24H8","S24D8" #rivaroxaban
#                     ,"S24H9","S24D9" #apixaban
# )]
# #merge all to the unified acsis:
# dat <- merge(dat, dat_10, by = c("HAKZAA","SOURCE"), all.x = T)
# dat <- merge(dat, dat_13, by = c("HAKZAA","SOURCE"), all.x = T)
# dat <- merge(dat, dat_16, by = c("HAKZAA","SOURCE"), all.x = T)
# dat <- merge(dat, dat_18, by = c("HAKZAA","SOURCE"), all.x = T)
# dat <- merge(dat, dat_21, by = c("HAKZAA","SOURCE"), all.x = T)
# dat <- merge(dat, dat_24, by = c("HAKZAA","SOURCE"), all.x = T)



# anticoag - hosp and disch: (Roy asked)
# dat$oral_anticoag_hosp <- factor(ifelse(dat$HLMW%in%"YES" |
#                                         dat$S10H2 %in% "YES" |
#                             dat$S13H5 %in% "YES" | dat$S13H10 %in% "YES" |  dat$S13H11 %in% "YES" |
#                             dat$S16H6 %in% "YES" | dat$S16H7 %in% "YES" |  dat$S16H8 %in% "YES" |  dat$S16H9 %in% "YES" |
#                             dat$S18H6 %in% "Yes" | dat$S18H7 %in% "Yes" |  dat$S18H8 %in% "Yes" |  dat$S18H9 %in% "Yes" |
#                             dat$S21H6 %in% "YES" | dat$S21H7 %in% "YES" |  dat$S21H8 %in% "YES" |  dat$S21H9 %in% "YES" |
#                             dat$S24H6 %in% "YES" | dat$S24H7 %in% "YES" |  dat$S24H8 %in% "YES" |  dat$S24H9 %in% "YES" ,
#                             "YES",
#                             ifelse((dat$SOURCE=="S2010" & is.na(dat$S10H2)) |
#             (dat$SOURCE=="S2013" & is.na(dat$S13H5)  & is.na(dat$S13H10) & is.na(dat$S13H11)) |
#               (dat$SOURCE=="S2016" & is.na(dat$S16H6)  & is.na(dat$S16H7) & is.na(dat$S16H8) & is.na(dat$S16H9)) |
#               (dat$SOURCE=="S2018" & is.na(dat$S18H6)  & is.na(dat$S18H7) & is.na(dat$S18H8) & is.na(dat$S18H9)) |
#               (dat$SOURCE=="S2021" & is.na(dat$S21H6)  & is.na(dat$S21H7) & is.na(dat$S21H8) & is.na(dat$S21H9)) |
#               (dat$SOURCE=="S2024" & is.na(dat$S24H6)  & is.na(dat$S24H7) & is.na(dat$S24H8) & is.na(dat$S24H9)) ,
#             NA, "NO")))
# #many missing in 2018 - turn to "no":
# dat$oral_anticoag_hosp <- factor(ifelse(is.na(dat$oral_anticoag_hosp), "NO", as.character(dat$oral_anticoag_hosp)))
# 
# #
# dat$oral_anticoag_disch <- factor(ifelse(dat$DLMW%in%"YES" |
#                                           dat$S10D2 %in% "YES" |
#                                           dat$S13D5 %in% "YES" | dat$S13D10 %in% "YES" |  dat$S13D11 %in% "YES" |
#                                           dat$S16D6 %in% "YES" | dat$S16D7 %in% "YES" |  dat$S16D8 %in% "YES" |  dat$S16D9 %in% "YES" |
#                                           dat$S18D6 %in% "Yes" | dat$S18D7 %in% "Yes" |  dat$S18D8 %in% "Yes" |  dat$S18D9 %in% "Yes" |
#                                           dat$S21D6 %in% "YES" | dat$S21D7 %in% "YES" |  dat$S21D8 %in% "YES" |  dat$S21D9 %in% "YES" ,
#                                           dat$S24D6 %in% "YES" | dat$S24D7 %in% "YES" |  dat$S24D8 %in% "YES" |  dat$S24D9 %in% "YES" ,
#                                          "YES",
#                                         ifelse((dat$SOURCE=="S2010" & is.na(dat$S10D2)) |
#                                                  (dat$SOURCE=="S2013" & is.na(dat$S13D5)  & is.na(dat$S13D10) & is.na(dat$S13D11)) |
#                                                  (dat$SOURCE=="S2016" & is.na(dat$S16D6)  & is.na(dat$S16D7) & is.na(dat$S16D8) & is.na(dat$S16D9)) |
#                                                  (dat$SOURCE=="S2018" & is.na(dat$S18D6)  & is.na(dat$S18D7) & is.na(dat$S18D8) & is.na(dat$S18D9)) |
#                                                  (dat$SOURCE=="S2021" & is.na(dat$S21D6)  & is.na(dat$S21D7) & is.na(dat$S21D8) & is.na(dat$S21D9)) ,
#                                                  (dat$SOURCE=="S2024" & is.na(dat$S24D6)  & is.na(dat$S24D7) & is.na(dat$S24D8) & is.na(dat$S24D9)) ,
#                                                NA, "NO")))
# #many missing in 2018 - turn to "no":
# dat$oral_anticoag_disch <- factor(ifelse(is.na(dat$oral_anticoag_disch), "NO", as.character(dat$oral_anticoag_disch)))



dat_10_24 <- dat
