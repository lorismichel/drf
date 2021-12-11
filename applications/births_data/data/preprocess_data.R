# birth data preprocessing

# what you need to know:
# this preprocessing is ONLY able to read data from at least 2005 to 2018
# as soon as the format of the explanation file change then the pre-processing
# is not valid anymore
#
# you need to download on https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
# the pdf file to understand the variables and the associated US or territory file
#
#

cntr=1
# function to parse one observation
readObservation <- function(l) {
  print(cntr)
  assign("cntr", cntr+1, envir = .GlobalEnv)
  # DOB_YY <- substr(l, start = 9, stop = 12)
  # DOB_MM <- substr(l, start = 13, stop = 14)
  # DOB_TT <- substr(l, start = 19, stop = 22)
  # DOB_WK <- substr(l, start = 23, stop = 23)
  # BFACIL <- substr(l, start = 32, stop = 32)
  # F_FACILITY <- substr(l, start = 33, stop = 33)
  # BFACIL3 <- substr(l, start = 50, stop = 50)
  # MAGE_IMPFLG <- substr(l, start = 73, stop = 73)
  # MAGE_REPFLG <- substr(l, start = 74, stop = 74)
  # MAGER <- substr(l, start = 75, stop = 76)
  # MAGER14 <- substr(l, start = 77, stop = 78)
  # MAGER9 <- substr(l, start = 79, stop = 79)
  # MBSTATE_REC <- substr(l, start = 85, stop = 103)
  # RESTATUS <- substr(l, start = 104, stop = 104)
  # 
  # MRACE31 <- substr(l, start = 105, stop = 106)
  # MRACE6 <- substr(l, start = 107, stop = 107)
  # MRACE15 <- substr(l, start = 108, stop = 109)
  # MRACE <- substr(l, start = 110, stop = 110)
  # MRACEIMP <- substr(l, start = 111, stop = 111)
  # 
  # MHISPX <- substr(l, start = 112, stop = 112)
  # MHISP_R <- substr(l, start = 115, stop = 115)
  # F_MHISP <- substr(l, start = 116, stop = 116)
  # MRACEHISP <- substr(l, start = 117, stop = 117)
  # 
  # MAR_P <- substr(l, start = 119, stop = 119)
  # DMAR <- substr(l, start = 120, stop = 120)
  # MAR_IMP <- substr(l, start = 121, stop = 121)
  # F_MAR_P <- substr(l, start = 123, stop = 123)
  # MEDUC <- substr(l, start = 124, stop = 124)
  # F_MEDUC <- substr(l, start = 126, stop = 126)
  # 
  # FAGERPT_FLG <- substr(l, start = 142, stop = 142)
  # FAGECOMB <- substr(l, start = 147, stop = 148)
  # FAGEREC11 <- substr(l, start = 149, stop = 150)
  # FRACE31 <- substr(l, start = 151, stop = 152)
  # FRACE6 <- substr(l, start = 153, stop = 153)
  # FRACE15 <- substr(l, start = 154, stop = 155)
  # FHISPX <- substr(l, start = 159, stop = 159)
  # FHISP_R <- substr(l, start = 160, stop = 160)
  # F_FHISP <- substr(l, start = 161, stop = 161)
  # FRACEHISP <- substr(l, start = 161, stop = 161)
  # FEDUC <- substr(l, start = 163, stop = 163)
  # f_FEDUC <- substr(l, start = 165, stop = 165)
  # 
  # ##
  # LBO_REC <- substr(l, start = 179, stop = 179)
  # ILLB_R <- substr(l, start = 198, stop = 200)
  # ##
  # 
  # PRECARE <- substr(l, start = 224, stop = 225)
  # F_MPCB <- substr(l, start = 226, stop = 226)
  # PRECARE5 <- substr(l, start = 227, stop = 227)
  # PREVIS <- substr(l, start = 238, stop = 239)
  # PREVIS_REC <- substr(l, start = 242, stop = 243)
  # F_TPCV <- substr(l, start = 244, stop = 244)
  # 
  # # cigarettes
  # WIC <- substr(l, start = 251, stop = 251)
  # F_WIC <- substr(l, start = 252, stop = 252)
  # CIG_0 <- substr(l, start = 253, stop = 254)
  # CIG_1 <- substr(l, start = 255, stop = 256)
  # CIG_2 <- substr(l, start = 257, stop = 258)
  # CIG_3 <- substr(l, start = 259, stop = 260)
  # CIG0_R <- substr(l, start = 261, stop = 261)
  # CIG1_R <- substr(l, start = 262, stop = 262)
  # CIG2_R <- substr(l, start = 263, stop = 263)
  # CIG3_R <- substr(l, start = 264, stop = 264)
  # F_CIGS_0 <- substr(l, start = 265, stop = 265)
  # F_CIGS_1 <- substr(l, start = 266, stop = 266)
  # F_CIGS_2 <- substr(l, start = 267, stop = 267)
  # F_CIGS_3 <- substr(l, start = 268, stop = 268)
  # CIG_REC <-substr(l, start = 269, stop = 269)
  # F_TOBACO <- substr(l, start = 270, stop = 270)
  # 
  # M_Ht_In <- substr(l, start = 280, stop = 281)
  # F_M_HT <- substr(l, start = 282, stop = 282)
  # BMI <- substr(l, start = 283, stop = 286)
  # BMI_R <- substr(l, start = 287, stop = 287)
  # PWgt_R <- substr(l, start = 292, stop = 294)
  # F_PWGT <- substr(l, start = 295, stop = 295)
  # DWgt_R <- substr(l, start = 296, stop = 298)
  # F_DWGT <- substr(l, start = 303, stop = 303)
  # WTGAIN <- substr(l, start = 304, stop = 305)
  # WTGAIN_REC <- substr(l, start = 306, stop = 306)
  # F_WTGAIN <- substr(l, start = 307, stop = 307)
  # 
  # RF_PDIAB <- substr(l, start = 313, stop = 313)
  # RF_GDIAB <- substr(l, start = 314, stop = 314)
  # RF_PHYPE <- substr(l, start = 315, stop = 314)
  # RF_GHYPE <- substr(l, start = 316, stop = 316)
  # RF_EHYPE <- substr(l, start = 317, stop = 317)
  # RF_PPTERM <- substr(l, start = 318, stop = 318)
  # F_RF_PDIAB <- substr(l, start = 319, stop = 319)
  # F_RF_GDIAB <- substr(l, start = 320, stop = 320)
  # F_RF_PHYPER <- substr(l, start = 321, stop = 321)
  # F_RF_GHYPER <- substr(l, start = 322, stop = 322)
  # F_RF_ECLAMP <- substr(l, start = 323, stop = 323)
  # F_RF_PPB <- substr(l, start = 324, stop = 324)
  # RF_INFTR <- substr(l, start = 325, stop = 325)
  # RF_FEDRG <- substr(l, start = 326, stop = 326)
  # RF_ARTEC <- substr(l, start = 327, stop = 327)
  # f_RF_INFT <- substr(l, start = 328, stop = 328)
  # F_RF_INF_DRG <- substr(l, start = 329, stop = 329)
  # F_RF_INF_ART <- substr(l, start = 330, stop = 330)
  # RF_CESAR <- substr(l, start = 331, stop = 331)
  # RF_CESARN <- substr(l, start = 332, stop = 333)
  # F_RF_CESAR <- substr(l, start = 335, stop = 335)
  # F_RF_NCESAR <- substr(l, start = 336, stop = 336)
  # NO_RISKS  <- substr(l, start = 337, stop = 337)
  # IP_GON <- substr(l, start = 343, stop = 343)
  # IP_SYPH <- substr(l, start = 344, stop = 344)
  # IP_CHLAM <- substr(l, start = 345, stop = 345)
  # IP_HEPB <- substr(l, start = 346, stop = 346)
  # IP_HEPC <- substr(l, start = 347, stop = 347)
  # F_IP_GONOR <- substr(l, start = 348, stop = 348)
  # F_IP_SYPH <- substr(l, start = 349, stop = 349)
  # F_IP_CHLAM <- substr(l, start = 350, stop = 350)
  # F_IP_HEPATB <- substr(l, start = 351, stop = 351)
  # F_IP_HEPATC <- substr(l, start = 352, stop = 352)
  # NO_INFEC <- substr(l, start = 353, stop = 353)
  # 
  # OB_ECVS <- substr(l, start = 360, stop = 360)
  # OB_ECVF <- substr(l, start = 361, stop = 361)
  # F_OB_SUCC <- substr(l, start = 363, stop = 363)
  # F_OB_FAIL <- substr(l, start = 364, stop = 364)
  # 
  # LD_INDL <- substr(l, start = 383, stop = 383)
  # LD_AUGM <- substr(l, start = 384, stop = 384)
  # LD_STER <- substr(l, start = 385, stop = 385)
  # LD_ANTB <- substr(l, start = 386, stop = 386)
  # LD_CHOR <- substr(l, start = 387, stop = 387)
  # LD_ANES <- substr(l, start = 388, stop = 388)
  # F_LD_INDL <- substr(l, start = 389, stop = 389)
  # F_LD_AUGM <- substr(l, start = 390, stop = 390)
  # F_LD_STER <- substr(l, start = 391, stop = 391)
  # F_LD_ANTB <- substr(l, start = 392, stop = 392)
  # F_LD_CHOR <- substr(l, start = 393, stop = 393)
  # F_LD_ANES <- substr(l, start = 394, stop = 394)
  # NO_LBRDLV <- substr(l, start = 395, stop = 395)
  # 
  # ME_PRES <- substr(l, start = 401, stop = 401)
  # ME_ROUT <- substr(l, start = 402, stop = 402)
  # ME_TRIAL <- substr(l, start = 403, stop = 403)
  # F_ME_PRES <- substr(l, start = 404, stop = 404)
  # F_ME_ROUT <- substr(l, start = 405, stop = 405)
  # F_ME_TRIAL <- substr(l, start = 406, stop = 406)
  # RDMETH_REC <- substr(l, start = 407, stop = 407)
  # DMETH_REC <- substr(l, start = 408, stop = 408)
  # F_DMETH_REC <- substr(l, start = 409, stop = 409)
  # 
  # MM_MTR <- substr(l, start = 415, stop = 415)
  # MM_PLAC <- substr(l, start = 416, stop = 416)
  # MM_RUPT <- substr(l, start = 417, stop = 418)
  # MM_UHYST <- substr(l, start = 418, stop = 418)
  # MM_AICU <- substr(l, start = 419, stop = 419)
  # F_MM_MTR <- substr(l, start = 421, stop = 421)
  # F_MM_PLAC <- substr(l, start = 422, stop = 422)
  # F_MM_RUP <- substr(l, start = 423, stop = 423)
  # F_MM_UHYST <- substr(l, start = 424, stop = 424)
  # F_MM_AICU <- substr(l, start = 425, stop = 425)
  # NO_MMORB <- substr(l, start = 427, stop = 427)
  # 
  # ATTEND <- substr(l, start = 433, stop = 433)
  # MTRAN <- substr(l, start = 434, stop = 434)
  # PAY <- substr(l, start = 435, stop = 435)
  # PAY_REC <- substr(l, start = 436, stop = 436)
  # F_PAY <- substr(l, start = 437, stop = 437)
  # F_PAY_REC <- substr(l, start = 438, stop = 438)
  # 
  # APGAR5 <- substr(l, start = 444, stop = 445)
  # APGAR5R <- substr(l, start = 446, stop = 446)
  # F_APGAR5 <- substr(l, start = 447, stop = 447)
  # APGAR10 <- substr(l, start = 448, stop = 449)
  # APGAR10R <- substr(l, start = 450, stop = 450)
  # DPLURAL <- substr(l, start = 454, stop = 454)
  # IMP_PLUR <- substr(l, start = 456, stop = 456)
  # SETORDER_R <- substr(l, start = 459, stop = 459)
  # 
  # SEX <- substr(l, start = 475, stop = 475)
  # IMP_SEX <- substr(l, start = 476, stop = 476)
  # DLMP_MM <- substr(l, start = 477, stop = 478)
  # DLMP_YY <- substr(l, start = 481, stop = 484)
  # COMPGST_IMP <- substr(l, start = 488, stop = 488)
  # OBGEST_FLG <- substr(l, start = 489, stop = 489)
  # COMBGEST <- substr(l, start = 490, stop = 491)
  # GESTREC10 <- substr(l, start = 492, stop = 493)
  # GESTREC3 <- substr(l, start = 494, stop = 494)
  # LMPUSED <- substr(l, start = 498, stop = 498)
  # OEGest_Comb <- substr(l, start = 499, stop = 500)
  # OEGest_R10 <- substr(l, start = 501, stop = 502)
  # OEGest_R3 <- substr(l, start = 503, stop = 503)
  # 
  # DBWT <- substr(l, start = 504, stop = 507)
  # BWTR12 <- substr(l, start = 509, stop = 510)
  # BWTR4 <- substr(l, start = 511, stop = 511)
  # 
  # AB_AVEN1 <- substr(l, start = 517, stop = 517)
  # AB_AVEN6 <- substr(l, start = 519, stop = 519)
  # AB_NICU <- substr(l, start = 520, stop = 520)
  # AB_SURF <- substr(l, start = 521, stop = 521)
  # AB_ANTI <- substr(l, start = 522, stop = 522)
  # AB_SEIZ <- substr(l, start = 523, stop = 523)
  # F_AB_VENT <- substr(l, start = 524, stop = 524)
  # F_AB_VENT6 <- substr(l, start = 525, stop = 525)
  # F_AB_NIUC <- substr(l, start = 526, stop = 526)
  # F_AB_SURFAC <- substr(l, start = 527, stop = 527)
  # F_AB_ANTIBIO <- substr(l, start = 528, stop = 528)
  # F_AB_SEIZ <- substr(l, start = 529, stop = 529)
  # NO_ABNORM <- substr(l, start = 531, stop = 531)
  # 
  # CA_ANEN <- substr(l, start = 537, stop = 537)
  # CA_MNSB <- substr(l, start = 538, stop = 538)
  # CA_CCHD <- substr(l, start = 539, stop = 539)
  # CA_CDH <- substr(l, start = 540, stop = 540)
  # CA_OMPH <- substr(l, start = 541, stop = 541)
  # CA_GAST <- substr(l, start = 542, stop = 542)
  # F_CA_ANEN <- substr(l, start = 543, stop = 543)
  # F_CA_MENIN <- substr(l, start = 544, stop = 544)
  # F_CA_HEART <- substr(l, start = 545, stop = 545)
  # F_CA_HERNIA <- substr(l, start = 546, stop = 546)
  # F_CA_OMPHA <- substr(l, start = 547, stop = 547)
  # F_CA_GASTRO <- substr(l, start = 548, stop = 548)
  # CA_LIMB <- substr(l, start = 549, stop = 549)
  # CA_CLEFT <- substr(l, start = 550, stop = 550)
  # CA_CLPAL <- substr(l, start = 551, stop = 551)
  # CA_DOWN <- substr(l, start = 552, stop = 552)
  # CA_DISOR <- substr(l, start = 553, stop = 553)
  # CA_HYPO <- substr(l, start = 554, stop = 554)
  # F_CA_LIMB <- substr(l, start = 555, stop = 555)
  # F_CA_CLEFTLP <- substr(l, start = 556, stop = 556)
  # F_CA_CLEFT <- substr(l, start = 557, stop = 557)
  # F_CA_DOWNS <- substr(l, start = 558, stop = 558)
  # F_CA_CHROM <- substr(l, start = 559, stop = 559)
  # F_CA_HYPOS <- substr(l, start = 560, stop = 560)
  # NO_CONGEN <- substr(l, start = 561, stop = 561)
  # 
  # ITRAN <- substr(l, start = 567, stop = 567)
  # ILIVE <- substr(l, start = 568, stop = 568)
  # BFED <- substr(l, start = 569, stop = 569)
  # F_BFED <- substr(l, start = 570, stop = 570)
  # 
  # # terr
  # OCTERR <- substr(l, start = 24, stop = 25)
  # OCNTYFIPS <- substr(l, start = 28, stop = 30)
  # OCNTYPOP <- substr(l, start = 31, stop = 31)
  # MBCNTRY <- substr(l, start = 80, stop = 81)
  # MRCNTRY <- substr(l, start = 85, stop = 86)
  # MRTERR <- substr(l, start = 89, stop = 90)
  # RCNTY <- substr(l, start = 91, stop = 93)
  # RCNTY_POP <- substr(l, start = 990, stop = 99)
  # RCITY_POP <- substr(l, start = 100, stop = 100)
  # RECTYPE <- substr(l, start = 103, stop = 103)
  
  ret = c()
  names = c()
  
  #mother
  MAGER <- substr(l, start = 75, stop = 76)
  ret = c(ret, MAGER) # mother's age, number 12-50
  names = c(names, 'age_mother')
  
  MRACE6 <- substr(l, start = 107, stop = 107)
  ret = c(ret, MRACE6) # mother's race
  names = c(names, 'race_mother')
  
  M_Ht_In <- substr(l, start = 280, stop = 281)
  ret = c(ret, M_Ht_In) # mother's height in inches, number
  names = c(names, 'height_mother')
  
  PWgt_R <- substr(l, start = 292, stop = 294)
  ret = c(ret, PWgt_R) # mother's weight before pregnancy in pounds
  names = c(names, 'weight_mother') 
  
  BMI <- substr(l, start = 283, stop = 286)
  ret = c(ret, BMI) # mother's BMI pre-pregnancy
  names = c(names, 'BMI_mother')
  
  WTGAIN <- substr(l, start = 304, stop = 305)
  ret = c(ret, WTGAIN) # mother's weight gain in pregnancy 
  names = c(names, 'weight_gain_mother')
  
  DMAR <- substr(l, start = 120, stop = 120)
  ret = c(ret, DMAR) # marital status 1 married, 2 unmarried 
  names = c(names, 'marital_status_mother')
  
  MEDUC <- substr(l, start = 124, stop = 124)
  ret = c(ret, MEDUC) # mother's education 
  names = c(names, 'education_mother')
  
  #father
  FAGECOMB <- substr(l, start = 147, stop = 148)
  ret = c(ret, FAGECOMB) # father's age, number
  names = c(names, 'age_father')
  
  FRACE6 <- substr(l, start = 153, stop = 153)
  ret = c(ret, FRACE6) # father's race
  names = c(names, 'race_father')
  
  FEDUC <- substr(l, start = 163, stop = 163)
  ret = c(ret, FEDUC) # father's education 
  names = c(names, 'education_father')
  
  #birth info
  DOB_YY <- substr(l, start = 9, stop = 12)
  ret = c(ret, DOB_YY) # year of birth
  names = c(names, 'birth_year')
  
  DOB_MM <- substr(l, start = 13, stop = 14)
  ret = c(ret, DOB_MM) # month of birth
  names = c(names, 'birth_month')
  
  DPLURAL <- substr(l, start = 454, stop = 454)
  ret = c(ret, DPLURAL) # how many babies born at once
  names = c(names, 'plurality')
  
  PRECARE <- substr(l, start = 224, stop = 225)
  ret = c(ret, PRECARE) # month of pregnancy when precare started, 0 for no precare
  names = c(names, 'precare_started')
  
  OEGest_Comb <- substr(l, start = 499, stop = 500)
  ret = c(ret, OEGest_Comb) # Obstretic estimate of the pregnancy duration
  names = c(names, 'pregnancy_duration')
  
  DMETH_REC <- substr(l, start = 408, stop = 408)
  ret = c(ret, DMETH_REC) # method of delivery: 1) vaginal 2) C-section 99) unknown
  names = c(names, 'delivery_method')
  
  #history
  LBO_REC <- substr(l, start = 179, stop = 179)
  ret = c(ret, LBO_REC) # live birth order
  names = c(names, 'birth_order')
  
  ILLB_R <- substr(l, start = 198, stop = 200)
  ret = c(ret, ILLB_R) # number of months since last live birth, 0-3 for plural deliveries
  names = c(names, 'birth_interval')
  
  CIG_0 <- substr(l, start = 253, stop = 254)
  ret = c(ret, CIG_0) # number of cigarettes per day before pregnancy
  names = c(names, 'cigarettes_before_pregnancy')
  
  CIG_1 <- as.numeric(substr(l, start = 255, stop = 256))
  CIG_1 = ifelse(CIG_1!=99, CIG_1, NA)
  CIG_2 <- as.numeric(substr(l, start = 257, stop = 258))
  CIG_2 = ifelse(CIG_2!=99, CIG_2, NA)
  CIG_3 <- as.numeric(substr(l, start = 259, stop = 260))
  CIG_3 = ifelse(CIG_3!=99, CIG_3, NA)
  ret = c(ret, mean(c(CIG_1, CIG_2, CIG_3), na.rm=TRUE)) # average number of cigarettes per day during pregnancy
  names = c(names, 'cigarettes_during_pregnancy')
  
  #baby
  SEX <- substr(l, start = 475, stop = 475)
  ret = c(ret, SEX) # gender of the baby
  names = c(names, 'gender')
  
  DBWT <- substr(l, start = 504, stop = 507)
  ret = c(ret, DBWT) # birthweight of baby in grams
  names = c(names, 'birthweight')
  
  APGAR5 <- substr(l, start = 444, stop = 445)
  ret = c(ret, APGAR5) # five minutes APGAR score
  names = c(names, 'apgar_5min')
  
  APGAR10 <- substr(l, start = 448, stop = 449)
  ret = c(ret, APGAR10) # ten minutes APGAR score
  names = c(names, 'apgar_10min')
  
  NO_ABNORM <- substr(l, start = 531, stop = 531)
  ret = c(ret, NO_ABNORM) # abnormal conditions of the newborn
  names = c(names, 'abnormal_conditions')
  
  NO_CONGEN <-substr(l, start = 561, stop = 561)
  ret = c(ret, NO_CONGEN) # congenital anomalies of the newborn
  names = c(names, 'congenital_anomalies')
  
  
  names(ret) = names
  return(as.data.frame.list(ret))
}

# read line by line saved as a list
# n specifies the number of rows to read
l <- readLines(con = "../../data/birth_data/downloaded_data/heterogeneity/birth_data/Nat2018PublicUS.c20190509.r20190717.txt")
l = l[sample(1:length(l), 300000, replace=FALSE)]
l = as.list(l)

# preprocess the data
# read each line independently
d <- lapply(l, function(ll) readObservation(ll))
# create data frame
library(data.table)
df = rbindlist(d)

dim(df)

#mother
df$age_mother = as.numeric(levels(df$age_mother))[df$age_mother]

levels(df$race_mother)[levels(df$race_mother)=="1"] = "white"
levels(df$race_mother)[levels(df$race_mother)=="2"] = "black"
levels(df$race_mother)[levels(df$race_mother)=="3"] = "AIAN"
levels(df$race_mother)[levels(df$race_mother)=="4"] = "asian"
levels(df$race_mother)[levels(df$race_mother)=="5"] = "NHOPI"
levels(df$race_mother)[levels(df$race_mother)=="6"] = "mix"

df$height_mother = as.numeric(levels(df$height_mother))[df$height_mother]
df$height_mother[df$height_mother==99] = NA

df$weight_mother = as.numeric(levels(df$weight_mother))[df$weight_mother]
df$weight_mother[df$weight_mother==999] = NA

df$BMI_mother = as.numeric(levels(df$BMI_mother))[df$BMI_mother]
df$BMI_mother[df$BMI_mother==99.9] = NA

df$weight_gain_mother = as.numeric(levels(df$weight_gain_mother))[df$weight_gain_mother]
df$weight_gain_mother[df$weight_gain_mother==99] = NA

levels(df$marital_status_mother)[levels(df$marital_status_mother)=="1"] = "married"
levels(df$marital_status_mother)[levels(df$marital_status_mother)=="2"] = "unmarried"
levels(df$marital_status_mother)[levels(df$marital_status_mother)==" "] = NA

df$education_mother = as.numeric(levels(df$education_mother))[df$education_mother]
df$education_mother[df$education_mother==9] = NA

#father
df$age_father = as.numeric(levels(df$age_father))[df$age_father]
df$age_father[df$age_father==99] = NA


levels(df$race_father)[levels(df$race_father)=="1"] = "white"
levels(df$race_father)[levels(df$race_father)=="2"] = "black"
levels(df$race_father)[levels(df$race_father)=="3"] = "AIAN"
levels(df$race_father)[levels(df$race_father)=="4"] = "asian"
levels(df$race_father)[levels(df$race_father)=="5"] = "NHOPI"
levels(df$race_father)[levels(df$race_father)=="6"] = "mix"
levels(df$race_father)[levels(df$race_father)=="9"] = NA
df$race_father = factor(df$race_father)

df$education_father = as.numeric(levels(df$education_father))[df$education_father]
df$education_father[df$education_father==9] = NA

#birth info
df$birth_year = as.numeric(levels(df$birth_year))[df$birth_year]

#df$birth_month = as.numeric(levels(df$birth_month))[df$birth_month]

df$plurality = as.numeric(levels(df$plurality))[df$plurality]

df$precare_started = as.numeric(levels(df$precare_started))[df$precare_started]
df$precare_started[df$precare_started==0] = Inf
df$precare_started[df$precare_started==99] = NA

df$pregnancy_duration = as.numeric(levels(df$pregnancy_duration))[df$pregnancy_duration]
df$pregnancy_duration[df$pregnancy_duration==99] = NA

levels(df$delivery_method)[levels(df$delivery_method)=="1"] = "vaginal"
levels(df$delivery_method)[levels(df$delivery_method)=="2"] = "C-section"
levels(df$delivery_method)[levels(df$delivery_method)=="9"] = NA

#history
df$birth_order = as.numeric(levels(df$birth_order))[df$birth_order]
df$birth_order[df$birth_order==9] = NA

df$birth_interval = as.numeric(levels(df$birth_interval))[df$birth_interval]
df$birth_interval[df$birth_interval==888] = Inf
df$birth_interval[df$birth_interval==999] = NA

df$cigarettes_before_pregnancy = as.numeric(levels(df$cigarettes_before_pregnancy))[df$cigarettes_before_pregnancy]
df$cigarettes_before_pregnancy[df$cigarettes_before_pregnancy==99] = NA

df$cigarettes_during_pregnancy = as.numeric(levels(df$cigarettes_during_pregnancy))[df$cigarettes_during_pregnancy]
df$cigarettes_during_pregnancy[is.nan(df$cigarettes_during_pregnancy)] = NA

#baby
df$birthweight = as.numeric(levels(df$birthweight))[df$birthweight]
df$birthweight[df$birthweight==9999] = NA

df$apgar_5min = as.numeric(levels(df$apgar_5min))[df$apgar_5min]
df$apgar_5min[df$apgar_5min==99] = NA

df$apgar_10min = as.numeric(levels(df$apgar_10min))[df$apgar_10min]
df$apgar_10min[df$apgar_10min==88] = Inf
df$apgar_10min[df$apgar_10min==99] = NA

levels(df$abnormal_conditions)[levels(df$abnormal_conditions)=="1"] = "no"
levels(df$abnormal_conditions)[levels(df$abnormal_conditions)=="0"] = "yes"
levels(df$abnormal_conditions)[levels(df$abnormal_conditions)=="9"] = NA

levels(df$congenital_anomalies)[levels(df$congenital_anomalies)=="1"] = "no"
levels(df$congenital_anomalies)[levels(df$congenital_anomalies)=="0"] = "yes"
levels(df$congenital_anomalies)[levels(df$congenital_anomalies)=="0"] = NA

births = df
save(births, file="../../data/birth_data/computed_data/births.dat")

