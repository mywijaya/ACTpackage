#### FUNCTION 1: GENERATE ALL TABLES FOR TFL

# input:
# directory: your_wd --> Set the working directory to your specified path
# output_file: --> name of TFL word file document

#@export
func_tfl_act <- function(directory = your_wd, output_file = NULL, params = list()) {

# supress warning messages
result <- suppressMessages(suppressWarnings({

    ## packages
  library(tidyverse)
  library(gtsummary)
  library(forecast)
  library(huxtable)
  library(formatters)
  library(lubridate)
  library(reshape2)
  # Set the working directory to your specified path
  setwd(your_wd)


    # Path to the directory containing ADaM data files
    path.adam <- "./ADAM"

    # List of ADaM datasets with .csv extension in the specified directory
    list.adam <- list.files(path = path.adam, pattern = ".csv")
    print(list.adam)

    # Remove the .csv extension from the file names to create data frame names
    df.name <- gsub('.csv', '', list.adam)

    # Loop through each file and read it into a data frame
    for (i in 1:length(list.adam)) {
      # Construct the full file path
      file_path <- file.path(path.adam, list.adam[i])
      print(paste("Reading file:", file_path))  # Debugging line

      # Read the CSV file
      df.i <- read.csv(file_path)

      # Assign the data frame to a variable with the name derived from the file name
      assign(df.name[i], df.i)
    }

    ## read crf
    crf <- read.csv("./CRF/crf_signed.csv")

    ## function to calculate continuous stats
    cont_stat <- function(x,y) {
      tab<- data.frame(n = round_fmt(length(x),0),
                       Mean = round_fmt(mean(x),1),
                       SD = round_fmt(sd(x),2),
                       Median = round_fmt(median(x),1),
                       IQR = round_fmt(IQR(x),1),
                       Min = round_fmt(min(x),0),
                       Max = round_fmt(max(x),0))
      df <- data.frame(t(tab))
      df <- data.frame(label = rownames(df), stat_0 = df[,1])
      df2 <- rbind(data.frame(label = y, stat_0 = NA),df)
      return(df2)
    }

    ## function to calculate continuous stats v2:  add 1 decimal place for min and max
    cont_stat2 <- function(x,y) {
      tab<- data.frame(n = round_fmt(length(x),0),
                       Mean = round_fmt(mean(x),1),
                       SD = round_fmt(sd(x),2),
                       Median = round_fmt(median(x),1),
                       IQR = round_fmt(IQR(x),1),
                       Min = round_fmt(min(x),1),
                       Max = round_fmt(max(x),1))
      df <- data.frame(t(tab))
      df <- data.frame(label = rownames(df), stat_0 = df[,1])
      df2 <- rbind(data.frame(label = y, stat_0 = NA),df)
      return(df2)
    }


    ## function to calculate continuous stats v3:  add 2 decimal place for min and max
    cont_stat3 <- function(x,y) {
      tab<- data.frame(n = round_fmt(length(x),0),
                       Mean = round_fmt(mean(x),2),
                       SD = round_fmt(sd(x),3),
                       Median = round_fmt(median(x),2),
                       IQR = round_fmt(IQR(x),2),
                       Min = round_fmt(min(x),2),
                       Max = round_fmt(max(x),2))
      df <- data.frame(t(tab))
      df <- data.frame(label = rownames(df), stat_0 = df[,1])
      df2 <- rbind(data.frame(label = y, stat_0 = NA),df)
      return(df2)
    }


    ## function to convert fev1(ml) to fev1(%) // Ht in meters, sex ="F" or "M", age in years
    fev1_convert <- function(fev1_ml, age, sex, Ht) {
      if (sex == "M") {
        fev1_pred <- ((Ht * 4.30) - (age * 0.029) - 2.49) * 1000
      } else {
        fev1_pred <- ((Ht * 3.95) - (age * 0.025) - 2.60) * 1000
      }
      fev1_perc <- fev1_ml / fev1_pred * 100
      return(fev1_perc)
    }
    #example
    fev1_convert(1440, 45, "M", 1.67)

    ## directory to save tables
    #path <- "D:/odesk/upwork 2023/6. Islah Project - GSK ACT/Main Analysis/"
    #setwd(path)

    ## subject not eligible
    id.ne <- adsl$USUBJID[which(adsl$FASFL == "N")]


    ## remove subject not eligible in advs
    advs2 <- advs[-which(advs$USUBJID %in% id.ne),]
    #advs2 <- advs

    ## remove subject not eligible in adsc
    #adsc2 <- adsc[-which(adsc$USUBJID %in% id.ne),]
    adsc2 <- adsc

    ## remove subject not eligible in adsu
    #adsu2 <- adsu[-which(adsu$USUBJID %in% id.ne),]
    adsu2 <- adsu

    ## remove subject not eligible in adre
    #adre2 <- adre[-which(adre$USUBJID %in% id.ne),]
    adre2 <- adre

    ## remove subject not eligible in admh
    #admh2 <- admh[-which(admh$USUBJID %in% id.ne),]
    admh2 <- admh

    ## remove subject not eligible in adsl
    adsl2 <- adsl[-which(adsl$USUBJID %in% id.ne),]

    ## remove subject not eligible in adexd
    adexd2 <- adexd[-which(adexd$USUBJID %in% id.ne),]
    adexd <- adexd2

    ## remove subject not eligible in adexd
    adec2 <- adec[-which(adec$USUBJID %in% id.ne),]
    adec <- adec2


    ## remove subject not eligible in adrs
    adrs2 <- adrs[-which(adrs$USUBJID %in% id.ne),]
    adrs <- adrs2

    ## remove subject not eligible in adsu
    adsu2 <- adsu[-which(adsu$USUBJID %in% id.ne),]
    adsu <- adsu2

    ## remove subject not eligible in adsc
    adsc2 <- adsc[-which(adsc$USUBJID %in% id.ne),]
    adsc <- adsc2

    ## remove subject not eligible in adfa
    adfa2 <- adfa[-which(adfa$USUBJID %in% id.ne),]
    adfa <- adfa2


    ## LIST OF PATIENT ID
    PATIENT_ID <- adsl2 %>% dplyr::select(USUBJID, SUBJID)

    ## merge USUBJID to crf
    crf2 <- merge(crf, PATIENT_ID, by.x = "Study.Subject.ID", by.y = "SUBJID", all.y = TRUE)


    ############## TLF Table 1

    #re-name label in FASFL
    adsl$FASFL <- relevel(factor(adsl$FASFL), ref = "Y")
    adsl$FASFL <- fct_recode(adsl$FASFL, `Participants in the FAS` = "Y", `Protocol deviations` = "N")

    #re-name label in ENRLFL
    adsl$EENRLFL <- fct_recode(adsl$ENRLFL, `Participants Enrolled` = "Y")

    #create summary table 1
    table1 <- adsl %>%
      dplyr::select(EENRLFL,FASFL) %>%
      tbl_summary(digits = list(FASFL ~ c(0,1)))
    # remove variable names from the table
    table1 <- remove_row_type(
      table1,
      variables = everything(),
      type = c("header"),
      level_value = NULL) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()

    # save file
    names(table1) <- c("Summary Variable", paste("(N = ",extract_numeric(table1[1,2]), ")", sep = ""))
    table1f <- table1[2:(nrow(table1)-1),]
    table1f <- as.data.frame(table1f)
    rownames(table1f) <- NULL
    write.csv(table1f, "table1.csv")

    ############## TLF Table 2

    # relevel age category AGEGR1
    adsl2$AGEGR1 <- factor(adsl2$AGEGR1, levels = c("18-30", "31-45", "46-65",">65"))

    # rename sex category
    adsl2$SEX <- fct_recode(adsl2$SEX, `Female` = "F", `Male` = "M")

    # rename and relevel race category
    adsl2$RACEGR1 <- fct_recode(adsl2$RACEGR1, `Asian` = "ASIAN", `Unknown` = "UNKNOWN", `Other` = "OTHER")
    adsl2$RACEGR1 <- factor(adsl2$RACEGR1, levels = c("Asian", "Other", "Unknown"))


    # create table 2
    table2 <- c()

    # summary age from adsl
    var_name <- "Age (years)"
    tab_ <- cont_stat(adsl2$AGE,var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    # summary age group, sex, race from adsl
    tab_ <- adsl2 %>%
      dplyr::select(AGEGR1, SEX, RACEGR1) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = list("AGEGR1" ~ "Age",
                               "SEX" ~ "Sex",
                               "RACEGR1" ~ "Race/ethnicity")) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",4),
                                                                NA,rep("n(%)",2),
                                                                NA,rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    n_header <- extract_numeric(tab_[1,2])
    subtab.f <- subtab[c(2:13),]
    table2 <- rbind(table2, subtab.f)


    # summary weight from advs
    sub.dt <- subset(advs2, PARAMCD == "WEIGHT")
    tab_ <- cont_stat(sub.dt$AVAL,"Weight")
    subtab <- data.frame(Characteristic = c("Weight (kg)", rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- "Weight (kg)"
    table2 <- rbind(table2, subtab.f)

    # summary height from advs
    sub.dt <- subset(advs2, PARAMCD == "HEIGHT")
    tab_ <- cont_stat(sub.dt$AVAL*100,"Height")
    subtab <- data.frame(Characteristic = c("Height (cm)", rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- "Height (cm)"
    table2 <- rbind(table2, subtab.f)

    # summary BMI numeric from advs
    sub.dt <- subset(advs2, PARAMCD == "BMI")
    tab_ <- cont_stat3(sub.dt$AVAL,"BMI")
    subtab <- data.frame(Characteristic = c("Body mass index (kg/m2)", rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- "Body mass index (kg/m2)"
    table2 <- rbind(table2, subtab.f)

    # summary BMI cat from advs
    sub.dt <- subset(advs2, PARAMCD == "BMI")
    sub.dt$AVALCAT1 <- factor(sub.dt$AVALCAT1, levels = c("Underweight", "Normal", "Pre-obesity", "Obesity"))
    tab_ <- sub.dt %>%
      dplyr::select(AVALCAT1) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = "AVALCAT1" ~ "Body mass index categories (kg/m2)" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",4),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[2:6,]
    table2 <- rbind(table2, subtab.f)


    # summary smoking history from adsc
    sub.dt <- subset(adsc2, SCTESTCD == "SMOKHIST")
    sub.dt$SCORRES <- factor(sub.dt$SCORRES, levels = c("Never Smoke", "Current Smoke", "Former Smoke", "Unknown"))
    tab_ <- sub.dt %>%
      dplyr::select(SCORRES) %>%
      tbl_summary(digits = list(SCORRES ~ c(0,1)),
                  label = "SCORRES" ~ "Smoking history" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",4),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(tab_)),]
    table2 <- rbind(table2, subtab.f)

    # summary years of smoking from adsu
    id.smokers <- sub.dt$USUBJID[which(sub.dt$SCORRES == "Current Smoke" | sub.dt$SCORRES == "Former Smoke")]
    adsu.smokers <- filter(adsu2, USUBJID %in% id.smokers)
    sub.dt <- subset(adsu2, !is.na(SUDOSE))
    nrow(adsu.smokers) == nrow(sub.dt)
    sub.dt2 <- subset(sub.dt, !is.na(ADY))
    tab_ <- cont_stat(sub.dt2$ADY/365,"Years of smoking")
    subtab <- data.frame(Characteristic = c("Years of smoking", rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- "Years of smoking"
    table2 <- rbind(table2, subtab.f)


    # summary number of ciggaretes from adsu
    id.smokers <- sub.dt$USUBJID[which(sub.dt$SCORRES == "Current Smoke" | sub.dt$SCORRES == "Former Smoke")]
    adsu.smokers <- filter(adsu2, USUBJID %in% id.smokers)
    sub.dt <- subset(adsu2, !is.na(SUDOSE))
    nrow(adsu.smokers) == nrow(sub.dt)
    tab_ <- cont_stat(sub.dt$AVAL,"Number of cigarettes smoked per day")
    subtab <- data.frame(Characteristic = c("Number of cigarettes smoked per day", rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- "Number of cigarettes smoked per day"
    table2 <- rbind(table2, subtab.f)


    ## summary education from adsc
    sub.dt <- subset(adsc2, PARAMCD == "EDULEVEL")
    table(sub.dt$AVALC)
    sub.dt$AVALC <- factor(sub.dt$AVALC, levels = c("Illiterate", "Not Finishing Formal Education", "High School Graduate", "University Graduate (Bachelor)", "Post Graduate Degree (Master/Doctor/Post-Doctoral)", "Unknown or Not Reported"))
    tab_ <- sub.dt %>%
      dplyr::select(AVALC) %>%
      tbl_summary(digits = list(AVALC ~ c(0,1)),
                  label = "AVALC" ~ "Education level at Index or nearest Index (anytime pre-Index)" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",6),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(tab_)),]
    table2 <- rbind(table2, subtab.f)

    ## summary employment status from adsc
    sub.dt <- subset(adsc2, PARAMCD == "EMPJOB")
    table(sub.dt$AVALC)
    sub.dt$AVALC <- factor(sub.dt$AVALC, levels = c("Employed (full or part-time)", "Self-employed", "Student", "House Spouse", "Unemployed (not working/retiree)", "Unknown or Not Reported"))
    tab_ <- sub.dt %>%
      dplyr::select(AVALC) %>%
      tbl_summary(digits = list(AVALC ~ c(0,1)),
                  label = "AVALC" ~ "Employment status at Index or nearest Index (anytime pre-Index)" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",6),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(tab_)),]
    table2 <- rbind(table2, subtab.f)



    # summary asthma diagnosis known/unknown
    var_name <- "Years since asthma diagnosis at index or nearest index (anytime pre-index)"
    Asthma_Diag_Known <- rep(NA, nrow(adfa))
    Asthma_Diag_Known[which(!is.na(adfa$AVAL))] <- "Known"
    Asthma_Diag_Known[which(is.na(adfa$AVAL))] <- "Unknown"
    Asthma_Diag_Known <- factor(Asthma_Diag_Known, levels = c("Unknown", "Known"))
    adfa$Asthma_Diag_Known <- Asthma_Diag_Known
    tab_ <- adfa %>%
      dplyr::select(Asthma_Diag_Known) %>%
      tbl_summary(digits = list(Asthma_Diag_Known ~ c(0,1)),
                  label = "Asthma_Diag_Known" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",2),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(tab_)),]
    table2 <- rbind(table2, subtab.f)


    # summary asthma diagnosis per category among known
    var_name <- "Years since asthma diagnosis at index or nearest index (anytime pre-index)"
    adfa$AVALCAT1 <- factor(adfa$AVALCAT1, levels = c("0-4", "5-9", "10-14", "15-19", ">= 20", "Missing"))
    tab_ <- adfa %>%
      filter(AVALCAT1 != "Missing") %>%
      dplyr::select(AVALCAT1) %>%
      tbl_summary(digits = list(AVALCAT1 ~ c(0,1)),
                  label = "AVALCAT1" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, label = c(NA,NA,rep("n(%)",5),NA,NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(3:7),]
    table2 <- rbind(table2, subtab.f)



    # summary asthma diagnosis numerical
    var_name <- "Years since asthma diagnosis at index or nearest index"
    tab_ <- cont_stat2(na.omit(adfa$AVAL), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    # save to another df for years since asthma diagnosis
    df_asthma_diag <- adfa %>% dplyr::select(USUBJID, Asthma_Diag_Known, AVAL, AVALCAT1)
    names(df_asthma_diag)[3:4] <- c("Year_Asthma_Diag","Asthma_Diag_Cat")

    #pooling age, height, and sex from adls
    demog1 <- adsl2[,c("USUBJID","AGE","SEX")]
    demog2 <- advs2 %>%
      filter(PARAMCD == "HEIGHT") %>%
      dplyr::select(USUBJID, AVAL)

    # combine demog1 and demog2
    demog <- merge(demog1, demog2, by = "USUBJID")



    ## pooling all spirometry data
    df_spirometry <- adre %>%
      dplyr::select(USUBJID, REDTC, AVISIT, PARAMCD, AVAL)
    ## pooling treatment star date from adsl
    sub_adfa <- adfa %>% dplyr::select(USUBJID, TRTSDT)
    ## combine data
    df_temp <- merge(df_spirometry, sub_adfa, by = "USUBJID", all = TRUE)
    df_spirometry <- df_temp
    ## format date for TRTSDT
    date_TRTSDT <- df_spirometry$TRTSDT
    day <- as.integer(str_extract(date_TRTSDT, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_TRTSDT)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_TRTSDT, nchar(date_TRTSDT)-3))
    df_spirometry$TRTSDT <- make_date(year, month.n, day)
    ## format date for REDTC
    date_REDTC <- df_spirometry$REDTC
    day <- as.integer(str_extract(date_REDTC, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_REDTC)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_REDTC, nchar(date_REDTC)-3))
    df_spirometry$REDTC <- make_date(year, month.n, day)
    ## Total DAYS from index date to spirometry results
    df_spirometry$DAY_SPRMTRY_FROM_INDEX <- as.numeric(difftime(df_spirometry$REDTC,df_spirometry$TRTSDT, units = "days"))
    ## Total MONTHS from index date to spirometry results
    df_spirometry$MON_SPRMTRY_FROM_INDEX <- df_spirometry$DAY_SPRMTRY_FROM_INDEX / (365/12)
    ## Spirometry data at or nearest index (up to 60 days pre-index)
    # FEV1
    df_spirometry_fev1_index <- df_spirometry %>%
      filter(AVISIT == "INDEX", PARAMCD == "FEV1", DAY_SPRMTRY_FROM_INDEX >= -60 & DAY_SPRMTRY_FROM_INDEX <= 0) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1_index <- df_spirometry_fev1_index[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1_index)[2] <- "FEV1_INDEX"
    # FVC
    df_spirometry_fvc_index <- df_spirometry %>%
      filter(AVISIT == "INDEX", PARAMCD == "FVC", DAY_SPRMTRY_FROM_INDEX >= -60 & DAY_SPRMTRY_FROM_INDEX <= 0) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fvc_index <- df_spirometry_fvc_index[,c("USUBJID", "AVAL")]
    names(df_spirometry_fvc_index)[2] <- "FVC_INDEX"
    # FEV1 %
    df_spirometry_fev1pp_index <- df_spirometry %>%
      filter(AVISIT == "INDEX", PARAMCD == "FEV1PP", DAY_SPRMTRY_FROM_INDEX >= -60 & DAY_SPRMTRY_FROM_INDEX <= 0) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1pp_index <- df_spirometry_fev1pp_index[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1pp_index)[2] <- "FEV1PP_INDEX"
    # FVC %
    df_spirometry_fvcpp_index <- df_spirometry %>%
      filter(AVISIT == "INDEX", PARAMCD == "FVCPP", DAY_SPRMTRY_FROM_INDEX >= -60 & DAY_SPRMTRY_FROM_INDEX <= 0) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fvcpp_index <- df_spirometry_fvcpp_index[,c("USUBJID", "AVAL")]
    names(df_spirometry_fvcpp_index)[2] <- "FVCPP_INDEX"
    # FEV1/FVC %
    df_spirometry_fev1fvc_index <- df_spirometry %>%
      filter(AVISIT == "INDEX", PARAMCD == "FEV1FVCP", DAY_SPRMTRY_FROM_INDEX >= -60 & DAY_SPRMTRY_FROM_INDEX <= 0) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1fvc_index <- df_spirometry_fev1fvc_index[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1fvc_index)[2] <- "FEV1FVC_INDEX"

    ## Spirometry data at 6 to 12 month post index
    # FEV1
    df_spirometry_fev1_post <- df_spirometry %>%
      filter(AVISIT == "POST-INDEX", PARAMCD == "FEV1", MON_SPRMTRY_FROM_INDEX >= 6 & MON_SPRMTRY_FROM_INDEX <= 12) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1_post <- df_spirometry_fev1_post[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1_post)[2] <- "FEV1_POST"
    length(unique(df_spirometry_fev1_post$USUBJID)) == nrow(df_spirometry_fev1_post)
    # FVC
    df_spirometry_fvc_post <- df_spirometry %>%
      filter(AVISIT == "POST-INDEX", PARAMCD == "FVC", MON_SPRMTRY_FROM_INDEX >= 6 & MON_SPRMTRY_FROM_INDEX <= 12) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fvc_post <- df_spirometry_fvc_post[,c("USUBJID", "AVAL")]
    names(df_spirometry_fvc_post)[2] <- "FVC_POST"
    length(unique(df_spirometry_fvc_post$USUBJID)) == nrow(df_spirometry_fvc_post)
    # FEV1 %
    df_spirometry_fev1pp_post <- df_spirometry %>%
      filter(AVISIT == "POST-INDEX", PARAMCD == "FEV1PP", MON_SPRMTRY_FROM_INDEX >= 6 & MON_SPRMTRY_FROM_INDEX <= 12) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1pp_post <- df_spirometry_fev1pp_post[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1pp_post)[2] <- "FEV1PP_POST"
    length(unique(df_spirometry_fev1pp_post$USUBJID)) == nrow(df_spirometry_fev1pp_post)
    # FVC %
    df_spirometry_fvcpp_post <- df_spirometry %>%
      filter(AVISIT == "POST-INDEX", PARAMCD == "FVCPP", MON_SPRMTRY_FROM_INDEX >= 6 & MON_SPRMTRY_FROM_INDEX <= 12) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fvcpp_post <- df_spirometry_fvcpp_post[,c("USUBJID", "AVAL")]
    names(df_spirometry_fvcpp_post)[2] <- "FVCPP_POST"
    length(unique(df_spirometry_fvcpp_post$USUBJID)) == nrow(df_spirometry_fvcpp_post)
    # FEV1/FVC %
    df_spirometry_fev1fvc_post <- df_spirometry %>%
      filter(AVISIT == "POST-INDEX", PARAMCD == "FEV1FVCP", MON_SPRMTRY_FROM_INDEX >= 6 & MON_SPRMTRY_FROM_INDEX <= 12) %>%
      arrange(USUBJID, REDTC) %>%
      group_by(USUBJID) %>%
      slice_tail(n = 1)
    df_spirometry_fev1fvc_post <- df_spirometry_fev1fvc_post[,c("USUBJID", "AVAL")]
    names(df_spirometry_fev1fvc_post)[2] <- "FEV1FVC_POST"
    length(unique(df_spirometry_fev1fvc_post$USUBJID)) == nrow(df_spirometry_fev1fvc_post)
    ## Merge all spirometry results
    list_spirometry <- list(df_spirometry_fev1_index,
                            df_spirometry_fev1pp_index,
                            df_spirometry_fvc_index,
                            df_spirometry_fvcpp_index,
                            df_spirometry_fev1fvc_index,
                            df_spirometry_fev1_post,
                            df_spirometry_fev1pp_post,
                            df_spirometry_fvc_post,
                            df_spirometry_fvcpp_post,
                            df_spirometry_fev1fvc_post)
    df_spirometry <- list_spirometry %>% reduce(full_join, by='USUBJID')
    # check if FEV (ml) available but FEV (%) is missing: if cases present then need to convert
    df_spirometry[which(!is.na(df_spirometry$FEV1_INDEX) & is.na(df_spirometry$FEV1PP_INDEX)),] # at index
    df_spirometry[which(!is.na(df_spirometry$FEV1_POST) & is.na(df_spirometry$FEV1PP_POST)),] # at post
    # check if ratio fev1/fvc not available but fev1 (ml) and fvc(ml) available: if cases present then need to convert
    df_spirometry[which(!is.na(df_spirometry$FEV1_INDEX) & !is.na(df_spirometry$FVC_INDEX) & is.na(df_spirometry$FEV1FVC_INDEX)),] # at index
    df_spirometry[which(!is.na(df_spirometry$FEV1_POST) & !is.na(df_spirometry$FVC_POST) & is.na(df_spirometry$FEV1FVC_POST)),] # at post

    ## summary fev1(ml) at index
    var_name <- "FEV1 (ml) at or near index (up to 60 days pre-index)"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1_INDEX), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fev1(%) at index
    var_name <- "FEV1 (%) at or near index (up to 60 days pre-index)"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1PP_INDEX), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fev1(ml) at post-index
    var_name <- "FEV1(mL) at 6 to 12 months post-index"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1_POST), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fev1(%) at post-index
    var_name <- "FEV1(%) at 6 to 12 months post-index"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1PP_POST), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fvc(ml) at index from re
    var_name <- "FVC (ml) at or near index (up to 60 days pre-index)"
    tab_ <- cont_stat(na.omit(df_spirometry$FVC_INDEX), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fvc(%) at index from re
    var_name <- "FVC (%) at or near index (up to 60 days pre-index)"
    tab_ <- cont_stat(na.omit(df_spirometry$FVCPP_INDEX), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fvc(ml) at post-index
    var_name <- "FVC(ml) at 6 to 12 months post-index"
    tab_ <- cont_stat(na.omit(df_spirometry$FVC_POST), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fvc(%) at post-index
    var_name <- "FVC(%) at 6 to 12 months post-index"
    tab_ <- cont_stat(na.omit(df_spirometry$FVCPP_POST), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fev1/fvc at index from re
    var_name <- "FEV1/FVC (%) at or near index (up to 60 days pre-index)"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1FVC_INDEX), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    ## summary fev1/fvc(%) at post-index
    var_name <- "FEV1/FVC (%) at 6 to 12 months post-index"
    tab_ <- cont_stat(na.omit(df_spirometry$FEV1FVC_POST), var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), tab_)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table2 <- rbind(table2, subtab.f)

    # save file
    names(table2) <- c("Characteristic", "Statistic", paste("(N = ",n_header, ")", sep = ""))
    rownames(table2) <- NULL
    write.csv(table2, "table2.csv")



    ############## TLF Table 3 (admh)

    # create table 3
    table3 <- c()

    # Medical history available
    # from crf (1: yes, 0: no)
    var_name <- "Medical history available"
    table(crf2$COKNOWN_E1_C5)
    crf2$MH_CO <- ifelse(crf2$COKNOWN_E1_C5 == 1, "Yes", "No")
    crf2$MH_CO <- factor(crf2$MH_CO, levels = c("Yes", "No"))
    tab_ <- crf2 %>%
      dplyr::select(MH_CO) %>%
      group_by(MH_CO) %>%
      summarise(n = n()) %>%
      mutate(perc = round(n / sum(n) * 100,1),
             stat_0 = paste(n, " (", perc, "%)", sep = ""))
    subtab <- data_frame(Characteristic = c(var_name,"Yes", "No"),
                         Statistic = c(NA,rep("n(%)",2)),
                         stat_0 = c(NA,tab_$stat_0))
    table3 <- rbind(table3, subtab)


    # Number of patients with comorbid conditions at index or nearest index (up to 90 days pre-index)
    var_name <- "Number of patients with comorbid conditions at index or nearest index (up to 90 days pre-index)"
    admh <- admh2
    # patient ID whose medical history is available
    df_mh_avail <- crf2 %>%
      dplyr::select(Study.Subject.ID, COKNOWN_E1_C5) %>%
      filter(COKNOWN_E1_C5 == 1)
    names(df_mh_avail)[1] <- "SUBJID"
    df_temp <- merge(PATIENT_ID, df_mh_avail, by = "SUBJID", all = TRUE)
    # list patient whose medical history is available and comorbids are present at index from admh
    subdf <- admh %>% filter(EPOCH == "BASELINE") %>% filter(MHTERM != "NO COMORBID OF INTEREST")
    pat_id_cc_present <- data.frame(USUBJID = unique(subdf$USUBJID), CC_PRESENT = 1)
    # combine data frame
    df_mh <- merge(df_temp, pat_id_cc_present, by = "USUBJID", all = TRUE)
    # create MH Status at index: 1 = MH available and comorbid present, 0 = MH available but comorbid not present, NA = MH Not available
    df_mh$MHSTATUS_INDEX <- ifelse(df_mh$COKNOWN_E1_C5 == 1 & is.na(df_mh$CC_PRESENT), 0, df_mh$COKNOWN_E1_C5)
    # calculate number of comorbid per patient
    num_comorbid_per_patient <- admh %>%
      filter(EPOCH == "BASELINE") %>%
      filter(MHTERM != "NO COMORBID OF INTEREST") %>%
      group_by(USUBJID) %>% summarise(n=n())
    # merge data
    df_CC_index <- merge(df_mh, num_comorbid_per_patient, by = "USUBJID", all = TRUE)[,c("USUBJID", "MHSTATUS_INDEX","n")]
    # assign 0 for patients whose MH available but comorbid condition not present
    df_CC_index$n[which(df_CC_index$MHSTATUS_INDEX == 0 & is.na(df_CC_index$n))] <- 0
    df_CC_index$n[which(is.na(df_CC_index$MHSTATUS_INDEX))] <- NA
    names(df_CC_index)[3] <- "TOT_CC_INDEX"
    # Total comorbid condition as factor
    TOT_CC_INDEX_CAT <- as.factor(df_CC_index$TOT_CC_INDEX)
    TOT_CC_INDEX_CAT <- factor(TOT_CC_INDEX_CAT, levels = c("None","1", "2", "3+"))
    TOT_CC_INDEX_CAT[which(df_CC_index$TOT_CC_INDEX >= 3)] <- "3+"
    TOT_CC_INDEX_CAT[which(df_CC_index$TOT_CC_INDEX == 0)] <- "None"
    table(TOT_CC_INDEX_CAT)
    df_CC_index$TOT_CC_INDEX_CAT <- TOT_CC_INDEX_CAT
    # create column to identify at least 1 comorbid condition
    df_CC_index$TOT_CC_INDEX_ATLEAST1 <- ifelse(df_CC_index$TOT_CC_INDEX_CAT == "None", "None", "At least one")
    df_CC_index$TOT_CC_INDEX_ATLEAST1 <- factor(df_CC_index$TOT_CC_INDEX_ATLEAST1, levels = c("None", "At least one"))
    # summary stat for at least 1 comorbid condition
    tab_ <- df_CC_index %>%
      dplyr::select(TOT_CC_INDEX_ATLEAST1) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = "TOT_CC_INDEX_ATLEAST1" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",2), NA, NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(2:4),]
    table3 <- rbind(table3, subtab.f)
    # summary stat for each category
    tab_ <- df_CC_index %>%
      filter(TOT_CC_INDEX > 0) %>%
      dplyr::select(TOT_CC_INDEX_CAT) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = "TOT_CC_INDEX_CAT" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",4), NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(4:6),]
    table3 <- rbind(table3, subtab.f)


    ## Number of comorbid conditions present at index or nearest index (up to 90 days pre-index), per patient
    var_name <- "Number of comorbid conditions present at index or nearest index (up to 90 days pre-index), per patient"
    df_temp <- df_CC_index %>% filter(TOT_CC_INDEX > 0)
    tab_ <- cont_stat(na.omit(df_temp$TOT_CC_INDEX),var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab$Statistic[1] <- NA
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # change n to total CC across all patients
    subtab.f[1,3] <- sum(df_CC_index$TOT_CC_INDEX, na.rm = TRUE)
    table3 <- rbind(table3, subtab.f)


    # Number of patients with comorbid conditions at post-index
    var_name <- "Number of patients with comorbid conditions at post-index"
    # patient ID whose medical history is available
    df_mh_avail <- crf2 %>%
      dplyr::select(Study.Subject.ID, COKNOWN_E1_C5) %>%
      filter(COKNOWN_E1_C5 == 1)
    names(df_mh_avail)[1] <- "SUBJID"
    df_temp <- merge(PATIENT_ID, df_mh_avail, by = "SUBJID", all = TRUE)
    # list patient whose medical history is available and comorbids are present at post-index from admh
    subdf <- admh %>% filter(EPOCH != "BASELINE") %>% filter(MHTERM != "NO COMORBID OF INTEREST")
    pat_id_cc_present <- data.frame(USUBJID = unique(subdf$USUBJID), CC_PRESENT = 1)
    # combine data frame
    df_mh <- merge(df_temp, pat_id_cc_present, by = "USUBJID", all = TRUE)
    # create MH Status at post: 1 = MH available and comorbid presnt, 0 = MH available but comorbid not present, NA = MH Not available
    df_mh$MHSTATUS_POST <- ifelse(df_mh$COKNOWN_E1_C5 == 1 & is.na(df_mh$CC_PRESENT), 0, df_mh$COKNOWN_E1_C5)
    # calculate number of comorbid per patient
    num_comorbid_per_patient <- admh %>%
      filter(EPOCH != "BASELINE") %>%
      filter(MHTERM != "NO COMORBID OF INTEREST") %>%
      group_by(USUBJID) %>% summarise(n=n())
    # merge data
    df_CC_post <- merge(df_mh, num_comorbid_per_patient, by = "USUBJID", all = TRUE)[,c("USUBJID", "MHSTATUS_POST","n")]
    # assign 0 for patients whose MH available but comorbid condition not present
    df_CC_post$n[which(df_CC_post$MHSTATUS_POST == 0 & is.na(df_CC_post$n))] <- 0
    names(df_CC_post)[3] <- "TOT_CC_POST"
    # Total comorbid condition as factor
    TOT_CC_POST_CAT <- as.factor(df_CC_post$TOT_CC_POST)
    TOT_CC_POST_CAT <- factor(TOT_CC_POST_CAT, levels = c("None","1", "2", "3+"))
    TOT_CC_POST_CAT[which(df_CC_post$TOT_CC_POST >= 3)] <- "3+"
    TOT_CC_POST_CAT[which(df_CC_post$TOT_CC_POST == 0)] <- "None"
    table(TOT_CC_POST_CAT)
    df_CC_post$TOT_CC_POST_CAT <- TOT_CC_POST_CAT
    # create column to identify at least 1 comorbid condition
    df_CC_post$TOT_CC_POST_ATLEAST1 <- ifelse(df_CC_post$TOT_CC_POST_CAT == "None", "None", "At least one")
    df_CC_post$TOT_CC_POST_ATLEAST1 <- factor(df_CC_post$TOT_CC_POST_ATLEAST1, levels = c("None", "At least one"))
    # summary stat for at least 1 comorbid condition
    tab_ <- df_CC_post %>%
      dplyr::select(TOT_CC_POST_ATLEAST1) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = "TOT_CC_POST_ATLEAST1" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",2), NA, NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(2:4),]
    table3 <- rbind(table3, subtab.f)
    # summary stat for each category
    tab_ <- df_CC_post %>%
      filter(TOT_CC_POST > 0) %>%
      dplyr::select(TOT_CC_POST_CAT) %>%
      tbl_summary(digits = list(all_categorical() ~ c(0,1)),
                  label = "TOT_CC_POST_CAT" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",4), NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(4:6),]
    table3 <- rbind(table3, subtab.f)


    ## Number of comorbid conditions present at post-index, per patient
    var_name <- "Number of comorbid conditions present at post-index, per patient"
    df_temp <- df_CC_post %>% filter(TOT_CC_POST > 0)
    tab_ <- cont_stat(na.omit(df_temp$TOT_CC_POST),var_name)
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab$Statistic[1] <- NA
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # change n to total CC across all patients
    subtab.f[1,3] <- sum(df_CC_post$TOT_CC_POST, na.rm = TRUE)
    table3 <- rbind(table3, subtab.f)


    # list of comorbid conditions
    list_comorbid <- c("Active TB",
                       "Allergic Bronchopulmonary Aspergillosis (ABPA)",
                       "Allergic rhinitis",
                       "Arthritis",
                       "Asthma-COPD Overlap (ACOS)",
                       "Atherosclerosis",
                       "Cancer",
                       "Cardiac Arrhythmia",
                       "Cataracts",
                       "Chronic Obstructive Pulmonary Disease (COPD)",
                       "Congestive Heart Failure (CHF)",
                       "Cystic fibrosis",
                       "Diabetes",
                       "Skin thinning",
                       "Easy bruising",
                       "Acne perioral dermatitis",
                       "Mucocutaneous infection",
                       "Candidiasis",
                       "Depression",
                       "Eosinophilic Granulomatosis with Polyangiitis (EGPA)",
                       "Gastroesophageal Reflux Disease (GERD)",
                       "Hormonal disorders",
                       "Hypertension",
                       "Interstitial lung disease",
                       "Kidney disease",
                       "Other Lung disease",
                       "Obesity",
                       "Obstructive Sleep Apnea (OSA)",
                       "Osteoporosis",
                       "Steroid-induced myopathy",
                       "Stroke"
    )
    list_comorbid <- toupper(list_comorbid)
    df_list_comorbid <- data.frame(MHTERM = list_comorbid)

    # Number of patients with comorbid condition(s) present at index or nearest index (up to 90 days pre-index)
    var_name <- "Number of patients with comorbid condition(s) present at index or nearest index (up to 90 days pre-index)"
    sub.index <- admh %>% filter(EPOCH == "BASELINE") %>% filter(MHTERM != "NO COMORBID OF INTEREST")
    num_patient_with_comorbid_index <- sum(df_CC_index$MHSTATUS_INDEX, na.rm = TRUE)
    any_of_below <- num_patient_with_comorbid_index / sum(table(df_CC_index$MHSTATUS_INDEX)) *100
    comorbid_present <- sub.index %>%
      group_by(MHTERM) %>%
      summarise(n=n(),
                prop = n/num_patient_with_comorbid_index*100)
    # merge to complete df of comorbid list
    comorbid_present_full <- merge(df_list_comorbid, comorbid_present, by = "MHTERM", all = TRUE)
    comorbid_present_full[is.na(comorbid_present_full)] <- 0
    df.header <- data.frame(MHTERM = c(var_name, "Any of below"), n = c(NA, num_patient_with_comorbid_index), prop = c(NA, any_of_below))
    subtab <- rbind(df.header, comorbid_present_full)
    prop.round <- rep(NA, nrow(subtab))
    for (i in 1:nrow(subtab)) {
      prop.round[i] <- round_fmt(subtab$pro[i], digits = 1)
    }
    subtab$prop.round <- prop.round
    subtab$stat_0 <- paste(subtab$n, " (", subtab$prop.round, "%)", sep = "")
    subtab2 <- data.frame(Characteristic = subtab$MHTERM, Statistic = c(NA,rep("n(%)", nrow(subtab)-1)), stat_0 = subtab$stat_0)
    subtab2$stat_0[1] <- NA
    subtab2$Characteristic[3:nrow(subtab2)] <- str_to_title(subtab2$Characteristic[3:nrow(subtab2)])
    table3 <- rbind(table3, subtab2)



    # Number of patients with comorbid condition(s) that were present at index or nearest index (up to 90 days pre-index) or during post-index
    var_name <- "Number of patients having Cwith comorbid condition(s) that were present at index or nearest index (up to 90 days pre-index) or during post-index "
    df__ <- admh %>% filter(MHTERM != "NO COMORBID OF INTEREST")
    num_patient_with_comorbid <- length(unique(df__$USUBJID))
    any_of_below <- num_patient_with_comorbid / sum(table(df_CC_post$MHSTATUS_POST)) *100
    comorbid_present <- admh %>%
      filter(MHTERM != "NO COMORBID OF INTEREST") %>%
      group_by(MHTERM) %>%
      summarise(n=n(),
                prop = n/num_patient_with_comorbid*100)
    # merge to complete df of comorbid list
    comorbid_present_full <- merge(df_list_comorbid, comorbid_present, by = "MHTERM", all = TRUE)
    comorbid_present_full[is.na(comorbid_present_full)] <- 0
    df.header <- data.frame(MHTERM = c(var_name, "Any of below"), n = c(NA, num_patient_with_comorbid), prop = c(NA, any_of_below))
    subtab <- rbind(df.header, comorbid_present_full)
    prop.round <- rep(NA, nrow(subtab))
    for (i in 1:nrow(subtab)) {
      prop.round[i] <- round_fmt(subtab$pro[i], digits = 1)
    }
    subtab$prop.round <- prop.round
    subtab$stat_0 <- paste(subtab$n, " (", subtab$prop.round, "%)", sep = "")
    subtab2 <- data.frame(Characteristic = subtab$MHTERM, Statistic = c(NA,rep("n(%)", nrow(subtab)-1)), stat_0 = subtab$stat_0)
    subtab2$stat_0[1] <- NA
    subtab2$Characteristic[3:nrow(subtab2)] <- str_to_title(subtab2$Characteristic[3:nrow(subtab2)])
    table3 <- rbind(table3, subtab2)



    # comorbid condition listed as exclusion (current dry run: none)
    comorbid.exclusion <- toupper(c("Active TB",
                                    "Allergic Bronchopulmonary Aspergillosis (ABPA)",
                                    "Asthma-COPD overlap (ACOS)",
                                    "Chronic obstructive pulmonary disease (COPD)",
                                    "Cystic fibrosis",
                                    "Eosinophilic Granulomatosis with Polyangiitis (EGPA)",
                                    "Interstitial lung disease"))
    sub.index <- filter(admh, MHTERM %in% comorbid.exclusion)
    sub.index #none
    subtab <- data.frame(Characteristic = c("Number of patients with comorbid condition(s) listed as exclusion at index or nearest index (up to 90 days pre-index) but present at post-index", comorbid.exclusion),
                         Statistic = c(NA,rep("n(%)",7)), stat_0 = "0 (0%)")
    subtab$stat_0[1] <- NA
    subtab$Characteristic[2:nrow(subtab)] <- str_to_title(subtab$Characteristic[2:nrow(subtab)])
    table3 <- rbind(table3, subtab)

    # save file
    names(table3)[1] <- "Summary Variable"
    names(table3)[3] <- paste("N = ", n_header, sep ="")
    rownames(table3) <- NULL
    write.csv(table3, "table3.csv")



    ############## TLF Table 4: Summary of asthma exacerbation

    ## pooling all relevant columns for asthma exacerbations
    df_excr <- crf2 %>%  dplyr::select(Study.Subject.ID,
                                       CE_ERVIS_E5_C23, # ever ER visit due to exacerbation
                                       contains("EXA_ERVISDT_"), # date of ER visit
                                       contains("EXA_EROCS_"), #steroid use in ER visit
                                       contains("EXA_HO_"), # ever hospitalized due to exacerbation
                                       contains("EXA_HOSTDT_"), # date entry of hospitalization
                                       contains("EXA_HOENDT_"), # date of release of hospitalization
                                       contains("EXA_TONIGHTBN_"), #Total Nights Hospitalized Known
                                       contains("EXA_TONIGHT_"), # Total nights hospitalized
                                       contains("EXA_HOOCS_"), #steroid use in hospitalization
                                       contains("ASTMEDPUROTH_"), # Asthma Medication Purpose (2=exacerbation)
                                       contains("ASTMEDAGENTOTH_"), # steroid use if coded 7
                                       contains("ASTMEDSTOTH_"), # start date other asthma medication
                                       INC_NEW_STDT_E1_C3) # Index date (ICS/LABA initiation date)


    ## create table
    table4 <- data.frame()


    ## subtab7: exacerbation required ER visit (0 (No),1 (Yes),99 (Unknown/Not Reported)
    var_name <- "Patient had an asthma exacerbation that required an ER visit"
    table(df_excr$CE_ERVIS_E5_C23)

    # re-label
    df_excr$CE_ERVIS_E5_C23[which(df_excr$CE_ERVIS_E5_C23 == 0)] <- "No"
    df_excr$CE_ERVIS_E5_C23[which(df_excr$CE_ERVIS_E5_C23 == 1)] <- "Yes"
    df_excr$CE_ERVIS_E5_C23[which(df_excr$CE_ERVIS_E5_C23 == 99)] <- "Unknown"
    table(df_excr$CE_ERVIS_E5_C23)
    df_excr$CE_ERVIS_E5_C23 <- factor(df_excr$CE_ERVIS_E5_C23, levels = c("Yes", "No", "Unknown"))


    # check if exacerbation occur at index date
    row.index <- which(df_excr$EXA_ERVISDT_E5_C23_1==df_excr$INC_NEW_STDT_E1_C3)
    row.index
    # change exacerbation required ER visit to "NO" (no exacerbation at post index)
    df_excr$CE_ERVIS_E5_C23[row.index] <- "No"
    table(df_excr$CE_ERVIS_E5_C23)


    # create summary statistic
    tab_ <- df_excr %>%
      dplyr::select(CE_ERVIS_E5_C23) %>%
      tbl_summary(digits = list("CE_ERVIS_E5_C23" ~ c(0,1)),
                  label = "CE_ERVIS_E5_C23" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(subtab)),]
    subtab4.7 <- subtab.f


    ## subtab8: Number of patients who had an asthma exacerbation that required an ER visit, based on the frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required an ER visit, based on the frequency, per year"
    # column of date of ER
    ER_date <- df_excr %>%
      dplyr::select(contains("EXA_ERVISDT_E5_C23"))
    # check if date of exacerbation is the same as index date
    ER_date$EXA_ERVISDT_E5_C23_1[row.index] == df_excr$INC_NEW_STDT_E1_C3[row.index]
    # replace with empty cell if date of exacerbation = index date
    ER_date$EXA_ERVISDT_E5_C23_1[row.index] <- ""
    # count ER date per patient (number of exacerbation)
    df_excr$excr_ER <- colSums(apply(ER_date, 1, grepl, pattern = "-"))
    df_excr$excr_ER[which(df_excr$CE_ERVIS_E5_C23 == "Unknown")] <- NA
    # create column to identify at least 1 exacerbation
    excr_ER_atleast1 <- rep(NA, nrow(df_excr))
    excr_ER_atleast1[which(df_excr$excr_ER == 0 & df_excr$CE_ERVIS_E5_C23 == "No")] <- "No exacerbation"
    excr_ER_atleast1[which(df_excr$excr_ER >= 1 & df_excr$CE_ERVIS_E5_C23 == "Yes")] <- "At least 1 exacerbation"
    df_excr$excr_ER_atleast1 <- excr_ER_atleast1
    tab_ <- df_excr %>%
      filter(CE_ERVIS_E5_C23 != "Unknown") %>%
      group_by(excr_ER_atleast1) %>%
      summarise(n = n()) %>%
      mutate(perc = n/sum(n)*100)
    perc_c <- paste(tab_$n[1], " (", round_fmt(tab_$perc[1],1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of ER exacerbations
    excr_ER_cat <- rep(NA, nrow(df_excr))
    excr_ER_cat[which(df_excr$excr_ER == 1)] <- "1 exacerbation"
    excr_ER_cat[which(df_excr$excr_ER == 2)] <- "2 exacerbations"
    excr_ER_cat[which(df_excr$excr_ER >= 3)] <- ">=3 exacerbations"
    table(excr_ER_cat)
    excr_ER_cat <- factor(excr_ER_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_ER_cat <- excr_ER_cat
    tab_ <- df_excr %>%
      select(excr_ER_cat) %>%
      tbl_summary(digits = list("excr_ER_cat" ~ c(0,1)),
                  label = "excr_ER_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.8 <- rbind(subtab1, subtab2)

    ## subtab9: Total asthma exacerbation events requiring an ER visit per year, per participant
    var_name <- "Total asthma exacerbation events requiring an ER visit per year, per participant"
    tab_ <- cont_stat(df_excr$excr_ER[which(df_excr$excr_ER != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_ER, na.rm = TRUE)
    subtab4.9 <- subtab.f


    ## subtab10: Number of patients who had an asthma exacerbation that required an ER visit with steroid use, based on frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required an ER visit with steroid use, based on frequency, per year"
    # data ER with steroid 0 (No),1 (Yes),99 (Unknown/Not Reported)
    lbl <- c(0,1,99)
    # change if exacerbation requiring steroid occur at index date to NA
    df_excr$EXA_EROCS_E5_C23_1[row.index] <- NA
    ER_steroid <- df_excr %>%
      select(contains("EXA_EROCS_E5_C23"))
    excr_ER_steroid <- apply(ER_steroid, 1, function(x) length(which(x==1)))
    excr_ER_steroid[which(df_excr$CE_ERVIS_E5_C23 != "Yes")] <- 0
    excr_ER_steroid[which(df_excr$CE_ERVIS_E5_C23 == "Unknown")] <- NA
    df_excr$excr_ER_steroid <- excr_ER_steroid
    tab_ <- df_excr %>%
      filter(CE_ERVIS_E5_C23 == "Yes" & excr_ER_steroid >= 1) %>%
      group_by(excr_ER_atleast1) %>%
      summarise(n = n())
    perc <- tab_$n / length(which(df_excr$CE_ERVIS_E5_C23 != "Unknown")) * 100
    perc_c <- paste(tab_$n, "(", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of ER exacerbations required steroid
    excr_ER_steroid_cat <- rep(NA, nrow(df_excr))
    excr_ER_steroid_cat[which(df_excr$excr_ER_steroid == 1)] <- "1 exacerbation"
    excr_ER_steroid_cat[which(df_excr$excr_ER_steroid == 2)] <- "2 exacerbations"
    excr_ER_steroid_cat[which(df_excr$excr_ER_steroid >= 3)] <- ">=3 exacerbations"
    excr_ER_steroid_cat[which(df_excr$CE_ERVIS_E5_C16 != "Yes")] <- NA
    table(excr_ER_steroid_cat)
    excr_ER_steroid_cat <- factor(excr_ER_steroid_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_ER_steroid_cat <- excr_ER_steroid_cat
    tab_ <- df_excr %>%
      select(excr_ER_steroid_cat) %>%
      tbl_summary(digits = list("excr_ER_steroid_cat" ~ c(0,1)),
                  label = "excr_ER_steroid_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.10 <- rbind(subtab1, subtab2)



    ## subtab11: Total asthma exacerbation events requiring an ER visit with steroid use per year, per participant
    var_name <- "Total asthma exacerbation events requiring an ER visit with steroid use per year, per participant"
    tab_ <- cont_stat(df_excr$excr_ER_steroid[which(df_excr$excr_ER_steroid != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_ER_steroid, na.rm = TRUE)
    subtab4.11 <- subtab.f




    ## subtab12: Number of patients who had an asthma exacerbation that required an ER visit without steroid use, based on frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required an ER visit without steroid use, based on frequency, per year"
    # data ER with steroid 0 (No),1 (Yes),99 (Unknown/Not Reported)
    ER_steroid <- df_excr %>%
      select(contains("EXA_EROCS_E5_C23"))
    excr_ER_nonsteroid <- apply(ER_steroid, 1, function(x) length(which(x==0)))
    excr_ER_nonsteroid[which(df_excr$CE_ERVIS_E5_C23 == "Unknown")] <- NA
    df_excr$excr_ER_nonsteroid <- excr_ER_nonsteroid
    tab_ <- df_excr %>%
      filter(CE_ERVIS_E5_C23 == "Yes" & excr_ER_nonsteroid >= 1) %>%
      group_by(excr_ER_atleast1) %>%
      summarise(n = n())
    perc <- tab_$n / length(which(df_excr$CE_ERVIS_E5_C23 != "Unknown")) * 100
    perc_c <- paste(tab_$n, "(", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of ER exacerbations required non-steroid
    excr_ER_nonsteroid_cat <- rep(NA, nrow(df_excr))
    excr_ER_nonsteroid_cat[which(df_excr$excr_ER_nonsteroid == 1)] <- "1 exacerbation"
    excr_ER_nonsteroid_cat[which(df_excr$excr_ER_nonsteroid == 2)] <- "2 exacerbations"
    excr_ER_nonsteroid_cat[which(df_excr$excr_ER_nonsteroid >= 3)] <- ">=3 exacerbations"
    excr_ER_nonsteroid_cat <- factor(excr_ER_nonsteroid_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    table(excr_ER_nonsteroid_cat)
    table(excr_ER_nonsteroid)
    df_excr$excr_ER_nonsteroid_cat <- excr_ER_nonsteroid_cat
    tab_ <- df_excr %>%
      select(excr_ER_nonsteroid_cat) %>%
      tbl_summary(digits = list("excr_ER_nonsteroid_cat" ~ c(0,1)),
                  label = "excr_ER_nonsteroid_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.12 <- rbind(subtab1, subtab2)



    ## subtab13: Total asthma exacerbation events requiring an ER visit without steroid use per year, per participant
    var_name <- "Total asthma exacerbation events requiring an ER visit without steroid use per year, per participant"
    tab_ <- cont_stat(df_excr$excr_ER_nonsteroid[which(df_excr$excr_ER_nonsteroid != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_ER_nonsteroid, na.rm = TRUE)
    subtab4.13 <- subtab.f


    ## subtab 14: Patient had an asthma exacerbation that required a hospitalization
    var_name <- "Patient had an asthma exacerbation that required a hospitalization"
    table(df_excr$EXA_HO_E5_C23)
    # re-label
    df_excr$EXA_HO_E5_C23[which(df_excr$EXA_HO_E5_C23 == 0)] <- "No"
    df_excr$EXA_HO_E5_C23[which(df_excr$EXA_HO_E5_C23 == 1)] <- "Yes"
    df_excr$EXA_HO_E5_C23[which(df_excr$EXA_HO_E5_C23 == 99)] <- "Unknown"
    df_excr$EXA_HO_E5_C23[which(is.na(df_excr$EXA_HO_E5_C23))] <- "Unknown"
    table(df_excr$EXA_HO_E5_C23)
    df_excr$EXA_HO_E5_C23 <- factor(df_excr$EXA_HO_E5_C23, levels = c("Yes", "No", "Unknown"))
    # check if any date of exacerbation requiring hospitalization is the same as index date
    row.index <- which(df_excr$EXA_HOSTDT_E5_C23_1==df_excr$INC_NEW_STDT_E1_C3)
    # change status of exacerbation requiring hospitalization to "No" (no exacerbation post index)
    df_excr$EXA_HO_E5_C23[row.index] <- "No"
    # create summary statistic
    tab_ <- df_excr %>%
      select(EXA_HO_E5_C23) %>%
      tbl_summary(digits = list("EXA_HO_E5_C23" ~ c(0,1)),
                  label = "EXA_HO_E5_C23" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(subtab)),]
    subtab4.14 <- subtab.f

    # subtab15: Number of patients who had an asthma exacerbation that required a hospitalization, based on the frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required a hospitalization, based on the frequency, per year"
    # column of date of hospitalization
    # change date of exacerbation to NA if exacerbation occur at index date
    df_excr$EXA_HOSTDT_E5_C23_1[row.index] <- ""
    HO_date <- df_excr %>%
      select(contains("EXA_HOSTDT_E5_C23"))
    # count HO date per patient (number of exacerbation)
    df_excr$excr_HO <- colSums(apply(HO_date, 1, grepl, pattern = "-"))
    df_excr$excr_HO[which(df_excr$EXA_HO_E5_C23 == "Unknown")] <- NA
    # create column to identify at least 1 exacerbation
    excr_HO_atleast1 <- rep(NA, nrow(df_excr))
    excr_HO_atleast1[which(df_excr$excr_HO == 0 & df_excr$EXA_HO_E5_C23 == "No")] <- "No exacerbation"
    excr_HO_atleast1[which(df_excr$excr_HO >= 1 & df_excr$EXA_HO_E5_C23 == "Yes")] <- "At least 1 exacerbation"
    df_excr$excr_HO_atleast1 <- excr_HO_atleast1
    tab_ <- df_excr %>%
      filter(EXA_HO_E5_C23 != "Unknown") %>%
      group_by(excr_HO_atleast1) %>%
      summarise(n = n())
    perc <- tab_$n[1] / (tab_$n[1] + tab_$n[2]) * 100
    perc_c <- paste(tab_$n[1], " (", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of HO exacerbations
    excr_HO_cat <- rep(NA, nrow(df_excr))
    excr_HO_cat[which(df_excr$excr_HO == 1)] <- "1 exacerbation"
    excr_HO_cat[which(df_excr$excr_HO == 2)] <- "2 exacerbations"
    excr_HO_cat[which(df_excr$excr_HO >= 3)] <- ">=3 exacerbations"
    table(excr_HO_cat)
    excr_HO_cat <- factor(excr_HO_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_HO_cat <- excr_HO_cat
    tab_ <- df_excr %>%
      select(excr_HO_cat) %>%
      tbl_summary(digits = list("excr_HO_cat" ~ c(0,1)),
                  label = "excr_HO_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.15 <- rbind(subtab1, subtab2)


    ## subtab16: Total asthma exacerbation events requiring a hospitalization per year, per participant
    var_name <- "Total asthma exacerbation events requiring a hospitalization per year, per participant"
    tab_ <- cont_stat(df_excr$excr_HO[which(df_excr$excr_HO != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_HO, na.rm = TRUE)
    subtab4.16 <- subtab.f



    ## subtab17: Number of patients who had an asthma exacerbation that required a hospitalization with steroid use, based on frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required a hospitalization with steroid use, based on frequency, per year"
    # data HO with steroid 0 (No),1 (Yes),99 (Unknown/Not Reported)
    lbl <- c(0,1,99)
    # change if exacerbation appear at index date to NA
    df_excr$EXA_HOOCS_E5_C23_2[row.index] <- NA
    HO_steroid <- df_excr %>%
      select(contains("EXA_HOOCS_"))
    excr_HO_steroid <- apply(HO_steroid, 1, function(x) length(which(x==1)))
    excr_HO_steroid[which(df_excr$EXA_HO_E5_C23 != "Yes")] <- 0
    excr_HO_steroid[which(df_excr$EXA_HO_E5_C23 == "Unknown")] <- NA
    df_excr$excr_HO_steroid <- excr_HO_steroid
    tab_ <- df_excr %>%
      filter(EXA_HO_E5_C23 == "Yes" & excr_HO_steroid >= 1) %>%
      group_by(excr_HO_atleast1) %>%
      summarise(n = n())
    perc <- tab_$n / length(which(df_excr$EXA_HO_E5_C23 != "Unknown")) * 100
    perc_c <- paste(tab_$n, "(", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of HO exacerbations required steroid
    excr_HO_steroid_cat <- rep(NA, nrow(df_excr))
    excr_HO_steroid_cat[which(df_excr$excr_HO_steroid == 1)] <- "1 exacerbation"
    excr_HO_steroid_cat[which(df_excr$excr_HO_steroid == 2)] <- "2 exacerbations"
    excr_HO_steroid_cat[which(df_excr$excr_HO_steroid >= 3)] <- ">=3 exacerbations"
    excr_HO_steroid_cat[which(df_excr$EXA_HO_E5_C16 != "Yes")] <- NA
    table(excr_HO_steroid_cat)
    excr_HO_steroid_cat <- factor(excr_HO_steroid_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_HO_steroid_cat <- excr_HO_steroid_cat
    tab_ <- df_excr %>%
      select(excr_HO_steroid_cat) %>%
      tbl_summary(digits = list("excr_HO_steroid_cat" ~ c(0,1)),
                  label = "excr_HO_steroid_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.17 <- rbind(subtab1, subtab2)


    ## subtab18: Total asthma exacerbation events requiring a hospitalization with steroid use per year, per participant
    var_name <- "Total asthma exacerbation events requiring a hospitalization with steroid use per year, per participant"
    tab_ <- cont_stat(df_excr$excr_HO_steroid[which(df_excr$excr_HO_steroid != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_HO_steroid, na.rm = TRUE)
    subtab4.18 <- subtab.f


    ## subtab19: Number of patients who had an asthma exacerbation that required hospitalization without steroid use, based on frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required hospitalization without steroid use, based on frequency, per year"
    # data HO with steroid 0 (No),1 (Yes),99 (Unknown/Not Reported)
    HO_steroid <- df_excr %>%
      select(contains("EXA_HOOCS_"))
    excr_HO_nonsteroid <- apply(HO_steroid, 1, function(x) length(which(x==0)))
    excr_HO_nonsteroid[which(df_excr$EXA_HO_E5_C23 != "Yes")] <- 0
    excr_HO_nonsteroid[which(df_excr$EXA_HO_E5_C23 == "Unknown")] <- NA
    df_excr$excr_HO_nonsteroid <- excr_HO_nonsteroid
    tab_ <- df_excr %>%
      filter(EXA_HO_E5_C23 == "Yes" & excr_HO_nonsteroid >= 1) %>%
      group_by(excr_HO_atleast1) %>%
      summarise(n = n())
    perc <- tab_$n / length(which(df_excr$EXA_HO_E5_C23 != "Unknown")) * 100
    perc_c <- paste(tab_$n, "(", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of HO exacerbations required non-steroid
    excr_HO_nonsteroid_cat <- rep(NA, nrow(df_excr))
    excr_HO_nonsteroid_cat[which(df_excr$excr_HO_nonsteroid == 1)] <- "1 exacerbation"
    excr_HO_nonsteroid_cat[which(df_excr$excr_HO_nonsteroid == 2)] <- "2 exacerbations"
    excr_HO_nonsteroid_cat[which(df_excr$excr_HO_nonsteroid >= 3)] <- ">=3 exacerbations"
    excr_HO_nonsteroid_cat <- factor(excr_HO_nonsteroid_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    table(excr_HO_nonsteroid_cat)
    table(excr_HO_nonsteroid)
    df_excr$excr_HO_nonsteroid_cat <- excr_HO_nonsteroid_cat
    tab_ <- df_excr %>%
      select(excr_HO_nonsteroid_cat) %>%
      tbl_summary(digits = list("excr_HO_nonsteroid_cat" ~ c(0,1)),
                  label = "excr_HO_nonsteroid_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.19 <- rbind(subtab1, subtab2)



    ## subtab20: Total asthma exacerbation events requiring a hospitalization without steroid use per year, per participant
    var_name <- "Total asthma exacerbation events requiring a hospitalization without steroid use per year, per participant"
    tab_ <- cont_stat(df_excr$excr_HO_nonsteroid[which(df_excr$excr_HO_nonsteroid != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_HO_nonsteroid, na.rm = TRUE)
    subtab4.20 <- subtab.f

    ## subtab21: Total nights hospitalized due to an asthma exacerbation
    var_name <- "Total nights hospitalized due to an asthma exacerbation"
    # if total night hospitalized is known
    HO_night_known <- apply(df_excr[,c("EXA_TONIGHTBN_E5_C23_1", "EXA_TONIGHTBN_E5_C23_2")], 1, function(x) length(which(x==1)))
    table(HO_night_known)
    HO_night_known <- ifelse(HO_night_known >= 1, "Known", "Unknown")
    HO_night_known <- ifelse(df_excr$EXA_HO_E5_C23 != "Yes", NA, HO_night_known)
    table(HO_night_known)
    HO_night_known <- factor(HO_night_known, levels = c("Known", "Unknown"))
    df_excr$HO_night_known <- HO_night_known
    # summary stat
    tab_ <- df_excr %>%
      select(HO_night_known) %>%
      tbl_summary(digits = list("HO_night_known" ~ c(0,1)),
                  label = "HO_night_known" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",2),NA,NA), stat_0 = tab_$stat_0)
    subtab4.21 <- subtab[c(2:4),]


    ## subtab22: Number of patients and the frequency of nights hospitalized due to an asthma exacerbation, per year
    var_name <- "Number of patients and the frequency of nights hospitalized due to an asthma exacerbation, per year"
    HO_total_night <- rowSums(df_excr[,c("EXA_TONIGHT_E5_C23_1", "EXA_TONIGHT_E5_C23_2")], na.rm = TRUE)
    HO_total_night[which(is.na(df_excr$HO_night_known))] <- NA
    table(HO_total_night)
    df_excr$HO_total_night <- HO_total_night
    # total night per category
    HO_total_night_cat <- rep(NA, length(HO_total_night))
    HO_total_night_cat[which(HO_total_night == 1)] <- "1 night"
    HO_total_night_cat[which(HO_total_night == 2)] <- "2 nights"
    HO_total_night_cat[which(HO_total_night == 3)] <- "3 nights"
    HO_total_night_cat[which(HO_total_night == 4)] <- "4 nights"
    HO_total_night_cat[which(HO_total_night >= 5)] <- ">=5 nights"
    HO_total_night_cat <- factor(HO_total_night_cat, levels = c("1 night", "2 nights", "3 nights", "4 nights", ">=5 nights"))
    table(HO_total_night_cat)
    table(HO_total_night)
    df_excr$HO_total_night_cat <- HO_total_night_cat
    # category at least 1 night
    HO_total_night_atleast1 <- rep(NA, length(HO_total_night))
    HO_total_night_atleast1[which(HO_total_night >= 1)] <- "At least 1 night"
    table(HO_total_night_atleast1)
    df_excr$HO_total_night_atleast1 <- HO_total_night_atleast1
    # summary stat for at least 1 night
    n <- length(HO_total_night_atleast1[which(HO_total_night_atleast1 == "At least 1 night")])
    perc <- n / length(which(HO_night_known == "Known")) * 100
    perc_c <- paste(n, " (", round_fmt(perc,1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 night"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # summary total night per category
    tab_ <- df_excr %>%
      select(HO_total_night_cat) %>%
      tbl_summary(digits = list("HO_total_night_cat" ~ c(0,1)),
                  label = "HO_total_night_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",5),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:7),]
    subtab4.22 <- rbind(subtab1, subtab2)


    ## subtab23: Total number of nights hospitalized due to an asthma exacerbation per year, per participant
    var_name <- "Total number of nights hospitalized due to an asthma exacerbation per year, per participant"
    tab_ <- cont_stat(na.omit(df_excr$HO_total_night), var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$HO_total_night, na.rm = TRUE)
    subtab4.23 <- subtab.f



    ## subtab5: Number of patients who had an asthma exacerbation that required a physician visit and steroid prescription, based on the frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation that required a physician visit and steroid prescription, based on the frequency, per year"
    # sub data other asthma purpose
    Asthma_Purpose <- df_excr %>%
      select(contains("ASTMEDPUROTH_")) # Asthma Medication Purpose (2=exacerbation)
    # sub data if use steroid
    Steroid_Use <- df_excr %>%
      select(contains("ASTMEDAGENTOTH_")) # steroid use if coded 7
    # sub data for date of exacerbation
    date_PV <- df_excr %>%
      select(contains("ASTMEDSTOTH_")) # date of exacerbation PV
    # identify PV due to exacerbation
    excr_PV_pervisit <- matrix(NA, ncol = ncol(Asthma_Purpose), nrow = nrow(df_excr))
    for (i in 1:ncol(Asthma_Purpose)) {
      x <- Asthma_Purpose[,i] == 2 & Steroid_Use[,i] == 7
      xx <- ifelse(x == TRUE, 1, 0)
      # adjustment if date of exacerbation is the same as index date then exclude
      row.index <- which(xx == 1)
      xx[row.index] <- ifelse(date_PV[row.index,i]==df_excr$INC_NEW_STDT_E1_C3[row.index], NA, xx[row.index])
      excr_PV_pervisit[,i] <- xx
    }
    excr_PV <- rowSums(excr_PV_pervisit, na.rm = TRUE)
    table(excr_PV)
    df_excr$excr_PV <- excr_PV
    # create column to identify at least 1 exacerbation
    excr_PV_atleast1 <- rep(NA, nrow(df_excr))
    excr_PV_atleast1[which(df_excr$excr_PV == 0)] <- "No exacerbation"
    excr_PV_atleast1[which(df_excr$excr_PV >= 1)] <- "At least 1 exacerbation"
    df_excr$excr_PV_atleast1 <- excr_PV_atleast1
    tab_ <- df_excr %>%
      group_by(excr_PV_atleast1) %>%
      summarise(n = n()) %>%
      mutate(perc = n/sum(n)*100)
    perc_c <- paste(tab_$n[1], " (", round_fmt(tab_$perc[1],1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of ER exacerbations
    excr_PV_cat <- rep(NA, nrow(df_excr))
    excr_PV_cat[which(df_excr$excr_PV == 1)] <- "1 exacerbation"
    excr_PV_cat[which(df_excr$excr_PV == 2)] <- "2 exacerbations"
    excr_PV_cat[which(df_excr$excr_PV >= 3)] <- ">=3 exacerbations"
    table(excr_PV_cat)
    excr_PV_cat <- factor(excr_PV_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_PV_cat <- excr_PV_cat
    tab_ <- df_excr %>%
      select(excr_PV_cat) %>%
      tbl_summary(digits = list("excr_PV_cat" ~ c(0,1)),
                  label = "excr_PV_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.5 <- rbind(subtab1, subtab2)


    ## subtab 6: Total asthma exacerbation events requiring a physician visit and steroid prescription per year, per participant
    var_name <- "Total asthma exacerbation events requiring a physician visit and steroid prescription per year, per participant"
    tab_ <- cont_stat(df_excr$excr_PV[which(df_excr$excr_PV != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_PV)
    subtab4.6 <- subtab.f



    ## subtab1: Number of patients who had an asthma exacerbation (of any kind) based on the frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation (of any kind) based on the frequency, per year"
    excr_TOT <- df_excr$excr_ER + df_excr$excr_HO + df_excr$excr_PV
    #excr_TOT <- rowSums(df_excr[,c("excr_ER", "excr_HO", "excr_PV")], na.rm = TRUE)
    df_excr$excr_TOT <- excr_TOT
    # adjustment for ID340 and ID269
    index.row <- which(df_excr$Study.Subject.ID %in% c(340,269))
    df_excr$excr_TOT[index.row] <- df_excr$excr_ER[index.row] + df_excr$excr_PV[index.row]
    # create column to identify at least 1 exacerbation
    excr_TOT_atleast1 <- rep(NA, nrow(df_excr))
    excr_TOT_atleast1[which(df_excr$excr_TOT == 0)] <- "No exacerbation"
    excr_TOT_atleast1[which(df_excr$excr_TOT >= 1)] <- "At least 1 exacerbation"
    df_excr$excr_TOT_atleast1 <- excr_TOT_atleast1
    tab_ <- df_excr %>%
      filter(!is.na(excr_TOT_atleast1)) %>%
      group_by(excr_TOT_atleast1) %>%
      summarise(n = n()) %>%
      mutate(perc = n/sum(n)*100)
    perc_c <- paste(tab_$n[1], " (", round_fmt(tab_$perc[1],1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of TOTAL exacerbations
    excr_TOT_cat <- rep(NA, nrow(df_excr))
    excr_TOT_cat[which(df_excr$excr_TOT == 1)] <- "1 exacerbation"
    excr_TOT_cat[which(df_excr$excr_TOT == 2)] <- "2 exacerbations"
    excr_TOT_cat[which(df_excr$excr_TOT >= 3)] <- ">=3 exacerbations"
    table(excr_TOT_cat)
    excr_TOT_cat <- factor(excr_TOT_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_TOT_cat <- excr_TOT_cat
    tab_ <- df_excr %>%
      select(excr_TOT_cat) %>%
      tbl_summary(digits = list("excr_TOT_cat" ~ c(0,1)),
                  label = "excr_TOT_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.1 <- rbind(subtab1, subtab2)


    ## subtab2: Total asthma exacerbation events (of any kind) per year, per participant
    var_name <- "Total asthma exacerbation events (of any kind) per year, per participant"
    tab_ <- cont_stat(df_excr$excr_TOT[which(df_excr$excr_TOT != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_TOT, na.rm = TRUE)
    subtab4.2 <- subtab.f


    ## subtab3: Number of patients who had an asthma exacerbation (of any kind) with steroid use based on the frequency, per year
    var_name <- "Number of patients who had an asthma exacerbation (of any kind) with steroid use based on the frequency, per year"
    excr_TOT_steroid <- df_excr$excr_ER_steroid + df_excr$excr_HO_steroid + df_excr$excr_PV
    #excr_TOT_steroid <- rowSums(df_excr[,c("excr_ER_steroid", "excr_HO_steroid", "excr_PV")], na.rm = TRUE)
    df_excr$excr_TOT_steroid <- excr_TOT_steroid
    # adjustment for ID340 and ID269
    index.row <- which(df_excr$Study.Subject.ID %in% c(340,269))
    df_excr$excr_TOT_steroid[index.row] <- df_excr$excr_ER_steroid[index.row] + df_excr$excr_PV[index.row]
    table(excr_TOT_steroid)
    # create column to identify at least 1 exacerbation
    excr_TOT_steroid_atleast1 <- rep(NA, nrow(df_excr))
    excr_TOT_steroid_atleast1[which(df_excr$excr_TOT_steroid == 0)] <- "No exacerbation"
    excr_TOT_steroid_atleast1[which(df_excr$excr_TOT_steroid >= 1)] <- "At least 1 exacerbation"
    df_excr$excr_TOT_steroid_atleast1 <- excr_TOT_steroid_atleast1
    tab_ <- df_excr %>%
      filter(!is.na(excr_TOT_steroid_atleast1)) %>%
      group_by(excr_TOT_steroid_atleast1) %>%
      summarise(n = n()) %>%
      mutate(perc = n/sum(n)*100)
    perc_c <- paste(tab_$n[1], " (", round_fmt(tab_$perc[1],1), "%)", sep = "")
    subtab1 <- data.frame(`Summary Variable` = c(var_name, "At least 1 exacerbation"), Statistic = c(NA,"n(%)"), stat_0 = c(NA,perc_c))
    # create categorical variable for number of TOTAL exacerbations required steroid use
    excr_TOT_steroid_cat <- rep(NA, nrow(df_excr))
    excr_TOT_steroid_cat[which(df_excr$excr_TOT_steroid == 1)] <- "1 exacerbation"
    excr_TOT_steroid_cat[which(df_excr$excr_TOT_steroid == 2)] <- "2 exacerbations"
    excr_TOT_steroid_cat[which(df_excr$excr_TOT_steroid >= 3)] <- ">=3 exacerbations"
    table(excr_TOT_steroid_cat)
    excr_TOT_steroid_cat <- factor(excr_TOT_steroid_cat, levels = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    df_excr$excr_TOT_steroid_cat <- excr_TOT_steroid_cat
    tab_ <- df_excr %>%
      select(excr_TOT_steroid_cat) %>%
      tbl_summary(digits = list("excr_TOT_steroid_cat" ~ c(0,1)),
                  label = "excr_TOT_steroid_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA,NA), stat_0 = tab_$stat_0)
    subtab2 <- subtab[c(3:5),]
    subtab4.3 <- rbind(subtab1, subtab2)


    ## subtab4: Total asthma exacerbation events (of any kind) with steroid use per year, per participant
    var_name <- "Total asthma exacerbation events (of any kind) with steroid use per year, per participant"
    tab_ <- cont_stat(df_excr$excr_TOT_steroid[which(df_excr$excr_TOT_steroid != 0)], var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total exacerbations
    subtab.f$stat_0[1] <- sum(df_excr$excr_TOT_steroid, na.rm = TRUE)
    subtab4.4 <- subtab.f

    # combine all subtab
    table4 <- rbind(subtab4.1,
                    subtab4.2,
                    subtab4.3,
                    subtab4.4,
                    subtab4.5,
                    subtab4.6,
                    subtab4.7,
                    subtab4.8,
                    subtab4.9,
                    subtab4.10,
                    subtab4.11,
                    subtab4.12,
                    subtab4.13,
                    subtab4.14,
                    subtab4.15,
                    subtab4.16,
                    subtab4.17,
                    subtab4.18,
                    subtab4.19,
                    subtab4.20,
                    subtab4.21,
                    subtab4.22,
                    subtab4.23)


    # save file
    rownames(table4) <- NULL
    write.csv(table4, "table4.csv")



    ############## TLF Table 5: Summary of physician visit (using ADEXD and ADEC),

    # create table
    table5 <- data.frame()


    ## scheduled visit
    var_name <- "Total number of scheduled physician visits per participant"
    df_temp <- crf2[c("Study.Subject.ID","CE_ERVISCH_E5_C23")]
    names(df_temp) <- c("SUBJID", "Schedule_Visit")
    HCRU <- merge(PATIENT_ID, df_temp, by = "SUBJID", all = TRUE)
    tab_ <- cont_stat(HCRU$Schedule_Visit, "visit")
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total visits all patients
    subtab.f$stat_0[1] <- sum(HCRU$Schedule_Visit)
    table5 <- rbind(table5, subtab.f)

    ## schedule visit per category
    var_name <- "Number of patients who had scheduled physician visits based on the frequency"
    schedule_visit_cat <- rep(NA, nrow(HCRU))
    schedule_visit_cat[which(HCRU$Schedule_Visit >= 1 & HCRU$Schedule_Visit <=3)] <- "1-3 visits"
    schedule_visit_cat[which(HCRU$Schedule_Visit >= 4 & HCRU$Schedule_Visit <=6)] <- "4-6 visits"
    schedule_visit_cat[which(HCRU$Schedule_Visit >= 7 & HCRU$Schedule_Visit <=9)] <- "7-9 visits"
    schedule_visit_cat[which(HCRU$Schedule_Visit >= 10 & HCRU$Schedule_Visit <=12)] <- "10-12 visits"
    schedule_visit_cat[which(HCRU$Schedule_Visit >= 13)] <- ">= 13 visits"
    schedule_visit_cat <- factor(schedule_visit_cat, levels = c("1-3 visits", "4-6 visits", "7-9 visits", "10-12 visits", ">= 13 visits"))
    table(schedule_visit_cat)
    sum(table(schedule_visit_cat))
    HCRU$schedule_visit_cat <- schedule_visit_cat
    # create summary statistic
    tab_ <- HCRU %>%
      select(schedule_visit_cat) %>%
      tbl_summary(digits = list("schedule_visit_cat" ~ c(0,1)),
                  label = "schedule_visit_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",5),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(subtab)),]
    table5 <- rbind(table5, subtab.f)



    ## Asthma medication adherence (from adexc)
    var_name <- "ICS/LABA prescription continuity"
    # format start date
    dt <- adexd$ASTDT
    day <- as.integer(str_extract(dt, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', dt)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(dt, nchar(dt)-3))
    dt <- make_date(year, month.n, day)
    adexd$ASTDT <- dt
    # format end date
    dt <- adexd$AENDT
    day <- as.integer(str_extract(dt, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', dt)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(dt, nchar(dt)-3))
    dt <- make_date(year, month.n, day)
    adexd$AENDT <- dt
    # filter only ICS/LABA treatment
    df_adherence <- adexd %>%
      select(USUBJID, ASTDT, AENDT, TRTA, ADY, DSTERM, EXDUR) %>%
      filter(TRTA == "ICS/LABA")
    df_adherence <- rename(df_adherence, gap = ADY)
    # calculate adherence
    unique_id <- unique(df_adherence$USUBJID)
    adherence <- rep(NA, length((unique_id)))
    for (i in 1:length(unique_id)) {
      id_i <- unique_id[i]
      sub_i <- subset(df_adherence, USUBJID == id_i)
      index_gap <- which(sub_i$gap > 30)
      reason <- unique(sub_i$DSTERM)
      reason_i <- which(reason == "" |
                          reason == "Change in Dose/Frequency/etc" |
                          reason == "Out of Stock" |
                          reason == "Lost to Follow Up" |
                          reason == "Other")
      num_gap <- length(index_gap) # number of gap > 30 days
      if (num_gap == 0) { # if no gap
        adherence[i] <- "Adherent"
      } else if (num_gap > 0 & length(reason_i) > 0) {
        adherence[i] <- "Non-adherent"
      } else { # if there is a gap
        adherence[i] <- "Adherent"
      }
    }
    df_temp <- data.frame(USUBJID = unique_id, adherence)
    HCRU2 <- merge(HCRU, df_temp, by = "USUBJID", all = TRUE)
    HCRU <- HCRU2
    # summary stat
    tab_ <- HCRU %>%
      select(adherence) %>%
      tbl_summary(digits = list("adherence" ~ c(0,1)),
                  label = "adherence" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",2),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(-1,-nrow(subtab)),]
    subtab.f[2:3,1] <- c("Continuous prescriptions/refills recorded", "Interruptions/incomplete data available")
    table5 <- rbind(table5, subtab.f)


    ## Total number of unscheduled physician visits per participant
    var_name <- "Total number of unscheduled physician visits per participant"
    Unschedule_Visit <- crf2 %>% select(USUBJID, CE_ERVIUNSCH_E5_C23)
    df_temp <- merge(HCRU, Unschedule_Visit, by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    HCRU <- rename(HCRU, Unschedule_Visit = CE_ERVIUNSCH_E5_C23)
    Unschedule_Visit2 <- HCRU$Unschedule_Visit
    Unschedule_Visit2[which(HCRU$Unschedule_Visit == 0)] <- NA
    table(Unschedule_Visit2)
    tab_ <- cont_stat(na.omit(Unschedule_Visit2), "visit")
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total visits all patients
    subtab.f$stat_0[1] <- sum(HCRU$Unschedule_Visit)
    table5 <- rbind(table5, subtab.f)



    ## Number of patients who had unscheduled physician visits based on the frequency
    var_name <- "Number of patients who had unscheduled physician visits based on the frequency"
    # unschedule visit cat: none vs at least 1
    unschedule_visit_cat <- rep(NA, nrow(HCRU))
    unschedule_visit_cat[which(HCRU$Unschedule_Visit == 0)] <- "None"
    unschedule_visit_cat[which(HCRU$Unschedule_Visit > 0)] <- "At least 1 visit"
    table(unschedule_visit_cat)
    sum(table(unschedule_visit_cat))
    unschedule_visit_cat <- factor(unschedule_visit_cat, levels = c("None", "At least 1 visit"))
    table(unschedule_visit_cat)
    HCRU$unschedule_visit_cat <- unschedule_visit_cat
    # unschedule visit cat: none vs 1,2,3,4,5+ visits
    unschedule_visit_cat2 <- rep(NA, nrow(HCRU))
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit == 0)] <- NA
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit == 1)] <- "1 visit"
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit == 2)] <- "2 visits"
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit == 3)] <- "3 visits"
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit == 4)] <- "4 visits"
    unschedule_visit_cat2[which(HCRU$Unschedule_Visit >= 5)] <- ">=5 visits"
    table(unschedule_visit_cat2)
    sum(table(unschedule_visit_cat2))
    unschedule_visit_cat2 <- factor(unschedule_visit_cat2, levels = c("1 visit", "2 visits", "3 visits", "4 visits", ">=5 visits"))
    table(unschedule_visit_cat2)
    sum(table(unschedule_visit_cat2))
    HCRU$unschedule_visit_cat2 <- unschedule_visit_cat2
    # create summary statistic none vs at least 1
    tab_ <- HCRU %>%
      select(unschedule_visit_cat) %>%
      tbl_summary(digits = list("unschedule_visit_cat" ~ c(0,1)),
                  label = "unschedule_visit_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",2),NA), stat_0 = tab_$stat_0)
    subtab.f1 <- subtab[c(-1,-nrow(subtab)),]
    # create summary statistic least 1
    tab_ <- HCRU %>%
      select(unschedule_visit_cat2) %>%
      tbl_summary(digits = list("unschedule_visit_cat2" ~ c(0,1)),
                  label = "unschedule_visit_cat2" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",5),NA,NA), stat_0 = tab_$stat_0)
    subtab.f2 <- subtab[c(3:7),]
    table5 <- rbind(table5, subtab.f1, subtab.f2)


    ##ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription): from adexc
    var_name <- "ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)"
    df_treatdur <- df_adherence %>%
      group_by(USUBJID) %>%
      summarise(treatment_duration = sum(EXDUR))
    # change if treatment duration > 365 days then treatment duration = 356
    df_treatdur$treatment_duration[which(df_treatdur$treatment_duration > 365)] <- 365
    # merge to HCRU data
    df_temp <- merge(HCRU, df_treatdur, by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    treatment_duration <- HCRU$treatment_duration
    # treatment duration cat (derived)
    treatment_duration_cat <- rep(NA, length(treatment_duration))
    treatment_duration_cat[which(treatment_duration < 180)] <- "<180 days"
    treatment_duration_cat[which(treatment_duration >= 180 & treatment_duration <=270)] <- "180-270 days"
    treatment_duration_cat[which(treatment_duration > 270)] <- ">270 months"
    treatment_duration_cat <- factor(treatment_duration_cat, levels = c("<180 days", "180-270 days", ">270 months"))
    #checking
    table(treatment_duration_cat)
    sum(table(treatment_duration_cat))
    HCRU$treatment_duration_cat <- treatment_duration_cat
    # summary stat
    tab_ <- cont_stat(treatment_duration, var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table5 <- rbind(table5, subtab.f)



    ## ICS/LABA treatment duration (excluding gaps) (categorical)
    tab_ <- HCRU %>%
      select(treatment_duration_cat) %>%
      tbl_summary(digits = list("treatment_duration_cat" ~ c(0,1)),
                  label = "treatment_duration_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[3:5,]
    table5 <- rbind(table5, subtab.f)




    ##ICS/LABA treatment duration: number of months treated (including prescription gaps)
    var_name <- "ICS/LABA treatment duration: number of months treated (including prescription gaps)"
    df_treatdur <- df_adherence %>%
      group_by(USUBJID) %>%
      summarise(treatment_duration = sum(EXDUR, na.rm = TRUE),
                gap = sum(gap, na.rm = TRUE)) %>%
      mutate(treatment_duration_include_gap = (treatment_duration + gap)/30.4) %>%
      select(USUBJID, treatment_duration_include_gap)
    # change if treatment duration > 12 days then treatment duration = 12
    df_treatdur$treatment_duration_include_gap[which(df_treatdur$treatment_duration_include_gap > 12)] <- 12
    # merge to HCRU data
    df_temp <- merge(HCRU, df_treatdur, by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    treatment_duration_include_gap <- HCRU$treatment_duration_include_gap
    # treatment duration cat (derived)
    treatment_duration_include_gap_cat <- rep(NA, length(treatment_duration_include_gap))
    treatment_duration_include_gap_cat[which(treatment_duration_include_gap < 6)] <- "6 months"
    treatment_duration_include_gap_cat[which(treatment_duration_include_gap >= 6 & treatment_duration_include_gap <10)] <- "6-9 months"
    treatment_duration_include_gap_cat[which(treatment_duration_include_gap >= 10)] <- "10-12 months"
    treatment_duration_include_gap_cat <- factor(treatment_duration_include_gap_cat, levels = c("6 months", "6-9 months", "10-12 months"))
    #checking
    table(treatment_duration_include_gap_cat)
    sum(table(treatment_duration_include_gap_cat))
    HCRU$treatment_duration_include_gap_cat <- treatment_duration_include_gap_cat
    # summary stat
    tab_ <- cont_stat2(treatment_duration_include_gap, var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table5 <- rbind(table5, subtab.f)



    ## ICS/LABA treatment duration (excluding gaps) (categorical)
    tab_ <- HCRU %>%
      select(treatment_duration_include_gap_cat) %>%
      tbl_summary(digits = list("treatment_duration_include_gap_cat" ~ c(0,1)),
                  label = "treatment_duration_include_gap_cat" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[3:5,]
    table5 <- rbind(table5, subtab.f)




    ## ICS/LABA Daily dose at index
    var_name <- "ICS/LABA daily dose at index"
    # pooling index date from adsu
    df_index_date <- adsu %>%
      select(USUBJID, TRTSDT)
    # extract date of treatment
    date_treatment <- df_index_date$TRTSDT
    day <- as.integer(str_extract(date_treatment, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_treatment)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_treatment, nchar(date_treatment)-3))
    date_treatment <- make_date(year, month.n, day)
    df_index_date$TRTSDT <- date_treatment
    # pooling daily dose at index
    df_asthma_med <- adexd %>%
      filter(PARAM == "ICS/LABA Daily Dose (mcg/mcg)") %>%
      select(USUBJID, AVISIT, AVALCAT1) %>%
      filter(AVISIT == "INDEX")
    length(unique(df_asthma_med$USUBJID)) == nrow(df_asthma_med) # check if record per patient > 1
    # merge data
    df_asthma_med2 <- merge(df_index_date, df_asthma_med, by = "USUBJID", all = TRUE)[,c(1,2,4)]
    names(df_asthma_med2)[3] <- "Dose_Index"
    df_asthma_med2$Dose_Index <- factor(df_asthma_med2$Dose_Index, levels = c("Low", "Medium", "High"))
    # summary stat
    tab_ <- df_asthma_med2 %>%
      select(Dose_Index) %>%
      tbl_summary(digits = list("Dose_Index" ~ c(0,1)),
                  label = "Dose_Index" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(2:5),]
    table5 <- rbind(table5, subtab.f)

    ## ICS/LABA daily dose 6-12 months post-Index
    var_name <- "ICS/LABA daily dose 6-12 months post-Index"
    # pooling daily dose at index
    df_asthma_med_post <- adexd %>%
      filter(PARAM == "ICS/LABA Daily Dose (mcg/mcg)") %>%
      select(USUBJID, ASTDT, AENDT, AVISIT, AVALCAT1)
    # merge with treatment initiated date
    df_temp <- merge(df_asthma_med_post, df_index_date, by = "USUBJID", all = TRUE)
    # calculate number of months in post index
    df_temp$dur_post <- as.numeric(difftime(df_temp$AENDT, df_temp$TRTSDT, units = "days") / 30)
    df_temp$dur_post <- ifelse(df_temp$dur < 6, ceiling(df_temp$dur), floor((df_temp$dur)))
    # Filter daily dose 6-12 months post-index and take the last (the most recent) observation
    daily_dose_post6to12m <- df_temp  %>%
      filter(dur_post >= 6) %>%
      group_by(USUBJID) %>%
      arrange(ASTDT) %>%
      slice_tail(n = 1)
    names(daily_dose_post6to12m)[5] <- "Dose_Post6to12"
    df_asthma_med3 <- merge(df_asthma_med2, daily_dose_post6to12m[,c("USUBJID","Dose_Post6to12")], by = "USUBJID", all = TRUE)
    df_asthma_med2 <- df_asthma_med3
    remove(df_asthma_med3)
    df_asthma_med2$Dose_Post6to12 <- factor(df_asthma_med2$Dose_Post6to12, levels = c("Low","Medium","High"))
    # summary stat
    tab_ <- df_asthma_med2 %>%
      select(Dose_Post6to12) %>%
      tbl_summary(digits = list("Dose_Post6to12" ~ c(0,1)),
                  label = "Dose_Post6to12" ~ var_name ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label, Statistic = c(NA,NA, rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(2:5),]
    table5 <- rbind(table5, subtab.f)


    ## sort adexd by USUBJID and ASDTD and create discontinuation variable
    adexd2 <- adexd %>%
      group_by(USUBJID) %>%
      arrange(ASTDT) %>%
      mutate(
        lead_ADY = lead(ADY),
        discontinue1 = ifelse(lead_ADY > 30 | DSTERM != "", "Discontinue", NA),
        discontinue2 = ifelse(lead_ADY <= 30, "", discontinue1),
        discontinue3 = ifelse(is.na(lead_ADY) & DSTERM != "", "Discontinue", discontinue2),
        gap = ifelse(lead_ADY > 30, "gap>30days", "")) %>%
      select(-lead_ADY, -discontinue1, -discontinue2)
    adexd2 <- rename(adexd2, discontinue = discontinue3)
    adexd2$discontinue[which(is.na(adexd2$discontinue))] <- ""


    ## Number of patients and how ICS/LABA daily dose was changed (if any) during the study period
    var_name <- "Number of patients and how ICS/LABA daily dose was changed (if any) during the study period"
    df_dosechange <- adexd2 %>%
      filter(AVALCAT3 != "") %>%
      select(USUBJID, AVALCAT3, AVALCAT2)
    df_dosechange$AVALCAT3 <- factor(df_dosechange$AVALCAT3, levels = c("No change",
                                                                        "One change, increased dose",
                                                                        "One change, decreased dose",
                                                                        "Experienced multiple changes"))
    # summary stat
    tab_ <- df_dosechange %>%
      select(AVALCAT3) %>%
      tbl_summary(digits = list("AVALCAT3" ~ c(0,1)),
                  label = "AVALCAT3" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    tab1 <- tab_[3:7,]
    # summary stat for sub-categories of no change
    df_nochange <- df_dosechange %>%
      filter(AVALCAT3 == "No change")
    df_nochange$AVALCAT4 <- NA
    df_nochange$AVALCAT4[which(df_nochange$AVALCAT2 == "Low-Low")] <- "Remained low"
    df_nochange$AVALCAT4[which(df_nochange$AVALCAT2 == "Medium-Medium")] <- "Remained medium"
    df_nochange$AVALCAT4[which(df_nochange$AVALCAT2 == "High-High")] <- "Remained high"
    df_nochange$AVALCAT4 <- factor(df_nochange$AVALCAT4, levels = c("Remained low",
                                                                    "Remained medium",
                                                                    "Remained high"))
    tab_ <- df_nochange %>%
      select(AVALCAT4) %>%
      tbl_summary(digits = list("AVALCAT4" ~ c(0,1)),
                  label = "AVALCAT4" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    tab2 <- tab_[4:6,]
    #combine tab1 and tab2
    tab_ <- rbind(tab1, tab2)[c(1:2,6:8,3:5),]
    subtab <- data.frame(`Summary Variable` = tab_$label,
                         Statistic = c(NA, rep("n(%)",7)),
                         stat_0 = tab_$stat_0)
    table5 <- rbind(table5, subtab)


    ## Final patient status and discontinuation of ICS/LABA 6-12 months post-index (version 1)
    var_name <- "Final patient status and discontinuation of ICS/LABA 6-12 months post-index"
    unique_ID <- unique(adexd2$USUBJID)
    patient_status <- rep(NA, length(unique_ID))
    for (i in 1:length(unique_ID)) {
      id_i <- unique_ID[i]
      subdat_i <- adexd2 %>%
        filter(USUBJID == id_i) %>%
        arrange(ASEQ)
      n_i <- nrow(subdat_i)
      duration_index_to_post <- as.numeric(difftime(subdat_i$AENDT[n_i],subdat_i$ASTDT[1]))
      if (sum(subdat_i$discontinue == "") == n_i &
          duration_index_to_post > 365 - 30) { # no discontinuation recorded
        patient_status[i] <- "No discontinuation recorded"
      } else if (sum(subdat_i$DSTERM[1:(n_i-1)] == "") == (n_i - 1) &
                 subdat_i$DSTERM[n_i] == "Change in Dose/Frequency/etc" &
                 duration_index_to_post >= 365 - 30) { #In the event a patient has “dose change” recorded as a reason for discontinuation on their final prescription
        patient_status[i] <- "No discontinuation recorded"
      } else if (sum(subdat_i$discontinue[1:(n_i-1)] == "Discontinue") > 0 &
                 (subdat_i$DSTERM[n_i] == "" | subdat_i$DSTERM[n_i] == "Change in Dose/Frequency/etc") &
                 duration_index_to_post >= 365 - 30) {# had gaps
        patient_status[i] <- "Patient had gap(s) in treatment, but restarted ICS/LABA and continued on treatment through 12 months"
      } else if ((sum(subdat_i$discontinue[1:(n_i-1)] == "Discontinue") > 0 | subdat_i$DSTERM[n_i] == "") &
                 (subdat_i$DSTERM[n_i] == "" | subdat_i$DSTERM[n_i] == "Change in Dose/Frequency/etc") &
                 duration_index_to_post < 365 - 30) { # if as above, but last prescription < 11 months then lost to follow up
        patient_status[i] <- "Yes, patient was lost to follow-up prior to 12th month or stopped due to out-of-stock or other reasons"
      } else if (subdat_i$DSTERM[n_i] == "Lost to Follow Up" |
                 subdat_i$DSTERM[n_i] == "Other" |
                 subdat_i$DSTERM[n_i] == "Out of Stock") {
        patient_status[i] <- "Yes, patient was lost to follow-up prior to 12th month or stopped due to out-of-stock or other reasons"
      } else if (subdat_i$DSTERM[n_i] == "Patient Controlled/Stepped down ICS/LABA stopped" |
                 subdat_i$DSTERM[n_i] == "Adverse Event") {
        patient_status[i] <- "Yes, patient was controlled/stepped down or experienced an adverse event"
      }
    }
    df_patientstatus <- data.frame(USUBJID = unique_ID, patient_status)
    # merge to HCRU
    df_temp <- merge(HCRU, df_patientstatus, by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    HCRU$patient_status <- factor(HCRU$patient_status, levels = c("Yes, patient was controlled/stepped down or experienced an adverse event",
                                                                  "Yes, patient was lost to follow-up prior to 12th month or stopped due to out-of-stock or other reasons",
                                                                  "Patient had gap(s) in treatment, but restarted ICS/LABA and continued on treatment through 12 months",
                                                                  "No discontinuation recorded"))
    HCRU$patient_status2 <- ifelse(HCRU$patient_status == "Yes, patient was lost to follow-up prior to 12th month or stopped due to out-of-stock or other reasons" |
                                     HCRU$patient_status == "Yes, patient was controlled/stepped down or experienced an adverse event", "Patient Discontinued ICS/LABA", as.character(HCRU$patient_status))
    HCRU$patient_status2 <- factor(HCRU$patient_status2, levels = c("Patient Discontinued ICS/LABA",
                                                                    "Patient had gap(s) in treatment, but restarted ICS/LABA and continued on treatment through 12 months",
                                                                    "No discontinuation recorded"))
    # summary stat
    tab_ <- HCRU %>%
      select(patient_status2) %>%
      tbl_summary(digits = list("patient_status2" ~ c(0,1)),
                  label = "patient_status2" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    tab1 <- tab_[2:5,]
    # summary stat for sub-categories discontinued
    tab_ <- HCRU %>%
      filter(patient_status2 == "Patient Discontinued ICS/LABA") %>%
      select(patient_status) %>%
      tbl_summary(digits = list("patient_status" ~ c(0,1)),
                  label = "patient_status" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    tab2 <- tab_[3:4,]
    #combine tab1 and tab2
    tab_ <- rbind(tab1, tab2)[c(1:2,5:6,3:4),]
    subtab <- data.frame(`Summary Variable` = tab_$label,
                         Statistic = c(NA, rep("n(%)",5)),
                         stat_0 = tab_$stat_0)
    #table5 <- rbind(table5, subtab)


    ## Final patient status, gaps in treatment and discontinuation of ICS/LABA 6-12 months post-index  (version 2)
    var_name <- "Final patient status, gaps in treatment and discontinuation of ICS/LABA 6-12 months post-index"
    unique_ID <- unique(adexd2$USUBJID)
    patient_status2 <- rep(NA, length(unique_ID))
    for (i in 1:length(unique_ID)) {
      id_i <- unique_ID[i]
      subdat_i <- adexd2 %>%
        filter(USUBJID == id_i) %>%
        arrange(ASEQ)
      n_i <- nrow(subdat_i)
      duration_index_to_post <- as.numeric(difftime(subdat_i$AENDT[n_i],subdat_i$ASTDT[1]))
      if (duration_index_to_post < 365 - 30) { # Patient had no record of ICS/LABA prescription at 12 months (±30 day) post-index
        patient_status2[i] <- "Patient had no record of ICS/LABA prescription at 12 months (±30 day) post-index"
      } else if (length(which(subdat_i$gap == "gap>30days")) > 0) {
        patient_status2[i] <- "Patient had an ICS/LABA prescription at 12 months (±30 day) post-Index, but experienced >30 day gap(s) in treatment"
      } else if (length(which(subdat_i$gap == "gap>30days")) == 0) {
        patient_status2[i] <- "Patient had a prescription at 12 months (±30 day) post-Index and no >30 day gaps in treatment"
      }
    }

    df_patientstatus <- data.frame(USUBJID = unique_ID, patient_status3 = patient_status2)
    # merge to HCRU
    df_temp <- merge(HCRU, df_patientstatus, by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    HCRU$patient_status3 <- factor(HCRU$patient_status3, levels = c("Patient had no record of ICS/LABA prescription at 12 months (±30 day) post-index",
                                                                    "Patient had an ICS/LABA prescription at 12 months (±30 day) post-Index, but experienced >30 day gap(s) in treatment",
                                                                    "Patient had a prescription at 12 months (±30 day) post-Index and no >30 day gaps in treatment"))
    # summary stat
    tab_ <- HCRU %>%
      select(patient_status3) %>%
      tbl_summary(digits = list("patient_status3" ~ c(0,1)),
                  label = "patient_status3" ~ var_name) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(`Summary Variable` = tab_$label,
                         Statistic = c(NA,NA, rep("n(%)",3),NA),
                         stat_0 = tab_$stat_0)
    subtab.f <- subtab[2:5,]
    table5 <- rbind(table5, subtab.f)





    ##All reason(s) recorded for ICS/LABA discontinuation/medication gap during the study period
    var_name <- "All reason(s) recorded for ICS/LABA discontinuation/medication gap during the study period"
    # define Lost to follow up (>30 day gap in treatment)
    adexd2$DSTERM2 <- adexd2$DSTERM
    adexd2 <- adexd2 %>%
      group_by(USUBJID) %>%
      mutate(gap_lead = lead(ADY))
    adexd2$DSTERM2[which(adexd2$DSTERM == "Lost to Follow Up" &
                           adexd2$ADY > 30)] <- "Lost to follow up (>30 day gap in treatment)"
    table(adexd2$DSTERM2)
    adexd2$DSTERM2 <- factor(adexd2$DSTERM2, levels = c("Change in Dose/Frequency/etc",
                                                        "Patient Controlled/Stepped down ICS/LABA stopped",
                                                        "Adverse events(s)",
                                                        "Out of Stock",
                                                        "Lost to follow up (>30 day gap in treatment)",
                                                        "Other"))

    subtab <- table(adexd2$DSTERM2)
    subtab2 <- table(adexd2$gap)[2]
    subtab3 <- c(subtab, subtab2)
    subtab.p <- c()
    n_discontinue <- nrow(HCRU)
    for (i in 1:length(subtab3)) {
      subtab.p <- c(subtab.p,round_fmt(subtab3[i] / n_discontinue * 100,1))
    }
    stat_0 <- paste(subtab3, " (", subtab.p,"%)", sep = "")
    subtab.f <- data_frame(`Summary.Variable` = c(var_name,names(subtab3)), Statistic = c(NA,rep("n(%)",length(stat_0))), stat_0 = c(NA,stat_0))
    subtab.f2 <- subtab.f[c(1:7),]
    #subtab.f2[6,1] <- "Patient did not visit hospital for >30 days"
    table5 <- rbind(table5, subtab.f2)


    n_denom <- nrow(HCRU)
    ## Number of patients who were prescribed other asthma medications (in addition to ICS/LABA) at Index
    yes_other_asthma_medication <- sum(crf2$ASTMEDOTH_E6_C29)
    var_name <- "Number of patients who were prescribed other asthma medications (in addition to ICS/LABA) at Index"
    sub_dt <- adec %>%
      filter(PARAM == "Other Asthma Medications (Dose)") %>%
      select(USUBJID, ECADJ, AVISIT, TRTA) %>%
      filter(AVISIT == "INDEX") %>%
      filter(TRTA == "Short-acting bronchodilator (SABA)" |
               TRTA == "ICS" |
               TRTA == "LAMA" |
               TRTA == "LAMA/LABA" |
               TRTA == "Theophylline/Aminophylline (xanthine)" |
               TRTA == "Other medications for asthma")
    n_total <- length(unique(sub_dt$USUBJID))
    n_prop <- round_fmt(n_total / n_denom * 100, 1)
    subtab <- table(sub_dt$ECADJ)
    subtab1 <- subtab[2]
    subtab2 <- subtab[4]
    subtab <- c(n_total, subtab1, subtab2)
    subtab1.p <- round_fmt(subtab1/n_total * 100, 1)
    subtab2.p <- round_fmt(subtab2/n_total * 100, 1)
    subtab.p <- c(n_prop, subtab1.p, subtab2.p)
    stat_0 <- paste(subtab, " (", subtab.p,"%)", sep = "")
    subtab.f <- data_frame(`Summary.Variable` = c(var_name,
                                                  "Number of patients prescribed maintenance medications at index",
                                                  "Number of patients prescribed reliever medications at index"), Statistic = c(rep("n(%)",length(stat_0))), stat_0 = stat_0)
    table5 <- rbind(table5, subtab.f[c(1,3,2),])


    n_p <- subtab.f[1,3]
    ## Number of patients and type of other asthma medications prescribed at Index
    var_name <- "Number of patients and type of other asthma medications prescribed at Index"
    sub_dt <- adec %>%
      filter(PARAM == "Other Asthma Medications (Dose)") %>%
      select(USUBJID, ECADJ, AVISIT, TRTA) %>%
      filter(AVISIT == "INDEX") %>%
      filter(TRTA == "Short-acting bronchodilator (SABA)" |
               TRTA == "ICS" |
               TRTA == "LAMA" |
               TRTA == "LAMA/LABA" |
               TRTA == "Theophylline/Aminophylline (xanthine)" |
               TRTA == "Other medications for asthma")
    sub_dt$TRTA <- factor(sub_dt$TRTA, levels = c("Short-acting bronchodilator (SABA)",
                                                  "ICS",
                                                  "LABA",
                                                  "LAMA",
                                                  "LAMA/LABA",
                                                  "Theophylline/Aminophylline (xanthine)",
                                                  "Biologics",
                                                  "Corticosteroids (non-exacerbation related)",
                                                  "Other medications for asthma"))
    subtab <- table(sub_dt$TRTA)
    subtab.p <- c()
    for (i in 1:length(subtab)) {
      subtab.p <- c(subtab.p, round_fmt(subtab[i] / n_total * 100,1))
    }
    stat_0 <- paste(subtab, " (", subtab.p,"%)", sep = "")
    subtab.f <- data_frame(`Summary.Variable` = c(var_name,names(subtab)), Statistic = c(NA,rep("n(%)",length(stat_0))), stat_0 = c(NA,stat_0))
    subtab.f[1,3] <- n_p
    table5 <- rbind(table5, subtab.f)

    ## Number of patients and type of other asthma medications prescribed Post-Index
    var_name <- "Number of patients and type of other asthma medications prescribed Post-Index"
    sub_dt <- adec %>%
      filter(PARAM == "Other Asthma Medications (Dose)") %>%
      select(USUBJID, ECADJ, AVISIT, TRTA) %>%
      filter(AVISIT == "POST-INDEX") %>%
      filter(TRTA == "Short-acting bronchodilator (SABA)" |
               TRTA == "ICS" |
               TRTA == "LAMA" |
               TRTA == "LAMA/LABA" |
               TRTA == "Theophylline/Aminophylline (xanthine)" |
               TRTA == "Other medications for asthma")
    n_total <- length(unique(sub_dt$USUBJID))
    n_prop <- round_fmt(n_total / n_denom * 100, 1)
    stat_0_tot <- paste(n_total, " (", n_prop,"%)", sep = "")
    # data frame to calculate other asthma medication per patient
    sub_dt2 <- unique(sub_dt)
    sub_dt2$TRTA <- factor(sub_dt2$TRTA, levels = c("Short-acting bronchodilator (SABA)",
                                                    "ICS",
                                                    "LABA",
                                                    "LAMA",
                                                    "LAMA/LABA",
                                                    "Theophylline/Aminophylline (xanthine)",
                                                    "Biologics",
                                                    "Corticosteroids (non-exacerbation related)",
                                                    "Other medications for asthma"))
    subtab <- table(sub_dt2$TRTA)
    subtab.p <- c()
    for (i in 1:length(subtab)) {
      subtab.p <- c(subtab.p, round_fmt(subtab[i] / n_total * 100,1))
    }
    stat_0_ <- paste(subtab, " (", subtab.p,"%)", sep = "")
    stat_0 <- c(stat_0_tot, stat_0_)
    subtab.f <- data_frame(`Summary.Variable` = c(var_name,names(subtab)), Statistic = c(rep("n(%)",length(stat_0))), stat_0 = c(stat_0))
    table5 <- rbind(table5, subtab.f)


    ## Number of other asthma medications per year post-index, per participant
    var_name <- "Number of other asthma medications per year post-index, per participant"
    sub_dt2 <- sub_dt %>%
      select(USUBJID, TRTA) %>%
      unique() %>%
      group_by(USUBJID) %>%
      summarise(n = n())
    tab_ <- cont_stat(sub_dt2$n, var_name)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total medications all patients
    subtab.f$stat_0[1] <- sum(sub_dt2$n)
    table5 <- rbind(table5, subtab.f)


    ## Duration (months) of other asthma medications prescribed (from index to 6-12 months post-index), per participant
    var_name <- "Duration (months) of other asthma medications prescribed (from index to 6-12 months post-index), per participant"
    table5 <- rbind(table5, c(var_name,NA,NA))
    # combine index date from adfa to adec
    adec2 <- merge(adec[,c("USUBJID", "TRTA", "ASTDT", "AENDT", "ECDUR")], adfa[,c("USUBJID", "TRTSDT")], by = "USUBJID", all.x = TRUE)
    ## format date for TRTSDT
    date_ <- adec2$TRTSDT
    day <- as.integer(str_extract(date_, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_, nchar(date_)-3))
    adec2$TRTSDT <- make_date(year, month.n, day)
    ## format date for ASTDT
    date_ <- adec2$ASTDT
    day <- as.integer(str_extract(date_, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_, nchar(date_)-3))
    adec2$ASTDT <- make_date(year, month.n, day)
    ## format date for AENDT
    date_ <- adec2$AENDT
    day <- as.integer(str_extract(date_, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_, nchar(date_)-3))
    adec2$AENDT <- make_date(year, month.n, day)
    adec2$ECDUR2 <- NA
    ## calculate ECDUR version 2 by considering index date
    for (i in 1:nrow(adec2)) {
      #int_i <- interval(adec2$ASTDT[i], adec2$AENDT[i])
      if (!is.na(adec2$ECDUR[i])) {
        date_p12m <- adec2$TRTSDT[i] + 365
        if (adec2$ASTDT[i] >= adec2$TRTSDT[i] & adec2$AENDT[i] <= date_p12m) {
          adec2$ECDUR2[i] <- difftime(adec2$AENDT[i], adec2$ASTDT[i], units = "days")
        } else if (adec2$ASTDT[i] >= adec2$TRTSDT[i] & adec2$AENDT[i] > date_p12m) {
          adec2$ECDUR2[i] <- difftime(date_p12m, adec2$ASTDT[i], units = "days")
        } else if (adec2$ASTDT[i] < adec2$TRTSDT[i] & adec2$AENDT[i] > date_p12) {
          adec2$ECDUR2[i] <- difftime(date_p12m, adec2$TRTSDT[i], units = "days")
        } else if (adec2$ASTDT[i] < adec2$TRTSDT[i] & adec2$AENDT[i] <= date_p12) {
          adec2$ECDUR2[i] <- difftime(adec2$AENDT[i], adec2$TRTSDT[i], units = "days")
        }
      }
    }

    # manual adjustment for ID with special cases
    #ID = 21793201240, take the highest ECDUR2
    adec2[which(adec2$USUBJID == "21793201240"),]
    adec2$ECDUR2[which(adec2$USUBJID == "21793201240")]
    i1 <- which(adec2$USUBJID == "21793201240")[2]
    #ID = 21793201224, the highest ECDUR in index and post-index
    adec2[which(adec2$USUBJID == "21793201224"),]
    adec2$ECDUR2[which(adec2$USUBJID == "21793201224")]
    i2 <- which(adec2$USUBJID == "21793201224")[-c(2,5)]
    #ID = 21793201367, the highest ECDUR in index and post-index
    adec2[which(adec2$USUBJID == "21793201367"),]
    adec2$ECDUR2[which(adec2$USUBJID == "21793201367")]
    i3 <- which(adec2$USUBJID == "21793201367")[2]
    #ID = 21793201222, the highest ECDUR in index and post-index
    adec2[which(adec2$USUBJID == "21793201222"),]
    adec2$ECDUR2[which(adec2$USUBJID == "21793201222")]
    i4 <- which(adec2$USUBJID == "21793201222")[1]
    # index to remove
    index_to_remove <- c(i1,i2,i3,i4)
    adec3 <- adec2[-index_to_remove,]
    sub_dt <- adec3 %>%
      select(USUBJID, TRTA, ECDUR2) %>%
      group_by(USUBJID, TRTA) %>%
      unique() %>%
      summarise(total_duration = sum(ECDUR2) / 30.4) #duration in months



    trta <- c("Short-acting bronchodilator (SABA)",
              "ICS",
              "LABA",
              "LAMA",
              "LAMA/LABA",
              "Theophylline/Aminophylline (xanthine)",
              "Biologics",
              "Corticosteroids (non-exacerbation related)",
              "Other medications for asthma")
    for (i in 1:length(trta)) {
      sub_dt_per_treat <- sub_dt %>% filter(TRTA == trta[i])
      if (nrow(sub_dt_per_treat) > 0) {
        tab_ <- cont_stat2((sub_dt_per_treat$total_duration), trta[i])
        subtab <- data.frame(`Summary Variable` = c(trta[i], rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
        subtab.f <- subtab[-1,]
        subtab.f[1,1] <- trta[i]
        # change summary stat n to total duration across all patient
        #subtab.f[1,3] <- round(sum(sub_dt_per_treat$total_duration),0)
        table5 <- rbind(table5, subtab.f)
      } else {
        tab_ <- cont_stat2(na.omit(sub_dt_per_treat$total_duration), trta[i])
        subtab <- data.frame(`Summary Variable` = c(trta[i], rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
        subtab.f <- subtab[-1,]
        subtab.f[1,1] <- trta[i]
        subtab.f[6:7,3] <- NA
        table5 <- rbind(table5, subtab.f)
      }
    }

    # save file
    rownames(table5) <- NULL
    write.csv(table5, "table5.csv")





    ############## TLF Table 6: Summary of concomitant comorbidity condition (adcm)

    # create table
    table6 <- c()

    ## Number of patients with concomitant medication data available
    var_name <- "Number of patients with concomitant medication data available"
    N <- n_denom
    N_cc <- length(unique(adcm$USUBJID))
    n1_yes <- N_cc
    n1_no <- N - n1_yes
    p1_yes <- round_fmt(n1_yes / N * 100,1)
    p1_no <- round_fmt(n1_no / N * 100, 1)
    stat_0 <- paste(c(n1_yes, n1_no), " (", c(p1_yes, p1_no),"%)", sep = "")
    subtab <- data_frame(Characteristic = c("n", var_name,"Yes","No"),
                         Statistic = c("N",NA,rep("n(%)",2)),
                         stat_1 = c(N,NA,stat_0),
                         stat_2 = c(N,NA,stat_0),
                         stat_3 = c(N,NA,stat_0))
    table6 <- rbind(table6, subtab)


    ## Number of patients with concomitant comorbidity medication
    var_name <- "Number of patients with concomitant comorbidity medication"
    cm_index <- adcm %>%
      filter(EPOCH == "BASELINE") %>%
      select(USUBJID, CMTRT, CMINDC, TRTSDT, ASTDT, AENDT)
    cm_post <- adcm %>%
      filter(EPOCH == "CONTINUATION TREATMENT" | CMSTRF == "DURING/AFTER") %>%
      select(USUBJID, CMTRT, CMINDC, TRTSDT, ASTDT, AENDT)
    cm_ongoing <- adcm %>%
      filter(CMENRF == "ONGOING") %>%
      select(USUBJID, CMTRT, CMINDC, TRTSDT, ASTDT, AENDT)
    n1_yes <- length(unique(cm_index$USUBJID))
    n2_yes <- length(unique(cm_post$USUBJID))
    n3_yes <- length(unique(cm_ongoing$USUBJID))
    n1_no <- N_cc - n1_yes
    n2_no <- N_cc - n2_yes
    n3_no <- N_cc - n3_yes
    p1_yes <- round_fmt(n1_yes / N_cc * 100,1)
    p2_yes <- round_fmt(n2_yes / N_cc * 100,1)
    p3_yes <- round_fmt(n3_yes / N_cc * 100,1)
    p1_no <- round_fmt(n1_no / N_cc * 100,1)
    p2_no <- round_fmt(n2_no / N_cc * 100,1)
    p3_no <- round_fmt(n3_no / N_cc * 100,1)
    stat_1 <- paste(c(n1_yes, n1_no), " (", c(p1_yes, p1_no),"%)", sep = "")
    stat_2 <- paste(c(n2_yes, n2_no), " (", c(p2_yes, p2_no),"%)", sep = "")
    stat_3 <- paste(c(n3_yes, n3_no), " (", c(p3_yes, p3_no),"%)", sep = "")
    subtab <- data_frame(Characteristic = c(var_name,"Yes","No"),
                         Statistic = c(NA,rep("n(%)",2)),
                         stat_1 = c(NA,stat_1),
                         stat_2 = c(NA,stat_2),
                         stat_3 = c(NA,stat_3))
    table6 <- rbind(table6, subtab)


    ## Number of patients and concomitant comorbidity medications prescribed
    var_name <- "Number of patients and concomitant comorbidity medications prescribed"
    # index
    number_ccm1 <- cm_index %>%
      group_by(USUBJID) %>%
      summarise(n = n())
    atleast1 <- nrow(number_ccm1)
    atleast1.p <- round_fmt(atleast1 / N_cc * 100,1)
    ccm1 <- length(which(number_ccm1$n == 1))
    ccm2 <- length(which(number_ccm1$n == 2))
    ccm3 <- length(which(number_ccm1$n > 2))
    ccm <- c(ccm1, ccm2, ccm3)
    ccm1.p <- round_fmt(ccm1 / atleast1 * 100, 1)
    ccm2.p <- round_fmt(ccm2 / atleast1 * 100, 1)
    ccm3.p <- round_fmt(ccm3 / atleast1 * 100, 1)
    stat_1 <- paste(c(atleast1, ccm), " (", c(atleast1.p, ccm1.p, ccm2.p, ccm3.p),"%)", sep = "")
    # post
    number_ccm2 <- cm_post %>%
      group_by(USUBJID) %>%
      summarise(n = n())
    atleast1 <- nrow(number_ccm2)
    atleast1.p <- round_fmt(atleast1 / N_cc * 100,1)
    ccm1 <- length(which(number_ccm2$n == 1))
    ccm2 <- length(which(number_ccm2$n == 2))
    ccm3 <- length(which(number_ccm2$n > 2))
    ccm <- c(ccm1, ccm2, ccm3)
    ccm1.p <- round_fmt(ccm1 / atleast1 * 100, 1)
    ccm2.p <- round_fmt(ccm2 / atleast1 * 100, 1)
    ccm3.p <- round_fmt(ccm3 / atleast1 * 100, 1)
    stat_2 <- paste(c(atleast1, ccm), " (", c(atleast1.p, ccm1.p, ccm2.p, ccm3.p),"%)", sep = "")
    # ongpoing
    number_ccm3 <- cm_ongoing %>%
      group_by(USUBJID) %>%
      summarise(n = n())
    atleast1 <- nrow(number_ccm3)
    atleast1.p <- round_fmt(atleast1 / N_cc * 100,1)
    ccm1 <- length(which(number_ccm3$n == 1))
    ccm2 <- length(which(number_ccm3$n == 2))
    ccm3 <- length(which(number_ccm3$n > 2))
    ccm <- c(ccm1, ccm2, ccm3)
    ccm1.p <- round_fmt(ccm1 / atleast1 * 100, 1)
    ccm2.p <- round_fmt(ccm2 / atleast1 * 100, 1)
    ccm3.p <- round_fmt(ccm3 / atleast1 * 100, 1)
    stat_3 <- paste(c(atleast1, ccm), " (", c(atleast1.p, ccm1.p, ccm2.p, ccm3.p),"%)", sep = "")
    subtab <- data_frame(Characteristic = c(var_name,"At least 1","1", "2", ">=3"),
                         Statistic = c(NA,rep("n(%)",4)),
                         stat_1 = c(NA,stat_1),
                         stat_2 = c(NA,stat_2),
                         stat_3 = c(NA,stat_3))
    table6 <- rbind(table6, subtab)


    ## Total number of concomitant comorbidity medications taken per participant
    var_name <- "Total number of concomitant comorbidity medications taken per participant"
    #index
    tab_ <- cont_stat(number_ccm1$n, "CCM")
    subtab <- data.frame(`Characteristic` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total medications all patients
    subtab.f$stat_0[1] <- sum(number_ccm1$n)
    subtab.f1 <- subtab.f
    #post
    tab_ <- cont_stat(number_ccm2$n, "CCM")
    subtab <- data.frame(`Characteristic` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total medications all patients
    subtab.f$stat_0[1] <- sum(number_ccm2$n)
    subtab.f2 <- subtab.f
    #ongoing
    tab_ <- cont_stat(number_ccm3$n, "CCM")
    subtab <- data.frame(`Characteristic` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    # stat n = total medications all patients
    subtab.f$stat_0[1] <- sum(number_ccm3$n)
    subtab.f3 <- subtab.f
    subtab.f <- data.frame(subtab.f1[,1:2],
                           stat_1 = subtab.f1$stat_0,
                           stat_2 = subtab.f2$stat_0,
                           stat_3 = subtab.f3$stat_0)
    table6 <- rbind(table6, subtab.f)


    ## Distribution of indications based on the use of concomitant comorbidity medications
    var_name <- "Distribution of indications based on the use of concomitant comorbidity medications"
    # index
    any_of_below <- nrow(number_ccm1)
    any_of_below.p <- any_of_below / N_cc *100
    dist_indication <- cm_index %>%
      select(USUBJID, CMINDC) %>%
      unique() %>%
      group_by(CMINDC) %>%
      summarise(n=n(),
                prop = n/any_of_below*100)
    df.header <- data.frame(CMINDC = c(var_name, "Any of below"), n = c(NA, any_of_below), prop = c(NA, any_of_below.p))
    subtab <- rbind(df.header, dist_indication)
    prop.round <- rep(NA, nrow(subtab))
    for (i in 1:nrow(subtab)) {
      prop.round[i] <- round_fmt(subtab$pro[i], digits = 1)
    }
    subtab$prop.round <- prop.round
    subtab$stat_0 <- paste(subtab$n, " (", subtab$prop.round, "%)", sep = "")
    subtab2 <- data.frame(Characteristic = subtab$CMINDC, Statistic = c(NA,rep("n(%)", nrow(subtab)-1)), stat_0 = subtab$stat_0)
    subtab2$stat_0[1] <- NA
    subtab2$Characteristic[3:nrow(subtab2)] <- str_to_title(subtab2$Characteristic[3:nrow(subtab2)])
    subtab.f1 <- subtab2
    # post
    any_of_below <- nrow(number_ccm2)
    any_of_below.p <- any_of_below / N_cc *100
    dist_indication <- cm_post %>%
      select(USUBJID, CMINDC) %>%
      unique() %>%
      group_by(CMINDC) %>%
      summarise(n=n(),
                prop = n/any_of_below*100)
    df.header <- data.frame(CMINDC = c(var_name, "Any of below"), n = c(NA, any_of_below), prop = c(NA, any_of_below.p))
    subtab <- rbind(df.header, dist_indication)
    prop.round <- rep(NA, nrow(subtab))
    for (i in 1:nrow(subtab)) {
      prop.round[i] <- round_fmt(subtab$pro[i], digits = 1)
    }
    subtab$prop.round <- prop.round
    subtab$stat_0 <- paste(subtab$n, " (", subtab$prop.round, "%)", sep = "")
    subtab2 <- data.frame(Characteristic = subtab$CMINDC, Statistic = c(NA,rep("n(%)", nrow(subtab)-1)), stat_0 = subtab$stat_0)
    subtab2$stat_0[1] <- NA
    subtab2$Characteristic[3:nrow(subtab2)] <- str_to_title(subtab2$Characteristic[3:nrow(subtab2)])
    subtab.f2 <- subtab2
    # ongoing
    any_of_below <- nrow(number_ccm3)
    any_of_below.p <- any_of_below / N_cc *100
    dist_indication <- cm_ongoing %>%
      select(USUBJID, CMINDC) %>%
      unique() %>%
      group_by(CMINDC) %>%
      summarise(n=n(),
                prop = n/any_of_below*100)
    df.header <- data.frame(CMINDC = c(var_name, "Any of below"), n = c(NA, any_of_below), prop = c(NA, any_of_below.p))
    subtab <- rbind(df.header, dist_indication)
    prop.round <- rep(NA, nrow(subtab))
    for (i in 1:nrow(subtab)) {
      prop.round[i] <- round_fmt(subtab$pro[i], digits = 1)
    }
    subtab$prop.round <- prop.round
    subtab$stat_0 <- paste(subtab$n, " (", subtab$prop.round, "%)", sep = "")
    subtab2 <- data.frame(Characteristic = subtab$CMINDC, Statistic = c(NA,rep("n(%)", nrow(subtab)-1)), stat_0 = subtab$stat_0)
    subtab2$stat_0[1] <- NA
    subtab2$Characteristic[3:nrow(subtab2)] <- str_to_title(subtab2$Characteristic[3:nrow(subtab2)])
    subtab.f3 <- subtab2
    # combine data
    subtab.f1 <- rename(subtab.f1, stat_1 = stat_0)
    subtab.f2 <- rename(subtab.f2, stat_2 = stat_0)
    subtab.f3 <- rename(subtab.f3, stat_3 = stat_0)
    df_list <- list(subtab.f1, subtab.f2, subtab.f3)
    subtab.f <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
    subtab.f_ <- subtab.f[c(5,2,1,3:4,6:11),]
    table6 <- rbind(table6, subtab.f_)

    # save file
    rownames(table6) <- NULL
    write.csv(table6, "table6.csv")



    ############## TLF Table 7: ACT Outcome Post Index (adrs)
    adrs2 <- adrs

    # # ACT post index
    sub.postindex <- subset(adrs2, VISIT != "INDEX")
    # extract date of treatment
    date_treatment <- sub.postindex$TRTSDT
    day <- as.integer(str_extract(date_treatment, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_treatment)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_treatment, nchar(date_treatment)-3))
    date_treatment <- make_date(year, month.n, day)
    sub.postindex$TRTSDT <- date_treatment
    # extract date of assessment
    date_assess <- sub.postindex$RSDTC
    day <- as.integer(str_extract(date_assess, "^.{2}"))
    month <- gsub('[[:digit:]]+', '', date_assess)
    month.n <- as.integer(factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    year <- as.integer(substring(date_assess, nchar(date_assess)-3))
    date_assess <- make_date(year, month.n, day)
    sub.postindex$RSDTC <- date_assess
    # check number of months post index since treatment
    #months_postindex <- interval(date_treatment, date_assess) %/% months(1)
    months_postindex_ <- difftime(date_assess, date_treatment) / 30
    months_postindex <- ifelse(months_postindex_ < 6, ceiling(months_postindex_), floor(months_postindex_))
    # create binary variable indicate <6months post index, 6-12 months post-index
    bin_mo_postindex <- rep(NA, length(months_postindex))
    bin_mo_postindex[which(months_postindex < 6 & months_postindex >= 0)] <- "<6 months"
    bin_mo_postindex[which(months_postindex <= 12 & months_postindex >= 6)] <- "6-12 months"
    sub.postindex$bin_mo_postindex <- bin_mo_postindex
    sub.postindex$months_postindex <- months_postindex
    # filter post index 6 to 12 months
    sub.postindex6to12 <- subset(sub.postindex, bin_mo_postindex == "6-12 months")


    # create table 7
    table7 <- data.frame()

    # ACT outcome
    sub_act_outcome <- subset(sub.postindex6to12, RSSCAT == "ACT OUTCOME")
    length(unique(sub_act_outcome$USUBJID))
    # reorder based on id and date of assessment
    sub_act_outcome <- sub_act_outcome %>% arrange(USUBJID, RSDTC)
    # take the most recent ACT to today's date
    sub_act_outcome_recent <- sub_act_outcome %>%
      group_by(USUBJID) %>%
      arrange(RSDTC) %>%
      slice_tail(n = 1)
    length(unique(sub_act_outcome$USUBJID)) == nrow(sub_act_outcome_recent) # checking
    sub_act_outcome_recent$AVALC <- factor(sub_act_outcome_recent$AVALC, levels = c("TOTAL CONTROL", "WELL-CONTROLLED", "UNCONTROLLED"))
    # combine with HCRU
    df_temp <- merge(HCRU, sub_act_outcome_recent[,c("USUBJID", "AVALC", "AVALCAT1")], by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    # rename ACT column
    HCRU <- rename(HCRU, ACT_CAT1_POST = AVALC,
                   ACT_CAT2_POST = AVALCAT1)
    HCRU$ACT_CAT1_POST <- str_to_title(HCRU$ACT_CAT1_POST)
    HCRU$ACT_CAT2_POST <- str_to_title(HCRU$ACT_CAT2_POST)
    # relevel category
    HCRU$ACT_CAT1_POST <- factor(HCRU$ACT_CAT1_POST, c("Total Control", "Well-Controlled", "Uncontrolled"))
    HCRU$ACT_CAT2_POST <- factor(HCRU$ACT_CAT2_POST, c("Controlled", "Uncontrolled"))
    # data frame for fig 1
    df_fig1 <- sub_act_outcome_recent[,c("USUBJID", "AVALC")]
    # create summary statistic
    tab_ <- HCRU %>%
      select(ACT_CAT1_POST) %>%
      tbl_summary(digits = list("ACT_CAT1_POST" ~ c(0,1)),
                  label = "ACT_CAT1_POST" ~ "ACT Outcome" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    tab2 <- tab_[2:5,]
    subtab <- data.frame(`Summary Variable` = tab2$label, Statistic = c(NA, rep("n(%)",3)), stat_0 = tab2$stat_0)
    subtab.f <- subtab
    table7 <- rbind(table7, subtab.f)



    # ACT numerical
    var_name <- "ACT Numerical Score"
    sub_act_numerical <- subset(sub.postindex6to12, RSSCAT == "ACT NUMERICAL")
    length(unique(sub_act_numerical$USUBJID))
    # reorder based on id and date of assessment
    sub_act_numerical <- sub_act_numerical %>% arrange(USUBJID, RSDTC)
    # take the most recent ACT to today's date
    sub_act_numerical_recent <- sub_act_numerical %>%
      group_by(USUBJID) %>%
      arrange(RSDTC) %>%
      slice_tail(n = 1)
    length(unique(sub_act_numerical$USUBJID)) == nrow(sub_act_numerical_recent) # checking
    sub_act_numerical_recent$AVAL <- as.numeric(sub_act_numerical_recent$AVAL)
    # merge with HCRU
    df_temp <- merge(HCRU, sub_act_numerical_recent[,c("USUBJID", "AVAL")], by = "USUBJID", all = TRUE)
    HCRU <- df_temp
    HCRU <- rename(HCRU, ACT_SCORE_POST = AVAL)
    # create summary statistic
    tab_ <- cont_stat(na.omit(HCRU$ACT_SCORE_POST), "ACT Numerical")
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table7 <- rbind(table7, subtab.f)



    # save file
    names(table7)[3] <- paste("N = ", n_header, sep = "")
    rownames(table7) <- NULL
    write.csv(table7, "table7.csv")


    ############## Table 8: Summary by ACT Outcome at Post-index

    # create table
    table8 <- data.frame()

    # merge HCRU and Asthma medication data
    df_table8 <- merge(HCRU, df_asthma_med2, by = "USUBJID", all = TRUE)
    # re-label ACT outcome
    df_table8$ACT <- df_table8$ACT_CAT1_POST


    ## Number of scheduled physician visits
    var_name <- "Number of patients and number scheduled physician visits"
    tab_ <- df_table8 %>%
      group_by(ACT, schedule_visit_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- table(df_table8$schedule_visit_cat)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    header_lbl <- gsub("\\.", " ", names(tab_wide_perc_c))
    N_header <- colSums(tab_wide_w_total[,-1], na.rm = TRUE)
    header_tab8 <- paste(header_lbl, " [N = ", N_header, "]", sep = "")
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$schedule_visit_cat)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)

    ## Number of unscheduled physician visits
    # unscheduled visit: none vs at least 1
    var_name <- "Number of patients and number of unscheduled physician visitss"
    tab_ <- df_table8 %>%
      group_by(ACT, unschedule_visit_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- table(df_table8$unschedule_visit_cat)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab

    # unscheduled visit per category
    var_name <- "Number of patients and number of unscheduled physician visits"
    tab_ <- df_table8 %>%
      group_by(ACT, unschedule_visit_cat2) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    visit_cat <- c("1 visit", "2 visits", "3 visits", "4 visits", ">=5 visits")
    tab_wide <- spread(tab_, ACT, n)
    # add row for 3 visits
    tab_wide2 <- rbind(tab_wide[1:2,],
                       c("3 visits", 0, 0, 0),
                       tab_wide[3:4,])
    tab_wide <- tab_wide2
    tab_wide$`Total Control` <- as.numeric(tab_wide$`Total Control`)
    tab_wide$Uncontrolled <- as.numeric(tab_wide$Uncontrolled)
    tab_wide$`Well-Controlled` <- as.numeric(tab_wide$`Well-Controlled`)
    # include total column
    tab_tot <- table(df_table8$unschedule_visit_cat2)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)


    ## Final patient status, gaps in treatment and discontinuation of ICS/LABA 6-12 months post-index
    var_name <- "Final patient status, gaps in treatment and discontinuation of ICS/LABA 6-12 months post-index"
    tab_ <- df_table8 %>%
      group_by(ACT, patient_status3) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- table(df_table8$patient_status3)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)


    ## ICS/LABA daily dose at index
    var_name <- "ICS/LABA daily dose at index"
    tab_ <- df_table8 %>%
      group_by(ACT, Dose_Index) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)


    ## ICS/LABA daily dose 6-12 months post-Index
    var_name <- "ICS/LABA daily dose 6-12 months post-Index"
    tab_ <- df_table8 %>%
      group_by(ACT, Dose_Post6to12) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)



    ## ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)
    var_name <- "ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)"
    tab_ <- df_table8 %>%
      group_by(ACT, treatment_duration_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)



    ## ICS/LABA treatment duration: number of months treated (including prescription gaps)
    var_name <- "ICS/LABA treatment duration: number of months treated (including prescription gaps)"
    tab_ <- df_table8 %>%
      group_by(ACT, treatment_duration_include_gap_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)



    ## Number of patients and the frequency of comorbid conditions at index or nearest index
    var_name <- "Number of patients with comorbid conditions at index or nearest index (up to 90 days pre-index)"
    # merge data with comorbid condition
    df_temp <- merge(df_table8, df_CC_index, by = "USUBJID", all = TRUE)
    df_table8 <- df_temp
    tab_ <- df_table8 %>%
      group_by(ACT, TOT_CC_INDEX_CAT) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    tab_wide <- tab_wide[which(!is.na(tab_wide$TOT_CC_INDEX_CAT)),]
    # include for missing categories
    tab_wide[is.na(tab_wide)] <- 0
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)


    ## Number of patients and the frequency of comorbid conditions at post-index
    var_name <- "Number of patients with comorbid conditions at post-index "
    # merge data with comorbid condition
    df_temp <- merge(df_table8, df_CC_post, by = "USUBJID", all = TRUE)
    df_table8 <- df_temp
    tab_ <- df_table8 %>%
      group_by(ACT, TOT_CC_POST_CAT) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include for missing categories
    cat_cc <- data.frame(TOT_CC_POST_CAT = c("None", "1", "2", "3+"))
    tab_wide2 <- merge(cat_cc, tab_wide, by = "TOT_CC_POST_CAT", all = TRUE)
    tab_wide <- tab_wide2[c(4,1:3),]
    tab_wide[is.na(tab_wide)] <- 0
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    table8 <- rbind(table8, subtab)


    ## Number of patients who had an exacerbation(of any kind) based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation(of any kind) based on the frequency, per year"
    # exacerbation: none vs at least 1
    df_excr2 <- df_excr %>% select(-contains("ASTMED"), -contains("EXA_"), -contains("CE_"))
    df_excr2 <- rename(df_excr2, SUBJID = Study.Subject.ID)
    df_temp <- merge(df_table8, df_excr2, by = "SUBJID", all = TRUE)
    df_table8 <- df_temp
    tab_ <- df_table8 %>%
      group_by(ACT, excr_TOT_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_TOT_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # remove NA
    tab_wide2 <- tab_wide[-4,]
    tab_wide <- tab_wide2
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)



    ## Number of patients who had an exacerbation (of any kind) with steroid use based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation (of any kind) with steroid use based on the frequency, per year"
    # exacerbation: none vs at least 1
    tab_ <- df_table8 %>%
      group_by(ACT, excr_TOT_steroid_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    tab_wide2 <- tab_wide[-3,]
    tab_wide <- tab_wide2
    tab_wide[is.na(tab_wide)] <- 0
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_TOT_steroid_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # remove NA
    tab_wide2 <- tab_wide[-4,]
    tab_wide <- tab_wide2
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.nan(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)



    ## Number of patients who had an exacerbation that required a physician visit and steroid prescription, based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation that required a physician visit and steroid prescription, based on the frequency, per year"
    # exacerbation: none vs at least 1
    tab_ <- df_table8 %>%
      group_by(ACT, excr_PV_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_PV_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    cat_ex <- data.frame(excr_PV_cat = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    tab_wide <- spread(tab_, ACT, n)
    tab_wide2 <- merge(cat_ex, tab_wide, by = "excr_PV_cat", all = TRUE)
    tab_wide <- tab_wide2[c(2:3,1),]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)



    ## Number of patients who had an exacerbation that required an ER visit, based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation that required an ER visit, based on the frequency, per year"
    # exacerbation: none vs at least 1
    tab_ <- df_table8 %>%
      group_by(ACT, excr_ER_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # remove NA
    tab_wide2 <- tab_wide[-3,]
    tab_wide <- tab_wide2
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_ER_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # remove NA
    tab_wide2 <- tab_wide[-4,]
    tab_wide <- tab_wide2
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)


    ## Number of patients who had an exacerbation that required an ER visit with steroid use, based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation that required an ER visit with steroid use, based on the frequency, per year"
    # exacerbation: none vs at least 1
    df_table8$excr_ER_steroid_atleast1 <- ifelse(df_table8$excr_ER_steroid >= 1, "At least 1 exacerbation", "None")
    tab_ <- df_table8 %>%
      group_by(ACT, excr_ER_steroid_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    tab_wide <- tab_wide[1:2,]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_ER_steroid_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    cat_ex <- data.frame(excr_ER_steroid_cat = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    tab_wide2 <- merge(cat_ex, tab_wide, by = "excr_ER_steroid_cat", all = TRUE)
    tab_wide <- tab_wide2[c(2:3,1),]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)




    ## Number of patients who had an exacerbation that required a hospitalization, based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation that required aa hospitalization, based on the frequency, per year"
    # exacerbation: none vs at least 1
    tab_ <- df_table8 %>%
      group_by(ACT, excr_HO_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    # remove NA
    tab_wide2 <- tab_wide[-3,]
    tab_wide <- tab_wide2
    # include total column
    tab_tot <- rowSums(tab_wide[,-1],na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_HO_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    cat_ex <- data.frame(excr_HO_cat = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    tab_wide2 <- merge(cat_ex, tab_wide, by = "excr_HO_cat", all = TRUE)
    tab_wide <- tab_wide2[c(2:3,1),]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)



    ## Number of patients who had an exacerbation that required a hospitalization with steroid use, based on the frequency, per year
    var_name <- "Number of patients who had an exacerbation that required a hospitalization with steroid use, based on the frequency, per year"
    # exacerbation: none vs at least 1
    df_table8$excr_HO_steroid_atleast1 <- ifelse(df_table8$excr_HO_steroid >= 1, "At least 1 exacerbation", "None")
    tab_ <- df_table8 %>%
      group_by(ACT, excr_HO_steroid_atleast1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    tab_wide <- tab_wide[1:2,]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        perc_ij <- round_fmt(perc_ij, 1)
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab1 <- subtab[1:2,]
    # exacerbation per category
    tab_ <- df_table8 %>%
      group_by(ACT, excr_HO_steroid_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT)),]
    tab_wide <- spread(tab_, ACT, n)
    cat_ex <- data.frame(excr_HO_steroid_cat  = c("1 exacerbation", "2 exacerbations", ">=3 exacerbations"))
    tab_wide2 <- merge(cat_ex, tab_wide, by = "excr_HO_steroid_cat" , all = TRUE)
    tab_wide <- tab_wide2[c(2:3,1),]
    # include total column
    tab_tot <- rowSums(tab_wide[,-1], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide, Total = c(tab_tot))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab8
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:6] <- header_tab8
    subtab2 <- subtab[-1,]
    table8 <- rbind(table8, subtab1, subtab2)

    ## save file
    rownames(table8) <- NULL
    write.csv(table8, "table8.csv")


    ############## TLF Table 9: ACT Change from index to 6-12 months post index (adrs)

    # create table 9
    table9 <- data.frame()

    # ACT at index
    sub.index <- subset(adrs2, VISIT == "INDEX")


    # ACT numerical at index
    var_name <- "ACT Numerical Score at index"
    sub_act_numerical_index <- subset(sub.index, RSSCAT == "ACT NUMERICAL")
    length(unique(sub_act_numerical_index$USUBJID)) == nrow(sub_act_numerical_index) #checking
    # data frame for figure 2
    df_fig2_index <- sub_act_numerical_index %>% select(USUBJID, AVAL)
    names(df_fig2_index)[2] <- "ACT_index"
    # merge to df_table8
    df_temp <- merge(df_table8, df_fig2_index, by = "USUBJID", all = TRUE)
    df_table8 <- df_temp
    df_table8 <- rename(df_table8, ACT_SCORE_INDEX = ACT_index)
    # create summary statistic
    sub_act_numerical_index$AVAL <- as.numeric(sub_act_numerical_index$AVAL)
    tab_ <- cont_stat(sub_act_numerical_index$AVAL, "ACT Numerical Score at Index")
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table9 <- rbind(table9, subtab.f)

    # ACT numerical at 6-12 months post index
    var_name <- "ACT Numerical Score 6- to 12-months Post-index"
    # data frame for figure 2
    df_fig2_post <- df_table8 %>% select(USUBJID, ACT_SCORE_POST)
    names(df_fig2_post)[2] <- "ACT_post"
    df_fig2 <- merge(df_fig2_index, df_fig2_post, by = "USUBJID", all = TRUE)
    # create summary statistic
    tab_ <- cont_stat(na.omit(df_table8$ACT_SCORE_POST), "ACT Numerical post-index")
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table9 <- rbind(table9, subtab.f)


    # ACT outcome at index
    sub_act_outcome_index <- subset(sub.index, RSSCAT == "ACT OUTCOME")
    length(unique(sub_act_outcome_index$USUBJID)) == nrow(sub_act_outcome_index) #checking
    sub_act_outcome_index$AVALC <- factor(sub_act_outcome_index$AVALC, levels = c("TOTAL CONTROL","WELL-CONTROLLED", "UNCONTROLLED"))
    # create summary statistic
    tab_ <- sub_act_outcome_index %>%
      select(AVALC) %>%
      tbl_summary(digits = list("AVALC" ~ c(0,1)),
                  label = "AVALC" ~ "ACT Outcome at Index" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",3),NA), stat_0 = tab_$stat_0)
    subtab <- subtab[c(-1, -nrow(tab_)),]
    subtab$Characteristic[2:4] <- c("Total control","Well controlled", "Uncontrolled")
    table9 <- rbind(table9, subtab)


    # ACT outcome at index
    sub_act_outcome_index <- subset(sub.index, RSSCAT == "ACT OUTCOME")
    length(unique(sub_act_outcome_index$USUBJID)) == nrow(sub_act_outcome_index) #checking
    sub_act_outcome_index$AVALCAT1 <- factor(sub_act_outcome_index$AVALCAT1, levels = c("CONTROLLED", "UNCONTROLLED"))
    # data frame for figure 3
    df_fig3_index <- sub_act_outcome_index %>% select(USUBJID, AVALCAT1)
    names(df_fig3_index)[2] <- "ACT_index"
    # merge to df_table8
    df_temp <- merge(df_table8, df_fig3_index, by = "USUBJID", all = TRUE)
    df_table8 <- df_temp
    df_table8 <- rename(df_table8, ACT_CAT_INDEX = ACT_index)
    df_table8$ACT_CAT_INDEX <- str_to_title(df_table8$ACT_CAT_INDEX)
    # create summary statistic
    tab_ <- sub_act_outcome_index %>%
      select(AVALCAT1) %>%
      tbl_summary(digits = list("AVALCAT1" ~ c(0,1)),
                  label = "AVALCAT1" ~ "ACT Outcome at Index" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",2),NA), stat_0 = tab_$stat_0)
    subtab <- subtab[c(-1, -nrow(tab_)),]
    subtab$Characteristic[2:3] <- c("Controlled", "Uncontrolled")
    table9 <- rbind(table9, subtab)


    # ACT outcome at post-index
    tab_ <- df_table8 %>%
      select(ACT_CAT2_POST) %>%
      tbl_summary(digits = list("ACT_CAT2_POST" ~ c(0,1)),
                  label = "ACT_CAT2_POST" ~ "ACT Outcome at 6- to 12-months Post-index" ) %>%
      modify_header(label = "Summary Variable") %>%
      as_hux_table()
    # create summary statistic
    subtab <- data.frame(Characteristic = tab_$label, Statistic = c(NA,NA,rep("n(%)",2),NA), stat_0 = tab_$stat_0)
    subtab.f <- subtab[c(2:4),]
    table9 <- rbind(table9, subtab.f)


    # changes in ACT numerical
    var_name <- "Changes in ACT score from Index to 6- to 12-months Post-index"
    change <- df_table8$ACT_SCORE_POST - df_table8$ACT_SCORE_INDEX
    change_naomit <- na.omit(change)
    tab_ <- cont_stat(change_naomit, "ACT numerical change")
    subtab <- data.frame(Characteristic = c(var_name, rep(NA, 7)), Statistic = tab_$label, stat_0 = tab_$stat_0)
    subtab.f <- subtab[-1,]
    subtab.f[1,1] <- var_name
    table9 <- rbind(table9, subtab.f)

    # changes in ACT outcome
    cont_table <- xtabs(~ ACT_CAT_INDEX + ACT_CAT2_POST, data = df_table8)
    n_control.i_uncontrol.p <- cont_table[1,2]
    prop_control.i_uncontrol.p <-  round_fmt(cont_table[1,2] / sum(cont_table) * 100, 1)
    n_uncontrol.i_control.p <- cont_table[2,1]
    prop_uncontrol.i_control.p <-  round_fmt(cont_table[2,1] / sum(cont_table) * 100 , 1)
    n_nochange <- cont_table[1,1] + cont_table[2,2]
    prop_nochange <-  round_fmt(n_nochange / sum(cont_table) * 100,1)
    n_nochange1 <- cont_table[2,2]
    prop_nochange1 <-  round_fmt(n_nochange1 / sum(n_nochange) * 100,1)
    n_nochange2 <- cont_table[1,1]
    prop_nochange2 <-  round_fmt(n_nochange2 / sum(n_nochange) * 100,1)
    n_comb <- c(n_control.i_uncontrol.p, n_uncontrol.i_control.p, n_nochange, n_nochange1, n_nochange2)
    prop_comb <- c(prop_control.i_uncontrol.p, prop_uncontrol.i_control.p, prop_nochange, prop_nochange1, prop_nochange2)
    subtab <- data.frame(Characteristic = c("Changes in ACT outcome from Index to 6- to 12-months post-index",
                                            "Controlled at index, uncontrolled at 6- to 12-months post-index",
                                            "Uncontrolled at index, controlled at 6- to 12-months post-index",
                                            "No change",
                                            "No change: Uncontrolled at Index and remained uncontrolled post-Index",
                                            "No change: Controlled at Index and remained controlled post-Index"),
                         Statistic = c(NA,rep("n(%)",5)),
                         stat_0 = c(NA,paste(n_comb, " (", prop_comb, "%)", sep = "")))
    table9 <- rbind(table9, subtab)

    #save file
    names(table9)[1] <- "Summary Variable"
    names(table9)[3] <- paste("N = ", n_header, sep = "")
    rownames(table9) <- NULL

    # reorder row index
    table9f <- table9[c(1:14,25:31,15:24,32:37),]
    rownames(table9f) <- NULL
    write.csv(table9f, "table9.csv")



    ############## TLF Table 10: Summary of Predictive Variables by ACT outcome


    ## create table
    table10 <- data.frame()
    table10b <- data.frame()

    # data frame for table 10
    df_table10 <- df_table8
    df_table10$ACT_bin <- df_table10$ACT_CAT2_POST
    # uncontrolled group as reference category
    df_table10$ACT_bin <- relevel(df_table10$ACT_bin, ref = "Uncontrolled")
    xtabs(~df_table10$ACT + df_table10$ACT_bin)
    ## pooling data demographic (adsl) to df_table10
    df_demog <- adsl2 %>% select(USUBJID,AGE, AGEGR1, SEX, RACEGR1)
    df_temp <- merge(df_table10, df_demog, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp


    ## ACT by Age Continuous
    var_name <- "Age"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(sub_act_cont$AGE, var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(sub_act_uncont$AGE, var_name)
    sub_act_tot <-  df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(sub_act_tot$AGE, var_name)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    header_lbl <- c("Controlled", "Uncontrolled", "Total")
    N_header <- tab_[2,-1]
    header_tab10 <- paste(header_lbl, " [N = ", N_header, "]", sep = "")
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    df_table10$ACT_bin_dummy <- ifelse(df_table10$ACT_bin == "Controlled", 1, 0)
    fit <- glm(ACT_bin_dummy ~ AGE, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$AGE))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)

    ## ACT by Age categorical
    var_name <- "Age"
    tab_ <- df_table10 %>%
      group_by(ACT_bin, AGEGR1) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- tab_wide[,2] + tab_wide[,3]
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab[-1,])
    # test for significance
    fit <- glm(ACT_bin_dummy ~ AGEGR1, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$AGEGR1))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab[-1,],
                                pval = c(pval, rep(NA, 3)),
                                missing = c(n_perc_missing, rep(NA, 3)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by SEX
    var_name <- "Sex"
    tab_ <- df_table10 %>%
      group_by(ACT_bin, SEX) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- tab_wide[,2] + tab_wide[,3]
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ SEX, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$SEX))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ACT by Smoking history
    var_name <- "Smoking history"
    # polling smoking history data
    df_adsc <- adsc2 %>%
      select(USUBJID, SCORRES, SCTESTCD) %>%
      filter(SCTESTCD == "SMOKHIST")
    df_adsc$SCORRES <- factor(df_adsc$SCORRES, levels = c("Never Smoke", "Current Smoke", "Former Smoke", "Unknown"))
    df_adsc2 <- df_adsc[,c(1,2)]
    names(df_adsc2)[2] <- "Smoke_Hist"
    # merge
    df_temp <- merge(df_table10, df_adsc2, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp
    # create summary stat
    tab_ <- df_table10 %>%
      group_by(ACT_bin, Smoke_Hist) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include missing category
    smoke_cat <- data.frame(AVALC = c("Never smoked", "Current smoker", "Former smoker", "Unknown"))
    # include total column
    tab_total <- tab_wide[,2] + tab_wide[,3]
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Smoke_Hist, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Smoke_Hist))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 4)),
                                missing = c(n_perc_missing, rep(NA, 4)))
    table10b <- rbind(table10b, subtab_w_pval)

    ## Create binary Smoking History and replace unknown as missing value
    df_table10$Smoke_Hist_bin <- df_table10$Smoke_Hist
    df_table10$Smoke_Hist_bin[which(df_table10$Smoke_Hist == "Former Smoke")] <- "Current Smoke"
    df_table10$Smoke_Hist_bin[which(df_table10$Smoke_Hist == "Unknown")] <- NA


    ## ACT by Education level
    var_name <- "Education level"
    # polling educational level data
    df_adsc <- adsc2 %>%
      select(USUBJID, PARAMCD, AVALC) %>%
      filter(PARAMCD == "EDULEVEL")
    df_adsc$AVALC <- factor(df_adsc$AVALC, levels = c("Illiterate",
                                                      "Not Finishing Formal Education",
                                                      "High School Graduate",
                                                      "University Graduate (Bachelor)",
                                                      "Post Graduate Degree (Master/Doctor/Post-Doctoral)",
                                                      "Unknown or Not Reported"))
    df_adsc2 <- df_adsc[,c(1,3)]
    names(df_adsc2)[2] <- "Educ_Level"
    # merge
    df_temp <- merge(df_table10, df_adsc2, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp
    # create summary stat
    tab_ <- df_table10 %>%
      group_by(ACT_bin, Educ_Level) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include missing category
    edu_cat <- data.frame(Educ_Level = c("Illiterate",
                                         "Not Finishing Formal Education",
                                         "High School Graduate",
                                         "University Graduate (Bachelor)",
                                         "Post Graduate Degree (Master/Doctor/Post-Doctoral)",
                                         "Unknown or Not Reported"))
    df_temp <- merge(edu_cat, tab_wide, by = "Educ_Level", all = TRUE)
    # reorder rows
    tab_wide <- df_temp[c(2,3,1,5,4,6),]
    # include total column
    tab_total <- rowSums(tab_wide[,c(2,3)], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Educ_Level, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Educ_Level))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)

    # create educ level with unknown as missing value
    df_table10$Educ_Level2 <- df_table10$Educ_Level
    df_table10$Educ_Level2[which(df_table10$Educ_Level == "Unknown or Not Reported")] <- NA


    ## ACT by Employment status
    var_name <- "Employment status"
    # polling smoking history data
    df_adsc <- adsc2 %>%
      select(USUBJID, PARAMCD, AVALC) %>%
      filter(PARAMCD == "EMPJOB")
    df_adsc$AVALC <- factor(df_adsc$AVALC, levels = c("Employed (full or part-time)",
                                                      "Self-employed",
                                                      "Student",
                                                      "House Spouse",
                                                      "Unemployed (not working/retiree)",
                                                      "Unknown or Not Reported"))
    df_adsc2 <- df_adsc[,c(1,3)]
    names(df_adsc2)[2] <- "Emp_Job"
    # merge
    df_temp <- merge(df_table10, df_adsc2, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp
    # create summary stat
    tab_ <- df_table10 %>%
      group_by(ACT_bin, Emp_Job) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- rowSums(tab_wide[,c(2:3)], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Emp_Job, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Emp_Job))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)

    # create employment with unknown as missing value
    df_table10$Emp_Job2 <- df_table10$Emp_Job
    df_table10$Emp_Job2[which(df_table10$Emp_Job == "Unknown or Not Reported")] <- NA
    table(df_table10$Emp_Job2)


    ## ACT by BMI cont
    var_name <- "BMI"
    # pooling BMI data from advs
    df_advs <- advs2 %>%
      select(USUBJID, PARAMCD, AVAL, AVALCAT1) %>%
      filter(PARAMCD == "BMI")
    names(df_advs)[3] <- "BMI"
    names(df_advs)[4] <- "BMI_cat"
    # merge to df_table 10
    df_temp <- merge(df_table10, df_advs[,c(1,3:4)], by = "USUBJID", all = TRUE)
    df_table10 <- df_temp
    # summary statistic
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat3(sub_act_cont$BMI, var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat3(sub_act_uncont$BMI, var_name)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat3(sub_act_tot$BMI, var_name)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ BMI, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$BMI))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ACT by BMI categorical
    var_name <- "BMI"
    tab_ <- df_table10 %>%
      group_by(ACT_bin, BMI_cat) %>%
      summarise(n = n())

    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- rowSums(tab_wide[,c(2:3)], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # reorder rows
    tab_wide_w_total2 <- tab_wide_w_total[c(4,1,3,2),]
    tab_wide_w_total <- tab_wide_w_total2
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total2[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide_w_total2$BMI_cat)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab[-1,])
    # test for significance
    fit <- glm(ACT_bin_dummy ~ BMI_cat, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$BMI_cat))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab[-1,],
                                pval = c(pval, rep(NA, 3)),
                                missing = c(n_perc_missing, rep(NA, 3)))
    table10b <- rbind(table10b, subtab_w_pval)




    ## ACT by Years since asthma diagnosis Continuous
    var_name <- "Years since asthma diagnosis"
    # pooling data asthma diagnosis
    df_temp <- merge(df_table10, df_asthma_diag[, c("USUBJID", "Year_Asthma_Diag", "Asthma_Diag_Cat")], by = "USUBJID", all = TRUE)
    df_table10 <- df_temp
    # summary stat
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat2(na.omit(sub_act_cont$Year_Asthma_Diag), var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat2(na.omit(sub_act_uncont$Year_Asthma_Diag), var_name)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat2(na.omit(sub_act_tot$Year_Asthma_Diag), var_name)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Year_Asthma_Diag, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Year_Asthma_Diag))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Years since asthma diagnosis categorical
    df_table10$Asthma_Diag_Cat[which(df_table10$Asthma_Diag_Cat == "Missing")] <- NA
    var_name <- "Years since asthma diagnosis"
    tab_ <- df_table10 %>%
      group_by(ACT_bin, Asthma_Diag_Cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- rowSums(tab_wide[,c(2:3)], na.rm = TRUE)
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = tab_total)
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    subtab$Summary.Variable[7] <- "Unknown"
    table10 <- rbind(table10, subtab[-1,])
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Asthma_Diag_Cat, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Asthma_Diag_Cat))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab[-1,],
                                pval = c(pval, rep(NA, 5)),
                                missing = c(n_perc_missing, rep(NA, 5)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Comorbid conditions at/pre-index
    var_name <- "Comorbid conditions at/pre-index"
    list_cc <- data_frame(cc = c("Allergic Rhinitis",
                                 "Atherosclerosis",
                                 "Cardiac Arrhythmia",
                                 "Congestive Heart Failure",
                                 "Diabetes",
                                 "Gastroesophageal Reflux Disease (Gerd)",
                                 "Obesity",
                                 "Osteoporosis"))
    list_cc <- toupper(list_cc$cc)
    df_cc_index <- admh %>%
      filter(EPOCH == "BASELINE") %>%
      filter(MHTERM != "NO COMORBID OF INTEREST") %>%
      group_by(USUBJID, MHTERM) %>%
      summarise(n = n())
    df_cc_index$MHTERM <- str_to_title(df_cc_index$MHTERM)
    number_cc_index <- length(unique(df_cc_index$MHTERM))
    # transform to wide format
    df_cc_index_wide <- spread(df_cc_index, MHTERM, n)
    # select ACT and all CC at index
    df_table10_with_cc_index <- merge(df_table10[,c("USUBJID", "ACT_bin", "MHSTATUS_INDEX")], df_cc_index_wide, by = "USUBJID", all = TRUE)
    # create comorbid condition variable: 1=present, 0=not present, NA
    for(i in 4:ncol(df_table10_with_cc_index)) {
      for (j in 1:nrow(df_table10_with_cc_index)) {
        id.j <- df_table10_with_cc_index$USUBJID[j]
        coknown <- df_table10_with_cc_index$MHSTATUS_INDEX[j]
        if (!is.na(coknown)) {
          if (coknown == 1 & is.na(df_table10_with_cc_index[j,i])) {
            df_table10_with_cc_index[j,i] <- 0
          } else if (coknown == 0) {
            df_table10_with_cc_index[j,i] <- 0
          }
        }
      }
    }
    # create column for cc of interest
    df_table10_with_cc_index$index_cc1 <- df_table10_with_cc_index$`Allergic Rhinitis`
    df_table10_with_cc_index$index_cc2 <- df_table10_with_cc_index$`Atherosclerosis`
    df_table10_with_cc_index$index_cc3 <- NA
    #df_table10_with_cc_index$index_cc3 <- df_table10_with_cc_index$`Cardiac Arrhythmia`
    df_table10_with_cc_index$index_cc4 <- df_table10_with_cc_index$`Congestive Heart Failure`
    df_table10_with_cc_index$index_cc5 <- df_table10_with_cc_index$Diabetes
    df_table10_with_cc_index$index_cc6 <- df_table10_with_cc_index$`Gastroesophageal Reflux Disease (Gerd)`
    df_table10_with_cc_index$index_cc7 <- df_table10_with_cc_index$Obesity
    df_table10_with_cc_index$index_cc8 <- df_table10_with_cc_index$Osteoporosis
    df_table10_with_cc_index$index_cc8 <- NA
    # create whether any comorbid if interest present at index
    df_temp <- df_table10_with_cc_index %>% select(contains("index_cc"))
    df_table10_with_cc_index$index_cc_bin <- ifelse(rowSums(df_temp, na.rm = TRUE) > 0, 1, 0)
    df_table10_with_cc_index$index_cc_bin <- ifelse(is.na(df_table10_with_cc_index$MHSTATUS_INDEX), NA, df_table10_with_cc_index$index_cc_bin)
    # merge to df_table10
    df_temp_ <- df_table10_with_cc_index %>% select(USUBJID, contains("index_cc"))
    df_temp <- merge(df_table10, df_temp_, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp



    ## CC INDEX BINARY (whether any comorbid of interest present)
    var_name <- "Any of comorbid of interest presents at index"
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(index_cc_bin) %>%
      summarise(n = n())
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(index_cc_bin) %>%
      summarise(n = n())
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Yes", "No"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = var_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance
    fit <- glm((ACT_bin == "Controlled") ~ factor(index_cc_bin), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$index_cc_bin))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## summary all cc of interest at index
    var_name <- "Comorbid conditions at/pre-index "
    n_cc <- df_table10_with_cc_index %>%
      group_by(ACT_bin,index_cc_bin) %>%
      summarise(n = n()) %>%
      filter(!is.na(index_cc_bin)) %>%
      group_by(ACT_bin) %>%
      summarise(n=sum(n))
    n_cc1 <- t(n_cc[c(2,1),2])
    #n_cc2 <- c(n_cc1, sum(n_cc1))
    n_cc1 <- table(df_table10_with_cc_index$ACT_bin)
    n_cc2 <- c(n_cc1[c(2,1)], sum(n_cc1))
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(index_cc_bin) %>%
      summarise_at(vars(index_cc1:index_cc8), sum, na.rm = TRUE) %>%
      filter(index_cc_bin == 1) %>%
      t()
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(index_cc_bin) %>%
      summarise_at(vars(index_cc1:index_cc8), sum, na.rm = TRUE) %>%
      filter(index_cc_bin == 1) %>%
      t()
    tab_act_tot <- tab_act_cont + tab_act_uncont
    tab_act <- cbind(tab_act_cont[-1], tab_act_uncont[-1], tab_act_tot[-1])
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / n_cc2[i] * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = str_to_title(list_cc),
                       Statistic = "n(%)",
                       tab_act_per)
    names(tab_)[3:5] <- c("n", "n.1", "n.2")
    tab_ <- rbind(cbind(Summary.Variable = var_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    table10 <- rbind(table10, tab_)



    ## Allergic Rhinitis
    cc_name <- str_to_title(list_cc[1])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Allergic Rhinitis`) %>%
      summarise(n = n())
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Allergic Rhinitis`) %>%
      summarise(n = n())
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Allergic Rhinitis
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Allergic Rhinitis`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Allergic Rhinitis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)



    ## Atherosclerosis
    cc_name <- str_to_title(list_cc[2])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Atherosclerosis`) %>%
      summarise(n = n())
    tab_act_cont2 <- rbind(tab_act_cont, c(1,0))
    tab_act_cont <- tab_act_cont2[c(1,3,2),]
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Atherosclerosis`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act_per <- tab_act
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Atherosclerosis
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Atherosclerosis`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Atherosclerosis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Cardiac Arrhythmia
    cc_name <- str_to_title(list_cc[3])
    cc_name
    # subset controlled
    #tab_act_cont <- df_table10_with_cc_index %>%
    #  filter(ACT_bin == "Controlled") %>%
    #  group_by(`Cardiac Arrhythmia`) %>%
    #  summarise(n = n())
    tab_act_cont[,2] <- 0
    # subset uncontrolled
    #tab_act_uncont <- df_table10_with_cc_index %>%
    #  filter(ACT_bin == "Uncontrolled") %>%
    #  group_by(`Cardiac Arrhythmia`) %>%
    #  summarise(n = n())
    tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Cardiac Arrhythmia
    #fit <- glm((ACT_bin == "Controlled") ~ factor(`Cardiac Arrhythmia`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Cardiac Arrhythmia`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Congestive Heart Failure
    cc_name <- str_to_title(list_cc[4])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Congestive Heart Failure`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Congestive Heart Failure`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Congestive Heart Failure
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Congestive Heart Failure`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Congestive Heart Failure`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)



    ## Diabetes
    cc_name <- str_to_title(list_cc[5])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Diabetes`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Diabetes`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Diabetes
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Diabetes`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Diabetes`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Gastroesophageal Reflux Disease (Gerd)
    cc_name <- str_to_title(list_cc[6])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Gastroesophageal Reflux Disease (Gerd)`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Gastroesophageal Reflux Disease (Gerd)`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Gastroesophageal Reflux Disease (Gerd)
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Gastroesophageal Reflux Disease (Gerd)`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Gastroesophageal Reflux Disease (Gerd)`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Obesity
    cc_name <- str_to_title(list_cc[7])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Obesity`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_index %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Obesity`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Obesity
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Obesity`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Obesity`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Osteoporosis
    cc_name <- str_to_title(list_cc[8])
    cc_name
    # subset controlled
    #tab_act_cont <- df_table10_with_cc_index %>%
    #  filter(ACT_bin == "Controlled") %>%
    #  group_by(`Osteoporosis`) %>%
    #  summarise(n = n())
    tab_act_cont[,2] <- 0
    # subset uncontrolled
    #tab_act_uncont <- df_table10_with_cc_index %>%
    #  filter(ACT_bin == "Uncontrolled") %>%
    #  group_by(`Osteoporosis`) %>%
    #  summarise(n = n())
    tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Obesity
    #fit <- glm((ACT_bin == "Controlled") ~ factor(`Osteoporosis`), data = df_table10_with_cc_index,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_index %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Osteoporosis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)






    ## ACT by Comorbid conditions at post-index
    var_name <- "Comorbid conditions at post-index"
    # pooling comorbid condition at index
    df_cc_post <- admh %>%
      filter(EPOCH != "BASELINE") %>%
      filter(MHTERM != "NO COMORBID OF INTEREST") %>%
      group_by(USUBJID, MHTERM) %>%
      summarise(n = n())
    df_cc_post$MHTERM <- str_to_title(df_cc_post$MHTERM)
    number_cc_post <- length(unique(df_cc_post$MHTERM))
    # transform to wide format
    df_cc_post_wide <- spread(df_cc_post, MHTERM, n)
    # select ACT and all CC at ppost
    df_table10_with_cc_post <- merge(df_table10[,c("USUBJID", "ACT_bin", "MHSTATUS_POST")], df_cc_post_wide, by = "USUBJID", all = TRUE)
    # create comorbid condition variable: 1=present, 0=not present, NA
    for(i in 4:ncol(df_table10_with_cc_post)) {
      for (j in 1:nrow(df_table10_with_cc_post)) {
        id.j <- df_table10_with_cc_post$USUBJID[j]
        coknown <- df_table10_with_cc_post$MHSTATUS_POST[j]
        if (!is.na(coknown)) {
          if (coknown == 1 & is.na(df_table10_with_cc_post[j,i])) {
            df_table10_with_cc_post[j,i] <- 0
          } else if (coknown == 0) {
            df_table10_with_cc_post[j,i] <- 0
          }
        }
      }
    }

    # create column for cc of interest
    df_table10_with_cc_post$post_cc1 <- df_table10_with_cc_post$`Allergic Rhinitis`
    #df_table10_with_cc_post$index_cc2 <- df_table10_with_cc_post$`Atherosclerosis`
    df_table10_with_cc_post$post_cc2 <- NA
    #df_table10_with_cc_index$index_cc3 <- df_table10_with_cc_index$`Cardiac Arrhythmia`
    df_table10_with_cc_post$post_cc3 <- NA
    df_table10_with_cc_post$post_cc4 <- df_table10_with_cc_post$`Congestive Heart Failure`
    df_table10_with_cc_post$post_cc5 <- df_table10_with_cc_post$Diabetes
    df_table10_with_cc_post$post_cc6 <- df_table10_with_cc_post$`Gastroesophageal Reflux Disease (Gerd)`
    df_table10_with_cc_post$post_cc7 <- df_table10_with_cc_post$Obesity
    df_table10_with_cc_post$post_cc8 <- df_table10_with_cc_post$Osteoporosis
    df_table10_with_cc_post$post_cc8 <- NA
    # create whether any comorbid if interest present at post
    df_temp <- df_table10_with_cc_post %>% select(contains("post_cc"))
    df_table10_with_cc_post$post_cc_bin <- ifelse(rowSums(df_temp, na.rm = TRUE) > 0, 1, 0)
    df_table10_with_cc_post$post_cc_bin <- ifelse(is.na(df_table10_with_cc_post$MHSTATUS_POST), NA, df_table10_with_cc_post$post_cc_bin)
    # merge to df_table10
    df_temp_ <- df_table10_with_cc_post %>% select(USUBJID, contains("post_cc"))
    df_temp <- merge(df_table10, df_temp_, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp



    ## CC POST BINARY (whether any comorbid of interest present)
    var_name <- "Any of comorbid of interest presents at post-index"
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(post_cc_bin) %>%
      summarise(n = n())
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(post_cc_bin) %>%
      summarise(n = n())
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Yes", "No"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = var_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance
    fit <- glm((ACT_bin == "Controlled") ~ factor(post_cc_bin), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$post_cc_bin))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## summary all cc of interest at post-index
    var_name <- "Comorbid conditions at post-index"
    n_cc <- df_table10_with_cc_post %>%
      group_by(ACT_bin,post_cc_bin) %>%
      summarise(n = n()) %>%
      filter(!is.na(ACT_bin)) %>%
      group_by(ACT_bin) %>%
      summarise(n=sum(n))
    n_cc1 <- t(n_cc[c(2,1),2])
    #n_cc2 <- c(n_cc1, sum(n_cc1))
    n_cc1 <- table(df_table10_with_cc_post$ACT_bin)
    n_cc2 <- c(n_cc1[c(2,1)], sum(n_cc1))
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(post_cc_bin) %>%
      summarise_at(vars(post_cc1:post_cc8), sum, na.rm = TRUE) %>%
      filter(post_cc_bin == 1) %>%
      t()
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(post_cc_bin) %>%
      summarise_at(vars(post_cc1:post_cc8), sum, na.rm = TRUE) %>%
      filter(post_cc_bin == 1) %>%
      t()
    tab_act_tot <- tab_act_cont + tab_act_uncont
    tab_act <- cbind(tab_act_cont[-1], tab_act_uncont[-1], tab_act_tot[-1])
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / n_cc2[i] * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = str_to_title(list_cc),
                       Statistic = "n(%)",
                       tab_act_per)
    names(tab_)[3:5] <- c("n", "n.1", "n.2")
    tab_ <- rbind(cbind(Summary.Variable = var_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    table10 <- rbind(table10, tab_)



    ## Allergic Rhinitis
    cc_name <- str_to_title(list_cc[1])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Allergic Rhinitis`) %>%
      summarise(n = n())
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Allergic Rhinitis`) %>%
      summarise(n = n())
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Allergic Rhinitis
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Allergic Rhinitis`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Allergic Rhinitis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)



    ## Atherosclerosis
    cc_name <- str_to_title(list_cc[2])
    cc_name
    # subset controlled
    #tab_act_cont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Controlled") %>%
    #  group_by(`Atherosclerosis`) %>%
    #  summarise(n = n())
    tab_act_cont[,2] <- 0
    # subset uncontrolled
    #tab_act_uncont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Uncontrolled") %>%
    #  group_by(`Atherosclerosis`) %>%
    #  summarise(n = n())
    tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Atherosclerosis
    #fit <- glm((ACT_bin == "Controlled") ~ factor(`Atherosclerosis`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Atherosclerosis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Cardiac Arrhythmia
    cc_name <- str_to_title(list_cc[3])
    cc_name
    # subset controlled
    #tab_act_cont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Controlled") %>%
    #  group_by(`Cardiac Arrhythmia`) %>%
    #  summarise(n = n())
    tab_act_cont[,2] <- 0
    # subset uncontrolled
    #tab_act_uncont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Uncontrolled") %>%
    #  group_by(`Cardiac Arrhythmia`) %>%
    #  summarise(n = n())
    tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Cardiac Arrhythmia
    #fit <- glm((ACT_bin == "Controlled") ~ factor(`Cardiac Arrhythmia`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Cardiac Arrhythmia`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Congestive Heart Failure
    cc_name <- str_to_title(list_cc[4])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Congestive Heart Failure`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Congestive Heart Failure`) %>%
      summarise(n = n())
    tab_act_uncont <- rbind(tab_act_uncont,c(1,0))
    tab_act_uncont2 <- tab_act_uncont[c(1,3,2),]
    tab_act_uncont <- tab_act_uncont2
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Congestive Heart Failure
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Congestive Heart Failure`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Congestive Heart Failure`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Diabetes
    cc_name <- str_to_title(list_cc[5])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Diabetes`) %>%
      summarise(n = n())
    tab_act_cont <- rbind(tab_act_cont, c(1,0))
    tab_act_cont2 <- tab_act_cont[c(1,3,2),]
    tab_act_cont <- tab_act_cont2
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Diabetes`) %>%
      summarise(n = n())
    tab_act_uncont <- rbind(tab_act_uncont, c(1,0))
    tab_act_uncont2 <- tab_act_uncont[c(1,3,2),]
    tab_act_uncont <- tab_act_uncont2
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Diabetes
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Diabetes`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Diabetes`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Gastroesophageal Reflux Disease (Gerd)
    cc_name <- str_to_title(list_cc[6])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Gastroesophageal Reflux Disease (Gerd)`) %>%
      summarise(n = n())
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Gastroesophageal Reflux Disease (Gerd)`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Gastroesophageal Reflux Disease (Gerd)
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Gastroesophageal Reflux Disease (Gerd)`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Gastroesophageal Reflux Disease (Gerd)`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Obesity
    cc_name <- str_to_title(list_cc[7])
    cc_name
    # subset controlled
    tab_act_cont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Controlled") %>%
      group_by(`Obesity`) %>%
      summarise(n = n())
    tab_act_cont <- rbind(tab_act_cont, c(1,0))
    tab_act_cont2 <- tab_act_cont[c(1,3,2),]
    tab_act_cont <- tab_act_cont2
    #tab_act_cont[,2] <- 0
    # subset uncontrolled
    tab_act_uncont <- df_table10_with_cc_post %>%
      filter(ACT_bin == "Uncontrolled") %>%
      group_by(`Obesity`) %>%
      summarise(n = n())
    #tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[,i]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Obesity
    fit <- glm((ACT_bin == "Controlled") ~ factor(`Obesity`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    #pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Obesity`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    #n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)


    ## Osteoporosis
    cc_name <- str_to_title(list_cc[8])
    cc_name
    # subset controlled
    #tab_act_cont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Controlled") %>%
    #  group_by(`Osteoporosis`) %>%
    #  summarise(n = n())
    tab_act_cont[,2] <- 0
    # subset uncontrolled
    #tab_act_uncont <- df_table10_with_cc_post %>%
    #  filter(ACT_bin == "Uncontrolled") %>%
    #  group_by(`Osteoporosis`) %>%
    #  summarise(n = n())
    tab_act_uncont[,2] <- 0
    # subset total
    tab_act_total <- tab_act_cont[,2] + tab_act_uncont[,2]
    # combine
    tab_act <- cbind(tab_act_cont[,2], tab_act_uncont[,2], tab_act_total)
    tab_act2 <- tab_act[c(2,1),]
    tab_act <- tab_act2
    tab_act_per <- tab_act
    for (i in 1:ncol(tab_act)) {
      for (j in 1:nrow(tab_act)) {
        n_ij <- tab_act[j,i]
        perc_ij <- n_ij / sum(tab_act[i,1]) * 100
        perc_ij_c <- round_fmt(perc_ij, 1)
        tab_act_per[j,i] <- paste(n_ij, " (", perc_ij_c, "%)", sep = "")
      }
    }
    tab_ <- data.frame(Summary.Variable = c("Present", "Not present"),
                       Statistic = "n(%)",
                       tab_act_per)
    tab_ <- rbind(cbind(Summary.Variable = cc_name,
                        Statistic = NA,
                        n = NA, n.1 = NA, n.2 = NA), tab_)
    names(tab_)[3:5] <- names(table10)[3:5]
    #table10 <- rbind(table10, tab_)
    # test for significance:  Obesity
    #fit <- glm((ACT_bin == "Controlled") ~ factor(`Osteoporosis`), data = df_table10_with_cc_post,family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- NA
    # check proportion of missingness
    sub_act_tot <- df_table10_with_cc_post %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$`Osteoporosis`))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    n_perc_missing <- NA
    subtab_w_pval <- data.frame(tab_,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    #table10b <- rbind(table10b, subtab_w_pval)








    ## ACT by Spirometry result
    df_temp <- data.frame("Spirometry", NA, NA, NA, NA)
    names(df_temp) <- names(table10)
    table10 <- rbind(table10, df_temp)
    df_temp <- merge(df_table10, df_spirometry, by = "USUBJID", all = TRUE)
    df_table10 <- df_temp

    ## ACT by FEV1 (%) at index
    var_name <- "FEV1 (%) at index"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(na.omit(sub_act_cont$FEV1PP_INDEX), var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$FEV1PP_INDEX), var_name)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Uncontrolled" | ACT_bin == "Controlled")
    tab_tot <- cont_stat(na.omit(sub_act_tot$FEV1PP_INDEX), var_name)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ FEV1PP_INDEX, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$FEV1PP_INDEX))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by FEV1 (%) at 6- to 12-months post-index
    var_name <- "FEV1 (%) at at 6- to 12-months post-index"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(na.omit(sub_act_cont$FEV1PP_POST), var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$FEV1PP_POST), var_name)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(na.omit(sub_act_tot$FEV1PP_POST), var_name)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ FEV1PP_POST, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$FEV1PP_POST))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Total asthma exacerbation events (of any kind) per year, per participant
    var_name <- "Total asthma exacerbation events (of any kind) per year, per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(excr_TOT != 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$excr_TOT), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$excr_TOT, na.rm = TRUE)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(excr_TOT != 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$excr_TOT), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$excr_TOT, na.rm = TRUE)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(na.omit(sub_act_tot$excr_TOT[sub_act_tot$excr_TOT != 0]), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$excr_TOT, na.rm = TRUE)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ excr_TOT, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$excr_TOT))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ACT by Total asthma exacerbation events (of any kind) with steroid use per year, per participant
    var_name <- "Total asthma exacerbation events (of any kind) with steroid use per year, per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(excr_TOT_steroid != 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$excr_TOT_steroid), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$excr_TOT_steroid, na.rm = TRUE)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(excr_TOT_steroid != 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$excr_TOT_steroid), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$excr_TOT_steroid, na.rm = TRUE)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled") %>% filter(excr_TOT_steroid != 0)
    tab_tot <- cont_stat(na.omit(sub_act_tot$excr_TOT_steroid[sub_act_tot$excr_TOT_steroid != 0]), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$excr_TOT_steroid, na.rm = TRUE)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ excr_TOT_steroid, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$excr_TOT_steroid))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Total asthma exacerbation events requiring a physician visit and steroid prescription per year, per participant
    var_name <- "Total asthma exacerbation events requiring a physician visit and steroid prescription per year, per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(excr_PV != 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$excr_PV), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$excr_PV, na.rm = TRUE)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(excr_PV != 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$excr_PV), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$excr_PV, na.rm = TRUE)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled") %>% filter(excr_PV != 0)
    tab_tot <- cont_stat(na.omit(sub_act_tot$excr_PV[sub_act_tot$excr_PV != 0]), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$excr_PV, na.rm = TRUE)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ excr_PV, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$excr_PV))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ACT by Total asthma exacerbation events requiring an ER visit per year, per participant
    var_name <- "Total asthma exacerbation events requiring an ER visit per year, per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(excr_ER != 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$excr_ER), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$excr_ER, na.rm = TRUE)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(excr_ER != 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$excr_ER), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$excr_ER, na.rm = TRUE)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled") %>% filter(excr_ER != 0)
    tab_tot <- cont_stat(na.omit(sub_act_tot$excr_ER[sub_act_tot$excr_ER != 0]), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$excr_ER, na.rm = TRUE)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ excr_ER, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$excr_ER))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ACT by Total asthma exacerbation events requiring a hospitalization per year, per participant
    var_name <- "Total asthma exacerbation events requiring a hospitalization per year, per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(excr_HO != 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$excr_HO), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$excr_HO, na.rm = TRUE)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(excr_HO != 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$excr_HO), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$excr_HO, na.rm = TRUE)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled") %>% filter(excr_HO != 0)
    tab_tot <- cont_stat(na.omit(sub_act_tot$excr_HO[sub_act_tot$excr_HO != 0]), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$excr_HO, na.rm = TRUE)# combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ excr_HO, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    #pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    pval <- "0.987"
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$excr_HO))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Total number of scheduled physician visits per participant
    var_name <- "Total number of scheduled physician visits per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(na.omit(sub_act_cont$Schedule_Visit), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$Schedule_Visit)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$Schedule_Visit), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$Schedule_Visit)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(na.omit(sub_act_tot$Schedule_Visit), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$Schedule_Visit)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Schedule_Visit, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Schedule_Visit))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by Total number of unscheduled physician visits per participant
    var_name <- "Total number of unscheduled physician visits per participant"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled") %>% filter(Unschedule_Visit > 0)
    tab_cont <- cont_stat(na.omit(sub_act_cont$Unschedule_Visit), var_name)
    tab_cont$stat_0[2] <- sum(sub_act_cont$Unschedule_Visit)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled") %>% filter(Unschedule_Visit > 0)
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$Unschedule_Visit), var_name)
    tab_uncont$stat_0[2] <- sum(sub_act_uncont$Unschedule_Visit)
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled") %>% filter(Unschedule_Visit > 0)
    tab_tot <- cont_stat(na.omit(sub_act_tot$Unschedule_Visit), var_name)
    tab_tot$stat_0[2] <- sum(sub_act_tot$Unschedule_Visit)
    # combine all subtabs
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ Unschedule_Visit, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$Unschedule_Visit))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)




    ## ACT by ICS/LABA prescription continuity
    var_name <- "ICS/LABA prescription continuity"
    # pooling data adherence
    tab_ <- df_table10 %>%
      group_by(ACT_bin, adherence) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    subtab[2:3,1] <- c("Continuous prescriptions/refills recorded", "Interruptions/incomplete data available")
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ adherence, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$adherence))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription) - continuous versions
    var_name <- "ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(na.omit(sub_act_cont$treatment_duration), var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$treatment_duration), var_name)
    treat_dur_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(na.omit(treat_dur_tot$treatment_duration), var_name)
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ treatment_duration, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$treatment_duration))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)


    ## ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)
    var_name <- "ICS/LABA treatment duration: number of days on treatment (excluding gaps in prescription)"
    # pooling data adherence
    tab_ <- df_table10 %>%
      group_by(ACT_bin, treatment_duration_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    tab_wide2 <- tab_wide[c(1:3),]
    tab_wide <- tab_wide2
    # include total column
    tab_total <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab[-1,])
    # test for significance
    fit <- glm(ACT_bin_dummy ~ treatment_duration_cat, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>%
      filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$treatment_duration_cat))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 3)),
                                missing = c(n_perc_missing, rep(NA, 3)))
    table10b <- rbind(table10b, subtab_w_pval)



    ## ACT by ICS/LABA treatment duration: number of months treated (including prescription gaps) - continuous versions
    var_name <- "ICS/LABA treatment duration: number of months treated (including prescription gaps)"
    sub_act_cont <- df_table10 %>% filter(ACT_bin == "Controlled")
    tab_cont <- cont_stat(na.omit(sub_act_cont$treatment_duration_include_gap), var_name)
    sub_act_uncont <- df_table10 %>% filter(ACT_bin == "Uncontrolled")
    tab_uncont <- cont_stat(na.omit(sub_act_uncont$treatment_duration_include_gap), var_name)
    treat_dur_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    tab_tot <- cont_stat(na.omit(treat_dur_tot$treatment_duration_include_gap), var_name)
    tab_ <- data.frame(tab_cont, tab_uncont$stat_0, tab_tot$stat_0)
    subtab <- data.frame(`Summary Variable` = c(var_name, rep(NA,6)), Statistic = tab_$label[-1], tab_[-1,-1])
    # header name
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab)
    # test for significance
    fit <- glm(ACT_bin_dummy ~ treatment_duration_include_gap, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>% filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$treatment_duration))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 6)),
                                missing = c(n_perc_missing, rep(NA, 6)))
    table10b <- rbind(table10b, subtab_w_pval)




    ## ICS/LABA treatment duration: number of months treated (including prescription gaps)
    var_name <- "ICS/LABA treatment duration: number of months treated (including prescription gaps)"
    # pooling data adherence
    tab_ <- df_table10 %>%
      group_by(ACT_bin, treatment_duration_include_gap_cat) %>%
      summarise(n = n())
    # crostab
    tab_ <- tab_[which(!is.na(tab_$ACT_bin)),]
    tab_wide <- spread(tab_, ACT_bin, n)
    # include total column
    tab_total <- rowSums(tab_wide[,-1])
    tab_wide_w_total <- data.frame(tab_wide[,c(1,3,2)], Total = c(tab_total))
    # formatting percentage to 1 decimal place and combine to count number
    tab_wide_perc_c <- tab_wide_w_total[,-1]
    for (i in 1:ncol(tab_wide_perc_c)){
      for (j in 1:nrow(tab_wide_perc_c)) {
        n_ij <- tab_wide_w_total[j,i+1]
        if (is.na(n_ij)) {
          n_ij <- 0
        }
        perc_ij <- n_ij / sum(tab_wide_w_total[,i+1], na.rm = TRUE) * 100
        if (is.na(perc_ij)) {
          perc_ij <- "0.0"
        } else {
          perc_ij <- round_fmt(perc_ij, 1)
        }
        tab_wide_perc_c[j,i] <- paste(n_ij, " (", perc_ij, "%)", sep = "")
      }
    }
    stat_0 <- rbind(NA, tab_wide_perc_c)
    names(stat_0) <- header_tab10
    names(tab_wide)[1] <- "label"
    subtab <- data.frame(`Summary Variable` = c(var_name, as.character(tab_wide$label)),
                         Statistic = c(NA, rep("n(%)", nrow(tab_wide))),
                         stat_0)
    names(subtab)[3:5] <- header_tab10
    table10 <- rbind(table10, subtab[-1,])
    # test for significance
    fit <- glm(ACT_bin_dummy ~ treatment_duration_include_gap_cat, data = df_table10, family = "binomial")
    summary(fit)
    fit.aov <- anova(fit, test = "Chisq")
    fit.aov
    pval <- round_fmt(fit.aov$`Pr(>Chi)`[2],3)
    # check proportion of missingness
    sub_act_tot <- df_table10 %>%
      filter(ACT_bin == "Controlled" | ACT_bin == "Uncontrolled")
    n_missing <- as.numeric(N_header[3]) - sum(table(sub_act_tot$treatment_duration_cat))
    perc_missing <- round_fmt(n_missing / as.numeric(N_header[3]) * 100, 1)
    n_perc_missing <- paste(n_missing, " (", perc_missing, "%)", sep = "")
    subtab_w_pval <- data.frame(subtab,
                                pval = c(pval, rep(NA, 2)),
                                missing = c(n_perc_missing, rep(NA, 2)))
    table10b <- rbind(table10b, subtab_w_pval)



    # save file
    rownames(table10) <- NULL
    write.csv(table10, "table10.csv")
    rownames(table10b) <- NULL
    write.csv(table10b, "table10b.csv")



    ## Table 11


    ## Fit multiple logistic regression
    df_glm <- df_table10
    write.csv(df_glm, "df_glm.csv")
    save(df_glm, file = "df_glm.Rdata")


    # Education change reference to post graduate degree
    df_glm$Educ_Level <- factor(df_glm$Educ_Level, levels = c("Illiterate",
                                                              "Not Finishing Formal Education",
                                                              "High School Graduate",
                                                              "University Graduate (Bachelor)",
                                                              "Post Graduate Degree (Master/Doctor/Post-Doctoral)",
                                                              "Unknown or Not Reported"))
    df_glm$Educ_Level <- relevel(df_glm$Educ_Level, ref = "Post Graduate Degree (Master/Doctor/Post-Doctoral)")

    # Combine bachelor and post doc into one category
    df_glm$Educ_Level2 <- df_glm$Educ_Level
    df_glm$Educ_Level2[which(df_glm$Educ_Level == "Post Graduate Degree (Master/Doctor/Post-Doctoral)")] <- "University Graduate (Bachelor)"
    df_glm$Educ_Level2 <- relevel(df_glm$Educ_Level2, ref = "University Graduate (Bachelor)")

    # Change reference category for sex
    df_glm$SEX <- factor(df_glm$SEX)
    df_glm$SEX <- relevel(df_glm$SEX, ref = "Male")

    # Change reference category for smoking history
    df_glm$Smoke_Hist[which(df_glm$Smoke_Hist == "Unknown")] <- NA
    df_glm$Smoke_Hist <- factor(df_glm$Smoke_Hist)
    df_glm$Smoke_Hist <- relevel(df_glm$Smoke_Hist, ref = "Never Smoke")

    # Change reference category for adherence
    df_glm$adherence <- factor(df_glm$adherence)
    df_glm$adherence <- relevel(df_glm$adherence, ref = "Adherent")

    # Change reference category for employment
    df_glm$Emp_Job <- factor(df_glm$Emp_Job, levels = c("Employed (full or part-time)",
                                                        "Self-employed",
                                                        "Student",
                                                        "House Spouse",
                                                        "Unemployed (not working/retiree)",
                                                        "Unknown or Not Reported"))
    df_glm$Emp_Job <- relevel(df_glm$Emp_Job, ref = "Employed (full or part-time)")



    ## fit base model
    library(glmtoolbox)
    library(logistf)
    fit_base1 <- logistf(ACT_bin_dummy ~ treatment_duration +
                           excr_PV +
                           Schedule_Visit,
                         data = df_glm,
                         control=logistf.control(maxit=100))
    #summary(fit_base1)
    #extractAIC(fit_base1)
    fit_base2 <- logistf(ACT_bin_dummy ~ SEX + BMI + Smoke_Hist + adherence + treatment_duration +
                           excr_PV +
                           Schedule_Visit,
                         data = df_glm,
                         control=logistf.control(maxit=100))
    #summary(fit_base2)
    #extractAIC(fit_base2)
    #anova(fit_base1, fit_base2)
    #drop1(fit_base2)


    ## fit reduced model 1
    fit_reduced <- logistf(ACT_bin_dummy ~ SEX + Smoke_Hist + adherence + treatment_duration +
                             excr_PV +
                             Schedule_Visit,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)
    #summary(fit_reduced)


    ## fit reduced model 2
    fit_reduced <- logistf(ACT_bin_dummy ~ SEX + Smoke_Hist + adherence  +
                             excr_PV +
                             Schedule_Visit,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)


    ## fit reduced model 3
    fit_reduced <- logistf(ACT_bin_dummy ~ SEX + adherence  +
                             excr_PV +
                             Schedule_Visit,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)


    ## fit reduced model 4
    fit_reduced <- logistf(ACT_bin_dummy ~  adherence  +
                             excr_PV +
                             Schedule_Visit,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)

    fit_reduced <- logistf(ACT_bin_dummy ~
                             excr_PV +
                             Schedule_Visit,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)


    ## fit reduced model 5
    fit_reduced <- logistf(ACT_bin_dummy ~
                             excr_PV,
                           data = df_glm,
                           control=logistf.control(maxit=100))
    #drop1(fit_reduced)
    #extractAIC(fit_reduced)
    #summary(fit_reduced)

    #anova(fit_base2, fit_reduced)






    ######## Figure 1: Pie Chart of ACT Outcome at 6-12 Months Post-index
    df_fig1$AVALC <- str_to_title(df_fig1$AVALC)
    fig1 <-  ggplot(df_fig1, aes(x = "", fill = AVALC)) +
      geom_bar(stat= "count", width = 1, color = "white") +
      geom_text(aes(label = scales::percent(..count.. / sum(..count..))), stat = "count", position = position_stack(vjust = .5)) +
      coord_polar("y", start = 0, direction = -1) +
      scale_fill_manual(values = c("#66CC99", "lightblue", "lightgrey")) +
      theme_void() +
      theme(legend.title=element_blank()) +
      ggtitle("N = 252") +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave("figure1.png", fig1, width = 5, height = 4, dpi = 300, units = "in", device='png')



    ######## Figure 2: Boxplot of ACT Score at Index and 6-12 Months Post-index
    label_index <- paste("Index [N = ", length(na.omit(df_fig2$ACT_index)), "]",sep = "")
    label_post <- paste("Post-Index [N = ", length(na.omit(df_fig2$ACT_post)), "]",sep = "")
    names(df_fig2)[2:3] <- c(label_index, label_post)
    df_fig_long <- melt(df_fig2, id.vars = "USUBJID")
    fig2 <-  ggplot(df_fig_long) +
      geom_boxplot(aes(x = variable, y = value, group = variable), fill = "lightblue") +
      xlab("") +
      ylab("ACT Numerical Score") +
      theme_bw()
    ggsave("figure2.png", fig2, width = 5, height = 4, dpi = 300, units = "in", device='png')


    ######## Figure 3: Stacked-bar Chart of ACT Outcome at Index and 6-12 Months Post-index
    label_index <- paste("Index [N = ", length(na.omit(df_table10$ACT_CAT_INDEX)), "]",sep = "")
    label_post <- paste("Post-Index [N = ", length(na.omit(df_table10$ACT_CAT2_POST)), "]",sep = "")
    # calculate frequency table for ACT at index
    df_freq_index <- df_table10 %>%
      group_by(ACT_CAT_INDEX) %>%
      count() %>%
      ungroup() %>%
      na.omit() %>%
      mutate(perc = `n` / sum(`n`) * 100,
             label = paste(round(perc,0), "%", sep =""))
    df_freq_index$Period <- label_index
    # remove NA
    df_freq_index <- df_freq_index[1:2,]
    names(df_freq_index)[1] <- "group"
    # calculate frequency table for ACT at post-index
    df_freq_post <- df_table10 %>%
      select(ACT_CAT2_POST) %>%
      na.omit() %>%
      group_by(ACT_CAT2_POST) %>%
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`) * 100,
             label = paste(round(perc,0), "%", sep =""))
    df_freq_post$Period <- label_post
    names(df_freq_post)[1] <- "group"
    # combine both table frequencies
    df_ <- rbind(df_freq_index, df_freq_post)
    df_$group <- str_to_title(df_$group)
    # stacked bar-chart
    fig3 <- ggplot(df_, aes(fill=group, y=perc, x=Period)) +
      geom_bar(position="fill", stat="identity") +
      geom_text(aes(y = perc/100, label = label), color = "black", size = 3, position = position_stack(vjust = 0.5)) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("lightgrey","lightblue")) +
      theme_minimal() +
      theme(legend.title=element_blank()) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    ggsave("figure3.png", fig3, width = 4, height = 4, dpi = 300, units = "in", device='png')




    ######## Figure 4:Plot of Total Number of ER and Hospitalization Asthma Exacerbations with and without Steroid Treatment Post-Index
    df_fig5 <- df_table10 %>% select(USUBJID,excr_ER, excr_HO,
                                     excr_ER_steroid, excr_HO_steroid,
                                     excr_ER_nonsteroid, excr_HO_nonsteroid)
    #df_fig5$USUBJID <- crf2$Study.Subject.ID
    df_fig5_long <- melt(df_fig5, id.vars = "USUBJID")
    # create column to identify whether ER or Hospitalization
    ER_HO <- rep(NA, nrow(df_fig5_long))
    ER_HO[grepl("ER", df_fig5_long$variable)] <- "ER Visit"
    ER_HO[grepl("HO", df_fig5_long$variable)] <- "Hospitalization"
    # create column to identify whether suing steroid use or not
    Steroid <- rep("Total Exacerbation", nrow(df_fig5_long))
    Steroid[grepl("steroid", df_fig5_long$variable)] <- "Total Exacerbation with Steroid Use"
    Steroid[grepl("nonsteroid", df_fig5_long$variable)] <- "Total Exacerbation without Steroid Use"
    df_fig5_long$ER_HO <- ER_HO
    df_fig5_long$Steroid <- Steroid
    # summary table
    sum_tab <- df_fig5_long %>% group_by(ER_HO, Steroid) %>%
      summarise(n = sum(value, na.rm = TRUE))
    fig4 <- ggplot(sum_tab) +
      geom_bar(aes(x = ER_HO, y = n, fill = Steroid), stat = "identity",  position = position_dodge()) +
      ylab("Number of exacerbations") +
      xlab("") +
      theme_minimal() +
      theme(legend.title=element_blank(), legend.position = 'bottom')
    ggsave("figure4.png", fig4, width = 7, height = 4, dpi = 300, units = "in", device='png')




    ######## Figure 5: Boxplot of FEV1(%) Measured from Index or Nearest Index (up to 60 days pre-Index) to 12 Months Post-Index
    df_fig5 <- df_table10 %>% select(USUBJID, FEV1PP_INDEX, FEV1PP_POST)
    df_fig5 <- rename(df_fig5, `Index or nearest index < 6 months` = FEV1PP_INDEX,
                      `6-12 months` = FEV1PP_POST)
    df_fig5_long <- melt(df_fig5, id.vars = "USUBJID")
    df_fig5_long_tot <- df_fig5_long
    df_fig5_long_tot$variable <- "Total"
    # combine
    df_ <- rbind(df_fig5_long, df_fig5_long_tot)
    df_$variable <- as.factor(df_$variable)
    df_$variable <- factor(df_$variable, levels = c("Total",
                                                    "Index or nearest index < 6 months",
                                                    "6-12 months"))
    fig5 <-  ggplot(df_) +
      geom_boxplot(aes(x = variable, y = value, group = variable), fill = "lightblue") +
      xlab("") +
      ylab("FEV1 (%)") +
      theme_bw()
    ggsave("figure5.png", fig5, width = 5, height = 4, dpi = 300, units = "in", device='png')


    ######## Figure 6: Estimated Odds Ratios (95% CI) for Predictive Parameters of ACT Control
    or <- exp(fit_base1$coefficients)
    ci.low <- exp(fit_base1$ci.lower)
    ci.up <- exp(fit_base1$ci.upper)
    data <- data.frame(predictor = names(or), odds_ratio = or, lower_ci = ci.low, upper_ci = ci.up) [-1,]
    data$predictor <- c("ICS/LABA treatment duration in days (exclude the gaps)",
                        "Number of exacerbation requiring physician visit with streoid use",
                        "Scheduled visit")
    data$predictor <- factor(data$predictor, levels = c("ICS/LABA treatment duration in days (exclude the gaps)",
                                                        "Number of exacerbation requiring physician visit with streoid use",
                                                        "Scheduled visit"))

    fig6 <- ggplot(data, aes(y = predictor, x = odds_ratio, xmin = lower_ci, xmax = upper_ci)) +
      geom_pointrange() +
      ylab("Asthma Control Factor") +
      xlab("Odds Ratio (95% CI)") +
      geom_vline(xintercept = 1, color = "red", linewidth = 0.1) +
      theme_bw()
    ggsave("figure6v2a.png", fig6, width = 6, height = 6, dpi = 300, units = "in", device='png')

    # plot base model 2
    or <- exp(fit_base2$coefficients)
    ci.low <- exp(fit_base2$ci.lower)
    ci.up <- exp(fit_base2$ci.upper)
    data <- data.frame(predictor = names(or), odds_ratio = or, lower_ci = ci.low, upper_ci = ci.up) [-1,]
    data$predictor <- c("Sex: Female",
                        "BMI",
                        "Smoking history: current smoker",
                        "Smoking history: former smoker",
                        "ICS/LABA  prescription continuity: Interruptions/incomplete data available",
                        "ICS/LABA treatment duration in days (exclude the gaps)",
                        "Number of exacerbation requiring physician visit with streoid use",
                        "Scheduled visit")
    data$predictor <- factor(data$predictor, levels = c("Sex: Female",
                                                        "BMI",
                                                        "Smoking history: current smoker",
                                                        "Smoking history: former smoker",
                                                        "ICS/LABA  prescription continuity: Interruptions/incomplete data available",
                                                        "ICS/LABA treatment duration in days (exclude the gaps)",
                                                        "Number of exacerbation requiring physician visit with streoid use",
                                                        "Scheduled visit"))

    fig6 <- ggplot(data, aes(y = predictor, x = odds_ratio, xmin = lower_ci, xmax = upper_ci)) +
      geom_pointrange() +
      ylab("Asthma Control Factor") +
      xlab("Odds Ratio (95% CI)") +
      geom_vline(xintercept = 1, color = "red", linewidth = 0.1) +
      theme_bw()
    ggsave("figure6v2b.png", fig6, width = 6, height = 6, dpi = 300, units = "in", device='png')


    # plot base model 3
    or <- exp(fit_reduced$coefficients)
    ci.low <- exp(fit_reduced$ci.lower)
    ci.up <- exp(fit_reduced$ci.upper)
    data <- data.frame(predictor = names(or), odds_ratio = or, lower_ci = ci.low, upper_ci = ci.up) [-1,]
    data$predictor <- c("Number of exacerbation requiring physician visit with streoid use")
    data$predictor <- factor(data$predictor, levels = c("Number of exacerbation requiring physician visit with streoid use"))

    fig6 <- ggplot(data, aes(y = predictor, x = odds_ratio, xmin = lower_ci, xmax = upper_ci)) +
      geom_pointrange() +
      ylab("Asthma Control Factor") +
      xlab("Odds Ratio (95% CI)") +
      geom_vline(xintercept = 1, color = "red", linewidth = 0.1) +
      theme_bw()
    ggsave("figure6v2c.png", fig6, width = 6, height = 6, dpi = 300, units = "in", device='png')

  }))


  input_file <- system.file("ACT Main Analysis - TFL Markdown.Rmd", package = "ACTpackage")
  # Check if the input file exists
  if (!file.exists(input_file)) {
    stop("The input file does not exist.")
  }

  # Set default output file if not provided
  if (is.null(output_file)) {
    output_file <- sub("\\.Rmd$", ".docx", input_file)
  }

  # Render the RMarkdown file to Word document
  rmarkdown::render(input = input_file,
                    output_format = "word_document",
                    output_file = output_file,
                    params = params)

  # Print message to the console
  cat("The tables and figures have been calculated.\n")
  cat("The TFL document has been generated at:", output_file, "\n")



}



