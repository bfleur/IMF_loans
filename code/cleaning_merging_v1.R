# Virag Bitto (ID: 1903164)
# Thesis
# ------------------------------------------------------------------------------------------------------
# I. setup
# It is advised to start a new session
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)
#install.packages("haven")
library(haven)
library(stargazer)
library(ggplot2)
library(car)
#install.packages("huxtable")
library(huxtable)
#install.packages("estimatr")
library(estimatr)
#install.packages("lmtest")
library(lmtest)
#install.packages("fixest")
library(fixest)
#install.packages("urca")
library(urca)
#install.packages("tidyr")
library(tidyr)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("naniar")
library(naniar)
#install.packages("UpSetR")
library(UpSetR)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")
getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis")


# set data dir, data used
#source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

data_dir= "/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

data_in <- paste0(data_dir,"data_in/")
data_out <- paste0(data_dir,"data_out/")
output <- paste0(getwd(),"/output/")
create_output_if_doesnt_exist(output)

#---------------------------------------------------------------------------
# I.
# Loading and cleaning data one by one (where makes sense to do it separately)

# I./1. Economic vars

# 1.1 World Bank
#install.packages("wbstats")
library(wbstats)


my_wb_indicators <- c(
  gdppc_gr = "NY.GDP.PCAP.KD.ZG", 
  res_im ="FI.RES.TOTL.MO", 
  debts_ratio = "DT.TDS.DECT.EX.ZS",
  tr_bal_gdp = "NE.RSB.GNFS.ZS",
  gdppc = "NY.GDP.PCAP.KD",
  fdi_ofgdp = "BX.KLT.DINV.WD.GD.ZS",
  std_totald = "DT.DOD.DSTC.ZS",
  gdpdefl = "NY.GDP.DEFL.KD.ZG.AD",
  ex_rate = "PA.NUS.FCRF",
  gdp_cusd = "NY.GDP.MKTP.CD"
)

wb <- wb_data(my_wb_indicators, start_date = 2010, end_date = 2020)

colnames(wb)[4] <- "year"

write.csv(wb, paste(data_out, 'WB_data_1020.csv', sep = "/"), row.names = FALSE)

# 1.2 IMF data

#install.packages("readxl")
library(readxl)

excel_sheets(paste(data_in,"MONA_all.xlsx", sep= "/"))

IMF_all <- read_excel(paste(data_in,"MONA_all.xlsx", sep= "/"), sheet = 1)

colnames(IMF_all)

IMF_sset <- subset(IMF_all, select = c("Arrangement Number","Country Name",
"Country Code", "Arrangement Type", "Approval Date", "Approval Year", 
"Initial End date", "Initial End Year", "Revised End Date", "Program Type", 
"Review Type", "Review Sequence", "Totalaccess", 
"Conditionality Text Box included in Staff Report", "Delayedby", "Cancelled",
"Comments", "Sort"))     

names(IMF_sset) <- c("arr_nr","cname","ccode","arrtype", "apprdate", "appryr",
                     "in_enddate", "in_endyr", "rev_enddate", "prtype", "revtype",
                     "revseq", "t_access", "con_textbox", "del_by", "canc", "comments", "sort")

# take only one observation (the latest) per arrangement
IMF_work <- IMF_sset %>%
  group_by(arr_nr) %>% top_n(1, sort)

#create add column for revised end year extraction, then use original or revised end date
# depending on which is available

class(IMF_work$rev_enddate)

IMF_work2 <- IMF_work %>%
  as.POSIXct(rev_enddate,
             format="%Y-%m-%dT%H:%M") #format time %>%
  #mutate(rev_endyr = format(rev_enddate, "%Y"))

