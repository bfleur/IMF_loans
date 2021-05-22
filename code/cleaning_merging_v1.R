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

#################################################################
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

library(lubridate)

parse_date_time(IMF_work$rev_enddate, orders = "ymd")

# we put the dates into Date format and create a new variable for extracting the 
# year from it
IMF_work$rev_enddate <- as.Date(IMF_work$rev_enddate)
class(IMF_work$rev_enddate)

IMF_work$rev_endyear <- year(IMF_work$rev_enddate)

IMF_work <- IMF_work %>%
  mutate(rev_endyr = year(rev_enddate)) %>%
  # mutate(real_endyr =  in_endyr) #%>%
  mutate(real_endyr = ifelse(is.na(rev_endyr), in_endyr, rev_endyr))

# delete revise and initial date variables
IMF_work <- subset(IMF_work, select = -c(rev_enddate, rev_endyr, in_enddate, in_endyr))

# add ISO-3 character country codes for merging
#install.packages("countrycode")
library(countrycode)

IMF_work$iso3code <- countrycode(IMF_work$cname, origin = 'country.name', destination = 'iso3c') 

IMF_work$iso3code[IMF_work$cname == 'KOSOVO, REPUBLIC OF'] <- 'XKX'
IMF_work$iso3code[IMF_work$cname == 'SERBIA AND MONTENEGRO'] <- 'SCG'

# expand dataframe
colnames(IMF_work)

exp_IMF <- rowwise(IMF_work) %>% 
  do(tibble(arr_nr = .$arr_nr, cname=.$cname, arrtype = .$arrtype, apprdate = .$apprdate,
            iso3code=.$iso3code, prtype=.$prtype, revtype=.$revtype, revseq=.$revseq, t_access=.$t_access,
            con_textbox=.$con_textbox, del_by=.$del_by, canc=.$canc, comments=.$comments, 
            z=.$appryr:.$real_endyr))

#save data file -- it contains only those countries and years where programs were
# undergoing!!! does not cover all years!
write.csv(exp_IMF, paste(data_out, 'IMF_MONA.csv', sep = "/"), row.names = FALSE)

#####################################################################
# 1.3
# additional data from quota revision file

excel_sheets(paste(data_in,"Q_shares_IMF.xlsx", sep= "/"))

quotash <- read_excel(paste(data_in,"Q_shares_IMF.xlsx", sep= "/"), sheet = 1)

colnames(quotash)
names(quotash) <- c("country", "quotas_bef", "quotas_aft")


?codelist
quotash$ccode <- countrycode(quotash$country, origin = 'country.name', destination = 'iso3c') 

quotash$ccode[quotash$country == 'Kosovo'] <- 'XKX'
quotash$ccode[quotash$country == 'Micronesia, FS of'] <- 'FSM'

#add year column to quotash
year = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
         "2020")

quotash <- quotash %>%
  group_by(ccode) %>%
  mutate(yr_st = "2011") %>%
  mutate(yr_end = "2020")

#install.packages("reshape")
#library(reshape)

exp_quotash <- rowwise(quotash) %>% 
  do(tibble(ccode = .$ccode, quotas_bef=.$quotas_bef, quotas_aft = .$quotas_aft,
                z=.$yr_st:.$yr_end))

# spread quota columns across years
sp_quotash <- exp_quotash %>%
  group_by(ccode) %>%
  mutate(quotash = ifelse(z <= 2015, quotas_bef, quotas_aft))

# delete quotash before and after columns
sp_quotash <- subset(sp_quotash, select = -c(quotas_bef, quotas_aft))


#IMF_m <- merge(x = exp_IMF, y = sp_quotash,
#                 by.x = c("iso3code", "z"),
#                 by.y=c("ccode", "z"),
#                 all.x = TRUE)

# save datafile
write.csv(sp_quotash, paste(data_out, 'quotashares1120.csv', sep = "/"), row.names = FALSE)

#--------------------------------------------------------------------
# I./2. political

# 2.1 Pol. proximity to US and China
data_raw <- read.csv(paste(data_in, "NFA 2018.csv", sep = ""))








