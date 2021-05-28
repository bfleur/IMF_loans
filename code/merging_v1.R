# Virag Bitto (ID: 1903164)
# Thesis
# FILE 2
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
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(countrycode)


# set working directory
getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis")


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
# Adding data one by one to merge

# World Bank
wb <- read.csv(paste(data_out, "WB_data_1020_pop.csv", sep = ""))

# IMF Program participation data
exp_IMF <- read.csv(paste(data_out, "IMF_MONA.csv", sep = ""))

# IMF quotashare data
sp_quotash <- read.csv(paste(data_out, "quotashares1120.csv", sep = ""))

# UN GA voting data
UNV_means <- read.csv(paste(data_out, "UNV_means.csv", sep = ""))

# V-dem electoral democracy
vdem <- read.csv(paste(data_out, "vdem.csv", sep = ""))

# WITS economic proximity
trade_US_CHN <- read.csv(paste(data_out, "eshares_US_CHN.csv", sep = ""))

# US aid data
US_aid <- read.csv(paste(data_out, "US_aid.csv", sep = ""))

# UN Security Council
UNSC <- read.csv(paste(data_out, "UNSC.csv", sep = ""))

# BRI participation dummy
BRI_w <- read.csv(paste(data_out, "BRI_part.csv", sep = ""))

# text conditionality
cm_m3 <- read.csv(paste(data_out, "corpus_meta_cond.csv", sep = ""))


# merging
# 1.
merged_raw <- merge(x = wb, y = sp_quotash,
                      by.x = c("iso3c", "year"),
                      by.y=c("ccode", "z"),
                      all = FALSE)

colnames(merged_raw)

merged_raw <- merged_raw %>%
  mutate(country_nr = as.integer(factor(iso3c)),
         logpop = log(pop)) %>%
  subset(select = -c(iso2c, country))

# 2.
# for a 5-yr moving average of participation, we need data from two time 
# periods before and after the time period that we are assigning an average value 
# to, therefore we import Program data from years before 2011

merged_raw2 <- merge(x = merged_raw, y = exp_IMF,
                     by.x = c("iso3c", "year"),
                     by.y=c("iso3code", "z"),
                     all = TRUE)


colnames(merged_raw2)

library(zoo)

# check how many observations denote cancelled arrangements (96)
length(which(merged_raw2$canc == "Y"))

merged_raw2 <- merged_raw2 %>%
  subset(select = -c(cname, revtype, revseq, con_textbox, del_by)) %>%
  mutate(IMF_program = ifelse(is.na(arr_nr), 0, 1),
         avg_part_yr = 5 * (rollmean(IMF_program, k = 5, fill = NA))) %>%
  filter(year > 2010 & year < 2020)

helptable_IMF <- merged_raw2 %>%
  group_by(iso3c) %>%
  count(factor(arr_nr))

helptable_IMF <- helptable_IMF[complete.cases(helptable_IMF), ]

merged_raw2 <- merge(x = merged_raw2, y = helptable_IMF,
                     by.x = c("iso3c", "arr_nr"),
                     by.y=c("iso3c", "factor(arr_nr)"),
                     all.x = TRUE)

merged_raw2 <- merged_raw2 %>%
  mutate(avg_yr_loansize = log(t_access/n)) %>%
  subset(select = -c(n))

# 3.
merged_raw3 <- merge(x = merged_raw2, y = UNV_means,
                     by.x = c("iso3c", "year"),
                     by.y=c("iso3code", "year"),
                     all.x = TRUE)

colnames(merged_raw3)

merged_raw3 <- merged_raw3 %>%
  subset(select = -c(member, vote, Countryname))

# 4.
merged_raw4 <- merge(x = merged_raw3, y = vdem,
                     by.x = c("iso3c", "year"),
                     by.y=c("iso3code", "year"),
                     all.x = TRUE)

colnames(merged_raw4)

merged_raw4 <- merged_raw4 %>%
  subset(select = -c(country_name, country_id, COWcode)) %>%
  rename(el_democr = v2x_polyarchy)

# 5.
merged_raw5 <- merge(x = merged_raw4, y = trade_US_CHN,
                     by.x = c("iso3c", "year"),
                     by.y=c("iso3code", "yr"),
                     all.x = TRUE)

colnames(merged_raw5)

merged_raw5 <- merged_raw5 %>%
  subset(select = -c(partner))

# 6.
unique(US_aid[c("transaction_type_name")])
unique(US_aid[c("fiscal_year")])
# year 2010 (Oct 2009 - sept 2010), year 2011 (Oct 2010 - Sept 2011) etc.


US_aid_dis <- US_aid %>%
  filter(transaction_type_name == "Disbursements") %>%
  group_by(fiscal_year, country_code) %>%
  summarise(USaid_current = sum(current_amount),
            USaid_constant = sum(constant_amount)) %>%
  mutate(sh_aid = USaid_constant/sum(USaid_constant),
         sumaid = sum(USaid_constant)) %>%
  subset(select = -c(USaid_current, USaid_constant, sumaid))

# check if summarise did the right thing
sum(US_aid$constant_amount[(US_aid$country_code == 'AFG') &
                        (US_aid$fiscal_year == '2011') &
                        (US_aid$transaction_type_name == 'Disbursements')])

merged_raw6 <- merge(x = merged_raw5, y = US_aid_dis,
                     by.x = c("iso3c", "year"),
                     by.y=c("country_code", "fiscal_year"),
                     all.x = TRUE)

colnames(merged_raw6)

# 7. UNSC
merged_raw7 <- merge(x = merged_raw6, y = UNSC,
                     by.x = c("iso3c", "year"),
                     by.y=c("iso3code", "year"),
                     all.x = TRUE)

colnames(merged_raw7)

merged_raw7 <- merged_raw7 %>%
  subset(select = -c(aclpname))

# 8. conditionality word and sentence counts per doc type
merged_raw8 <- merge(x = merged_raw7, y = cm_m3,
                     by.x = c("iso3c", "year"),
                     by.y=c("ccode", "year"),
                     all.x = TRUE)

colnames(merged_raw8)


# saving datafiles
write.csv(merged_raw, paste(data_out, 'merged_raw.csv', sep = "/"), row.names = FALSE)
#merged_raw <- read.csv(paste(data_out, "merged_raw.csv", sep = ""))

write.csv(merged_raw2, paste(data_out, 'merged_raw2.csv', sep = "/"), row.names = FALSE)

write.csv(merged_raw3, paste(data_out, 'merged_raw3.csv', sep = "/"), row.names = FALSE)

write.csv(merged_raw4, paste(data_out, 'merged_raw4.csv', sep = "/"), row.names = FALSE)
#merged_raw4 <- read.csv(paste(data_out, "merged_raw4.csv", sep = ""))

write.csv(merged_raw5, paste(data_out, 'merged_raw5.csv', sep = "/"), row.names = FALSE)
#merged_raw5 <- read.csv(paste(data_out, "merged_raw5.csv", sep = ""))

write.csv(merged_raw6, paste(data_out, 'merged_raw6.csv', sep = "/"), row.names = FALSE)

write.csv(merged_raw7, paste(data_out, 'merged_raw7.csv', sep = "/"), row.names = FALSE)
merged_raw7 <- read.csv(paste(data_out, "merged_raw7.csv", sep = ""))

write.csv(merged_raw8, paste(data_out, 'merged_raw8.csv', sep = "/"), row.names = FALSE)

write.csv(US_aid_dis, paste(data_out, 'US_aid_dis.csv', sep = "/"), row.names = FALSE)


