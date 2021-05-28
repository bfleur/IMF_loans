# Virag Bitto (ID: 1903164)
# Thesis
# FILE 1
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
  pop = "SP.POP.TOTL"
)

wb <- wb_data(my_wb_indicators, start_date = 2010, end_date = 2020)

colnames(wb)
colnames(wb)[4] <- "year"

write.csv(wb, paste(data_out, 'WB_data_1020_pop.csv', sep = "/"), row.names = FALSE)

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
UNV <- read.csv(paste(data_in, "UNVotes.csv", sep = ""))

UNV_work <- UNV %>%
  filter(year > 2009)

# save datafile
write.csv(UNV_work, paste(data_out, 'UNV_work.csv', sep = "/"), row.names = FALSE)

UNV_work <- read.csv(paste(data_out, 'UNV_work.csv', sep = ""))

# give 1 per vote if same as US (or China), 0 if not, 0.5 if abstain or absent

#US
UNV_work <- UNV_work %>%
  group_by(resid) %>%
  mutate(same_US = ifelse((vote == "2") | (vote == "8") | (vote == "9"),
                          0.5, 0)) %>%
  mutate(same_US = ifelse((vote[Country == "USA"] == vote) & (same_US[Country == "USA"] != "0.5"),
                          1, ifelse((vote == "2") | (vote == "8") | (vote == "9"),
                                    0.5, 0)))
  
# China
UNV_work <- UNV_work %>%
  group_by(resid) %>%
  mutate(same_CHN = ifelse((vote == "2") | (vote == "8") | (vote == "9"),
                          0.5, 0)) %>%
  mutate(same_CHN = ifelse((vote[Country == "CHN"] == vote) & (same_US[Country == "CHN"] != "0.5"),
                          1, ifelse((vote == "2") | (vote == "8") | (vote == "9"),
                                    0.5, 0)))

 # calculate average points per year from these
UNV_work <- UNV_work %>%
  group_by(year, Country) %>%
  mutate(meanv_US = mean(same_US)) %>%
  mutate(meanv_CHN = mean(same_CHN)) %>%
  ungroup() %>%
  mutate(iso3code = countrycode(Countryname, origin = 'country.name', destination = 'iso3c'))
  
UNV_work$iso3code[UNV_work$Countryname == 'Serbia and Montenegro'] <- 'SCG'
UNV_work$iso3code[UNV_work$Countryname == 'Yugoslavia'] <- 'YUG'

#put the necessary vars into a new dataframe
UNV_means <- UNV_work %>%
  group_by(year, iso3code) %>%
  filter(row_number(meanv_US) == 1) %>%
  subset(select = c("member", "vote", "Countryname", "year", "iso3code",
                     "meanv_US", "meanv_CHN"))


# save data
write.csv(UNV_work, paste(data_out, 'UNV_work.csv', sep = "/"), row.names = FALSE)

write.csv(UNV_means, paste(data_out, 'UNV_means.csv', sep = "/"), row.names = FALSE)

#UNV_means <- read.csv(paste(data_out, 'UNV_means.csv', sep = ""))

#######################################################################
#2.2 V-Dem electoral democracy

vdem <- read.csv(paste(data_in, 'V-Dem-CY-Core-v11.1.csv', sep = ""))

colnames(vdem)

vdem <- vdem %>%
  filter(year >= 2010) %>%
  subset(select = c("country_name", "year", "country_id", "COWcode", 
                    "v2x_polyarchy")) %>%
  mutate(iso3code = countrycode(COWcode, origin = 'cown', destination = 'iso3c'))

vdem$iso3code[vdem$COWcode == '345'] <- 'YUG'
vdem$iso3code[vdem$COWcode == '347'] <- 'XKX'
vdem$iso3code[vdem$COWcode == '511'] <- 'TZA'

write.csv(vdem, paste(data_out, 'vdem.csv', sep = "/"), row.names = FALSE)

########################################################################
# 2.3 WITS trade data
# first we load IM and EX data and then merge them

# WITS IM USA
library(readxl)
excel_sheets(paste(data_in,"US_IM_0018_WITS_shares.xlsx", sep= "/"))

IM_US <- read_excel(paste(data_in,"US_IM_0018_WITS_shares.xlsx", sep= "/"), sheet = 2)

year_col <- c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', 
              '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017',
              '2018')

IM_US_l <- IM_US %>% 
  pivot_longer(all_of(year_col), names_to = "year", values_to = "EX_touS")

# by EX to US I mean the country perspectives which are exporting to the US,
# that counts as import for the US

#filter the data for year and keep only the necessary vars
colnames(IM_US_l)

names(IM_US_l) <- c("reporter", "partner", "tradefl", "indic", "yr",
                    "EX_toUS")

library(countrycode)

IM_US_l <- IM_US_l %>%
  filter(yr >= 2010) %>%
  subset(select = c("partner", "yr", "EX_toUS")) %>%
  mutate(iso3code = countrycode(partner, origin = 'country.name', destination = 'iso3c'))

IM_US_l$iso3code[IM_US_l$partner == 'Ethiopia(excludes Eritrea)'] <- 'ETH'
IM_US_l$iso3code[IM_US_l$partner == 'Fr. So. Ant. Tr'] <- 'ATF'
IM_US_l$iso3code[IM_US_l$partner == 'Occ.Pal.Terr'] <- 'PSE'
IM_US_l$iso3code[IM_US_l$partner == 'Serbia, FR(Serbia/Montenegro)'] <- 'SCG'


# delete: Netherlands Antilles, Other Asia, nes as the first one has only NAs,
# and the second one is comprised of multiple countries and regions which could 
# not be identified or were deliberately not disclosed (so it is impossible to
# attach any values to individual countries in this group)
IM_US_l <- IM_US_l %>%
  filter((partner != "Netherlands Antilles") & (partner != "Other Asia, nes"))

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# import EX US data

excel_sheets(paste(data_in,"US_EX_0018_WITS_shares.xlsx", sep= "/"))

EX_US <- read_excel(paste(data_in,"US_EX_0018_WITS_shares.xlsx", sep= "/"), sheet = 2)


EX_US_l <- EX_US %>% 
  pivot_longer(all_of(year_col), names_to = "year", values_to = "IM_fromUS")

#filter the data for year and keep only the necessary vars
colnames(EX_US_l)

names(EX_US_l) <- c("reporter", "partner", "tradefl", "indic", "yr",
                    "IM_fromUS")

library(countrycode)

EX_US_l <- EX_US_l %>%
  filter(yr >= 2010) %>%
  subset(select = c("partner", "yr", "IM_fromUS")) %>%
  mutate(iso3code = countrycode(partner, origin = 'country.name', destination = 'iso3c'))

EX_US_l$iso3code[EX_US_l$partner == 'Ethiopia(excludes Eritrea)'] <- 'ETH'
EX_US_l$iso3code[EX_US_l$partner == 'Fr. So. Ant. Tr'] <- 'ATF'
EX_US_l$iso3code[EX_US_l$partner == 'Occ.Pal.Terr'] <- 'PSE'
EX_US_l$iso3code[EX_US_l$partner == 'Serbia, FR(Serbia/Montenegro)'] <- 'SCG'


# delete: Netherlands Antilles, Other Asia, nes as the first one has only NAs,
# and the second one is comprised of multiple countries and regions which could 
# not be identified or were deliberately not disclosed (so it is impossible to
# attach any values to individual countries in this group)
EX_US_l <- EX_US_l %>%
  filter((partner != "Netherlands Antilles") & (partner != "Other Asia, nes") &
        (partner != "Unspecified"))

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# import CHN IM data 
excel_sheets(paste(data_in,"CHN_IM_0018_WITS_shares.xlsx", sep= "/"))

IM_CHN <- read_excel(paste(data_in,"CHN_IM_0018_WITS_shares.xlsx", sep= "/"), sheet = 2)

colnames(IM_CHN)

IM_CHN_l <- IM_CHN %>% 
  pivot_longer(all_of(year_col), names_to = "year", values_to = "EX_toCHN")

#filter the data for year and keep only the necessary vars
colnames(IM_CHN_l)

names(IM_CHN_l) <- c("reporter", "partner", "tradefl", "indic", "yr",
                    "EX_toCHN")

library(countrycode)

IM_CHN_l <- IM_CHN_l %>%
  filter(yr >= 2010) %>%
  subset(select = c("partner", "yr", "EX_toCHN")) %>%
  mutate(iso3code = countrycode(partner, origin = 'country.name', destination = 'iso3c'))

IM_CHN_l$iso3code[IM_CHN_l$partner == 'Ethiopia(excludes Eritrea)'] <- 'ETH'
IM_CHN_l$iso3code[IM_CHN_l$partner == 'Occ.Pal.Terr'] <- 'PSE'
IM_CHN_l$iso3code[IM_CHN_l$partner == 'Serbia, FR(Serbia/Montenegro)'] <- 'SCG'
IM_CHN_l$iso3code[IM_CHN_l$partner == 'Bonaire'] <- 'BES'
IM_CHN_l$iso3code[IM_CHN_l$partner == 'Netherlands Antilles'] <- 'ANT'


# delete: Free Zones, Czechoslovakia, Belgium-Luxembourg, Other Asia, ne, 
# Unspecified as some of them have only NAs, others are comprised of multiple 
# countries and regions which could 
# not be identified or were deliberately not disclosed (so it is impossible to
# attach any values to individual countries in this group)
IM_CHN_l <- IM_CHN_l %>%
  filter((partner != "Other Asia, nes") &
           (partner != "Unspecified"))

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# CHN EX data 
excel_sheets(paste(data_in,"CHN_EX_0018_WITS_shares.xlsx", sep= "/"))

EX_CHN <- read_excel(paste(data_in,"CHN_EX_0018_WITS_shares.xlsx", sep= "/"), sheet = 2)

EX_CHN_l <- EX_CHN %>% 
  pivot_longer(all_of(year_col), names_to = "year", values_to = "IM_fromCHN")

#filter the data for year and keep only the necessary vars
colnames(EX_CHN_l)

names(EX_CHN_l) <- c("reporter", "partner", "tradefl", "indic", "yr",
                     "IM_fromCHN")

library(countrycode)

EX_CHN_l <- EX_CHN_l %>%
  filter(yr >= 2010) %>%
  subset(select = c("partner", "yr", "IM_fromCHN")) %>%
  mutate(iso3code = countrycode(partner, origin = 'country.name', destination = 'iso3c'))

EX_CHN_l$iso3code[EX_CHN_l$partner == 'Ethiopia(excludes Eritrea)'] <- 'ETH'
EX_CHN_l$iso3code[EX_CHN_l$partner == 'Occ.Pal.Terr'] <- 'PSE'
EX_CHN_l$iso3code[EX_CHN_l$partner == 'Serbia, FR(Serbia/Montenegro)'] <- 'SCG'
EX_CHN_l$iso3code[EX_CHN_l$partner == 'Bonaire'] <- 'BES'
EX_CHN_l$iso3code[EX_CHN_l$partner == 'Netherlands Antilles'] <- 'ANT'


# delete: Other Asia, nes, Unspecified as those are comprised of multiple 
# countries and regions which could 
# not be identified or were deliberately not disclosed (so it is impossible to
# attach any values to individual countries in this group)
EX_CHN_l <- EX_CHN_l %>%
  filter((partner != "Unspecified") & (partner != "Other Asia, nes"))

# -----------------------------------------------------------------------
# merge data together

trade_US_CHN <- merge(x = EX_US_l, y = IM_US_l,
                 by.x = c("iso3code", "yr"),
                 by.y=c("iso3code", "yr"),
                 all = TRUE)

trade_US_CHN <- trade_US_CHN %>%
  subset(select = -c(partner.x, partner.y))

trade_US_CHN <- merge(x = trade_US_CHN, y = EX_CHN_l,
                      by.x = c("iso3code", "yr"),
                      by.y=c("iso3code", "yr"),
                      all = TRUE)

trade_US_CHN <- trade_US_CHN %>%
  subset(select = -c(partner))

trade_US_CHN <- merge(x = trade_US_CHN, y = IM_CHN_l,
                      by.x = c("iso3code", "yr"),
                      by.y=c("iso3code", "yr"),
                      all = TRUE)

#save datafiles
write.csv(trade_US_CHN, paste(data_out, 'eshares_US_CHN.csv', sep = "/"), row.names = FALSE)

#write.csv(trade_US_CHN_f, paste(data_out, 'econ_proxim_US_CHN.csv', sep = "/"), row.names = FALSE)

########################################################################
# 2.4 China lends and US aid data

# CHINA BRI (Belt and Road Initiative)
BRI <- read_excel(paste(data_in,"bri_countries.xlsx", sep= "/"), sheet = 1)

BRI_w <- BRI %>%
  mutate(iso3code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  group_by(iso3code) %>%
  mutate(yr_st = "2011") %>%
  mutate(yr_end = "2019")

BRI_w$iso3code[BRI_w$country == 'Micronesia'] <- 'FSM'

# spread additional years across countries, artificially adding new observations
BRI_w <- rowwise(BRI_w) %>% 
  do(tibble(iso3code = .$iso3code, year=.$year,
            z=.$yr_st:.$yr_end))

#adding bri participation dummy, dropping joining year variable
BRI_w <- BRI_w %>%
  mutate(BRI_part = ifelse(z >= year, 1, 0)) %>%
  subset(select = -c(year))

#save datafiles
write.csv(BRI_w, paste(data_out, 'BRI_part.csv', sep = "/"), row.names = FALSE)


# USA
US_aid_huge <- read.csv(paste(data_in, "us_foreign_aid_complete.csv", sep = ""))

colnames(US_aid_huge)

US_aid <- US_aid_huge %>%
  filter((fiscal_year > "2009") & (fiscal_year < "2020")) %>%
  subset(select = c(country_code, country_name, assistance_category_name,
                    transaction_type_name, fiscal_year, constant_amount,
                    current_amount, 
                    activity_start_date, activity_end_date, assistance_category_id))

#save datafiles
write.csv(US_aid, paste(data_out, 'US_aid.csv', sep = "/"), row.names = FALSE)

########################################################################
# 2.5 UN SC data
excel_sheets(paste(data_in,"UNSCdata_Dreher.xls", sep= "/"))

UNSC_raw <- read_excel(paste(data_in,"UNSCdata_Dreher.xls", sep= "/"), sheet = 2)

UNSC <- UNSC_raw %>%
  filter(year > 2009) %>%
  mutate(code = na_if(code, ".")) %>%
  mutate(unsc = na_if(unsc, ".")) %>%
  subset(select = -c(aclpcode)) 

# we drop the "code" variable as it contains many missing country codes, instead
# we recreate it
gg_miss_var(UNSC, show_pct = TRUE)

UNSC <- UNSC %>%
  subset(select = -c(code)) %>%
  mutate(iso3code = countrycode(aclpname, origin = 'country.name', destination = 'iso3c'))

UNSC$iso3code[UNSC$aclpname == 'Yugoslavia'] <- 'YUG'

# drop observations where no ISO3 codes could be found, neither by the package,
# nor manually (as those countries are not existing anymore, or are not recognized
# internationally (i.e. Somaliland))
UNSC <- UNSC %>%
  filter((aclpname != "Germany, East") & (aclpname != "Somaliland") &
           (aclpname != "Yemen Arab Republic") & (aclpname != "Czechoslovakia") &
           (aclpname != "Yemen PDR (South)") & (aclpname != "Yugoslavia") &
           (aclpname != "Yugoslavia2"))

gg_miss_var(UNSC, show_pct = TRUE)
# no missing values

# save data file
write.csv(UNSC, paste(data_out, 'UNSC.csv', sep = "/"), row.names = FALSE)

