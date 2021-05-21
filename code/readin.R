# install.packages("installr")
#library(installr)
#updateR()

getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data")

data <- read.csv("/Users/Virág/Documents/CEU/2nd year/2nd trimester/Data4/Term_pr_DA4/gun-violence-data_01-2013_03-2018.csv")

summary(data)

comtr_g <- read.csv("/Users/Virág/Documents/letöltések/comtrade (3).csv")

summary(data_2)

comtr_serv_0018 <- read.csv("/Users/Virág/Documents/letöltések/comtrade (7).csv")

library(haven)
df = read_dta("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/Moser_sturm_2011_dta.dta")

str(df)

#data_3 = read.table(file = "/Users/Virág/Documents/letöltések/MONA_revised_all.txt", sep="\t", header = TRUE)

#install.packages("readxl")
library(readxl)

data_4 = read_excel("/Users/Virág/Documents/letöltések/MONA_all.xlsx")

data_5 <- read_excel("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/Barro_Lee_2005_data.xls")


install.packages("wbstats")
library(wbstats)

# wb_indicators()

wb <- wb_data("NY.GDP.PCAP.KD.ZG")

#install.packages("rJava")
#install.packages("RJSDMX")
#library(rJava)
#library(RJSDMX)

# China IM 1992-2018 WITS data

excel_sheets("/Users/Virág/Documents/letöltések/WITS-Partner-Timeseries (2).xlsx")

data_6 <- read_excel("/Users/Virág/Documents/letöltések/WITS-Partner-Timeseries (2).xlsx", sheet = 2)

# China EX 2000-2018 WITS data
excel_sheets("/Users/Virág/Documents/letöltések/WITS-Partner-Timeseries (4).xlsx")

data_7 <- read_excel("/Users/Virág/Documents/letöltések/WITS-Partner-Timeseries (4).xlsx", sheet = 2)

excel_sheets("/Users/Virág/Documents/letöltések/IMF_quota_0817.xlsx")

data_8 <- read_excel("/Users/Virág/Documents/letöltések/IMF_quota_0817.xlsx", sheet = 3)

# V-Dem data package
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite", type = "source")

pkgbuild::find_rtools(debug = TRUE)

install.packages("remotes")
library(ellipsis)
library(devtools)
library(remotes)
devtools::install_github("vdeminstitute/vdemdata")

library(vdemdata)
?vdem

vdemdata <- vdem

#install.packages("rtools")
#library(rtools)