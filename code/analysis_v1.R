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
# Load final merged data

merged_raw9 <- read.csv(paste(data_out, "merged_raw9.csv", sep = ""))

#---------------------------------------------------------------------------
# I. summary stats

# extracting only numeric vars
t(t(sapply(merged_raw9, class)))
numvars <- names(merged_raw9)[sapply(merged_raw9, is.numeric)]

print(numvars)

merged_raw9_sum <- merged_raw9 %>%
  subset(select = c(year, avg_yr_loansize, logavg_yr_loansize, sumtok_AIV, sumsent_AIV, sumstok_P, sumssent_P,
         fdi_ofgdp, std_totald, debts_ratio, res_im, 
         tr_bal_gdp, gdpdefl, gdppc_gr, logpop, loggdppc, quotash, pop, gdppc,
         avg_part_yr,
         meanv_US, meanv_CHN, el_democr, IM_fromUS,
         EX_toUS, IM_fromCHN, EX_toCHN, sh_aid))

summary(merged_raw9_sum)

#install.packages("pastecs")
library(pastecs)

stat.desc(merged_raw9_sum)

#install.packages("gtsummary")
library(gtsummary)
# make dataset with a few variables to summarize
#trial2 <- trial %>% select(age, grade, response, trt)

# summarize the data with our package
#descr <- tbl_summary(merged_raw8)


#---------------------------------------------------------------------------
# II. corr matrix

library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)

corr_df <- merged_raw9_sum %>%
  subset(select = -c(avg_yr_loansize))

# Correlation matrix
#corr <- round(cor(merged_raw8_num), 1)
corr <- cor(corr_df, use = "complete.obs")
print(corr)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = FALSE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of all vars", 
           ggtheme=theme_bw)

# frequencies 

merged_raw9 <- merged_raw9 %>%
  group_by(iso3c) %>%
  mutate(conc = ifelse(arrtype == "ECF" | arrtype == "PSI"| arrtype == "PRGF"|
         arrtype == "ECF-EFF"| arrtype == "SBA-SCF"| arrtype == "SCF"| arrtype == "PRGF-EFF"|
         arrtype == "SBA-ESF", 1, 0)) 


max(merged_raw9$country_nr, na.rm=T)

# obs by program type 
install.packages("summarytools")
library(summarytools)

summarytools::freq(merged_raw9$arrtype, order = "freq")
#summarytools::freq(merged_raw9$conc, order = "freq")

summarytools::freq(merged_raw9$IMF_program, order = "freq")

summarytools::freq(merged_raw9$conc, order = "freq")
summarytools::freq(merged_raw9$reform, order = "freq")


# histograms! 
# log loan size
gplot_loan <- ggplot(merged_raw9, aes(avg_yr_loansize)) + scale_fill_brewer(palette = "Spectral")

gplot_loan + geom_histogram(aes(fill=arrtype), 
                   binwidth = .3, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram of log average loansize", 
       subtitle="across arrangement types")  

# number of words
# for Article IV reports
gplot_wordcA <- ggplot(merged_raw9, aes(sumtok_AIV)) #+ scale_fill_brewer(palette = "Spectral")

gplot_wordcA + geom_histogram() +  # change binwidth
  labs(title="Histogram of sum wordcount", 
       subtitle="for Article IV reports")  

# for IMF program reports
gplot_wordcP <- ggplot(merged_raw9, aes(sumstok_P)) #+ scale_fill_brewer(palette = "Spectral")

gplot_wordcP + geom_histogram() +  # change binwidth
  labs(title="Histogram of sum wordcount", 
       subtitle="for IMF Program reports")  

# there is one with extremely large wordcount at GRC
max(merged_raw9$sumstok_P, na.rm=T)


#---------------------------------------------------------------------------
# III. Check missing values
gg_miss_var(merged_raw9) + labs(y = "Look at all the missing ones")
gg_miss_var(merged_raw9, show_pct = TRUE)

# our main variables have very significant fraction of missing values
# missing values are not real ones in case of IMF program-related vars

#---------------------------------------------------------------------------
# IV. Robustness checks

ur.df(window(TB3MS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")



#---------------------------------------------------------------------------
# VI. OLS

# 1. logit on loan particip
library(foreign)

# all obs

lgit1 <- glm(IMF_program ~ gdppc_gr + res_im + debts_ratio, family=binomial(link="logit"), data=merged_raw9) 

lgit2 <- glm(IMF_program ~ gdppc_gr + res_im + avg_part_yr, family=binomial(link="logit"), data=merged_raw9) 

lgit3 <- glm(IMF_program ~ loggdppc + tr_bal_gdp + std_totald + gdpdefl, family=binomial(link="logit"), data=merged_raw9) 

lgit4 <- glm(IMF_program ~ loggdppc + tr_bal_gdp + std_totald + gdpdefl + avg_part_yr, family=binomial(link="logit"), data=merged_raw9) 

lgit5 <- glm(IMF_program ~ loggdppc + res_im + std_totald + gdpdefl + avg_part_yr + logpop, family=binomial(link="logit"), data=merged_raw9) 


# extended

lgit6 <- glm(IMF_program ~ loggdppc + res_im + std_totald 
             #+ avg_part_yr 
             + meanv_US + EX_toUS + IM_fromUS + sh_aid, family=binomial(link="logit"), data=merged_raw9)

lgit7 <- glm(IMF_program ~ loggdppc + res_im + std_totald 
             #+ avg_part_yr 
             + meanv_CHN + EX_toCHN + IM_fromCHN + el_democr + BRI_part, family=binomial(link="logit"), data=merged_raw9)

lgit8 <- glm(IMF_program ~ loggdppc + res_im + std_totald 
             #+ avg_part_yr 
             + unsc + BRI_part, family=binomial(link="logit"), data=merged_raw9)


# summary
stargazer(lgit1, lgit2, lgit3, lgit4, lgit5,
          title="Logit estimates with main econ indep vars", type = "text", 
          out='logit15.txt')

stargazer(lgit6, lgit7, lgit8,
          title="Logit estimates with control vars", type = "text", 
          out='logit68.txt')


# 2. normal on loan size or Poisson? conditional!
#lm1 <- reg_pov_a <- lm(poverty80 ~ dist_cutoff, data = transfers_a)

# whole sample (as loansize is na at obs whithout loan, we do not cut the dset)
lsize1 <- glm(formula=avg_yr_loansize ~ gdppc + res_im + debts_ratio,
              family=poisson(link="log"), data=merged_raw9)

lsize2 <- glm(avg_yr_loansize ~ gdppc + std_totald + tr_bal_gdp + pop,
              family=poisson(link="log"), data=merged_raw9)

lsize3 <- glm(avg_yr_loansize ~ gdppc_gr + res_im + debts_ratio + avg_part_yr,
              family=poisson(link="log"), data=merged_raw9)

lsize4 <- glm(avg_yr_loansize ~ gdppc + std_totald + res_im + pop + avg_part_yr,
              family=poisson(link="log"), data=merged_raw9)

# political
lsize5 <- glm(formula=avg_yr_loansize ~ gdppc_gr + res_im + debts_ratio +
                meanv_US + EX_toUS + IM_fromUS,
              family=poisson(link="log"), data=merged_raw9)

lsize6 <- glm(formula=avg_yr_loansize ~ gdppc + res_im +
                meanv_CHN + EX_toCHN + IM_fromCHN + BRI_part,
              family=poisson(link="log"), data=merged_raw9)

lsize7 <- glm(formula=avg_yr_loansize ~ gdppc + res_im + debts_ratio +
                + el_democr + unsc,
              family=poisson(link="log"), data=merged_raw9)

# summary
stargazer(lsize1, lsize2, lsize3, lsize4,
          title="Loansize regression estimates with main econ indep vars", type = "text", 
          out='lsize14.txt')

stargazer(lsize5, lsize6, lsize7,
          title="Logit estimates with control vars", type = "text", 
          out='lsize57.txt')



# concessional
# sample
merged_raw9_conc <- merged_raw9 %>%
  filter(conc == 1)



# non-concessional
# sample
merged_raw9_nonconc <- merged_raw9 %>%
  filter(conc == 0)


# 3. Poisson on conditionality
# whole sample (as token and sentence count for programs is na at obs without 
# subsequent program, we do not cut the dset)

# word count of program documents
wcount1 <- glm(formula=sumstok_P ~ gdppc + res_im + debts_ratio,
              family=poisson(link="log"), data=merged_raw9)

wcount2 <- glm(sumstok_P ~ fdi_ofgdp + std_totald + tr_bal_gdp + pop,
              family=poisson(link="log"), data=merged_raw9)

wcount3 <- glm(sumstok_P ~ gdppc_gr + res_im + debts_ratio + avg_part_yr,
              family=poisson(link="log"), data=merged_raw9)

wcount4 <- glm(sumstok_P ~ gdppc + std_totald + res_im + pop + avg_part_yr,
              family=poisson(link="log"), data=merged_raw9)

# political
wcount5 <- glm(sumstok_P ~ gdppc_gr + res_im + debts_ratio +
                meanv_US + EX_toUS + IM_fromUS,
              family=poisson(link="log"), data=merged_raw9)

wcount6 <- glm(sumstok_P ~ gdppc + res_im +
                meanv_CHN + EX_toCHN + IM_fromCHN + BRI_part,
              family=poisson(link="log"), data=merged_raw9)

wcount7 <- glm(sumstok_P ~ gdppc + res_im + debts_ratio +
                + el_democr + unsc,
              family=poisson(link="log"), data=merged_raw9)

# summary
stargazer(wcount1, wcount2, wcount3, wcount4,
          title="wordcount regression estimates with main econ indep vars", type = "text", 
          out='wordcount14.txt')

stargazer(wcount5, wcount6, wcount7,
          title="wordcount estimates with control vars", type = "text", 
          out='wordcount57.txt')

#################################
# sentence count of program documents
scount1 <- glm(sumssent_P ~ gdppc + res_im + debts_ratio,
               family=poisson(link="log"), data=merged_raw9)

scount2 <- glm(sumssent_P ~ fdi_ofgdp + std_totald + tr_bal_gdp + pop,
               family=poisson(link="log"), data=merged_raw9)

scount3 <- glm(sumssent_P ~ gdppc_gr + res_im + debts_ratio + avg_part_yr,
               family=poisson(link="log"), data=merged_raw9)

scount4 <- glm(sumssent_P ~ gdppc + std_totald + res_im + pop + avg_part_yr,
               family=poisson(link="log"), data=merged_raw9)

# political
scount5 <- glm(sumssent_P ~ gdppc_gr + res_im + debts_ratio +
                 meanv_US + EX_toUS + IM_fromUS,
               family=poisson(link="log"), data=merged_raw9)

scount6 <- glm(sumssent_P ~ gdppc + res_im +
                 meanv_CHN + EX_toCHN + IM_fromCHN + BRI_part,
               family=poisson(link="log"), data=merged_raw9)

scount7 <- glm(sumssent_P ~ gdppc + res_im + debts_ratio +
                 + el_democr + unsc,
               family=poisson(link="log"), data=merged_raw9)

# summary
stargazer(scount1,scount2, scount3, scount4,
          title="sentence count regression estimates with main econ indep vars", type = "text", 
          out='sentcount14.txt')

stargazer(scount5, scount6, scount7,
          title="sentence count estimates with control vars", type = "text", 
          out='sentcount57.txt')



#---------------------------------------------------------------------------
# VII. Matching

#install.packages("MatchIt")
#install.packages("cobalt")

merged_raw9 <- merged_raw9 %>%
  group_by(country_nr) %>%
  mutate(reform = 0,
         reform = ifelse((quotash[year == "2016"] == quotash) &
                           (quotash[year == "2015"] == quotash), 0, 1))
           
#merged_raw9 <- merged_raw9 %>%
#  group_by(iso3c) %>%
#  mutate(reform = ifelse((quotash[year == "2016"] - 
#                            quotash[year == "2015"]) != 0, 1, 0))

summarytools::freq(merged_raw9$reform, order = "freq")
# in 1403 cases there was a change in quota shares due to the reform,
# in 635 cases quota shares remained unchanged


# balancedness check
# by table
bal1 <- merged_raw9 %>%
  group_by(reform) %>%
  summarise(n_obs = n(),
            mean_IMFp = mean(IMF_program),
            std_error_IMFp = sd(IMF_program) / sqrt(n_obs))

bal2 <- merged_raw9 %>%
  group_by(reform) %>%
  na.omit() %>%
  summarise(n_obs = n(),
            mean_lsize = mean(avg_yr_loansize),
            stderror_lsize = sd(avg_yr_loansize) / sqrt(n_obs))

bal3 <- merged_raw9 %>%
  group_by(reform) %>%
  na.omit() %>%
  summarise(n_obs = n(),
            mean_stoken = mean(sumstok_P),
            stderror_stoken = sd(sumstok_P) / sqrt(n_obs))


# See lack of overlap in fig. below
ggplot(merged_raw9, aes(x = debts_ratio, y = IMF_program,
                    shape = factor(reform),
                    linetype = factor(reform))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black")

ggplot(merged_raw9, aes(x = loggdppc, y = IMF_program,
                        shape = factor(reform),
                        linetype = factor(reform))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black")

ggplot(merged_raw9, aes(x = gdppc_gr, y = IMF_program,
                        shape = factor(reform),
                        linetype = factor(reform))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black")

# check on covariates
cov <- c('loggdppc', 'res_im', 'debts_ratio', 'logpop', 'avg_part_yr', 'meanv_US',
         'meanv_CHN', 'el_democr', 'EX_toUS', 'IM_fromUS', 'EX_toCHN', 'IM_fromCHN',
         'BRI_part')

cov <- merged_raw9 %>%
  group_by(reform) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# matching
library(MatchIt)
# program part
merged_raw9_nomiss <- merged_raw9 %>%  # MatchIt does not allow missing values
  subset(select = c(iso3c, year, country_nr, IMF_program, reform, loggdppc, res_im, 
                    debts_ratio, logpop, avg_part_yr, meanv_US,
         meanv_CHN, el_democr, EX_toUS, IM_fromUS, EX_toCHN, IM_fromCHN,
         BRI_part)) %>%
  na.omit()

# loansize and counts
merged_raw9_nom2 <- merged_raw9 %>%  # MatchIt does not allow missing values
  subset(select = c(iso3c, year, country_nr, IMF_program, reform, loggdppc, res_im, 
                    debts_ratio, logpop, avg_part_yr, meanv_US,
                    meanv_CHN, el_democr, EX_toUS, IM_fromUS, EX_toCHN, IM_fromCHN,
                    BRI_part, avg_yr_loansize, sumstok_P, sumssent_P)) %>%
  na.omit()


#mod_match <- matchit(reform ~ logpop + el_democr + EX_toUS + IM_fromUS,
#                     method = "nearest", data = merged_raw9_nomiss)

mod_match <- matchit(reform ~ logpop + el_democr + meanv_US + meanv_CHN,
                     method = "nearest", data = merged_raw9_nomiss)

mod_match2 <- matchit(reform ~ logpop + el_democr + meanv_US + meanv_CHN,
                     method = "nearest", data = merged_raw9_nom2)


merged_raw9_matched <- match.data(mod_match)
merged_raw9_matched2 <- match.data(mod_match2)


library(cobalt)
love.plot(mod_match, stars = "std")


#---------------------------------------------------------------------------
# VIII. Diff-in-diff

# parallel trend assumption! - matching takes care of it?

# program particip
merged_raw9_matched <- merged_raw9_matched %>%
  mutate(t = ifelse(year >= 2016, 1, 0),
         interact = reform * t)

Did_pp = lm(IMF_program ~ reform + t + interact, data = merged_raw9_matched)
Did_pp

# loan size
merged_raw9_matched2 <- merged_raw9_matched2 %>%
  mutate(t = ifelse(year >= 2016, 1, 0),
         interact = reform * t)

Did_ls = lm(avg_yr_loansize ~ reform + t + interact, data = merged_raw9_matched2)
Did_ls


# word and sent count
Did_tc = lm(sumstok_P ~ reform + t + interact, data = merged_raw9_matched2)
Did_tc

Did_sc = lm(sumssent_P ~ reform + t + interact, data = merged_raw9_matched2)
Did_sc


# summary
stargazer(Did_pp,Did_ls, Did_tc, Did_sc,
          title="DiD regression estimates", type = "text", 
          out='DiD.txt')

















