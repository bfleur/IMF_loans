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

merged_raw8 <- read.csv(paste(data_out, "merged_raw8.csv", sep = ""))

#---------------------------------------------------------------------------
# I. summary stats

# extracting only numeric vars
t(t(sapply(merged_raw8, class)))
numvars <- names(merged_raw8)[sapply(merged_raw8, is.numeric)]

merged_raw8_num <- merged_raw8 %>%
  subset(select = numvars)

summary(merged_raw8_num)

install.packages("pastecs")
library(pastecs)

stat.desc(merged_raw8_num)

install.packages("gtsummary")
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

corr_df <- merged_raw8_num %>%
  subset(select = -c(IMF_program, avg_yr_loansize))

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


# histograms!


#---------------------------------------------------------------------------
# III. Check missing values
gg_miss_var(merged_raw8) + labs(y = "Look at all the missing ones")
gg_miss_var(merged_raw8, show_pct = TRUE)

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

lgit1 <- glm(IMF_program ~ gdppc_gr + res_im + debts_ratio, family=binomial(link="logit"), data=merged_raw8) 

lgit2 <- glm(IMF_program ~ gdppc_gr + res_im + avg_part_yr + IM_fromUS + EX_toUS, family=binomial(link="logit"), data=merged_raw8) 

lgit3 <- glm(IMF_program ~ gdppc_gr + res_im + avg_part_yr + meanv_US + meanv_CHN, family=binomial(link="logit"), data=merged_raw8)

# summary
stargazer(lgit1, lgit2, lgit3,
          title="Logit estimates with main econ indep vars", type = "text", 
          out='logit13.txt')

# 2. normal on loan size or Poisson? conditional!
lm1 <- reg_pov_a <- lm(poverty80 ~ dist_cutoff, data = transfers_a)


# 3. Poisson on conditionality



#---------------------------------------------------------------------------
# VII. Matching

# treatment dummy
#merged_raw8_diff <- merged_raw8 %>%
#  mutate(reform = ifelse(year >= 2016, 1, 0))


# balancedness check
library(MatchIt)
match.1 <- matchit(x~ w, data = dat_mat,
                   method = "exact", replace = FALSE)

install.packages("cobalt")
library(cobalt)
love.plot(match.1, stars = "std")



#---------------------------------------------------------------------------
# VIII. Diff-in-diff






















