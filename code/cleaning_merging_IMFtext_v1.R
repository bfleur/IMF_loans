# Virag Bitto (ID: 1903164)
# Thesis
# FILE 1/b
# ------------------------------------------------------------------------------------------------------
# I. setup
# It is advised to start a new session
# CLEAR MEMORY
rm(list=ls())

library(quanteda)
library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)
library(tidyr)
library(tm)
#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
install.packages("qdap")
library(qdap)
#install.packages("corpus")
library(corpus)
#install.packages("quanteda.textmodels")
#install.packages("quanteda.textstats")
#install.packages("quanteda.textplots")
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)



load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/program_single_dfm.Rda") # bigram dfm file
#load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_program_corpus.Rda") 
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/articleiv_corpus_meta.Rda") #meta data file
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_corpus.Rda") 
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_articleiv_panel_final.Rda") 


str(articleiv_corpus_meta[1])
str(program_corpus[2])
str(program_single_dfm)
str(imf_articleiv_final)

corpus_meta <- rename(articleiv_corpus_meta, doc_name = Text) %>% 
  filter(type %in% c("Article IV", "IMF program document"))

head(docvars(program_corpus))
head(docvars(articleiv_corpus))


#text_ntoken(articleiv_corpus_meta[1])

#nrow(corpus_meta)

#dtm <- DocumentTermMatrix(corpus)
#sparse <- removeSparseTerms(dtm, 0.80)
#freq <- findFreqTerms(dtm, 2)

#freq <- findFreqTerms(articleiv_corpus_meta, lowfreq=articleiv_corpus_meta$nrow/100)

frequent_terms <- freq_terms(program_corpus$texts)

articleiv_corpus_meta$text



