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


load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/articleiv_corpus_meta.Rda") #meta data file

#load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_program_corpus.Rda") 
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_corpus.Rda") 
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/program_single_dfm.Rda") # bigram dfm file
load("/Users/Virág/Documents/CEU/2nd year/Thesis/Dataman_thesis/data/data_in/DMihalyi/Rda-files/imf_articleiv_panel_final.Rda") 


str(articleiv_corpus_meta[1]) # this is a dataframe
str(program_corpus[2]) # this is a corpus but without the texts(?)
str(articleiv_corpus) # this is the largest corpus but without the texts
str(program_single_dfm) # this is a dfm
str(imf_articleiv_final) # this is a df containing the texts we need

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

#frequent_terms <- freq_terms(program_corpus$texts)

colnames(imf_articleiv_final)

# break up the strings in each row by " "
imf_articleiv_work <- imf_articleiv_final %>%
  filter(type == "Article IV" | type == "IMF program document")

temp <- imf_articleiv_work %>%
  subset(select = c(ccode, year_p, text)) %>%
  #strsplit(text, split=" ") %>%
  #sapply(text, function(x) strsplit(x, split=" "))

imf_articleiv_work$temp <- strsplit(as.character(imf_articleiv_work$text), split=" ")

class(imf_articleiv_work$text)
imf_articleiv_work$text <- as.character(imf_articleiv_work$text)

lst <- strsplit(trimws(as.character(imf_articleiv_work$text)), "\\s")
names(lst) <- trimws(df[, 1])

# count the number of words as the length of the vectors
df$wordCount <- sapply(temp, length)


