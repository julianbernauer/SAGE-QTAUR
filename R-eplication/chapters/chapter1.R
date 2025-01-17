# Replication Code for Chapter 1

## Figure 1.1 

# R libraries
library(quanteda)
library(readtext)
library(quanteda.textplots)
# Reading text into a data frame object named ’sahra’
sahra <- readtext(paste0(datadir,"/bsw"),
encoding="UTF-8")
# Preparing the text
sahra_dfm_trim <- dfm_remove(dfm(tokens(corpus(sahra),
remove_punct = TRUE)),
stopwords(language = "de"))
# A word cloud
textplot_wordcloud(sahra_dfm_trim,
max_words = 100,
color = "black")