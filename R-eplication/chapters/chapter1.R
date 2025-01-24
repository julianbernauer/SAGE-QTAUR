# Julian Bernauer and Anna Wohlmann (2025): Quantitative Text Analysis Using R. London: SAGE.
# Replication Code for Chapter 1: CALCULATING WITH LETTERS

# R libraries
library(quanteda)
library(readtext)
library(quanteda.textplots)
# needed for ncsrabble()
library(quanteda.textstats)

# Figure 1.1: Word cloud programmatic text of ‘Bündnis Sahra Wagenknecht’

# Reading text into a data frame object named ’sahra’
datadir <- getwd()
sahra <- readtext(paste0(datadir,"/Data/chapter1"),
encoding="UTF-8")

# Preparing the text
sahra_dfm_trim <- dfm_remove(dfm(tokens(corpus(sahra),
remove_punct = TRUE)),
stopwords(language = "de"))

# A word cloud

png(filename = paste0(datadir, "/Figures/fig1_1.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(sahra_dfm_trim,
                   max_words = 100,
                   color = "black")
dev.off()

# quanteda command example 
quanteda_options("threads")

# scrabble function 
nscrabble(c("quanteda", "tm"))
