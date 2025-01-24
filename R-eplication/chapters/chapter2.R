# Julian Bernauer and Anna Wohlmann (2025): Quantitative Text Analysis Using R. London: SAGE.
# Replication Code for Chapter 2:

## If needed
# install.packages(quanteda)

library("quanteda")
library("quanteda.textplots")
library("tidyverse")
library("readtext")

# ?quanteda

# Reading text into a data frame object named ’lamar’
datadir <- getwd()
lamar <- readtext(paste0(datadir,"/Data/chapter2/kendrick_lamar"), encoding="UTF-8")

# creating a corpus object with quanteda 
lamar_corpus <- corpus(lamar)
summary(lamar_corpus)

print(lamar_corpus, max_ndoc=4, max_nchar=50)

as.character(lamar_corpus)[1] #this would print out the first song in our corpus

docvars(lamar_corpus, "artist") <- "Kendrick Lamar"
head(docvars(lamar_corpus))

titles <- names(lamar_corpus)
titles_trim <- gsub(".txt", "", titles)
titles_trim <- gsub("-", " ", titles_trim)
docvars(lamar_corpus, "title") <- titles_trim
head(docvars(lamar_corpus))

# lamar_corpus_lines <- corpus_reshape(lamar_corpus, to = "sentences")
# lamar_corpus_para <- corpus_reshape(lamar_corpus, to = "paragraphs")

stuff_in_brackets = "(\\[.*?\\])"
alright_verses <- corpus_segment(lamar_corpus[1],
pattern = stuff_in_brackets,
valuetype = "regex")
summary(alright_verses)

# Load API-generated from local folder saved in 'datadir' data into workspace (tibble format)
load(paste0(datadir,"/Data/chapter2/lamar_allsongs.Rda"))

lamar_pure <- filter(lamar_allsongs, section_artist == "Kendrick Lamar", section_name %in% c("Verse 1", "Verse 2", "Verse 3", "Verse 4", "Verse 5", "Verse 6", "Verse", "Verso"))

lamar_puresongs <- aggregate(lamar_pure$line,
list(lamar_pure$song_name),
paste, collapse=" ")

lamar_songcorpus <- corpus(lamar_puresongs, text_field="x")
summary(lamar_songcorpus, n=6)


kwic(tokens(corpus_subset(lamar_songcorpus,
Group.1 == "Alright")),
pattern = "kill", window = 7)
## Keyword-in-context with 0 matches.


head(kwic(tokens(lamar_songcorpus), pattern = "kill",
window = 7))

head(kwic(tokens(lamar_songcorpus),
pattern = phrase("black lives matter")))
## Keyword-in-context with 0 matches.

head(kwic(tokens(lamar_songcorpus),
pattern = phrase("father* dead"), window = 9))

head(kwic(tokens(lamar_songcorpus),
pattern = phrase("we hate po*"), window = 9), n=10)

options(width = 80)
lamar_all_dfm <- dfm(tokens(lamar_songcorpus))
head(lamar_all_dfm)

options(width = 80)
lamar_dfm_trim <- dfm_remove(dfm(tokens(lamar_songcorpus,
remove_punct = TRUE)),
stopwords(language = "en"))
head(lamar_dfm_trim)


options(width = 60)
lamar_dfm_trim2 <- dfm_remove(lamar_dfm_trim, pattern =
"****")
head(lamar_dfm_trim2)

options(width = 60)
lamar_dfm_trim2 <- dfm_remove(lamar_dfm_trim, pattern =
"/*/*/*/*")
head(lamar_dfm_trim2)



head(docvars(lamar_dfm_trim))


# SOME BAG-OF-WORDS (BOW) DESCRIPTIVE ANALYSIS

topfeatures(lamar_dfm_trim, n = 25)


library(quanteda.textstats)
lamar_frequ <- textstat_frequency(lamar_dfm_trim)
head(lamar_frequ)

lamar_frequ_grouped <- textstat_frequency(lamar_dfm_trim,
groups = Group.1)
head(lamar_frequ_grouped)

lamar_dfm_alright <- dfm_subset(lamar_dfm_trim,
Group.1=="Alright")
lamar_frequ_alright <- textstat_frequency(lamar_dfm_alright)
head(lamar_frequ_alright)

topfeatures(lamar_dfm_alright, n = 10)


# Figure 2.1 Most frequent words in Kendrick Lamar’s ‘Alright’

library("ggplot2")
plot <- ggplot(lamar_frequ_alright[1:23,],
               aes(x = reorder(feature, frequency),
                   y = frequency)) +
        geom_point(color = "blue") +
        coord_flip() +
        labs(x = NULL, y = "Word Frequency 'Alright'")
ggsave(filename = paste0(datadir, "/Figures/fig2_1.png"), plot = plot, width = 10, height = 8, dpi = 400)


# Figure 2.2 Most frequent words in the Kendrick Lamar song corpus
plot <- ggplot(lamar_frequ[1:23,],
aes(x = reorder(feature, frequency),
y = frequency)) +
geom_point(color = "blue") +
coord_flip() +
labs(x = NULL, y = "Word Frequency Lamar's Lyrics")
ggsave(filename = paste0(datadir, "/Figures/fig2_2.png"), plot = plot, width = 10, height = 8, dpi = 400)


# Figure 2.3 Word cloud of Kendrick Lamar’s song corpus
png(filename = paste0(datadir,"/Figures/fig2_3.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(lamar_dfm_trim,
                   comparison = F,
                   max_words = 100,
                   color = "black")
dev.off()


# Sound of da police example 

krsone <- readtext(paste0(datadir,"/Data/chapter2/krsone_soundofdapolice.txt"), encoding="UTF-8")

krsone_corpus <- corpus(krsone, text_field="text")
krsone_dfm_trim <- dfm_remove(dfm(tokens(krsone_corpus,
remove_punct = TRUE)),
stopwords(language = "en"))

# Figure 2.4 Word cloud of KRS-One’s ‘Sound of da Police’
png(filename = paste0(datadir,"/Figures/fig2_4.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(krsone_dfm_trim,
                   comparison = F,
                   max_words = 100,
                   color = "black")
dev.off()








