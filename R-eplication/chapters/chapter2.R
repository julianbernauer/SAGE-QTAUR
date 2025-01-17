# Replication Code for Chapter 2

# install.packages(quanteda)


library("quanteda")
library("readtext")

# ?quanteda

# Reading text into a data frame object named ’lamar’

lamar <- readtext(paste0(datadir,"/kendrick_lamar"), encoding="UTF-8")

lamar_corpus <- corpus(lamar)
summary(lamar_corpus)

print(lamar_corpus, max_ndoc=4, max_nchar=50)

as.character(lamar_corpus)[1] #this would print out the first
song in our corpus

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

# Load API-generated from local folder saved in 'datadir' data
into workspace (tibble format)
load(paste0(datadir,"/lamar_allsongs.Rda"))

lamar_pure <- filter(lamar_allsongs,
section_artist == "Kendrick Lamar",
section_name == "Verse 1" |
section_name == "Verse 2" |
section_name == "Verse 3" |
section_name == "Verse 4" |
section_name == "Verse 5" |
section_name == "Verse 6" |
section_name == "Verse" |
section_name == "Verso"
)

lamar_puresongs <- aggregate(lamar_pure$line,
list(lamar_pure$song_name),
paste, collapse=" ")


options(width = 60)
lamar_songcorpus <- corpus(lamar_puresongs, text_field="x")
summary(lamar_songcorpus, n=6)


options(width = 60)
kwic(tokens(corpus_subset(lamar_songcorpus,
Group.1 == "Alright")),
pattern = "kill", window = 7)
## Keyword-in-context with 0 matches.


options(width = 60)
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
ggplot(lamar_frequ_alright[1:23,],
aes(x = reorder(feature, frequency),
y = frequency)) +
geom_point(color = "blue") +
coord_flip() +
labs(x = NULL, y = "Word Frequency 'Alright'")


# Figure 2.2 Most frequent words in the Kendrick Lamar song corpus
ggplot(lamar_frequ[1:23,],
aes(x = reorder(feature, frequency),
y = frequency)) +
geom_point(color = "blue") +
coord_flip() +
labs(x = NULL, y = "Word Frequency Lamar's Lyrics")


# Figure 2.3 Word cloud of Kendrick Lamar’s song corpus

textplot_wordcloud(lamar_dfm_trim,
comparison = F,
max_words = 100,
color = "black")


krsone <- readtext(paste0(datadir,"/krsone_soundofdapolice.txt"),
encoding="UTF-8")
krsone_corpus <- corpus(krsone, text_field="text")
krsone_dfm_trim <- dfm_remove(dfm(tokens(krsone_corpus,
remove_punct = TRUE)),
stopwords(language = "en"))

# Figure 2.4 Word cloud of KRS-One’s ‘Sound of da Police’
textplot_wordcloud(krsone_dfm_trim,
comparison = F,
max_words = 100,
color = "black")

