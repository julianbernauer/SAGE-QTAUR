df_uk <- readtext(paste0(datadir,"/UK17"), encoding="UTF-8")

head(df_uk)

corpus_uk <- corpus(df_uk)
summary(corpus_uk,n=5)

docvars(corpus_uk, "party") <- c("GP","PC","Lab","LD","Con",
"SNP","DUP","UKIP")
summary(corpus_uk, n=5)

print(corpus_uk, max_ndoc=3, max_nchar=100)

corpus_uk["51101.000.2017.1.1.txt"]

corpus_uk[["51101.000.2017.1.1.txt"]]

tok_uk <- tokens(corpus_uk, remove_punct = TRUE, remove_
symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)
head(tok_uk, 3)

library(stopwords)
stopwords::stopwords_getlanguages("snowball")

head(stopwords::stopwords("en", source = "snowball"), 20)

tok_uk_stop <- tokens_select(tok_uk, pattern = stopwords("en"),
selection = "remove")
head(tok_uk_stop, 3)

tok_uk_stop2 <- tokens_select(tok_uk_stop, pattern = c("DUP",
"Plaid", "Cymru",
"Conservatives", "Labour",
"UKIP", "Green"),
selection = "remove")
head(tok_uk_stop2, 3)

dfm_uk <- dfm(tok_uk_stop2)
head(dfm_uk)

library(quanteda.textplots) #load necessary package
textplot_wordcloud(dfm_uk, max_words = 100, min_count = 3,
color = "black")
Figure

dfm_green <- dfm_subset(dfm_uk, party == "GP")
textplot_wordcloud(dfm_green, max_words = 100, min_count = 3,
color = "black")

dfm_ukip <-dfm_subset(dfm_uk, party == "UKIP")
textplot_wordcloud(dfm_ukip, max_words = 100, min_count = 3,
color = "black")

# Wikipedia data 

#
install.packages("getwiki")
library("getwiki")
diwali <- get_wiki("Diwali",clean = T)
class(diwali)
