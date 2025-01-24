# Julian Bernauer and Anna Wohlmann (2025): Quantitative Text Analysis Using R. London: SAGE.
# Replication Code for Chapter 3
# TEXT AS DATA: OBTAINING, PREPARING AND CLEANING

library("quanteda")
library("quanteda.textplots")

datadir <- getwd()

df_uk <- readtext(paste0(datadir,"/Data/chapter3/UK17"), encoding="UTF-8")
head(df_uk)

corpus_uk <- corpus(df_uk)
summary(corpus_uk,n=5)

docvars(corpus_uk, "party") <- c("GP","PC","Lab","LD","Con","SNP","DUP","UKIP")
summary(corpus_uk, n=5)

print(corpus_uk, max_ndoc=3, max_nchar=100)

corpus_uk["51101.000.2017.1.1.txt"]

# full text 
# corpus_uk[["51101.000.2017.1.1.txt"]]


tok_uk <- tokens(corpus_uk, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)


head(tok_uk, 3)

library(stopwords)
stopwords::stopwords_getlanguages("snowball")

head(stopwords::stopwords("en", source = "snowball"), 20)

tok_uk_stop <- tokens_select(tok_uk, pattern = stopwords("en"), selection = "remove") 

head(tok_uk_stop, 3)

tok_uk_stop2 <- tokens_select(tok_uk_stop, pattern = c("DUP", "Plaid", "Cymru", "Conservatives", "Labour", "UKIP", "Green"), selection = "remove")

head(tok_uk_stop2, 3)

dfm_uk <- dfm(tok_uk_stop2)
head(dfm_uk)

# library(quanteda.textplots) #load necessary package for plotting 

png(filename = paste0(datadir,"/Figures/fig3_1.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(dfm_uk, max_words = 100, min_count = 3, color = "black")
dev.off()


# fig 3.2 
dfm_green <- dfm_subset(dfm_uk, party == "GP")
png(filename = paste0(datadir,"/Figures/fig3_2.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(dfm_green, max_words = 100, min_count = 3,
color = "black")
dev.off()

# fig. 3.3 
dfm_ukip <-dfm_subset(dfm_uk, party == "UKIP")
png(filename = paste0(datadir,"/Figures/fig3_3.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(dfm_ukip, max_words = 100, min_count = 3,
color = "black")
dev.off()


## Wikipedia data 

# R package 
# install.packages("getwiki")
library("getwiki")
diwali <- get_wiki("Diwali",clean = T)
class(diwali)


holidays <- c("Diwali", "Holi", "Christmas", "Easter", "Eid_al_Fitr", "Eid_al_Adha", "Kathina", "Vesakh", "Pessach", "Jom_Kippur")
holiday_texts <- c() #empty
for (holiday in holidays){ #for loop
    holiday_texts <- c(holiday_texts, get_wiki(holiday, clean = T))
    Sys.sleep(3) #take a break
}

holiday_corpus <- corpus(holiday_texts)
head(holiday_corpus,5) #only show first 5



## Twitter data 

load(paste0(datadir,"/Data/chapter3/fff_200124_1415.Rdata"))

fff$text[c(28,766, 799, 814)]

fff$text[fff$lang=="ur"]

fff$our_name <- paste("user", seq(1, nrow(fff), 1), sep = "_")
#change names
fff_en <- subset(fff, lang == "en") #only english tweets
fff_en <- fff_en[, c("created_at", "our_name", "text")]
#remove other variables


fff_corpus <- corpus(fff_en)
summary(fff_corpus, n=10)


as.character(tokens(fff_corpus[14]))


fff_toks <- tokens(fff_corpus, what = "word", remove_punct = TRUE, remove_url=TRUE)
fff_toks_txt <- tokens_remove(fff_toks, pattern = "^[#@].+$", valuetype = "regex")
fff_toks_txt[14]


fff_toks_haha <- tokens_select(fff_toks, pattern = "^[#@].+$", valuetype = "regex")
fff_toks_haha[14]


fff_dfm <- dfm(fff_toks_txt)
fff_dfm <- dfm_remove(fff_dfm, stopwords("english"))
head(fff_dfm, 10, n = 10)

frequ <- as.numeric(topfeatures(fff_dfm,20))
word <- as.character(names(topfeatures(fff_dfm, 20)))

fff_plot <- as.data.frame(frequ,word)

plot <- ggplot(fff_plot, aes(x = reorder(word, frequ), y = frequ)) +
geom_point() +
coord_flip() +
labs(x = NULL, y = "Frequency")
ggsave(filename = paste0(datadir, "/Figures/fig3_4.png"), plot = plot, width = 10, height = 8, dpi = 400)

# fig 3.5 Fridays for Future word cloud
png(filename = paste0(datadir,"/Figures/fig3_5.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(fff_dfm, max_words = 100, min_count = 3,
color = "black")
dev.off()


## genius.com data 

# alternative data source 
load(paste0(datadir,"/Data/chapter3/rap_songs_clean.Rdata"))


# load(paste0(datadir,"/Data/chapter3/lamar_lyrics.Rdata"))



library(geniusr)

# requires personal access token 
genius_token()

search_song("DNA") #the correct id is 3035222

dna <- get_song(song_id = 3035222)
dna$content$full_title
dna$content$release_date_for_display





oldskool <- c("Grandmaster-flash-and-the-furious-five-themessage-
lyrics",
"Nwa-straight-outta-compton-lyrics",
"Ice-t-6-n-the-mornin-lyrics",
"Krs-one-sound-of-da-police-lyrics",
"A-tribe-called-quest-check-the-rhime-lyrics")
new_gangsta <- c("Rick-ross-hustlin-lyrics",
"Bobby-shmurda-hot-nigga-lyrics",
"Drakeo-the-ruler-flu-flamming-lyrics",
"Lil-wayne-a-milli-lyrics",
"Wiz-khalifa-we-dem-boyz-lyrics")
new_conscious <- c("Eminem-the-storm-2017-bet-hip-hop-awardscypher-
verse-lyrics",
"Wale-ambition-lyrics",
"Tyler-the-creator-yonkers-lyrics",
"Kendrick-lamar-humble-lyrics",
"Logic-everybody-lyrics")
lyrics <- c(oldskool,new_gangsta,new_conscious)
genius_urls <- paste0("https://genius.com/",lyrics)


for (i in 1:length(genius_urls)) {
nam <- paste("lyr",i, sep = "_")
assign(nam, get_lyrics_url(genius_urls[i]))
Sys.sleep(11)
}


lyr_2 <- get_lyrics_url(genius_urls[2])


rap_lyrics <- tibble()
rap_lyrics <- rbind(lyr_1,lyr_2,lyr_3,lyr_4,lyr_5,lyr_6,lyr_7,l
yr_8,lyr_9,lyr_10,lyr_11,lyr_12,lyr_13,lyr_14,lyr_15)

head(rap_lyrics, 5)


rap_songs <- aggregate(rap_lyrics$line, list(rap_lyrics$song_name), paste, collapse=" ")


## EXPL stands for explicit language, we are censoring the text here already in the scraped source file (not included in repo)
# source(clean_rep.R)



# text prep in the background 

rap_songs$sosho <- ""
rap_songs$sosho[rap_songs$Group.1=="The Message"] <- "Message"
rap_songs$sosho[rap_songs$Group.1=="Straight Outta Compton"] <- "Compton"
rap_songs$sosho[rap_songs$Group.1=="6 ‘N the Mornin’"] <- "Mornin"
rap_songs$sosho[rap_songs$Group.1=="Sound of da Police"] <- "Police"
rap_songs$sosho[rap_songs$Group.1=="Check the Rhime"] <- "Check"
rap_songs$sosho[rap_songs$Group.1=="Hustlin’"] <- "Hustlin"
rap_songs$sosho[rap_songs$Group.1=="Hot EXPL"] <- "Hot"
rap_songs$sosho[rap_songs$Group.1=="Flu Flamming"] <- "Flu"
rap_songs$sosho[rap_songs$Group.1=="A Milli"] <- "Milli"
rap_songs$sosho[rap_songs$Group.1=="We Dem Boyz"] <- "Boyz"
rap_songs$sosho[rap_songs$Group.1=="The Storm (2017 BET Hip-Hop Awards Cypher Verse)"] <- "Storm"
rap_songs$sosho[rap_songs$Group.1=="Ambition"] <- "Ambition"
rap_songs$sosho[rap_songs$Group.1=="Yonkers"] <- "Yonkers"
rap_songs$sosho[rap_songs$Group.1=="HUMBLE."] <- "Humble"
rap_songs$sosho[rap_songs$Group.1=="Everybody"] <- "Everybody"



rap_songs$class <- ""
rap_songs$class[rap_songs$sosho=="Message"] <- "old"
rap_songs$class[rap_songs$sosho=="Compton"] <- "old"
rap_songs$class[rap_songs$sosho=="Mornin"] <- "old"
rap_songs$class[rap_songs$sosho=="Police"] <- "old"
rap_songs$class[rap_songs$sosho=="Check"] <- "old"
rap_songs$class[rap_songs$sosho=="Hustlin"] <- "gang"
rap_songs$class[rap_songs$sosho=="Hot"] <- "gang"
rap_songs$class[rap_songs$sosho=="Flu"] <- "gang"
rap_songs$class[rap_songs$sosho=="Milli"] <- "gang"
rap_songs$class[rap_songs$sosho=="Boyz"] <- "gang"
rap_songs$class[rap_songs$sosho=="Storm"] <- "cons"
rap_songs$class[rap_songs$sosho=="Ambition"] <- "cons"
rap_songs$class[rap_songs$sosho=="Yonkers"] <- "cons"
rap_songs$class[rap_songs$sosho=="Humble"] <- "cons"
rap_songs$class[rap_songs$sosho=="Everybody"] <- "cons"


rap_corpus <- corpus(rap_songs, text_field="x")
summary(rap_corpus, n=6)

rap_songs_dfm <- dfm_remove(dfm(tokens( #create token object, then dfm
                            rap_corpus, #from the corpus
                            remove_punct = TRUE,
                            remove_numbers = TRUE)), 
                            stopwords(language = "en") #remove
                            )

rap_dfm_old <- dfm_subset(rap_songs_dfm, class=="old") #subset to only have oldschool rap
rap_frequ_old <- textstat_frequency(rap_dfm_old) #term frequency
rap_dfm_cons <- dfm_subset(rap_songs_dfm, class=="cons")
rap_frequ_cons <- textstat_frequency(rap_dfm_cons)
rap_dfm_gang <- dfm_subset(rap_songs_dfm, class=="gang")
rap_frequ_gang <- textstat_frequency(rap_dfm_gang)





# fig 3.6
png(filename = paste0(datadir,"/Figures/fig3_6.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(rap_songs_dfm , max_words = 100, min_count = 3, color = "black")
dev.off()

# fig 3.7
png(filename = paste0(datadir,"/Figures/fig3_7.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(rap_dfm_old, color="black", max_words = 150)
dev.off()

# fig 3.8
png(filename = paste0(datadir,"/Figures/fig3_8.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(rap_dfm_cons, color="black", max_words = 150)
dev.off()

# fig 3.9
png(filename = paste0(datadir,"/Figures/fig3_9.png"), width = 800, height = 800, res = 400)
textplot_wordcloud(rap_dfm_gang, color="black", max_words = 150)
dev.off()

rap_dfm_grouped <- dfm_group(rap_songs_dfm, groups = class)
#group by class
rap_dfm_rel <- dfm_weight(rap_dfm_grouped, scheme = "prop")
#relative frequencies
#proportions: freq of term/total nr of terms in document
relfreq <- textstat_frequency(rap_dfm_rel, n = 10, groups =
class) #term frequency


plot <- ggplot(data = relfreq, aes(x = nrow(relfreq):1, y = frequency)) +
geom_point() +
facet_wrap(~ group, scales = "free") + #adds facets
coord_flip() +
scale_x_continuous(breaks = nrow(relfreq):1,
labels = relfreq$feature) +
labs(x = NULL, y = "Relative frequency") +
scale_y_continuous(labels = scales::number_format(accuracy =
0.01))

ggsave(filename = paste0(datadir,"/Figures/fig3_10.png"), plot = plot, width = 10, height = 8, dpi = 400)


library(robotstxt)
paths_allowed("http://google.com/")
paths_allowed("http://google.com/search")


