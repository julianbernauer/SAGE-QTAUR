# Julian Bernauer and Anna Wohlmann (2025): Quantitative Text Analysis Using R. London: SAGE.
# Replication Code for Chapter 4
# EXTRACTING AND VISUALISING INFORMATION FROM TEXT

# Load the required packages
library(ggplot2)
library(quanteda)
library(readtext)
library(quanteda.textplots)
library(quanteda.textstats)
library(gridExtra)

datadir <- getwd()

data()
data(msleep)
msleep

# fig 4.1 
plot <- ggplot(msleep, #first the dataset
aes(vore)) + #then the variable
geom_bar() #then what kind of graph
ggsave(filename = paste0(datadir,"/Figures/fig4_1.png"), plot = plot, width = 10, height = 8, dpi = 400)


# fig 4.2 
plot <- ggplot(msleep, #first the dataset
aes(x= vore,y=sleep_total)) + #now we add 2 variables
geom_boxplot() + #then what kind of graph
xlab("Animal Eating Habits") + #label the axis
ylab("Total Sleeping Hours") +
ggtitle("Animals eating and sleeping habits") #add a title for the plot
ggsave(filename = paste0(datadir,"/Figures/fig4_2.png"), plot = plot, width = 10, height = 8, dpi = 400)


# fig 4.3

#subset data
msleep_small <- subset(msleep[c(1:3, 7:9, 12:14),]) #choosing some animals
# Aggregate data by name of animal
library(dplyr)
agg_data <- msleep_small %>%
    group_by(name, order) %>%
    summarise(total_sleep = sum(sleep_total))
unique(agg_data$order) #3 order categories in the subset

#create the plot
plot <- ggplot(agg_data, aes(x = name, y = total_sleep, fill = order)) +
#add 3 variables, the third through fill
geom_bar(stat = "identity") + #bar plot
scale_fill_manual(values = gray.colors(3)) + #you can chooseva lot of pretty colours here
xlab("Animal Name") + #label the axis
ylab("Total Sleep Hours") +
ggtitle("Animals sleeping and eating habits") #add main title
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -1)) #because the animal names are long, we adjust the x-axis labelling
ggsave(filename = paste0(datadir,"/Figures/fig4_3.png"), plot = plot, width = 10, height = 8, dpi = 400)


# fig 4.4 
plot <- ggplot(agg_data, aes(x = name, y = total_sleep, fill = order)) +
#add 3 variables, the third through fill
geom_bar(stat = "identity") + #bar plot
scale_fill_manual(values = gray.colors(3)) + #you can choose a lot of pretty colours here
xlab("Animal Name") + #label the axis
ylab("Total Sleep Hours") +
ggtitle("Animals sleeping and eating habits") +#add main title
theme(plot.title = element_text(family = "serif", face = "bold", colour = "blue", size = 14)) + #change font, appearance, colour and size of title
coord_flip() +#turn from horizontal bars to vertical
theme(legend.position="bottom") #change legend position
ggsave(filename = paste0(datadir,"/Figures/fig4_4.png"), plot = plot, width = 10, height = 8, dpi = 400)


# fig 4.5

noNA_data <- na.omit(msleep)

plot <- ggplot(noNA_data, aes(x = sleep_total, y = bodywt)) + geom_point() + # Scatterplot
geom_smooth(method = "lm", se = FALSE, color = "blue") + # Regression line
xlab("Total Sleep Hours") +
ylab("Body Weight") +
ggtitle("Regression of Body Weight on Total Sleep Hours")
ggsave(filename = paste0(datadir,"/Figures/fig4_5.png"), plot = plot, width = 10, height = 8, dpi = 400)



## dictionaries 


pop_dict_en <- dictionary(list(
populism = c( "elit*", "consensus*", "undemocratic*", "referend*",
"corrupt*", "propagand*", "politici*", "*deceit*",
"*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
"dishonest*", "establishm*", "ruling*")
))
pop_dict_en

pop_dict_en2 <- dictionary(list(
elites = c("elit*", "politici*", "establishm*", "ruling*"),
corrupt = c("corrupt*", "propagand*", "*deceit*", "*deceiv*",
"*betray*", "shame*", "scandal*", "truth*",
"dishonest*"),
sovereignty = c("consensus*", "undemocratic*", "referend*")
))
pop_dict_en2

df_uk <- readtext(paste0(datadir,"/Data/chapter3/UK17"), encoding="UTF-8")
corpus_uk <- corpus(df_uk)
docvars(corpus_uk, "party") <- c("GP","PC","Lab","LD","Con","SNP","DUP","UKIP")
tok_uk <- tokens(corpus_uk, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
tok_uk_stop <- tokens_select(tok_uk, pattern = stopwords("en"), selection = "remove")

tok_uk_stop2 <- tokens_select(tok_uk_stop, pattern = c("DUP", "Plaid", "Cymru", "Conservatives", "Labour", "UKIP", "Green"), selection = "remove")

dfm_uk <- dfm(tok_uk_stop2)

dict_toks_uk_pop <- tokens_lookup(tok_uk_stop2, dictionary = pop_dict_en)
print(dict_toks_uk_pop)

dict_toks_uk_pop2 <- tokens_lookup(tok_uk_stop2, dictionary = pop_dict_en2)
dfm_uk_pop2 <- dfm(dict_toks_uk_pop2)
dfm_uk_pop2

dfm_uk_pop <- dfm(dict_toks_uk_pop)
poprel <- as.numeric(ntoken(dfm_uk_pop)/ntoken(dfm_uk))*100
poprel

party <- as.character(docvars(corpus_uk, "party"))
relpop <- data.frame(party,poprel)
relpop

relpop_sort <- relpop[order(-poprel),]
pop <- ggplot(data = relpop_sort, aes(x = nrow(relpop_sort):1,
y = poprel)) +
geom_point() +
coord_flip() +
scale_x_continuous(breaks = nrow(relpop_sort):1,
labels = relpop_sort$party) +
labs(x = NULL,
y = "Total Share of Populist Words (Per Cent)") +
scale_y_continuous(labels =
scales::number_format(accuracy = 0.01))
ggsave(filename = paste0(datadir,"/Figures/fig4_6.png"), plot = plot, width = 10, height = 8, dpi = 400)


eli_rel <- as.numeric(dfm_uk_pop2[,"elites"]/ntoken(dfm_uk))*100
cor_rel <- as.numeric(dfm_uk_pop2[,"corrupt"]/ntoken(dfm_uk))*100
sov_rel <- as.numeric(dfm_uk_pop2[,"sovereignty"]/ntoken(dfm_uk))*100
party <- as.character(docvars(corpus_uk,"party"))
relpop2 <- data.frame(party,eli_rel,cor_rel,sov_rel)
relpop2_sort <- relpop2[order(-eli_rel),]

pop_1 <- ggplot(data = relpop2_sort,
aes(x = nrow(relpop2_sort):1, y = eli_rel)) +
geom_point() +
coord_flip() +
scale_x_continuous(breaks = nrow(relpop2_sort):1,
labels = relpop2_sort$party) +
labs(x = NULL, y = "Share of Elite References")+
scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

relpop2_sort <- relpop2[order(-cor_rel),]

pop_2 <- ggplot(data = relpop2_sort, aes(x = nrow(relpop2_sort):1,
y = cor_rel)) +
geom_point() +
coord_flip() +
scale_x_continuous(breaks = nrow(relpop2_sort):1,
labels = relpop2_sort$party) +
labs(x = NULL, y = "Share of Corruption Refererences")+
scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
relpop2_sort <- relpop2[order(-sov_rel),]

pop_3 <- ggplot(data = relpop2_sort,
aes(x = nrow(relpop2_sort):1, y = sov_rel)) +
geom_point() +
coord_flip() +
scale_x_continuous(breaks = nrow(relpop2_sort):1, labels = relpop2_sort$party) +
labs(x = NULL, y = "Share of Sovereignty References")+
scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

plot <- grid.arrange(pop, pop_1, pop_2, pop_3, nrow = 2)

ggsave(filename = paste0(datadir,"/Figures/fig4_7.png"), plot = plot, width = 10, height = 8, dpi = 400)



# KWIC() ANALYSIS
elite_context <- kwic(corpus_uk, c("elit*", "politici*", "establishm*", "ruling*"), window = 10)
elite_context[c(4,7,8,12,28),]


# SENTIMENT ANALYSIS

load(paste0(datadir,"/Data/chapter4/rap_lyrics.Rdata"))

# DANN source("./code/ch4_prep.R") !!!

# rap_corpus <- corpus(rap_songs, text_field="x")
# summary(rap_corpus, n=6)

# load(paste0(datadir,"/Data/chapter4/rap_songs_clean.Rda"))


rap_dfm <- dfm(tokens(rap_corpus))
rap_sent <- dfm_lookup(rap_dfm, dictionary = data_dictionary_LSD2015)
rap_sent

pos <- as.numeric(rap_sent[,"positive"]) - as.numeric(rap_sent[,"neg_positive"])

neg <- as.numeric(rap_sent[,"negative"]) - as.numeric(rap_sent[,"neg_negative"])


n <- as.numeric(ntoken(rap_corpus))
net_tone_rap <- pos/n-neg/n
net_tone_rap

n <- as.numeric(ntoken(rap_corpus))
net_tone_rap <- pos/n-neg/n
net_tone_rap


song <- as.character(docvars(rap_corpus, "sosho"))
plot_sent <- data.frame(net_tone_rap,song)

plot <- ggplot(plot_sent, aes(x = reorder(song, net_tone_rap), y = net_tone_rap)) +
geom_point(color = "blue", size = 2) +
coord_flip() +
labs(x = NULL, y = "Net Sentiment (Lexicoder)")

ggsave(filename = paste0(datadir,"/Figures/fig4_8.png"), plot = plot, width = 10, height = 8, dpi = 400)



rap_lexdiv_ttr <- textstat_lexdiv(rap_dfm, measure = "TTR")
plot_ttr <- data.frame(rap_lexdiv_ttr,song)

plot <- ggplot(plot_ttr, aes(x = reorder(song, TTR), y = TTR)) +
geom_point(color = "blue", size = 2) +
coord_flip() +
labs(x = NULL, y = "Lexical Diversity (Types/Tokens)")

ggsave(filename = paste0(datadir,"/Figures/fig4_9.png"), plot = plot, width = 10, height = 8, dpi = 400)


rap_lexdiv_k <- textstat_lexdiv(rap_dfm, measure = "K")
plot_k <- data.frame(rap_lexdiv_k,song)

plot <- ggplot(plot_k, aes(x = reorder(song, K), y = K)) +
geom_point(color = "blue", size = 2) + 
coord_flip() +
labs(x = NULL, y = "Lexical Homogeneity (Yule’s K)")

ggsave(filename = paste0(datadir,"/Figures/fig4_10.png"), plot = plot, width = 10, height = 8, dpi = 400)

cor.test(rap_lexdiv_k$K,rap_lexdiv_ttr$TTR)

nscrabble("quanteda")

read_rap <- textstat_readability(rap_corpus, measure = "ARI")
plot_read <- data.frame(read_rap,song) 

plot <- ggplot(plot_read, aes(x = reorder(song, ARI), y = ARI)) +
geom_point(color = "blue", size = 2) +
coord_flip() +
labs(x = NULL, y = "(Non-)Readability (ARI)")

ggsave(filename = paste0(datadir,"/Figures/fig4_11.png"), plot = plot, width = 10, height = 8, dpi = 400)


TTR <- rap_lexdiv_ttr$TTR
plot_comp <- data.frame(net_tone_rap, TTR,song)

plot <- ggplot(plot_comp, aes(x = TTR, y = net_tone_rap)) +
geom_point(colour = "blue", size = 2) +
labs(x = "Lexical Diversity (TTR)", y = "Net Sentiment (Lexicoder)") +
geom_text(aes(label=song), hjust=-0.1, vjust=-0.1) +
xlim(0.3,0.6)

ggsave(filename = paste0(datadir,"/Figures/fig4_12.png"), plot = plot, width = 10, height = 8, dpi = 400)


cor.test(net_tone_rap,rap_lexdiv_ttr$TTR)


sub_lyr <- dfm_subset(rap_dfm, sosho=="Message" | sosho=="Hustlin" | sosho=="Storm")
sim_cor <- textstat_simil(sub_lyr)
sim_cos <- textstat_simil(sub_lyr, method = "cosine")
sim_cor
sim_cos

dist_euc <- textstat_dist(sub_lyr)
dist_man <- textstat_dist(sub_lyr, method = "manhattan")
dist_euc
dist_man

simvec <- as.numeric(sim_cor)
distvec <- as.numeric(dist_euc)
simvec <- simvec[c(2,3,6)]
distvec <- distvec[c(2,3,6)]
so <- c("mess-hust","mess-storm","hust-storm")
plot_sim <- data.frame(simvec,distvec,so)


plot <- ggplot(plot_sim, aes(x = simvec, y = distvec)) +
geom_point(colour = "blue", size = 2) +
geom_smooth(mapping = aes(x = simvec, y = distvec), method='lm', se = F, linetype = "dashed") +
labs(x = "Cosine Similarities (Pairwise)", y = "Euclidean Distances (Pairwise)") +
geom_text(aes(label=so),hjust=-0.1, vjust=-0.1) +
xlim(.45,0.75)

ggsave(filename = paste0(datadir,"/Figures/fig4_13.png"), plot = plot, width = 10, height = 8, dpi = 400)



