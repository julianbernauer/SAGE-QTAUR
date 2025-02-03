# Julian Bernauer and Anna Wohlmann (2025): Quantitative Text Analysis Using R. London: SAGE.
# Replication Code for Chapter 5
# SUPERVISED MACHINE LEARNING FOR TEXT DATA

# Load the required packages
library(tidyverse)
library(readtext)
library(caret)
library(rsample)
library(modeldata)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(grafzahl)

datadir <- getwd()

load(paste0(datadir,"/Data/chapter3/fff_200124_1415.Rdata"))

# source("./code/nb_fullcode.R")

# naive bayes

fff_de <- fff %>% filter(lang=="de")
set.seed(423)
split <- initial_split(fff_de, prop = .075)
train <- training(split)
test  <- testing(split)
length(train$is_quote)
length(test$is_quote)
table(train$is_quote) %>% prop.table()
table(test$is_quote) %>% prop.table()
train_hand_coded <- read.csv2("./Data/chapter5/train.csv")
train_code_full <- bind_cols(train, train_hand_coded)
table(train_code_full$handcoding)
set.seed(123)
split_val <- initial_split(train_code_full, prop = .334)
val_final <- training(split_val)
train_final <- testing(split_val)
train_final$profff <- 0
train_final$profff[train_final$handcoding=="1"] <- 1
train_final$profff <- as.factor(train_final$profff)
bayes_corpus_train <- corpus(train_final, text_field = "text...5")
summary(bayes_corpus_train, n=10)
val_final$profff <- 0
val_final$profff[val_final$handcoding=="1"] <- 1
val_final$profff <- as.factor(val_final$profff)
bayes_corpus_val <- corpus(val_final, text_field = "text...5")
summary(bayes_corpus_val, n=10)
toks_bayes_train <- tokens(bayes_corpus_train, remove_punct = TRUE, remove_number = TRUE) %>%
  tokens_remove(pattern = stopwords("de")) %>%
  tokens_wordstem()
dfmt_bayes_train <- dfm(toks_bayes_train)
toks_bayes_val <- tokens(bayes_corpus_val, remove_punct = TRUE, remove_number = TRUE) %>%
  tokens_remove(pattern = stopwords("de")) %>%
  tokens_wordstem()
dfmt_bayes_val <- dfm(toks_bayes_val)
tmod_nb <- textmodel_nb(dfmt_bayes_train, dfmt_bayes_train$profff)
summary(tmod_nb)
dfmat_matched <- dfm_match(dfmt_bayes_val, features = featnames(dfmt_bayes_train))
actual_class <- dfmat_matched$profff
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything", positive = "1")


# word embeddings 



embeddings <- text_input %>%
layer_embedding(input_dim = n_tokens+1, output_dim = word2vecdim
#mask_zero = TRUE
) %>%
layer_global_average_pooling_1d()

base_model <- embeddings %>%
layer_dense(units = word2vecdim, activation = 'relu',
kernel_regularizer = regularizer_l2(l = 0.001)) %>%
layer_dropout(rate = 0.5) %>%
layer_dense(units = 32, activation = 'relu',
kernel_regularizer = regularizer_l2(l = 0.001))

output_topic <- base_model %>% layer_dense(units = n_topics,
activation = 'softmax', name = 'output_topic')

output_relev <- base_model %>% layer_dense(units = 1,
activation = 'sigmoid', name = 'output_relev')
model <- keras_model(
inputs = text_input,
outputs = list(output_topic, output_relev)
)
model %>% compile(
optimizer = "adam", #optimizer_adam(learning_rate = 1e-3),
loss = list(
output_topic = "sparse_categorical_crossentropy",
output_relev = "binary_crossentropy"
),
loss_weights = list(output_topic = 1, output_relev = .3),
metrics = list(list('accuracy'), list('accuracy'))
)


table(pred$topic %>% apply(., 1, which.max), y_topic_test+1)


caret::confusionMatrix(pred_topic, test_topic)


# find words that are related to another word
token = "mutterschutz"
embedding_vector <- t(matrix(word2vec_embedding[token,]))
cos_sim = text2vec::sim2(x = word2vec_embedding, y =
embedding_vector, method = "cosine", norm = "l2")
# similar words
print(head(sort(cos_sim[,1], decreasing = TRUE), 20))


print(head(sort(cos_sim[,1], decreasing = TRUE), 20))



# figure 5.2    


CODE!? 







## BERT

df_uk <- readtext(paste0(datadir,"/data/chapter3//UK17"), encoding="UTF-8")
corpus_uk <- corpus(df_uk)
docvars(corpus_uk, "party") <- c("GP","PC","Lab","LD","Con", "SNP","DUP","UKIP")

corpus_uk_sentences <- corpus_reshape( corpus_uk, to = c("sentences"), use_docvars = TRUE)
head(corpus_uk_sentences)

# Set a seed for reproducibility
set.seed(123)
# Draw a random sample of 50 sentences
corpus_uk_sample <- corpus_sample(corpus_uk_sentences, size = 50)
corpus_uk_sample <- corpus_sample(corpus_uk_sample, size = 50)
summary(head(corpus_uk_sample))


set.seed(123)
corpus_uk_ukip <- corpus_uk_sentences[docvars(corpus_uk_sentences, "party") == "UKIP", ]
corpus_uk_ukip_sample <- corpus_sample(corpus_uk_ukip, size = 50)
summary(head(corpus_uk_ukip_sample))


# Add a document-level variable
docvars(corpus_uk_sentences, "pop_train") <- 2


# Replace the value of "pop_train" with 0 for the sentences that are part of corpus_uk_sample/corpus_uk_ukip_sample
docvars(corpus_uk_sentences, "pop_train")[names(corpus_uk_sentences) %in% names(corpus_uk_sample)] <- 0
docvars(corpus_uk_sentences, "pop_train")[names(corpus_uk_sentences) %in%names(corpus_uk_ukip_sample)] <- 0
# replace docvar "pop_train" for sentences coded as populist with 1 - done for multiple sentences
doc_position <- which(docnames(corpus_uk_sentences) == "51951.000.2017.1.1.txt.181")
docvars(corpus_uk_sentences, "pop_train")[doc_position] <- 1
# Subset the data, only using the training data
corpus_uk_sent_train <- corpus_uk_sentences[docvars(corpus_uk_sentences, "pop_train") <2, ]
# summarise the docvar
summary(docvars(corpus_uk_sent_train, "pop_train"))
# This line would display the first 1000 values of the docvar
# corpus_uk_sentences$pop_train


# run one time to set up grafzahl (without GPU)
# setup_grafzahl(cuda = FALSE)
# specifying a BERT model via grafzahl
bert_model <- grafzahl(corpus_uk_sent_train, y="pop_train", model_type = "bert", model_name = "bert-base-uncased", num_train_epochs = 1, output_dir = "./output", verbose = TRUE)

# prediction
new_data <- c("We represent the ordinary people, not the privileged few.","Our democratic society values diversity and encourages inclusive dialogue.")
predict(bert_model, new_data)




## wordscores 

df_uk <- readtext(paste0(datadir,"/Data/chapter3/UK17"), encoding="UTF-8")
corpus_uk <- corpus(df_uk)

docvars(corpus_uk, "party") <- c("GP","Plaid","Lab","LD", "Con","SNP","DUP","UKIP")
docvars(corpus_uk, "ref_score") <- c(0,NA,NA,NA,NA,NA,NA,1)
docvars(corpus_uk, "ref_scoreII") <- c(NA,NA,4.4,NA,7.4,NA,NA,NA)
summary(corpus_uk)

dfm_uk <- dfm_remove(dfm(tokens(corpus_uk, remove_punct = TRUE)), 
                     c(stopwords(language = "en"), "DUP", "Plaid", "Cymru", "Conservatives", "Labour", "UKIP", "Green"))

head(dfm_uk, 8, n = 6)


wscore <- textmodel_wordscores(dfm_uk, y = as.numeric(docvars(dfm_uk, "ref_score")), smooth = .5)
wscore[["wordscores"]][["britain"]]
wscore[["wordscores"]][["environment"]]


pred_wsa <- predict(wscore, se.fit = TRUE, interval = "confidence", newdata = dfm_uk, rescaling = "none")
pred_wsb <- predict(wscore, se.fit = TRUE, interval = "confidence", newdata = dfm_uk, rescaling = "lbg")
pred_wsc <- predict(wscore, se.fit = TRUE, interval = "confidence", newdata = dfm_uk, rescaling = "mv")
par(mfrow=c(1,3)) 

# figure 5.3
png(filename = paste0(datadir, "/Figures/fig5_3.png"), width = 10, height = 8, units = "in", res = 300)
plot <- textplot_scale1d(pred_wsa, doclabels = docvars(dfm_uk, "party"))
print(plot)
dev.off()

# scaling variants 
# textplot_scale1d(pred_wsb, doclabels = docvars(dfm_uk, "party"))
# textplot_scale1d(pred_wsc, doclabels = docvars(dfm_uk, "party"))


# figure 5.4 (!)

wscoreII <- textmodel_wordscores(dfm_uk, y = as.numeric(docvars(dfm_uk,
"ref_scoreII")), smooth = 0.5) 
pred_wsII <- predict(wscoreII, se.fit = TRUE, newdata = dfm_uk, interval = "confidence", rescaling = "lbg")
wsII <- pred_wsII$fit

png(filename = paste0(datadir, "/Figures/fig5_4.png"), width = 10, height = 8, units = "in", res = 300)
textplot_scale1d(pred_wsII, margin = "documents", doclabels = docvars(dfm_uk, "party"))
dev.off()

