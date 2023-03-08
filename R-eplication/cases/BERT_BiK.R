## BERT BiK ##
## Code massively based on Steffen Zittlaus work 

# Individually 
setwd("./code/0x_case_BERT_BiK")

# TO DO: CHECK AND UPDATE DATA 
load("textdata_clean.Rdata")

##### R packages needed 
packages <- c("reticulate", "keras", "tensorflow", "tfdatasets", "tidyverse")
for (p in packages) if (!(p %in% installed.packages()[,1])) install.packages(p, character.only = TRUE) else require(p, character.only = TRUE)
rm(packages, p)


### 1-time setup of miniconda (Windows)
#reticulate::install_miniconda(force = TRUE) # 1 time
# TO DO... see Asana 

# dealing with python environments 
use_python("C:/Users/homeoffice/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
reticulate::use_condaenv("C:/Users/HOMEOF~1/AppData/Local/R-MINI~1/envs/r-reticulate")

# just do it 
Sys.setenv(TF_KERAS=1) 

# 1-time install of tensorflow 
# install_tensorflow(envname = "r-reticulate")

# check out python configuration and tensorflow version 
reticulate::py_config()
tensorflow::tf_version() 



##### setup ----
# idea: get pretrained model to adapt for our case 
# here: DistilBERT 
# setup based on python called through reticulate 

transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')
builtins <- import_builtins() #built in python methods

set.tf.repos <- "distilbert-base-german-cased"

tokenizer <- transformer$AutoTokenizer$from_pretrained(set.tf.repos)  # 
tokenizer_vocab_size <- length(tokenizer$vocab)

###### load model
model_tf = transformer$TFDistilBertModel$from_pretrained(set.tf.repos, from_pt = T, trainable = T)
model_tf$config

# set configs
model_tf$config$output_hidden_states = TRUE
summary(model_tf)


#### tokenize ----
# idea: load our data and tokenize it 
# training, test and validation data split into .6/.2/.2 
# -> validation data set not used for training but for fine-tuning (https://machinelearningmastery.com/difference-test-validation-datasets)
# target multiple categories from main_code (substantive areas)

# Define function to encode text data in batches
set.max_length =  512

# split train test val
set.seed(1234)
splitter <- sample(c("train", "test", "valid"), size = nrow(data), prob = c(.6, .2, .2), replace = TRUE)

# N_total
N_total <- nrow(data)

table(splitter)/N_total

get.tokens <- function(x){
  t <- tokenizer(
    x,
    max_length = set.max_length %>% as.integer(),
    padding = 'max_length', #'longest' #implements dynamic padding
    truncation = TRUE,
    return_attention_mask = TRUE,
    return_token_type_ids = FALSE
  )
  list(t[["input_ids"]], t[["attention_mask"]])
}

x_train <- data$text[splitter == "train"] %>% get.tokens()
x_test <- data$text[splitter == "test"] %>% get.tokens()
x_valid <- data$text[splitter == "valid"] %>% get.tokens()


### outcomes
data <- data %>%
  mutate(main_code = replace_na(main_code, "kein kodierbarer Inhalt"))
data$main_code %>% table(exclude = NULL)
ncat_main = levels(data$main_code) %>% length()
prep.maincode <- function(split = "train"){  
  data[splitter == "train",] %>% 
    mutate(main_code = as.numeric(main_code)-1) %>%
    pull(main_code) %>%
    to_categorical(num_classes = ncat_main)
}
y_main_train <- prep.maincode()
y_main_test <- prep.maincode(split = "test")
y_main_valid <- prep.maincode(split = "valid")



#### model ----
# idea: define model input, output, and optimization 

## architecture
set.hidden1 = 128
set.hidden2 = 32
set.hidden3 = 32
set.dropout = .2

input_word_ids <- layer_input(shape = c(set.max_length), dtype = 'int32', name = "input_word_ids")
input_mask <- layer_input(shape = c(set.max_length), dtype = 'int32', name = "input_attention_mask")

last_hidden_state <- model_tf(input_word_ids, attention_mask = input_mask)[[1]]
cls_token <- last_hidden_state[, 1,]

output <- cls_token %>%
  layer_dropout(rate = set.dropout) %>%
  layer_dense(units = set.hidden1, input_shape = c(set.max_length, 768), activation = 'relu') %>%
  layer_dense(units = set.hidden2, activation = 'relu') %>%
  #layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) # ?
  layer_dense(units = 9, activation = 'softmax')

model <- keras_model(inputs = list(input_word_ids, input_mask), outputs = output)

model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy"
)

model %>% summary()


#### pretest ----
# idea: test BERT

# tokenizer test
tokenizer("test das ist ein satz. der geht weiter kvbgtg")
print(tokenizer$vocab)[1:50]
"§29 Art. 5 Absatz 1" %>% tokenizer$encode() %>% tokenizer$convert_ids_to_tokens() # bert breaks up words

tokenizer_vocab_size <- length(tokenizer$vocab)


#### run ----
# idea: run BERT
# validate with validation subset
# predict on test subset 

set.epochs = 10
set.batchsize = 12

# prepare the data
tf_x_train <- tensor_slices_dataset(x_train) 
tf_y_main_train <- tensor_slices_dataset(y_main_train)
tf_train <- zip_datasets(tf_x_train, tf_y_main_train) %>% dataset_batch(set.batchsize)

tf_x_valid <- tensor_slices_dataset(x_valid) 
tf_y_main_valid <- tensor_slices_dataset(y_main_valid)
tf_valid <- zip_datasets(tf_x_valid, tf_y_main_valid) %>% dataset_batch(set.batchsize)

#model_tf$trainable = T # ? 

history = model %>%
  keras::fit(
    tf_train,
    epochs = set.epochs,
    metrics = "accuracy",
    validation_data = tf_valid
  )

# predict
tf_x_test <- tensor_slices_dataset(x_test) 
tf_y_main_test <- tensor_slices_dataset(y_main_test)
tf_test <- zip_datasets(tf_x_test, tf_y_main_test) %>% dataset_batch(set.batchsize)

pmat <- predict(model, tf_test)

true_y <- y_main_test %>% apply(., 1, which.max)
p_y <- pmat %>% apply(., 1, which.max)

table(p_y, true_y)


# train seperately  https://www.kaggle.com/code/angyalfold/hugging-face-bert-with-custom-classifier-pytorch/notebook

## validation ---- 

## visualization 

