import codecs
import nltk
import string
import os
import numpy as np
import gensim
from sklearn.metrics.pairwise import cosine_similarity

exclude = set(string.punctuation)

def text_embedding(text):
  text = text.lower()
  text = nltk.tokenize.WordPunctTokenizer().tokenize(text)
  text = [token for token in text if token not in exclude and token.isalpha()]
  doc_embed = []
  for word in text:
    try:
      embed_word = emb_model[word]
      doc_embed.append(embed_word)
    except KeyError:
      continue
  if len(doc_embed)>0:
    avg = [float(sum(col))/len(col) for col in zip(*doc_embed)]
    avg = np.array(avg).reshape(1, -1)
    return avg
  else:
    return "Empty"

emb_model = gensim.models.KeyedVectors.load_word2vec_format('./wordembeddings/wiki.en.vec', binary=False)

query = "people elite sovereignty"

query_emb = text_embedding(query)

collection_path = "[PATH]"

collection = {}

for filename in os.listdir(collection_path):
  content = codecs.open(collection_path+filename,"r","utf-8").read()
  content = nltk.sent_tokenize(content)
  content = [[sent, text_embedding(sent)] for sent in content if type(text_embedding(sent))!= str]
  collection[filename] = content

for filename,sentences in collection.items():
  ranking = [[sent, cosine_similarity(query_emb,sent_emb)[0][0]] for sent, sent_emb in sentences]
  ranking.sort(key=lambda x: x[1],reverse=True)
  print (filename)
  for sent, score in ranking[:1]:
    print (sent, score)
  print (" \n")