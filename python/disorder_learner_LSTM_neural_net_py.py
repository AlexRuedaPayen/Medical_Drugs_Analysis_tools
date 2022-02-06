import os
# os.system('conda install sklearn')
# os.system('conda install pandas')
# os.system('conda install keras')
# os.system('conda install tensorflow')
# os.system('conda config --set allow_conda_downgrades true')
# os.system('conda install conda=4.6.11')
# os.system('conda install -c conda-forge numpy=1.19.5') <<- need to downgrade numpy to this version -could last hours-
# conda install -c conda-forge/label/cf202003 tensorflow
# os.system('conda info --envs')


import numpy as np 
import pandas as pd

from sklearn.feature_extraction.text import CountVectorizer
from tensorflow.keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense, Embedding, LSTM, SpatialDropout1D
from sklearn.model_selection import train_test_split
from keras.utils.np_utils import to_categorical
import re

####if that ain't working through reticulate send csv data through ssh to GCP VM and anlyze their

def disorder_learner_LSTM_neural_net_py_train(file_name):
  
  data=pd.read_csv(file=file_name)
  
  max_fatures = 2000
  tokenizer = Tokenizer(num_words=max_fatures, split=' ')
  tokenizer.fit_on_texts(data['review'].values)
  X = tokenizer.texts_to_sequences(data['review'].values)
  X = pad_sequences(X)
  embed_dim = 128
  lstm_out = 196

  model = Sequential()
  model.add(Embedding(max_fatures, embed_dim,input_length = X.shape[1]))
  model.add(SpatialDropout1D(0.4))
  model.add(LSTM(lstm_out, dropout=0.2, recurrent_dropout=0.2))
  model.add(Dense(2,activation='softmax')) ###multilabel classification <<- softmax activation

  model.compile(loss = 'categorical_crossentropy', optimizer='adam',metrics = ['accuracy'])
  
  Y = pd.get_dummies(data['condition']).values ###ONE HOT ENCODE
  
  X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = 0.33, random_state = 42)
  
  batch_size = 32
  model.fit(X_train, Y_train, epochs = 7, batch_size=batch_size, verbose = 2)
  
  validation_size = 1500

  X_validate = X_test[-validation_size:]
  Y_validate = Y_test[-validation_size:]
  X_test = X_test[:-validation_size]
  Y_test = Y_test[:-validation_size]
  
  keras.models.save_model(model,filepath='~/Projects/Medical_Drugs_Analysis_tools/python/Neural_net/disorder_learner_LSTM_neural_net_keras')
  
  
def disorder_learner_LSTM_neural_net_py_train(file_name):
  
  model=keras.models.laod_model(filepath='~/Projects/Medical_Drugs_Analysis_tools/python/Neural_net/disorder_learner_LSTM_neural_net_keras',compile = True)
  
  score,acc = model.evaluate(X_test, Y_test, verbose = 2, batch_size = batch_size)
  print("score: %.2f" % (score))
  print("acc: %.2f" % (acc))
  
  
import sys

file_name_=sys.argv[1]
disorder_learner_LSTM_neural_net_py(file_name_)
