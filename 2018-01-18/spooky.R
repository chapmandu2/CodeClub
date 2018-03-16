#spooky authors!

.libPaths('/scratch/working/R-libs/')

library(readr)
library(tm)
library(tidytext)
library(LDAvis)

train_data <- read_csv('/home/rstudio/spooky/data/train.csv',
                       skip=1,
                       col_names = c('doc_id', 'text', 'author'))
train_data <- as.data.frame(train_data)

ds <- DataframeSource(train_data)
x <- VCorpus(ds)
inspect(x[[5]])
meta(x[[10]], 'author')

t_data <- tm_map(x, stripWhitespace)
t_data <- tm_map(x, removePunctuation)
t_data <- tm_map(t_data, content_transformer(tolower))
t_data <- tm_map(t_data, removeWords, stopwords("english"))
t_data <- tm_map(t_data, stemDocument)

dtm <- DocumentTermMatrix(t_data)
inspect(dtm)

dtm_sparse <- removeSparseTerms(dtm, 0.99)
dim(dtm_sparse)

dtm_m <- as.matrix(dtm_sparse)

#check that doc ids have persisted
identical(rownames(dtm_m), train_data$doc_id)

#try out tidytext
library(tidyverse)
dtm_tidy <- tidy(dtm_sparse) %>%
  inner_join(train_data[,c(1,3)], by=c('document'='doc_id'))

dtm_spread <- tidyr::spread(dtm_tidy, author, count, fill=0)
