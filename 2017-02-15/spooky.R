#spooky authors!

library(readr)
library(tm)
library(tidytext)
library(LDAvis)
library(stm)
library(quanteda)
library(dplyr)
library(ggplot2)

train_data <- read_csv('/home/rstudio/train.csv',
                       skip=1,
                       col_names = c('doc_id', 'text', 'author'))

tidy_data <- train_data %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

counts_tf_idf <- tidy_data %>%
  count(doc_id, author, word, sort=TRUE) %>%
  bind_tf_idf(word, doc_id, n)

counts_tf_idf %>%
  arrange(-tf_idf) %>%
  group_by(author) %>%
  top_n(10) %>%
  ungroup()

counts_sparse <- counts_tf_idf %>%
  cast_sparse(doc_id, word, n)

topic_model <- stm(counts_sparse, K=6, init.type='Spectral', max.em.its=10)
summary(topic_model)

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(counts_sparse))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       y = "Number of docs", x = expression(gamma))

#does a particular author tend to associate with a particular topic?
td_gamma_withauthors <- train_data %>%
  dplyr::select(-text) %>%
  inner_join(td_gamma, by=c('doc_id'='document')) %>%
  group_by(doc_id, author) %>%
  mutate(top_topic=dense_rank(desc(gamma)))
  
  

ggplot(dplyr::filter(td_gamma_withauthors, top_topic == 1), aes(topic)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ author, ncol = 3) 

ggplot(td_gamma_withauthors, aes(x=author, y=gamma, colour=as.factor(topic))) + 
  geom_violin() + facet_wrap(~ as.factor(topic), ncol = 3) 

thts <- stm::findThoughts(topic_model, train_data[match(rownames(counts_sparse), train_data$doc_id),]$text)
plotQuote(thts)

#try out tidytext
library(tidyverse)
dtm_tidy <- tidy(dtm_sparse) %>%
  inner_join(train_data[,c(1,3)], by=c('document'='doc_id'))

dtm_spread <- tidyr::spread(dtm_tidy, author, count, fill=0)
