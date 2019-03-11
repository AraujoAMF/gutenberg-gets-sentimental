library(data.table)
library(ggplot2)
library(stringr)
library(tm)
library(qdap)
library(gutenbergr)
library(wordcloud)
library(ggdendro)
library(RWeka)
library(tidytext)
library(textstem)
library(lexicon)
library(ggdendro)
library(treemapify)
library(visNetwork)
#nrc - 8 sentiments
#bing - positive/negative
#affin = -5/5
gutenberg_base <- data.table(gutenberg_works())
gutenberg_base <- gutenberg_base[gutenberg_id > 0, .(gutenberg_id, title, author)]

gutenberg_download2 <- function(x) {
  data.table(gutenberg_download(x))
}

#raw_text <-  data.table(gutenberg_download(choice))

preprocess_gutenberg <- function(raw_text) {
  
  text1 <- raw_text[, text]
  text1[text1 == ""] <- '\n'
  #text1 <- strsplit(paste(text1, collapse = ' '), split = '\\.')[[1]]
  text1 <- strsplit(paste(text1, collapse = ' '), split = '\n')[[1]]
  text1 <- replace_contraction(text1)
  text1 <- text1[!(text1 == "")]
  text1 <- tolower(text1)
  text1 <- removePunctuation(text1)
  #remove special x=character to avoid bugs
  text1<- gsub("[^\x20-\x7E]", "", text1)
  text1 <- removeWords(text1, stopwords("en"))
  text1 <- Trim(text1)
  text1 <- text1[text1 != ""]
  text1 <- lemmatize_strings(text1)
}
#text_corpus <- preprocess_gutenberg(50)

make_most_frequent_ngrams <- function(text1, n = 1) {

  tokenizer <- function(x) {
    NGramTokenizer(x ,Weka_control(min = n, max = n))
  }
  
  text_corpus <- VCorpus(VectorSource(text1))
  text_m <-  TermDocumentMatrix(text_corpus, control = list(tokenize = tokenizer))
  text_m <- as.matrix(text_m)
  
  term_frequency <- rowSums(text_m)
  term_frequency <- sort(term_frequency, decreasing = TRUE)
  term_frequency_dt <- data.table(Word = names(term_frequency), N = term_frequency)
  
  term_frequency_dt <- term_frequency_dt[1:25]
  #nrc <- data.table(get_sentiments("nrc"))
  #setnames(nrc, "word", "Word")
  #term_frequency_dt <- merge(term_frequency_dt, nrc, by = "Word", all.x =  TRUE)
  setorder(term_frequency_dt, N)
  term_frequency_dt[, Word := factor(Word, levels = Word, labels = Word)]
  #term_frequency_dt[, n_times := .N, Word]
  #term_frequency_dt[, N := N/n_times]
  
  #term_frequency_dt[, polarity_score := polarity(Word)[["group"]][["ave.polarity"]] , Word]
  #term_frequency_dt[polarity_score != 0, N := polarity_score*N ]
  #term_frequency_dt[polarity_score == 0, sentiment := "neutral"]
  #term_frequency_dt[polarity_score == 1, sentiment := "positive"]
  #term_frequency_dt[polarity_score == -1, sentiment := "negative"]
  #term_frequency_dt[, sentiment := factor(sentiment, levels = c("positive", "negative", "neutral"))]
  #term_frequency_dt[, sentiment := as.factor(sentiment)]
  #setorder(term_frequency_dt, N)
  g <- ggplot(term_frequency_dt)
  g <- g + geom_bar(aes(x = Word, y = N, fill = Word), stat = "identity")
  g <- g + coord_flip()
  g <- g + ggtitle(sprintf("Top %s-grams", n))
  g
}


make_wordcloud <- function(text_corpus){
  term_frequency <- rowSums(text_m)
  term_frequency <- sort(term_frequency, decreasing = TRUE)
  term_frequency_dt <- data.table(Word = names(term_frequency), N = term_frequency)
  term_frequency_dt[, Word := factor(Word, levels = rev(names(term_frequency)), labels = rev(names(term_frequency)))]
  
  wordcloud(words = term_frequency_dt[, Word], freq = term_frequency_dt[, N], max.words = 25, scale=c(4,.18))
  
}

make_narrative_arc <- function(text1) {
  
  text_dt <- data.table(text =  text1)
  text_dt[, polarity_score := polarity(text)[["group"]][["ave.polarity"]] , text]
  text_dt[, index := .I]
  
  text_dt[polarity_score > 0, sentiment := "positive" ]
  text_dt[polarity_score == 0, sentiment := "neutral" ]
  text_dt[polarity_score < 0, sentiment := "negative" ]
  text_dt[, sentiment := factor(sentiment, levels = c("positive", "negative", "neutral"))]
  
  g <- ggplot(text_dt)
  g <- g + geom_bar(aes(x = index, y =  polarity_score, fill = sentiment), stat = "identity")
  g <- g + ggtitle("Narrative Arc")
  g
}

make_dendogram <- function(text1) {
  text_corpus <- VCorpus(VectorSource(text1))
  text_m <-  TermDocumentMatrix(text_corpus)
  text_m <- removeSparseTerms(text_m, sparse = 0.95)
  text_m <- as.matrix(text_m)
  text_m <- dist(text_m)
  hc <- hclust(text_m)
  ggdendrogram(hc)
  
}



make_treemap <- function(text1) {
  
  text_dt <- data.table(word =  unlist(str_split(text1, " ")))
  text_dt <- text_dt[, .N, word]
  
  nrc <- data.table(get_sentiments("nrc"))
  setorder(nrc, sentiment)
  
  nrc <- nrc[, .(sentiment = paste(unique(sentiment), collapse = " ")),  word]
  setkey(text_dt, word)
  text_dt <- text_dt[nrc, nomatch = 0]
  text_dt <- text_dt[, .(N = sum(N)), sentiment]
  setorder(text_dt, -N)
  g <- ggplot(text_dt)
  g <- g + geom_treemap(aes(area = N, fill = sentiment))
  g <- g + geom_treemap_text(aes(area = N, label = sentiment))
  g <- g + ggtitle("NRC Sentiments Treemap")
  g <- g+ theme(legend.position="none")
  g
}

if(FALSE) {
  
  source("/home/allan/Documentos/gutenberg_vis/processing.R")
  raw_text <- gutenberg_download2(1)
  text1 <- preprocess_gutenberg(raw_text)
  n <- 2
  tokenizer <- function(x) {
    NGramTokenizer(x ,Weka_control(min = n, max = n))
  }
  
  text_corpus <- VCorpus(VectorSource(text1))
  text_m <-  TermDocumentMatrix(text_corpus, control = list(tokenize = tokenizer))
  text_m <- as.matrix(text_m)
  term_frequency <- rowSums(text_m)
  term_frequency <- sort(term_frequency, decreasing = TRUE)
  term_frequency_dt <- data.table(Word = names(term_frequency), N = term_frequency)
  term_frequency_dt[, from := str_split(Word, " ")[[1]][1], Word]
  term_frequency_dt[, to := str_split(Word, " ")[[1]][2], Word]
  nodes <- unique(term_frequency_dt[N >= 3, .(id = (c(from, to)))], by = NULL)
  edges <- term_frequency_dt[N >= 3, .(from, to)]
  visNetwork(nodes, edges )
  
}
