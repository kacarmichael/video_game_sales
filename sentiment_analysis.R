library(wordcloud)
library(udpipe)
library(lattice)
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(stringr)
library(tidyverse)


library(SnowballC)
library(hunspell)


#ud_model = udpipe_download_model(language = "english")
#install.packages('textdata')

data("stop_words")

setwd("C:\\Users\\Aaron\\Google Drive\\School Stuff\\Fall 2019\\Data Science Programming I\\DSP1 Semester Project\\Deliverable 1 Materials")
full_data <- read.csv("vgMetaFullCleanFinal.csv", header = TRUE)

#This script takes the unique summaries found in the data and performs a sentiment analysis and constructs a word cloud


summaries <- as.data.frame(full_data$summary)
colnames(summaries) <- c("summary")
summaries$summary <- as.character(summaries$summary)
summaries_unique <- distinct(summaries, summary)

#Extract unique summaries into one block of text
summaries_full <- ""
for (i in c(1:length(summaries_unique$summary))) {
    summaries_full <- paste(summaries_full, summaries_unique$summary[i], sep = " ")
}

patterndigits <- '\\b[0-9]+\\b'

#remove stop words
tidy_summaries <- summaries_unique %>%
    unnest_tokens(word, summary) %>%
    anti_join(stop_words) 

#remove spaces and digits
tidy_summaries$word <- tidy_summaries$word %>%
    str_replace_all(patterndigits, '') %>%
    tolower() %>%
    str_replace_all('[:space:]', '')

#remove blank words
tidy_summaries <- tidy_summaries %>%
    filter(!(word == ''))

#stem words
tidy_summaries <- tidy_summaries %>%
    mutate_at("word", funs(wordStem((.), language = "en")))

#find most commonly appearing words, while calculating the proportion of the individual words to the total
tidy_summaries <- tidy_summaries %>%
    count(word) %>%
    arrange(desc(n)) %>%
    mutate(proportion = (n / sum(n) * 100)) 

plot <- ggplot(tidy_summaries, aes(x = proportion, y = word)) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    theme(legend.position = "none") +
    labs(y = 'Word', x = 'Proportion')

plot

#positive and negative sentiments from the NRC Lexicon
nrc_posneg <- get_sentiments('nrc') %>%
    filter(sentiment == 'positive' | sentiment == 'negative')

summary_posneg <- tidy_summaries %>%
    inner_join(nrc_posneg) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(contentment = positive - negative, linenumber = row_number()) %>%
    #sentiment acts as a binary variable, is it more positive or negative, this is primarily used for the plot
    mutate(sentiment = (positive > 0)) %>%
    arrange(desc(contentment))

#Player is counted as negative, should be positive in this context
summary_posneg[summary_posneg$word == 'player',]$contentment = 304
summary_posneg[summary_posneg$word == 'player',]$negative = 0
summary_posneg[summary_posneg$word == 'player',]$positive = 304
summary_posneg[summary_posneg$word == 'player',]$sentiment = TRUE

contentplot_posneg <- ggplot(summary_posneg, aes(x = linenumber, y = contentment, fill = sentiment)) +
    geom_col() +
    scale_x_continuous() +
    coord_flip() +
    labs(x = 'Word', y = 'Sentiment') +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
          ggtitle("Negativity/Positivity Sentiment")
          geom_hline(yintercept = 0, color = "black", size = 1)

nrc_joysad <- get_sentiments('nrc') %>%
    filter(sentiment == 'joy' | sentiment == 'sadness')

summary_joysad <- tidy_summaries %>%
    inner_join(nrc_joysad) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(contentment = joy - sadness, linenumber = row_number()) %>%
    mutate(sentiment = (joy > 0)) %>%
    arrange(desc(contentment))

contentplot_joysad <- ggplot(summary_joysad, aes(x = linenumber, y = contentment, fill = sentiment)) +
    geom_col() +
    scale_x_continuous() +
    coord_flip() +
    labs(x = 'Word', y = 'Sentiment') +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
          ggtitle("Sadness/Joy Sentiment") +
          geom_hline(yintercept = 0, color = "black", size = 1)


#wordcloud
corp <- Corpus(VectorSource(summaries_full))
corp <- corp %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)

tdm <- TermDocumentMatrix(corp)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

d <- d %>%
    anti_join(stop_words)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



