library(hunspell)
library(topicmodels)
library(reshape2)
library(rvest)
library(dplyr)
library(tm)
library(tibble)
library(textclean)
library(textstem)
library(tidytext)
library(ggplot2)
library(ggraph)
library(igraph)
library(ggridges)
library(LDAvis)
library(servr)
library(viridis)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(tidygraph)
library(tidyr)


# Scrape book reviews
book_url <- "https://www.goodreads.com/book/show/62119780-animal-liberation-now?ref=rae_0"
book_page <- read_html(book_url)

book_reviews <- book_page %>%
  html_elements("#ReviewsSection .Formatted") %>%
  html_text(trim = TRUE)

book_reviews_df <- data.frame(reviews = book_reviews, stringsAsFactors = FALSE) %>%
  filter(!is.na(reviews), trimws(reviews) != "")

# Preprocess text
book_corpus <- VCorpus(VectorSource(book_reviews_df$reviews))
book_corpus <- tm_map(book_corpus, content_transformer(tolower))
book_corpus <- tm_map(book_corpus, content_transformer(replace_contraction))
book_corpus <- tm_map(book_corpus, content_transformer(replace_emoticon))
book_corpus <- tm_map(book_corpus, removePunctuation)
book_corpus <- tm_map(book_corpus, removeNumbers)
book_corpus <- tm_map(book_corpus, removeWords, stopwords("en"))
book_corpus <- tm_map(book_corpus, stripWhitespace)
book_corpus <- tm_map(book_corpus, content_transformer(lemmatize_strings))
book_corpus <- tm_map(book_corpus, stemDocument)

# Spell checking
tokens <- strsplit(sapply(book_corpus, as.character), " ")

correct_spelling <- function(token_list) {
  corrected <- lapply(token_list, function(words) {
    misspelled <- hunspell_check(words)
    corrected_words <- words
    for (i in seq_along(words)) {
      if (!misspelled[i]) {
        suggestions <- hunspell_suggest(words[i])[[1]]
        if (length(suggestions) > 0) {
          corrected_words[i] <- suggestions[1]
        }
      }
    }
    return(corrected_words)
  })
  return(corrected)
}

corrected_tokens <- correct_spelling(tokens)
corrected_text <- sapply(corrected_tokens, paste, collapse = " ")
book_corpus <- VCorpus(VectorSource(corrected_text))

# Save cleaned reviews
book_reviews_cleaned_df <- data.frame(
  updated_book_reviews = sapply(book_corpus, as.character),
  stringsAsFactors = FALSE
)
write.csv(book_reviews_cleaned_df, "D:/Final_term_corpus.csv", row.names = FALSE)

# Document-Term Matrix
book_dtm <- DocumentTermMatrix(book_corpus)
as.matrix(book_dtm)
tidy_dtm <- tidy(book_dtm)
head(tidy_dtm)

# LDA Model
num_topics <- 12
book_lda_model <- LDA(book_dtm, k = num_topics, method = "Gibbs", control = list(seed = 1234, burnin = 1000, iter = 2000))

# Get top terms
book_term_probs <- tidy(book_lda_model)

book_top_terms <- book_term_probs %>%
  group_by(topic) %>%
  arrange(desc(beta)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  drop_na()

topic_labels <- c(
  "Topic 1: Historical & Biographical Narratives",
  "Topic 2: Christian & Faith-Based Perspectives",
  "Topic 3: Vegetarian Cooking & Nutrition",
  "Topic 4: Animal Ethics & Vegan Advocacy",
  "Topic 5: Food Product Reviews & Recipe Critiques",
  "Topic 6: Philosophical Arguments For Diet Choices",
  "Topic 7: Species Conservation & Specialist Care",
  "Topic 8: Animal Farming & WElfare",
  "Topic 9: Animal Rights Philosophy & Treatment Theories",
  "Topic 10: Legal Bans In Animal Welfare",
  "Topic 11: Singer & Utilitarian Ethics",
  "Topic 12: Human Omnivory & Moral Consciousness"
)

book_top_terms <- book_top_terms %>%
  mutate(topic_label = topic_labels[topic])

# Bar chart
ggplot(book_top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Word Probabilities by Topic (Book Reviews)",
       x = "Keywords/Terms",
       y = "Probability (Beta)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# LDAvis - Manual extraction since posterior() doesn't work for Gibbs
phi <- exp(book_lda_model@beta)
phi <- phi / rowSums(phi)

theta <- book_lda_model@gamma
theta <- theta / rowSums(theta)

vocab <- book_lda_model@terms
doc.length <- rowSums(as.matrix(book_dtm))
term.frequency <- colSums(as.matrix(book_dtm))

book_lda_json <- createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  
  doc.length = doc.length,
  term.frequency = term.frequency
)

serVis(book_lda_json)

# Network Graph
top_terms <- book_term_probs %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup()

edges <- top_terms %>%
  mutate(from = paste0("Topic ", topic), to = term)

nodes <- data.frame(
  name = unique(c(edges$from, edges$to)),
  type = ifelse(grepl("Topic", unique(c(edges$from, edges$to))), "topic", "term")
)

tbl_graph(nodes = nodes, edges = edges) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(color = from), show.legend = FALSE) +
  geom_node_point(aes(color = type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Ridge Plot
book_term_probs %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = topic, y = term, fill = beta)) +
  geom_tile() +
  scale_fill_viridis(option = "C") +
  scale_y_reordered() +
  labs(x = "Topic", y = "Term", fill = "Probability (Beta)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wordclouds
par(mfrow = c(3, 4), mar = c(1, 1, 1, 1))
for (i in 1:num_topics) {
  topic_terms <- subset(book_term_probs, topic == i)
  wordcloud(
    words = topic_terms$term,
    freq = topic_terms$beta * 10,
    max.words = 20,
    colors = brewer.pal(8, "Dark2"),
    main = paste("Topic", i)
  )
}

# Topic Proportions
book_doc_probs <- tidy(book_lda_model, matrix = "gamma")
book_doc_probs %>%
  group_by(topic) %>%
  summarize(avg_gamma = mean(gamma)) %>%
  ggplot(aes(x = factor(topic), y = avg_gamma)) +
  geom_col(fill = "steelblue") +
  labs(x = "Topic", y = "Average Proportion in Corpus",
       title = "Dominance of Topics in Reviews") +
  theme_minimal()

# Topic Clustering Dendrogram
topic_term_matrix <- book_term_probs %>%
  pivot_wider(names_from = topic, values_from = beta, values_fill = 0) %>%
  column_to_rownames("term") %>%
  as.matrix()

dist_matrix <- dist(t(topic_term_matrix), method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Topic Clustering Dendrogram", xlab = "", sub = "")
