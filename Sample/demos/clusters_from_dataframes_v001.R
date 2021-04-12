# clusters_from_dataframes_v001.R

# Generic Idea: Take a Dataframe. Select one of its columns, keep it as reference.
# Next, put together "as text" all other columns of the dataframe, and put together that for each unique entry of the selected base column.
# Next, use the TF-IDF trick (where each entry of the selected column becomes a "document") to create a numeric matrix to then pass that to
# K-Means. That way, one can use in theory K-Means on "any" text-based dataframe. Probably NOT a good idea to do that without a 
# great understanding of the underlying data, so USE WITH CAUTION. NOT TO BE USED LIGHTLY (and possibly WRONG in many cases ;))

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(tm) # for Corpus
library(SnowballC)
library(wordcloud)
library(factoextra)
library(NbClust)

objs_clusters_from_df <- function(df, col_cluster = 1, n_cluster = 2) {
  
  cp_df <- df
  cp_df[] <- lapply(cp_df, as.character)
  # Take all columns MINUS the column on which to cluster stuff, and put
  # together its text. I did it by hand in a former version with apply but...
  # tidyr has a function for just that!
  for_nlp <- cp_df %>% unite("text", names(cp_df[-col_cluster]), 
                             sep = " ", na.rm = TRUE, remove = TRUE)
  # # Then group by the selected column ALL text from all other columns for that entry.
  for_nlp <- for_nlp %>% group_by_at(names(cp_df[col_cluster])) %>% summarise(text = paste(text, collapse = ' '))

  # Then use the TF-IDF trick already used here:
  # https://www.kaizen-r.com/2021/02/logs-classification-using-ml-2-2/
  review_corpus <- Corpus(VectorSource(for_nlp$text))
  review_corpus <- tm_map(review_corpus, content_transformer(tolower))
  ## There are more options, but we will NOT use them here.
  # review_corpus <- tm_map(review_corpus, removePunctuation)
  # review_corpus <- tm_map(review_corpus, stripWhitespace)
  
  #Let's have a look:
  # inspect(review_corpus[1])
  # inspect(review_corpus[101])
  
  ## OK so now we will work with the presence, or not, of words in a line:
  review_dtm <- DocumentTermMatrix(review_corpus)
  # # How many distinct "words" do we have?
  # dim(review_dtm)
  
  # remove sparse terms. These are rarely present:
  review_dtm <- removeSparseTerms(review_dtm, 0.99)
 
  # # What are the most frequent terms in each subset?
  # findFreqTerms(review_dtm[1:100,], 10)
  # findFreqTerms(review_dtm[101:196,], 10)
  # 
  # wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
  # # Not very interesting in and of itself. But who knows if more generally
  
  
  # But now instead of classifying as we did in the past... Try to cluster things

  # Another concept is the representation of one word compared to the number of
  # documents it appears in. A word can be unfrequent in one document, but
  # very common across documents, for example. Or the contrary.
  # Let's assign a value for that:
  
  # tf-idf: terms Frecuency vs inverse-document-frecuency
  review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
  review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.95)
  #review_dtm_tfidf
  
  # OK we still have the original log and the class for each entry.
  # Let's create a "reviews" object with the class and the tf-idf matrix
  mytfidf <- as.matrix(review_dtm_tfidf)

  # But before we go on, could WE find patterns visually in there?
  # Visualize difference between terms distributions:
  heatmap(mytfidf)
  
  # Let's call K_clus the result of KMeans
  K_clus <- kmeans(mytfidf, centers = n_cluster)
  
  return(merge(df, data.frame(for_nlp[names(cp_df[col_cluster])], cluster = K_clus$cluster), all.x = TRUE))
}

# Demo/Test:
col1 <- c("John Doe", "Jane Smith", "Peter Someone", "Nick", "Nick")
col2 <- letters[5:9]
col3 <- c("apple", "apple", "orange", "banana", "orange")
col4 <- c("car", "car", "boat", "boat", "plane")
col5 <- c("non-sports: siesta", "sports: swimming", "sport: swimming", "sport: swimming", "sport: running")
col6 <- c("Some text here", "Some text there", "but no text for our goal here,", "nor there", "Surely not Shakespeare")
test_df <- data.frame(col1, col2, col3, col4, col5, col6)

objs_clusters_from_df(test_df, 1, 3)

