# Working on "Parts of Speech" tagging
# Note: Heavily documented code for demo purposes...
library(dplyr) # for select()
library(NLP)
library(openNLP)

# We get the data using code similar to that here:
# https://github.com/kaizen-R/R/blob/master/Sample/NLP/scraping_our_blog_v001.R
# Let's assume we saved it into: "all_articles_df_vfuture"

# The first (last published) article content (text) is a list of char strings:
# let's make it explicit:
t_strings <- as.String(all_articles_df_vfuture$article_content[1])

# Let's create a sentence annotator.
sent_annotator <- Maxent_Sent_Token_Annotator(language = "en", 
                                              probs = TRUE, model = NULL)
# Now let's annotate the sentences in our latest article:
annotated_sentences <- annotate(t_strings, sent_annotator)

# And let's have a look:
t_strings[annotated_sentences]
# Works pretty nicely!
# It clearly does look for more than "simply" punctuation and spaces...

# Can we go further and annotate Words and their "POS" tag?

# We'll focus on the first few "sentences" for simplicity:
t_strings <- t_strings[annotated_sentences[1:3]]
t_strings <- as.String(t_strings)
annotated_sentences <- annotate(t_strings, sent_annotator)
t_strings[annotated_sentences]



# Let's first extract words, from annotated sentences.
word_annotator <- Maxent_Word_Token_Annotator(language = "en",
                                              probs = TRUE, model = NULL)
annotated_words <- annotate(t_strings, word_annotator, annotated_sentences)
t_strings[annotated_words]

pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en", 
                                              probs = TRUE, model = NULL)
#pos_tag_annotator

pos_tagged_sentences <- annotate(t_strings, pos_tag_annotator, annotated_words)
head(pos_tagged_sentences, n = 30)
# One thing you can do then is extract a "sentence" using positions:
sentence1 <- t_strings[1, 108]

# Let's focus on the words only and for the first sentence only:
pos_tagged_words_sent1 <- pos_tagged_sentences %>%
  subset(type == "word") %>%
  subset(end <= 108) # 108 was the end of the first sentence, so it makes sense.

# Now we have all words limits for the first string:
sentence1[pos_tagged_words_sent1]

# Let's make it more readable now
# For reference: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
pos_simple_explainer <- data.frame(
  tag = c("CC", "CD", "JJ", "NN", "NNS", "NNP", "VB", "VBD", "IN", "DT", "RB"),
  desc = c("Coordinating Conjunction",
           "Cardinal Number",
           "Adjective",
           "Noun, singular or mass",
           "Noun, plural",
           "Noun, proper",
           "Verb, base form",
           "Verb, past tense",
           "Preposition or subordinating conjunction",
           "Determiner", "Adverb")
)

word_types_vector <- sapply(pos_tagged_words_sent1$features, function(x) {
  x$POS[[1]]
})

# Finally, let's have a look at the tagged POS:
merge(data.frame(word = sentence1[pos_tagged_words_sent1],
           type = word_types_vector),
      pos_simple_explainer,
      by.x = "type",
      by.y = "tag",
      all.x = TRUE)
