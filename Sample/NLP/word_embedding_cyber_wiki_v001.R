library(WikipediR) # Get Wiki data
library(text2vec) # NLP stuff
library(udpipe) # NLP stuff
library(word2vec) # NLP stuff (embeddings)
library(tm)

## A manual function, to explain "traditional ML" is not magical
clean_text_set <- function(t_input) {
    ## Input is Wikipedia "clean text":
    t_input <- tolower(t_input$text$`*`)
    ## Put it in vector:
    t_input <- strsplit(t_input, "\n")[[1]]
    
    ## HTML stuff needs cleaning...
    strings_to_replace <- c("\\[", "\\]", "\\{", "\\}", "'", "\\:", "\\&",
                            "\\#", ";", "http(s)?:[\\/a-z\\.]+", "<\\!--.*?>",
                            "<.*?>", "\\.[0-9a-z\\:\\-]+", " [a-z]{1,2}(\\,)? ",
                            " [0-9]+", " div ", " ifont", " dt", "  ")
    for(i in 1:length(strings_to_replace))
        t_input <- gsub(strings_to_replace[i], " ", t_input)
    
    ## Remove english "Stopwords":
    t_input <- sapply(t_input, function(x) {
        t_str <- strsplit(x, " ", fixed=TRUE)
        vapply(t_str, function(y) paste(y[!tolower(y) %in% tm::stopwords("english")], collapse = " "), character(1))
    })
    
    ## Duplicates are useless
    t_input <- unique(t_input)
    t_input <- t_input[nchar(t_input) > 4] ## Too small a word? Out.
    
    t_input
}

## Simple wrapper
my_page_content <- function(keywords) {
    page_content(language = "en",
                 project = "wikipedia",
                 page_name = keywords,
                 as_wikitext = FALSE,
                 clean_response = TRUE) |>
        clean_text_set()
}

## Explicitly for explanation:
firewall_wiki <- my_page_content("firewall (computing)")
firewall_wiki <- firewall_wiki[1:82]
switch_wiki <- my_page_content("network switch")
switch_wiki <- switch_wiki[2:96]
router_wiki <- my_page_content("router (computing)")
router_wiki <- router_wiki[1:65]
virus_wiki <- my_page_content("computer virus")
virus_wiki <- virus_wiki[1:89]
hacker_wiki <- my_page_content("hacker")
hacker_wiki <- hacker_wiki[1:116]
computer_wiki <- my_page_content("computer")
computer_wiki <- computer_wiki[1:101]
network_card_wiki<- my_page_content("network interface controller")
network_card_wiki <- network_card_wiki[1:56]
cpu_wiki <- my_page_content("central processing unit")
cpu_wiki <- cpu_wiki[1:144]


## From word2vec doc:
model <- word2vec(firewall_wiki, type="cbow", dim=15, iter = 20)
embeddings <- as.matrix(model)
embeddings

library(uwot)
viz <- umap(embeddings, n_neighbors = 15, n_threads = 2)

## Static plot
library(ggplot2)
library(ggrepel)
df  <- data.frame(word = gsub("//.+", "", rownames(embeddings)), 
                  xpos = gsub(".+//", "", rownames(embeddings)), 
                  x = viz[, 1], y = viz[, 2], 
                  stringsAsFactors = FALSE)
##df  <- subset(df, xpos %in% c("JJ"))
ggplot(df, aes(x = x, y = y, label = word)) + 
    geom_text_repel() + theme_void() + 
    labs(title = "word2vec - adjectives in 2D using UMAP")



## Lets' move on to something more... Substantial:
full_text <- c(
    switch_wiki,
    router_wiki,
    firewall_wiki,
    hacker_wiki,
    computer_wiki,
    cpu_wiki,
    virus_wiki
)

## Again:
model <- word2vec(full_text, type="cbow", dim=30, iter = 50)
embeddings <- as.matrix(model)
embeddings

lookslike <- predict(model,
                     c("virus", "firewall"), 
                     type = "nearest", 
                     top_n = 5)
lookslike

viz <- umap(embeddings, n_neighbors = 15, n_threads = 4)
df  <- data.frame(word = gsub("//.+", "", rownames(embeddings)), 
                  xpos = gsub(".+//", "", rownames(embeddings)), 
                  x = viz[, 1], y = viz[, 2], 
                  stringsAsFactors = FALSE)

## Interactive plot
library(plotly)
plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word)

df[df$word == 'firewall',]
df[df$word == 'virus',]

## Let's add some of what we learnt about DBScan clustering and 3D visu.

library(dbscan) # Clustering (distances, i.e. "sub-symbolic" stuff)
library(rgl) # For 3D Visus & Fun
kNNdistplot(viz, minPts = 3)
abline(h=0.2, col= "red", lty = 2) ## Noise at .7 for 4-NN distance
## Step 1: Clustering using "KNN"
res <- dbscan(viz, eps = 0.25, minPts = 3)
plot(viz, col = res$cluster)
hullplot(viz, res)


plot_ly(df,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = 'text',
        color = as.factor(res$cluster),
        text = ~word) |>
    layout(font = list(color = res$cluster))


library(cluster)
mds_wiki <- cmdscale(dist(embeddings), k = 3)
library(rgl) # For 3D Visus & Fun
plot3d(mds_wiki, type = 's', size = 2, col=res$cluster+1L)
movie3d( spin3d(), duration = 10, fps = 40 )

