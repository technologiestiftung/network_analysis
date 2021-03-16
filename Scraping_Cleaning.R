# Loading the libraries
library(shiny)
library(visNetwork)
library(visNetwork)
library(geomnet)
library(igraph)
library(ckanr)
library(data.table) 
library(dplyr)
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(networkD3)
library(tibble)
library(dplyr)
library(gtools)

# Setting a seed
set.seed(123)

# Getting the API data
ckanr_setup(url = "https://datenregister.berlin.de/") 

# Saving the dataset
x1 <- package_search(rows=1000, as = 'table')
x2 <- package_search(rows=1000, start=1001, as = 'table')
x3 <- package_search(rows=1000, start=2001, as = 'table')

# Creating dataset
myData <- dplyr::bind_rows(x1$results,x2$results,x3$results)
rm(x1,x2,x3)

# Reading data from "tags" column and creating a data frame
allTags <- rbindlist(myData$tags, fill=TRUE)

# Sanity check
dim(allTags)[1]==sum(myData$num_tags) # Works!

# Attributing an ID by number of tags 
allTags$id_dataset <- NA
allTags$id_dataset <- rep(myData$id,myData$num_tags)

# Sanity check
length(rep(myData$id,myData$num_tags))==dim(allTags)[1] # Works!

# Attributing a dataset title by number of tags 
allTags$title_dataset <- NA
allTags$title_dataset <- rep(myData$title,myData$num_tags)

# Clean tags manually
allTags_clean <- allTags[(which(nchar(allTags$name) > 2)),]

allTags_clean$name <-gsub("\\:","",allTags_clean$name)
allTags_clean$name <-gsub("\\.","",allTags_clean$name)

allTags_clean$name <-gsub(" ","_",allTags_clean$name)
allTags_clean$name <-gsub("__","_",allTags_clean$name)
allTags_clean$name <-gsub("-","_",allTags_clean$name)

allTags_clean <- allTags_clean[(which(nchar(allTags_clean$name) > 2)),]
allTags_clean <- allTags_clean[(which(nchar(allTags_clean$name) < 25)),]

allTags_clean <- allTags_clean[allTags_clean$name!="Berlin",]

allTags_clean$name <-gsub("afs","AFS",allTags_clean$name)
allTags_clean$name <-gsub("alb","ALB",allTags_clean$name)
allTags_clean$name <-gsub("alk","ALK",allTags_clean$name)
allTags_clean$name <-gsub("alkis","ALKIS",allTags_clean$name)
allTags_clean$name <-gsub("atkis","ATKIS",allTags_clean$name)
allTags_clean$name <-gsub("bimschg","BIMSCHG",allTags_clean$name)
allTags_clean$name <-gsub("dlm","DLM",allTags_clean$name)
allTags_clean$name <-gsub("dop","DOP",allTags_clean$name)
allTags_clean$name <-gsub("gaa","GAA",allTags_clean$name)
allTags_clean$name <-gsub("gsi","GSI",allTags_clean$name)
allTags_clean$name <-gsub("hwrk","HWRK",allTags_clean$name)
allTags_clean$name <-gsub("lor","LOR",allTags_clean$name)
allTags_clean$name <-gsub("nox","NOX",allTags_clean$name)
allTags_clean$name <-gsub("slp","SLP",allTags_clean$name)


allTags_clean$name <-gsub("Afs","AFS",allTags_clean$name)
allTags_clean$name <-gsub("Alb","ALB",allTags_clean$name)
allTags_clean$name <-gsub("Alk","ALK",allTags_clean$name)
allTags_clean$name <-gsub("Alkis","ALKIS",allTags_clean$name)
allTags_clean$name <-gsub("Atkis","ATKIS",allTags_clean$name)
allTags_clean$name <-gsub("Bimschg","BIMSCHG",allTags_clean$name)
allTags_clean$name <-gsub("Dlm","DLM",allTags_clean$name)
allTags_clean$name <-gsub("Dop","DOP",allTags_clean$name)
allTags_clean$name <-gsub("Gaa","GAA",allTags_clean$name)
allTags_clean$name <-gsub("Gsi","GSI",allTags_clean$name)
allTags_clean$name <-gsub("Hwrk","HWRK",allTags_clean$name)
allTags_clean$name <-gsub("Lor","LOR",allTags_clean$name)
allTags_clean$name <-gsub("Nox","NOX",allTags_clean$name)
allTags_clean$name <-gsub("Slp","SLP",allTags_clean$name)

allTags_clean$name <-gsub("böden","boden",allTags_clean$name)
allTags_clean$name <-gsub("bodenfunktionskarte","bodenfunktion",allTags_clean$name)
allTags_clean$name <-gsub("bodenfunktionen","bodenfunktion",allTags_clean$name)
allTags_clean$name <-gsub("bodenrichtwertatlas","bodenrichtwert",allTags_clean$name)
allTags_clean$name <-gsub("hochwasserrisiken","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("hochwasserrisikokarten","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("hochwasserrisikokarte","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("stadtklimatisch","stadtklima",allTags_clean$name)
allTags_clean$name <-gsub("topografisch","topografie",allTags_clean$name)
allTags_clean$name <-gsub("topographie","topografie",allTags_clean$name)
allTags_clean$name <-gsub("topographisch","topografie",allTags_clean$name)
allTags_clean$name <-gsub("überflutungen","überflutung",allTags_clean$name)
allTags_clean$name <-gsub("bodenrichtwerte","bodenrichtwert",allTags_clean$name)

allTags_clean$name <-gsub("Böden","boden",allTags_clean$name)
allTags_clean$name <-gsub("Bodenfunktionskarte","bodenfunktion",allTags_clean$name)
allTags_clean$name <-gsub("Bodenfunktionen","bodenfunktion",allTags_clean$name)
allTags_clean$name <-gsub("Bodenrichtwertatlas","bodenrichtwert",allTags_clean$name)
allTags_clean$name <-gsub("Hochwasserrisiken","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("Hochwasserrisikokarten","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("Hochwasserrisikokarte","hochwasserrisiko",allTags_clean$name)
allTags_clean$name <-gsub("Stadtklimatisch","stadtklima",allTags_clean$name)
allTags_clean$name <-gsub("Topografisch","topografie",allTags_clean$name)
allTags_clean$name <-gsub("Topographie","topografie",allTags_clean$name)
allTags_clean$name <-gsub("Topographisch","topografie",allTags_clean$name)
allTags_clean$name <-gsub("Überflutungen","überflutung",allTags_clean$name)
allTags_clean$name <-gsub("Überflutung","überflutung",allTags_clean$name)
allTags_clean$name <-gsub("Bodenrichtwerte","bodenrichtwert",allTags_clean$name)

# Analyzing the network 
finalTags <- allTags_clean %>% 
  group_by(id_dataset) %>% arrange(name) %>%
  summarise(text  = paste(name, collapse =","), times = length(name))

# Make all tags lower case
finalTags$text <- tolower(finalTags$text)

# Remove any duplicate tags in the same dataset
temp <- sapply(finalTags$text, function(x) paste(unique(unlist(str_split(x,","))), collapse = ","))
temp <- as.data.frame(test)
finalTags$text <- temp$temp

# Convert data to tibble
raw.df <- as_tibble(finalTags)

# Convert , to white space
raw.df$text <- gsub(","," ",raw.df$text)

# Create bigram data frame
bi.gram.words <- raw.df %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) 

colnames(bi.gram.words)[1] <- "bigram"
colnames(bi.gram.words)[2] <- "times"

bi.gram.count <- bi.gram.words
colnames(bi.gram.count)[3] <- "weight"

bi.gram.count$word1 <- gsub("_"," ",bi.gram.count$word1)
bi.gram.count$word2 <- gsub("_"," ",bi.gram.count$word2)

# Remove stopwords
library(stopwords)

stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)

# Format the content of the dataset
bi.gram.count <- bi.gram.count[!(tolower(bi.gram.count$word1) %in% stop_german$word),]
bi.gram.count <- bi.gram.count[!(tolower(bi.gram.count$word2) %in% stop_german$word),]

# Remove self-cooccurence
bi.gram.count <- bi.gram.count[bi.gram.count$word1!=bi.gram.count$word2,]

# For visualization purposes we scale by a global factor 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

# Create nodes dataset
nodes <- data.frame(id=sort(unique(bi.gram.count$word1)),
                    label=sort(unique(bi.gram.count$word1)))

# Create edges dataset
edges <- bi.gram.count
colnames(edges) <- c("from", "to", "width")

