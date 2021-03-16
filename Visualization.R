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

# Defining a threshold for co-occurence for the analysis
threshold <- 20

# Create a network dataset
network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

#Louvain Comunity Detection
cluster <- cluster_louvain(network)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

# Renaming the cluster labels
table <- cluster_df %>% group_by(V1) %>% 
  summarize(label = paste(sort(unique(label)),collapse=", "))

# Recode clusters
cluster_df$V1 <- recode(cluster_df$V1,
                        "1"="Geodaten",
                        "2"="Geodaten",
                        "3"="Boden",
                        "4"="Gesundheit und Soziales",
                        "5"="Demografie",
                        "6"="Wasser",
                        "7"="Temperatur",
                        "8"="Luft",
                        "9"="Geodaten",
                        "10"="Demografie",
                        "11"="Preise und Werte",
                        "12"="Preise und Werte",
                        "13"="Überflutung",
                        "14"="Geostatistik",
                        "15"="Geodaten",
                        "16"="Strom",
                        "17"="Geodaten",
                        "18"="Geodaten",
                        "19"="Geodaten",
                        "20"="Pflege",
                        "21"="Lärm und Luft Einrichtungen",
                        "22"="Lärm und Luft Einrichtungen",
                        "23"="Pflege",
                        "24"="Wasser",
                        "25"="Verletzungen",
                        "26"="Geodaten")

# Create group column
nodes <- left_join(nodes, cluster_df, by = "label")
nodes <- na.omit(nodes)

# Rename the column name
colnames(nodes)[3] <- "group"

# Create working dataset
edges_test <- edges[(edges$from %in% nodes$label),]
edges_test <- edges_test[(edges_test$to %in% nodes$label),]

# Save co-occurrences value
edges_test$width2 <- edges_test$width

# Make some modifications...
x <- edges_test %>%
  mutate(quantile = ntile(width, 17))

edges_test$width <- x$quantile

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

nodes$label <- capFirst(nodes$label)

nodes$id <- capFirst(nodes$id)

edges_test$from <- capFirst(edges_test$from)

edges_test$to <- capFirst(edges_test$to)

nodes$color <- palette()[edges$value]

nodes$node.size <- NA

all_words <- unlist( # flatten word list from individual strings into one vector
  regmatches(finalTags$text,  gregexpr('\\w+', finalTags$text))) # extract all words
# count frequencies
freq_count <- as.data.frame(table(all_words))
freq_count
summary(freq_count)

freq_count$all_words <- gsub("_"," ",freq_count$all_words)
freq_count$all_words <- tolower(freq_count$all_words)
freq_count$all_words <- capFirst(freq_count$all_words)

occDF <- freq_count

colnames(occDF)[1] <- "label"

extraDF <- left_join(nodes,occDF,by="label")

extraDF$Freq_bkt <- as.numeric(quantcut(extraDF$Freq, q=10, na.rm=TRUE))

# Define visuals
nodes$value <- (extraDF$Freq)

nodes$value_real <- extraDF$Freq
nodes$font.size <- (extraDF$Freq_bkt*15)
nodes$font.size[nodes$font.size<100 ] <- 100
nodes$strokeWidth <- 11

nodes$title = paste0("<div style='margin: -5px;  border: 10px solid white; background-color: #ffffff;pointer-events: none; height = 1000px, width=1000px'> <b style='color:#2f2fa5'> Tag: </b> ", nodes$label, " <br> <b style='color:#f44d74'> Häufigkeit: </b> ", nodes$value_real," Datensätze<br> <b style='color:#4dbdf0'> Kategorie: </b>", nodes$group," </div>")

nodes$color.background = "#f44d74"

# Attribute a color per category
nodes$color.background[nodes$group=="Boden"] <- "#5fd97a"
nodes$color.background[nodes$group=="Demografie"] <- "#f0ce71"
nodes$color.background[nodes$group=="Flur"] <- "#dfe376"
nodes$color.background[nodes$group=="Geodaten"] <- "#f2838e"
nodes$color.background[nodes$group=="Gesundheit und Soziales"] <- "#48b2d9"
nodes$color.background[nodes$group=="Lärm und Luft Einrichtungen"] <- "#93d466"
nodes$color.background[nodes$group=="Luft"] <- "#d590e8"
nodes$color.background[nodes$group=="Preise und Werte"] <- "#9fb5a0"
nodes$color.background[nodes$group=="Pflege"] <- "#cfb993"
nodes$color.background[nodes$group=="Strom"] <- "#82b9d9"
nodes$color.background[nodes$group=="Temperatur"] <- "#f76ac8"
nodes$color.background[nodes$group=="Verkehr"] <- "#3f8723"
nodes$color.background[nodes$group=="Verletzungen"] <- "#f09902"
nodes$color.background[nodes$group=="Wasser"] <- "#8ebae6"
nodes$color.background[nodes$group=="Überflutung"] <- "#b3b2b1"

nodes$color.border = "#f44d74"

nodes$color.border[nodes$group=="Boden"] <- "#048a22"
nodes$color.border[nodes$group=="Demografie"] <- "#fab700"
nodes$color.border[nodes$group=="Flur"] <- "#e2eb05"
nodes$color.border[nodes$group=="Geodaten"] <- "#c22d3b"
nodes$color.border[nodes$group=="Gesundheit und Soziales"] <- "#077fab"
nodes$color.border[nodes$group=="Lärm und Luft Einrichtungen"] <- "#4fba06"
nodes$color.border[nodes$group=="Luft"] <- "#c202f7"
nodes$color.border[nodes$group=="Preise und Werte"] <- "#537855"
nodes$color.border[nodes$group=="Pflege"] <- "#a3834b"
nodes$color.border[nodes$group=="Strom"] <- "#00a0fc"
nodes$color.border[nodes$group=="Temperatur"] <- "#a35589"
nodes$color.border[nodes$group=="Verkehr"] <- "#1a4709"
nodes$color.border[nodes$group=="Verletzungen"] <- "#9e6502"
nodes$color.border[nodes$group=="Wasser"] <- "#077df2"
nodes$color.border[nodes$group=="Überflutung"] <- "black"

nodes$color.highlight = "#2f2fa5"
nodes$color.highlight.border = "#2f2fa5"
nodes$color.hover.border = "#2f2fa5"
nodes$color.hover = "#2f2fa5"
edges_test$width <- edges_test$width*1.3
edges_test$title = paste0("<div style='margin: -5px;  border: 10px solid white; background-color: #ffffff;pointer-events: none; height = 1000px, width=1000px'> <b style='color:#2f2fa5'> Tag Co-Vorkommen: </b> ", edges_test$from, " & ", edges_test$to, " <br> <b style='color:#f44d74'> Häufigkeit: </b> ", edges_test$width2," Datensätze</div>")

words <- c("Advmis","Alb","Afs","Alkis","Alk","Atkis","Bimschg","Boris","Dlm","Dop","Gaa","Gsi","Hwrk","Lor","Nox","Slp")

for(i in 1:length(words)){
  
  eval(parse(text=paste0("nodes$id <- gsub('",words[i],"','",toupper(words[i]),"',nodes$id)")))
  eval(parse(text=paste0("nodes$label <- gsub('",words[i],"','",toupper(words[i]),"',nodes$label)")))
  eval(parse(text=paste0("edges_test$from <- gsub('",words[i],"','",toupper(words[i]),"',edges_test$from)")))
  eval(parse(text=paste0("edges_test$to <- gsub('",words[i],"','",toupper(words[i]),"',edges_test$to)")))
  
}

# Here is the actual graph:
visNetwork(nodes, edges_test,height = "1500px", width = "1500px") %>%
  visLayout(improvedLayout = T) %>%
  visNodes(
    scaling = list(min = 40, max = 150),
    shape = "dot",
    font=list(strokeWidth=11,face="verdana"),
    color = list(
      border = "#f44d74",
      highlight = "#2f2fa5",
      hover = "#2f2fa5"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -500, centralGravity =0.008,avoidOverlap=1,damping=2),
             stabilization = T,timestep=0.1) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#74cef7", highlight = "#908cca", hover = "#908cca")
  ) %>%
  visInteraction(zoomView=F) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = list(variable = "group", multiple = T, main = "Kategorie auswählen", style = 'width: 480px; height: 30px; background-color:white; font-size:110%;  font-family:verdana; color:#4dbdf0 ;font-weight: bold'),manipulation=F,
             nodesIdSelection = list(enabled = TRUE, main = "Tag auswählen", style = 'width: 480px; height: 30px; background-color:white; font-size:110%; font-family:verdana; color:#2f2fa5 ;font-weight: bold')) %>% 
  visLayout(randomSeed = 10)  %>%
  visInteraction(navigationButtons = TRUE,keyboard = TRUE) %>%
  addFontAwesome() 


