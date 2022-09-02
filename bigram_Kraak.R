library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(stringr)
library(widyr)

setwd("D:/CARTO_4_Thesis/converted_txt_books")
input_text <- read.delim("./Kraak_4.txt", header = FALSE)
input_text <- as_tibble(input_text)

Kraak_bigrams <- input_text %>%
  unnest_tokens(bigram, V1, token = "ngrams", n = 2)

# Cut them into bigrams
Kraak_bigrams %>%
  count(bigram, sort = TRUE)

# Define stop_words that are meaningless
data(stop_words)
custom_stop_words <- tibble(
  word = c(
    "figure",
    "000",
    "e.g",
    "chapter",
    "chapters",
    "section",
    "sections",
    "plate",
    "plates",
    "based",
    c(0:99)
  ),
  lexicon = "CUSTOM"
)
stop_words_combined <- bind_rows(stop_words, custom_stop_words)

# Remove cases where either word is a stop-word
bigrams_separated <- Kraak_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove certain plural forms
bigrams_separated_stemmed <- bigrams_separated %>%
  mutate(word1 = case_when(word1 == "maps" ~ "map"
                          ,word1 == "exploratory" ~ "exploration"
                          ,word1 == "values" ~ "value"
                          ,word1 == "symbols" ~ "symbol"
                          ,word1 == "scales" ~ "scale"
                          ,word1 == "methods" ~ "method"
                          ,word1 == "projections" ~ "projection"
                          ,word1 == "types" ~ "type"
                          ,word1 == "approaches" ~ "approach"
                          ,word1 == "classes" ~ "class"
                          ,word1 == "lines" ~ "line"
                          ,word1 == "attributes" ~ "attribute"
                          ,word1 == "variables" ~ "variable"
                          ,word1 == "units" ~ "unit"
                          ,word1 == "elements" ~ "element"
                          ,word1 == "schemes" ~ "scheme"
                          ,word1 == "sets" ~ "set"
                          ,word1 == "devices" ~ "device"
                          ,word1 == "atlases" ~ "atlas"
                          ,word1 == "environments" ~ "environment"
                          ,word1 == "files" ~ "file"
                          ,word1 == "packages" ~ "package"
                          ,word1 == "applications" ~ "application"
                          ,word1 == "interfaces" ~ "interface"
                          ,word1 == "encyclopedias" ~ "encyclopedia"
                          ,word1 == "digitally" ~ "digital"
                          ,word1 == "readers" ~ "reader"
                          ,word1 == "patterns" ~ "pattern"
                          ,word1 == "sounds" ~ "sound"
                          ,word1 == "giss" ~ "gis"
                          ,word1 == "virtually" ~ "virtual"
                          ,word1 == "differences" ~ "difference"
                          ,word1 == "objects" ~ "object"
                          ,word1 == "analyses" ~ "analysis"
                          ,word1 == "sizes" ~ "size"
                          ,word1 == "systems" ~ "system"
                          ,word1 == "databases" ~ "database"
                          ,word1 == "packages" ~ "package"
                          ,word1 == "screens" ~ "screen"
                          ,word1 == "images" ~ "image"
                          ,word1 == "locations" ~ "location"
                          ,word1 == "cartographical" ~ "cartographic"
                          ,word1 == "geographical" ~ "geographic"
                          ,word1 == "graphical" ~ "graphic"
                          ,word1 == "coordinates" ~ "coordinate"
                          ,word1 == "monitors" ~ "monitor"
                          ,word1 == "terrains" ~ "terrain"
                          ,word1 == "landscapes" ~ "landscape"
                          ,word1 == "boundaries" ~ "boundary"
                          ,word1 %in% c("digitizing", "digitized") ~ "digitize"
                          ,word1 %in% c("colors", "colored") ~ "color"
                          ,word1 %in% c("colours", "coloured") ~ "colour"
                          ,word1 %in% c("user’s", "user's", "users") ~ "user"
                          ,word1 %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word1 %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word1 %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word1 %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word1 %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word1 %in% c("includes", "including", "included") ~ "include"
                          ,word1 %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word1 %in% c("developed", "develops", "developing") ~ "develop"
                          ,word1 %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word1 %in% c("computes", "computed") ~ "compute"
                          ,word1 %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word1)) %>%
  mutate(word2 = case_when(word2 == "maps" ~ "map"
                          ,word2 == "exploratory" ~ "exploration"
                          ,word2 == "values" ~ "value"
                          ,word2 == "symbols" ~ "symbol"
                          ,word2 == "scales" ~ "scale"
                          ,word2 == "methods" ~ "method"
                          ,word2 == "projections" ~ "projection"
                          ,word2 == "types" ~ "type"
                          ,word2 == "approaches" ~ "approach"
                          ,word2 == "classes" ~ "class"
                          ,word2 == "lines" ~ "line"
                          ,word2 == "attributes" ~ "attribute"
                          ,word2 == "variables" ~ "variable"
                          ,word2 == "units" ~ "unit"
                          ,word2 == "elements" ~ "element"
                          ,word2 == "schemes" ~ "scheme"
                          ,word2 == "sets" ~ "set"
                          ,word2 == "devices" ~ "device"
                          ,word2 == "atlases" ~ "atlas"
                          ,word2 == "environments" ~ "environment"
                          ,word2 == "files" ~ "file"
                          ,word2 == "packages" ~ "package"
                          ,word2 == "applications" ~ "application"
                          ,word2 == "interfaces" ~ "interface"
                          ,word2 == "encyclopedias" ~ "encyclopedia"
                          ,word2 == "digitally" ~ "digital"
                          ,word2 == "readers" ~ "reader"
                          ,word2 == "patterns" ~ "pattern"
                          ,word2 == "sounds" ~ "sound"
                          ,word2 == "giss" ~ "gis"
                          ,word2 == "virtually" ~ "virtual"
                          ,word2 == "differences" ~ "difference"
                          ,word2 == "objects" ~ "object"
                          ,word2 == "analyses" ~ "analysis"
                          ,word2 == "sizes" ~ "size"
                          ,word2 == "systems" ~ "system"
                          ,word2 == "databases" ~ "database"
                          ,word2 == "packages" ~ "package"
                          ,word2 == "screens" ~ "screen"
                          ,word2 == "images" ~ "image"
                          ,word2 == "locations" ~ "location"
                          ,word2 == "cartographical" ~ "cartographic"
                          ,word2 == "geographical" ~ "geographic"
                          ,word2 == "graphical" ~ "graphic"
                          ,word2 == "coordinates" ~ "coordinate"
                          ,word2 == "monitors" ~ "monitor"
                          ,word2 == "terrains" ~ "terrain"
                          ,word2 == "landscapes" ~ "landscape"
                          ,word2 == "boundaries" ~ "boundary"
                          ,word2 %in% c("digitizing", "digitized") ~ "digitize"
                          ,word2 %in% c("colors", "colored") ~ "color"
                          ,word2 %in% c("colours", "coloured") ~ "colour"
                          ,word2 %in% c("user’s", "user's", "users") ~ "user"
                          ,word2 %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word2 %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word2 %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word2 %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word2 %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word2 %in% c("includes", "including", "included") ~ "include"
                          ,word2 %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word2 %in% c("developed", "develops", "developing") ~ "develop"
                          ,word2 %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word2 %in% c("computes", "computed") ~ "compute"
                          ,word2 %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word2))
                          

  
bigrams_filtered <- bigrams_separated_stemmed %>%
  filter(!word1 %in% stop_words_combined$word) %>%
  filter(!word2 %in% stop_words_combined$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# Only do it for the digital keywords
# Merge all the digital keywords to create a single vector
digital_keywords_union <- union(digital_keywords1, digital_keywords2)
digital_keywords_union <- union(digital_keywords_union, digital_keywords3)
digital_keywords_union <- union(digital_keywords_union, digital_keywords4)

# Filter so that either word1 or word2 contains the digital keyword
bigram_counts_filtered1 <- bigram_counts %>%
  filter(word1 %in% digital_keywords_union)
bigram_counts_filtered2 <- bigram_counts %>%
  filter(word2 %in% digital_keywords_union)
bigram_counts_union <- union_all(bigram_counts_filtered1, bigram_counts_filtered2)
bigram_counts_union <- distinct(bigram_counts_union)



# [One Edition] Convert to a graph
bigram_graph_digital <- bigram_counts_union %>%
  filter(n > 9) %>%
  graph_from_data_frame()

# [One Edition] Generate a network graph
ggraph(bigram_graph_digital, layout = "fr") +
  geom_edge_fan(aes(alpha = n,
                    start_cap = label_rect(node1.name, fontsize=8), end_cap = label_rect(node2.name, fontsize=8)), show.legend = TRUE,
                arrow = arrow(length = unit(1, "mm")), strength = 2) +
  geom_node_text(aes(label = name), size = 3) +
  theme_graph(background = "white")

ggsave("test1.png", width = 20, height = 15, units = "cm", dpi = 300)




# [All Editions] Insert edition as a new column
bigram_counts_union4 <- bigram_counts_union %>%
  mutate(edition = "4th ed.")

bigram_counts_combined <- union_all(bigram_counts_union1, bigram_counts_union2)
bigram_counts_combined <- union_all(bigram_counts_combined, bigram_counts_union3)
bigram_counts_combined <- union_all(bigram_counts_combined, bigram_counts_union4)

# [All Editions] Convert to a graph
bigram_graph_digital <- bigram_counts_combined %>%
  filter(n > 9) %>%
  graph_from_data_frame()

# [All Editions] Generate a network graph
ggraph(bigram_graph_digital, layout = "fr") +
  geom_edge_fan(aes(alpha = n, color = edition,
                     start_cap = label_rect(node1.name, fontsize=8), end_cap = label_rect(node2.name, fontsize=8)),
                 arrow = arrow(length = unit(1, "mm")), strength = 2) +
  geom_node_text(aes(label = name), size = 3) +
  theme_graph(background = "white")


ggsave("test1.png", width = 20, height = 15, units = "cm", dpi = 300)
















#ggraph(bigram_graph_digital, layout = "stress") +
#geom_edge_link(aes(alpha = n), color = "gray", arrow = arrow(length = unit(4, "mm")), end_cap = circle(3, "mm")) +
#geom_node_point(size = 5) +
#geom_node_text(aes(label = name), size = 5, nudge_x = 0.05, nudge_y = 0.05, repel = TRUE) +
#theme_graph()




# which words are the most likely to come after these keywords?
#digital_words_front <- bigrams_filtered %>%
  #filter(word1 %in% digital_words_list1) %>%
  #count(word1, word2, sort = TRUE)

# only select digital-related keywords
#digital_words_list2 <- c("map", "data", "software", "computer")

# Which words are the most likely to come before these keywords?
#digital_words_back <- bigrams_filtered %>%
  #filter(word2 %in% digital_words_list2) %>%
  #count(word1, word2, sort = TRUE)


