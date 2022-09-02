library(dplyr)
library(tidytext)
library(tibble)
library(stringr)
library(ggplot2)
library(forcats)


setwd("D:/CARTO_4_Thesis/converted_txt_books")

input_text1 <- read.delim("./Kraak_1.txt", header = FALSE)
input_text1 <- as_tibble(input_text1)

# Insert chapter numbers next to each row
input_text1 <- input_text1 %>%
  mutate(chapter = cumsum(str_detect(V1, regex("^CHAPTER [\\divxlc]"))))

# Tokenize words so that each row represents a word
# and add the chapter name as a new column
text_tokenized1 <- input_text1 %>%
  unnest_tokens(word, V1)

# Remove stop_words that are meaningless
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

text_tokenized_cleaned1 <- text_tokenized1 %>%
  anti_join(stop_words_combined)

# Remove certain plural forms
text_tokenized_cleaned_stemmed1 <- text_tokenized_cleaned1 %>%
  mutate(word = case_when(word == "maps" ~ "map"
                          ,word == "exploratory" ~ "exploration"
                          ,word == "values" ~ "value"
                          ,word == "symbols" ~ "symbol"
                          ,word == "scales" ~ "scale"
                          ,word == "methods" ~ "method"
                          ,word == "projections" ~ "projection"
                          ,word == "types" ~ "type"
                          ,word == "approaches" ~ "approach"
                          ,word == "classes" ~ "class"
                          ,word == "lines" ~ "line"
                          ,word == "attributes" ~ "attribute"
                          ,word == "variables" ~ "variable"
                          ,word == "units" ~ "unit"
                          ,word == "elements" ~ "element"
                          ,word == "schemes" ~ "scheme"
                          ,word == "sets" ~ "set"
                          ,word == "devices" ~ "device"
                          ,word == "atlases" ~ "atlas"
                          ,word == "environments" ~ "environment"
                          ,word == "files" ~ "file"
                          ,word == "packages" ~ "package"
                          ,word == "applications" ~ "application"
                          ,word == "interfaces" ~ "interface"
                          ,word == "encyclopedias" ~ "encyclopedia"
                          ,word == "digitally" ~ "digital"
                          ,word == "readers" ~ "reader"
                          ,word == "patterns" ~ "pattern"
                          ,word == "sounds" ~ "sound"
                          ,word == "giss" ~ "gis"
                          ,word == "virtually" ~ "virtual"
                          ,word == "differences" ~ "difference"
                          ,word == "objects" ~ "object"
                          ,word == "analyses" ~ "analysis"
                          ,word == "sizes" ~ "size"
                          ,word == "systems" ~ "system"
                          ,word == "databases" ~ "database"
                          ,word == "packages" ~ "package"
                          ,word == "screens" ~ "screen"
                          ,word == "images" ~ "image"
                          ,word == "locations" ~ "location"
                          ,word == "cartographical" ~ "cartographic"
                          ,word == "geographical" ~ "geographic"
                          ,word == "graphical" ~ "graphic"
                          ,word == "coordinates" ~ "coordinate"
                          ,word == "monitors" ~ "monitor"
                          ,word == "terrains" ~ "terrain"
                          ,word == "landscapes" ~ "landscape"
                          ,word == "boundaries" ~ "boundary"
                          ,word %in% c("digitizing", "digitized") ~ "digitize"
                          ,word %in% c("colors", "colored") ~ "color"
                          ,word %in% c("colours", "coloured") ~ "colour"
                          ,word %in% c("user’s", "user's", "users") ~ "user"
                          ,word %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word %in% c("includes", "including", "included") ~ "include"
                          ,word %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word %in% c("developed", "develops", "developing") ~ "develop"
                          ,word %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word %in% c("computes", "computed") ~ "compute"
                          ,word %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word))

# Count the occurrence of words
word_count1 <- text_tokenized_cleaned_stemmed1 %>%
  count(word, sort = TRUE)

# Plot the occurrence of words
word_count1 %>%
  mutate(word = fct_reorder(word, n)) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 2000) +
  geom_point(size=1, color="orange") +
  ggtitle("Top 15 Words, Kraak_1")

# Plot only top 15 digital-related keywords
digital_keywords1 <- c("gis", "digital", "user", "file", "system", "database", "animation", "package", "software", 
                       "digitize", "printing", "electronic", "computer", "interactive", "screen")

digital_word_count1 <- filter(word_count1, word %in% digital_keywords1)
digital_word_count1 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#85c1e9", alpha=.6, width=.4) +
  geom_text(aes(label = n), hjust = 1.2, vjust = 0.4, color = "#366EE7", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 200) +
  ggtitle("1st ed.")



input_text2 <- read.delim("./Kraak_2.txt", header = FALSE)
input_text2 <- as_tibble(input_text2)

# Insert chapter numbers next to each row
input_text2 <- input_text2 %>%
  mutate(chapter = cumsum(str_detect(V1, regex("^CHAPTER [\\divxlc]"))))

# Tokenize words so that each row represents a word
# and add the chapter name as a new column
text_tokenized2 <- input_text2 %>%
  unnest_tokens(word, V1)

text_tokenized_cleaned2 <- text_tokenized2 %>%
  anti_join(stop_words_combined)

# Remove certain plural forms
text_tokenized_cleaned_stemmed2 <- text_tokenized_cleaned2 %>%
  mutate(word = case_when(word == "maps" ~ "map"
                          ,word == "exploratory" ~ "exploration"
                          ,word == "values" ~ "value"
                          ,word == "symbols" ~ "symbol"
                          ,word == "scales" ~ "scale"
                          ,word == "methods" ~ "method"
                          ,word == "projections" ~ "projection"
                          ,word == "types" ~ "type"
                          ,word == "approaches" ~ "approach"
                          ,word == "classes" ~ "class"
                          ,word == "lines" ~ "line"
                          ,word == "attributes" ~ "attribute"
                          ,word == "variables" ~ "variable"
                          ,word == "units" ~ "unit"
                          ,word == "elements" ~ "element"
                          ,word == "schemes" ~ "scheme"
                          ,word == "sets" ~ "set"
                          ,word == "devices" ~ "device"
                          ,word == "atlases" ~ "atlas"
                          ,word == "environments" ~ "environment"
                          ,word == "files" ~ "file"
                          ,word == "packages" ~ "package"
                          ,word == "applications" ~ "application"
                          ,word == "interfaces" ~ "interface"
                          ,word == "encyclopedias" ~ "encyclopedia"
                          ,word == "digitally" ~ "digital"
                          ,word == "readers" ~ "reader"
                          ,word == "patterns" ~ "pattern"
                          ,word == "sounds" ~ "sound"
                          ,word == "giss" ~ "gis"
                          ,word == "virtually" ~ "virtual"
                          ,word == "differences" ~ "difference"
                          ,word == "objects" ~ "object"
                          ,word == "analyses" ~ "analysis"
                          ,word == "sizes" ~ "size"
                          ,word == "systems" ~ "system"
                          ,word == "databases" ~ "database"
                          ,word == "packages" ~ "package"
                          ,word == "images" ~ "image"
                          ,word == "locations" ~ "location"
                          ,word == "cartographical" ~ "cartographic"
                          ,word == "geographical" ~ "geographic"
                          ,word == "graphical" ~ "graphic"
                          ,word == "coordinates" ~ "coordinate"
                          ,word == "monitors" ~ "monitor"
                          ,word == "terrains" ~ "terrain"
                          ,word == "landscapes" ~ "landscape"
                          ,word == "boundaries" ~ "boundary"
                          ,word %in% c("digitizing", "digitized") ~ "digitize"
                          ,word %in% c("colors", "colored") ~ "color"
                          ,word %in% c("colours", "coloured") ~ "colour"
                          ,word %in% c("user’s", "user's", "users") ~ "user"
                          ,word %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word %in% c("includes", "including", "included") ~ "include"
                          ,word %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word %in% c("developed", "develops", "developing") ~ "develop"
                          ,word %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word %in% c("computes", "computed") ~ "compute"
                          ,word %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word))

# Count the occurrence of words
word_count2 <- text_tokenized_cleaned_stemmed2 %>%
  count(word, sort = TRUE)

# Plot the occurrence of words
word_count2 %>%
  mutate(word = fct_reorder(word, n)) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 2000) +
  geom_point(size=1, color="orange") +
  ggtitle("Top 15 Words, Kraak_2")

# Plot only top 15 digital-related keywords
digital_keywords2 <- c("gis", "user", "system", "file", "digital", "database", "animation", "interactive", "web", 
                       "digitize", "package", "electronic", "software", "screen", "printing")
digital_word_count2 <- filter(word_count2, word %in% digital_keywords2)
digital_word_count2 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#85c1e9", alpha=.6, width=.4) +
  geom_text(aes(label = n), hjust = 1.2, vjust = 0.4, color = "#366EE7", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 200) +
  ggtitle("2nd ed.")



input_text3 <- read.delim("./Kraak_3.txt", header = FALSE)
input_text3 <- as_tibble(input_text3)

# Insert chapter numbers next to each row
input_text3 <- input_text3 %>%
  mutate(chapter = cumsum(str_detect(V1, regex("^CHAPTER [\\divxlc]"))))

# Tokenize words so that each row represents a word
# and add the chapter name as a new column
text_tokenized3 <- input_text3 %>%
  unnest_tokens(word, V1)

text_tokenized_cleaned3 <- text_tokenized3 %>%
  anti_join(stop_words_combined)

# Remove certain plural forms
text_tokenized_cleaned_stemmed3 <- text_tokenized_cleaned3 %>%
  mutate(word = case_when(word == "maps" ~ "map"
                          ,word == "exploratory" ~ "exploration"
                          ,word == "values" ~ "value"
                          ,word == "symbols" ~ "symbol"
                          ,word == "scales" ~ "scale"
                          ,word == "methods" ~ "method"
                          ,word == "projections" ~ "projection"
                          ,word == "types" ~ "type"
                          ,word == "approaches" ~ "approach"
                          ,word == "classes" ~ "class"
                          ,word == "lines" ~ "line"
                          ,word == "attributes" ~ "attribute"
                          ,word == "variables" ~ "variable"
                          ,word == "units" ~ "unit"
                          ,word == "elements" ~ "element"
                          ,word == "schemes" ~ "scheme"
                          ,word == "sets" ~ "set"
                          ,word == "devices" ~ "device"
                          ,word == "atlases" ~ "atlas"
                          ,word == "environments" ~ "environment"
                          ,word == "files" ~ "file"
                          ,word == "packages" ~ "package"
                          ,word == "applications" ~ "application"
                          ,word == "interfaces" ~ "interface"
                          ,word == "encyclopedias" ~ "encyclopedia"
                          ,word == "digitally" ~ "digital"
                          ,word == "readers" ~ "reader"
                          ,word == "patterns" ~ "pattern"
                          ,word == "sounds" ~ "sound"
                          ,word == "giss" ~ "gis"
                          ,word == "virtually" ~ "virtual"
                          ,word == "differences" ~ "difference"
                          ,word == "objects" ~ "object"
                          ,word == "analyses" ~ "analysis"
                          ,word == "sizes" ~ "size"
                          ,word == "systems" ~ "system"
                          ,word == "databases" ~ "database"
                          ,word == "packages" ~ "package"
                          ,word == "images" ~ "image"
                          ,word == "locations" ~ "location"
                          ,word == "cartographical" ~ "cartographic"
                          ,word == "geographical" ~ "geographic"
                          ,word == "graphical" ~ "graphic"
                          ,word == "coordinates" ~ "coordinate"
                          ,word == "monitors" ~ "monitor"
                          ,word == "terrains" ~ "terrain"
                          ,word == "landscapes" ~ "landscape"
                          ,word == "boundaries" ~ "boundary"
                          ,word %in% c("digitizing", "digitized") ~ "digitize"
                          ,word %in% c("colors", "colored") ~ "color"
                          ,word %in% c("colours", "coloured") ~ "colour"
                          ,word %in% c("user’s", "user's", "users") ~ "user"
                          ,word %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word %in% c("includes", "including", "included") ~ "include"
                          ,word %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word %in% c("developed", "develops", "developing") ~ "develop"
                          ,word %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word %in% c("computes", "computed") ~ "compute"
                          ,word %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word))


# Count the occurrence of words
word_count3 <- text_tokenized_cleaned_stemmed3 %>%
  count(word, sort = TRUE)

# Plot the occurrence of words
word_count3 %>%
  mutate(word = fct_reorder(word, n)) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  geom_point(size=1, color="orange") +
  ylim(0, 2000) +
  ggtitle("Top 15 Words, Kraak_3")

# Plot only top 15 digital-related keywords
digital_keywords3 <- c("gis", "user", "file", "system", "digital", "database", "animation", "web", "package", "screen", 
                       "interactive", "software", "printing", "digitize", "electronic")
digital_word_count3 <- filter(word_count3, word %in% digital_keywords3)
digital_word_count3 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#85c1e9", alpha=.6, width=.4) +
  geom_text(aes(label = n), hjust = 1.2, vjust = 0.4, color = "#366EE7", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 200) +
  ggtitle("3rd ed.")



input_text4 <- read.delim("./Kraak_4.txt", header = FALSE)
input_text4 <- as_tibble(input_text4)

# Insert chapter numbers next to each row
input_text4 <- input_text4 %>%
  mutate(chapter = cumsum(str_detect(V1, regex("^CHAPTER [\\divxlc]"))))

# Tokenize words so that each row represents a word
# and add the chapter name as a new column
text_tokenized4 <- input_text4 %>%
  unnest_tokens(word, V1)

text_tokenized_cleaned4 <- text_tokenized4 %>%
  anti_join(stop_words_combined)

# Remove certain plural forms
text_tokenized_cleaned_stemmed4 <- text_tokenized_cleaned4 %>%
  mutate(word = case_when(word == "maps" ~ "map"
                          ,word == "exploratory" ~ "exploration"
                          ,word == "values" ~ "value"
                          ,word == "symbols" ~ "symbol"
                          ,word == "scales" ~ "scale"
                          ,word == "methods" ~ "method"
                          ,word == "projections" ~ "projection"
                          ,word == "types" ~ "type"
                          ,word == "approaches" ~ "approach"
                          ,word == "classes" ~ "class"
                          ,word == "lines" ~ "line"
                          ,word == "attributes" ~ "attribute"
                          ,word == "variables" ~ "variable"
                          ,word == "units" ~ "unit"
                          ,word == "elements" ~ "element"
                          ,word == "schemes" ~ "scheme"
                          ,word == "sets" ~ "set"
                          ,word == "devices" ~ "device"
                          ,word == "atlases" ~ "atlas"
                          ,word == "environments" ~ "environment"
                          ,word == "files" ~ "file"
                          ,word == "packages" ~ "package"
                          ,word == "applications" ~ "application"
                          ,word == "interfaces" ~ "interface"
                          ,word == "encyclopedias" ~ "encyclopedia"
                          ,word == "digitally" ~ "digital"
                          ,word == "readers" ~ "reader"
                          ,word == "patterns" ~ "pattern"
                          ,word == "sounds" ~ "sound"
                          ,word == "giss" ~ "gis"
                          ,word == "virtually" ~ "virtual"
                          ,word == "differences" ~ "difference"
                          ,word == "objects" ~ "object"
                          ,word == "analyses" ~ "analysis"
                          ,word == "sizes" ~ "size"
                          ,word == "systems" ~ "system"
                          ,word == "databases" ~ "database"
                          ,word == "packages" ~ "package"
                          ,word == "images" ~ "image"
                          ,word == "locations" ~ "location"
                          ,word == "cartographical" ~ "cartographic"
                          ,word == "geographical" ~ "geographic"
                          ,word == "graphical" ~ "graphic"
                          ,word == "coordinates" ~ "coordinate"
                          ,word == "monitors" ~ "monitor"
                          ,word == "terrains" ~ "terrain"
                          ,word == "landscapes" ~ "landscape"
                          ,word == "boundaries" ~ "boundary"
                          ,word %in% c("digitizing", "digitized") ~ "digitize"
                          ,word %in% c("colors", "colored") ~ "color"
                          ,word %in% c("colours", "coloured") ~ "colour"
                          ,word %in% c("user’s", "user's", "users") ~ "user"
                          ,word %in% c("generalized", "generalizing", "generalize", "generalizes", "generalizations") ~ "generalization"
                          ,word %in% c("interaction", "interact", "interactively", "interactivity", "interacting") ~ "interactive"
                          ,word %in% c("print", "printed", "prints", "printable", "printers", "printer") ~ "printing"
                          ,word %in% c("animates", "animated", "animations", "animate", "animating") ~ "animation"
                          ,word %in% c("emphasizes", "emphasizing", "emphasized") ~ "emphasize"
                          ,word %in% c("includes", "including", "included") ~ "include"
                          ,word %in% c("collects", "collected", "collecting") ~ "collect"
                          ,word %in% c("developed", "develops", "developing") ~ "develop"
                          ,word %in% c("showed", "shown", "shows", "showing") ~ "show"
                          ,word %in% c("computes", "computed") ~ "compute"
                          ,word %in% c("computers", "microcomputer", "microcomputers") ~ "computer"
                          ,TRUE ~ word))


# Count the occurrence of words
word_count4 <- text_tokenized_cleaned_stemmed4 %>%
  count(word, sort = TRUE)

# Plot the occurrence of words
word_count4 %>%
  mutate(word = fct_reorder(word, n)) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  geom_point(size=1, color="orange") +
  ylim(0, 2000) +
  ggtitle("Top 15 Words, Kraak_4")

# Plot only top 15 digital-related keywords
digital_keywords4 <- c("user", "gis", "file", "system", "database", "digital", "interactive", "animation", "web", 
                       "screen", "software", "electronic", "package", "printing", "computer")
digital_word_count4 <- filter(word_count4, word %in% digital_keywords3)
digital_word_count4 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#85c1e9", alpha=.6, width=.4) +
  geom_text(aes(label = n), hjust = 1.2, vjust = 0.4, color = "#366EE7", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Occurrence") +
  ylim(0, 200) +
  ggtitle("4th ed.")


#ggsave(file="Slocum_3.png", width=5, height=10)
















# Stem the words so that conjugations and plurals are reduced to the base
# and count the occurrence of stems
#text_autostemmed <- text_tokenized_cleaned %>%
  #mutate(stem = wordStem(word)) %>%
  #count(stem, sort = TRUE)

# Plot the occurrence of stems
#text_autostemmed %>%
  #slice_max(n, n = 15) %>%
  #mutate(stem = reorder(stem, n)) %>%
  #ggplot(aes(n, stem)) +
  #geom_col() +
  #labs(y = NULL) +
  #ggtitle("Top 15 Stems, Slocum_3")