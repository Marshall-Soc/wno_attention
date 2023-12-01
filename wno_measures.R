
##############################
##  wno_measures.R: Code to reproduce measures that need computing
##  Author: Marshall A. Taylor
##############################

### BEGIN ###


######################################
#  Necessary packages and functions
######################################

pacman::p_load(tidyverse, 
               tidytext, 
               quanteda,
               tm,
               readtext,
               textreg,
               textclean,
               textstem,
               stringi,
               udpipe,
               text2map,
               stm,
               Hmisc,
               install = T)

remotes::install_gitlab("culturalcartography/text2map.dictionaries")

library(text2map.dictionaries)


######################################
#  Data
######################################

data <- readtext(paste0(getwd(), "/TXTs/*"))
pre.meta <- read.csv("pre_topic_meta.csv", row.names = 1, header = T)
data("concreteness", package = "text2map.dictionaries")


######################################
#  Preprocessing
######################################

  #pre STM cleaning
data <- data %>%
  #transliterate
  mutate(text = stri_trans_general(text,
                                   id = "Any-Latin;Latin-ASCII"),
         #lowercase
         text = tolower(text),
         #replace curly quotes
         text = replace_curly_quote(text),
         #remove punctuation external to a word group
         text = gsub("(\\w+[_'-]+\\w+)|[[:punct:]]+", "\\1", text),
         #replace contractions
         text = replace_contraction(text),
         #remove numbers
         text = gsub("[[:digit:]]+", " ", text),
         #remove excess whitespace between words
         text = gsub("[[:space:]]+", " ", text),
         #remove excess whitespace at begging and end of strings
         text = trimws(text))

pre.meta <- pre.meta[order(match(rownames(pre.meta), data$doc_id)),]
# identical(rownames(pre.meta), data$doc_id) hould be TRUE

pre.meta$doc_id <- paste0("doc", 1:nrow(data))

  # Load in English language model for POS and lemmatization
eng <- udpipe_download_model(language = "english-gum")
ud_eng <- udpipe_load_model(eng$file_model)

lemmas <- udpipe_annotate(
  x = data$text,
  object = ud_eng) %>%
  as_tibble() %>%
  select(doc_id, lemma) %>%
  group_by(doc_id, lemma) %>%
  summarize(n = n())

  # Get triplet matrix
df <- lemmas %>% #This can take a long time
  #remove words that are 2 characters long or less
  filter(!nchar(lemma) <= 2) %>%
  #remove English stop words in the Onix, SMART, or Snowball stop lists
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  filter(!(lemma %in% c("page")))

  # get DTM
dtm <- df %>%
  cast_dtm(
    document = doc_id,
    term = lemma,
    value = n,
    weighting = tm::weightTf
  )

  # remove sparse terms with .4 factor
dtm <- removeSparseTerms(dtm, 0.6) 


######################################
#  STM
######################################

  #Prep for STM
pre.meta <- pre.meta[order(match(pre.meta$doc_id, rownames(dtm))),]
# identical(pre.meta$doc_id, rownames(dtm)) should be TRUE

out <- readCorpus(dtm, type = "slam")
out <- prepDocuments(out$documents, out$vocab, pre.meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta 
# identical(names(docs), meta$doc_id) Should be TRUE

  # Find optimal k
wn_modelk <- searchK(docs, vocab, K = seq(2, 20, by = 1), N = 50,
                     heldout.seed = 0.5, init.type = "Spectral", 
                     prevalence =~ s(year) + ideo,
                     data = meta)

plot(wn_modelk)
ggplot(data = wn_modelk$results, aes(x = as.numeric(semcoh), y = as.numeric(exclus))) +
  geom_text(aes(label = K)) +
  xlab("Semantic Coherence") +
  ylab("Exclusivity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid = element_blank()) #ideal statistical fit = 8-ish

  # Get model
wn.stm <- stm(docs, vocab, K = 8,
              prevalence =~ s(year) + ideo,
              data = meta,
              init.type = "Spectral")

  # Interpret the topics
plot(wn.stm, type = "summary", n = 5, main = "", labeltype = "prob")

sageLabels(wn.stm) #Topic #2 = Borders and Immigration

data <- data[order(match(data$doc_id, rownames(pre.meta))),]
# identical(data$doc_id, rownames(pre.meta)) should be TRUE

findThoughts(wn.stm, texts = as.character(data$text),
             n = 3, topics = 2) #Topic #2 = Borders and Immigration

  # Add topic proportions to dataset
data.final <- cbind(meta, wn.stm$theta)


######################################
#  Style Scores
######################################

  # Create dictionaries
concrete <- concreteness %>%
  filter(concrete_mean >= median(concrete_mean)) %>%
  select(term)

abstract <- concreteness %>%
  filter(concrete_mean < median(concrete_mean)) %>%
  select(term)

  # Create non-lemmatized DTM
df.nolemma <- data %>%
  unnest_tokens(word, text) %>%
  filter(!nchar(word) <= 2) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("page")))

  # get DTM
dtm.nolemma <- df.nolemma %>%
  group_by(doc_id, word) %>%
  summarize(n = n()) %>%
  cast_dtm(
    document = doc_id,
    term = word,
    value = n,
    weighting = tm::weightTf
  )

# remove sparse terms with .4 factor
dtm.nolemma <- removeSparseTerms(dtm.nolemma, 0.6) 

  # Get scores
word.concrete <- intersect(colnames(dtm.nolemma), concrete$term)
word.abstract <- intersect(colnames(dtm.nolemma), abstract$term)

dtm.concrete <- dtm.nolemma[, word.concrete]
dtm.abstract <- dtm.nolemma[, word.abstract]

concrete.sum <- rowSums(as.matrix(dtm.concrete))
abstract.sum <- rowSums(as.matrix(dtm.abstract))

construal <- (concrete.sum - abstract.sum) / (concrete.sum + abstract.sum)

construal <- as.data.frame(construal) %>%
  rownames_to_column(var = "article_id")

data.final <- data.final %>%
  rownames_to_column(var = "article_id") %>%
  left_join(construal, by = "article_id") %>%
  group_by(org, year) %>%
  summarize_if(is.numeric, ~ mean(.x, na.rm = TRUE))

  # get rolling means and then style variables (including discursive style)
data.final <- data.final %>% 
  arrange(org, year) %>% 
  group_by(org) %>%
  mutate(construal_rolling = cummean(construal),
         immigration_rolling = cummean(`2`)) %>% 
  mutate(construal_style = Hmisc::Lag(construal_rolling, shift = 1),
         discursive_style = Hmisc::Lag(immigration_rolling, shift = 1),
         immigration = log(`2`/(1 - `2`)),
         discursive_style = log(discursive_style/(1 - discursive_style))) %>% 
  arrange(org, year) %>% 
  ungroup()


######################################
#  Environmental Shock Variable
######################################

data.final$terror_nr <- data.final$terror_nright / 
  (data.final$population_y2 * (1 - data.final$hisp_inter))


######################################
#  Violent Crime Rate Variable
######################################

  # Violent crime rate
data.final$vcrime_rate <- log((data.final$vcrime/(data.final$population_y2/1000))+1)





























































