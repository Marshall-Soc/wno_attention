
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
  # Not sure why doc_id sometimes gets duplicated, but below fixes it if it happens
# data$doc_id <- gsub(".*/", "", data$doc_id)
# data <- unique(data)

pre.meta <- readRDS("pre_topic_meta.rds")
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
         #remove excess whitespace at begining and end of strings
         text = trimws(text))

pre.meta <- pre.meta[order(match(rownames(pre.meta), data$doc_id)),]
# identical(rownames(pre.meta), data$doc_id) #should be true

pre.meta$doc_id <- paste0("doc", 1:nrow(data))

  # Load in English language model for POS and lemmatization
eng <- udpipe_download_model(language = "english-gum")
ud_eng <- udpipe_load_model(eng$file_model)

lemmas <- udpipe_annotate( #this can take a while; you can instead load 
                           #the lemmas.rds file in the repo
  x = data$text,
  object = ud_eng) %>%
  as_tibble() %>%
  select(doc_id, lemma) %>%
  group_by(doc_id, lemma) %>%
  dplyr::summarize(n = n())

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

  # remove sparse terms
dtm <- removeSparseTerms(dtm, 0.6) 


######################################
#  STM
######################################

  #Prep for STM
pre.meta <- pre.meta[order(match(pre.meta$doc_id, rownames(dtm))),]
# identical(pre.meta$doc_id, rownames(dtm)) #should be TRUE

out <- readCorpus(dtm, type = "slam")
out <- prepDocuments(out$documents, out$vocab, pre.meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta 
# identical(names(docs), meta$doc_id) #should be TRUE

  # Find optimal k
wn_modelk <- searchK(docs, vocab, K = seq(2, 20, by = 1), N = 50,
                     heldout.seed = 0.5, init.type = "Spectral", 
                     prevalence =~ s(year) + ideo,
                     data = meta)

plot(wn_modelk)
sc.exc <- ggplot(data = wn_modelk$results, aes(x = as.numeric(semcoh), y = as.numeric(exclus))) +
  geom_text(aes(label = K)) +
  xlab("Semantic Coherence") +
  ylab("Exclusivity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid = element_blank()) #ideal statistical fit = 8-ish

png("figures/sc_exc.png", res = 750, height = 4, width = 6, units = "in")
  sc.exc
dev.off()

  # Get model
wn.stm <- stm(docs, vocab, K = 8,
              prevalence =~ s(year) + ideo,
              data = meta,
              init.type = "Spectral")

  # Interpret the topics
colMeans(wn.stm$theta) #marginal topic probabilities

plot(wn.stm, type = "summary", n = 5, main = "", labeltype = "prob")

sageLabels(wn.stm, 10) #Topic #2 = Borders and Immigration

data <- data[order(match(data$doc_id, rownames(pre.meta))),]
# identical(data$doc_id, rownames(pre.meta)) #Should be TRUE

findThoughts(wn.stm, texts = as.character(data$text),
             n = 3, topics = 8) #Topic #2 = Borders and Immigration

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
  dplyr::summarize(n = n()) %>%
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

  # Get word counts
word.count <- rowSums(as.matrix(dtm.nolemma)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "article_id") %>%
  rename("word_count" = 2)

data.final <- data.final %>%
  rownames_to_column(var = "article_id") %>%
  left_join(word.count, by = "article_id")

  # temporarily convernt admin to numeric
data.final$admin <- recode(data.final$admin, Reagan = 1,
         HWBush = 2,
         Clinton = 3,
         WBush = 4)

  # Add construal scores
data.final <- data.final %>%
  left_join(construal, by = "article_id") %>%
  group_by(org, year) %>%
  summarize_if(is.numeric, ~ mean(.x, na.rm = TRUE))

  # Take admin back to factor
data.final$admin <- recode(data.final$admin, `1` = "Reagan",
                           `2` = "HWBush",
                           `3` = "Clinton",
                           `4` = "WBush")

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
#  Environmental Shock Variables
######################################

  #Logged number of terror threats/attacks in U.S. in previous year
data.final$log_terror <- log(data.final$terror_nright+1)
  
  #Logged estimated size of Hispanic/Latinx population in county
data.final$hisp_pop <- data.final$population * data.final$hisp_interprop
data.final$log_hisppop <- log(data.final$hisp_pop)


######################################
#  Violent Crime Rate Variable
######################################

  # Violent crime rate
data.final$vcrime_rate <- log((data.final$vcrime/(data.final$population/1000))+1)


######################################
#  Vocality
######################################

# Violent crime rate
data.final$log_vocality <- log(data.final$vocality+1)


######################################
#  Save final data
######################################

saveRDS(data.final, "wno_data.rds")


### END ###















































