
##############################
##  wno_topics.R: Code to reproduce NLP-based measures
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
               install = T)

remotes::install_gitlab("culturalcartography/text2map.dictionaries")

library(text2map.dictionaries)


######################################
#  Data
######################################

data <- readtext(paste0(getwd(), "/TXTs/*"))
pre.meta <- read.csv("pre_topic_meta.csv", row.names = 1, header = T)


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
#  Construal scores
######################################






































































