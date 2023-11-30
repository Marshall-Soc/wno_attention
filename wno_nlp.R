
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
# identical(rownames(pre.meta), data$doc_id)

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
df <- lemmas %>%
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
pre.meta <- pre.meta[order(match(rownames(pre.meta), rownames(dtm))),]
identical(rownames(pre.meta), rownames(dtm))

out <- readCorpus(dtm, type = "slam")
out <- prepDocuments(out$documents, out$vocab, pre.meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta 











































































