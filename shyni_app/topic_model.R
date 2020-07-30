# ===============================================================================================================
# Generate LDA-RAKE model
# ===============================================================================================================

LdaRakeModel <- function(raw_data, num_topics = 2) {
  if(isTruthy(raw_data)){
    
    # input maximum number of words default setting
    if (!is.numeric(num_topics) || num_topics < 2) {
      num_topics <- 2
    }
    
    # pre processing data file
    if (is.character(raw_data)) {
      
      # remove HTML tags
      preprocess_data_ldarake = gsub(pattern = "<.*?>", raw_data, replace=" ") 
      
      # remove numbers  
      preprocess_data_ldarake = gsub(pattern = "[[:digit:]]", preprocess_data_ldarake, replace=" ") 
      
      # remove whitespaces 
      preprocess_data_ldarake <- stripWhitespace(preprocess_data_ldarake) 
      
      # remove leading and trailing white space
      preprocess_data_ldarake = gsub("^\\s+|\\s+$", "", preprocess_data_ldarake) 
      
    }
    
    # create tibble of document,text
    doc_tibble_ldarake <- rowid_to_column(tibble(text = preprocess_data_ldarake), var = "document")
    
    # split into words
    doc_words_ldarake <- doc_tibble_ldarake %>%
      unnest_tokens(word, text)
    
    # split word and pos tags
    doc_word_split <- str_split_fixed(doc_words_ldarake$word, "_", 2)
    
    # create tibble of document, word, pos
    input_ldarake<-tibble(document = doc_words_ldarake$document, word = doc_word_split[,1], pos = doc_word_split[,2])
    
    # keyword extraction
    keywds_rake <- keywords_rake(x = input_ldarake, 
                               term = "word", 
                               group = c("document"),
                               relevant = input_ldarake$pos %in% c("nnm","nnn","nnpa","nnpi","nnf","vfm","vnf","vnn"),
                               ngram_max = 3, n_min = 1)
    
    # record terms to keyword
    input_ldarake$term <- input_ldarake$word
    input_ldarake$term <- txt_recode_ngram(input_ldarake$term, compound = keywds_rake$keyword, ngram = keywds_rake$ngram)
    
    # Keep keyword or proper noun inanimate
    input_ldarake$term <- ifelse(input_ldarake$pos %in% "nnpi", input_ldarake$word,
                         ifelse(input_ldarake$term %in% c(keywds_rake$keyword), input_ldarake$term, NA))
    
    # remove not applicable and empty terms
    input_ldarake <- as.data.frame(input_ldarake)
    input_ldarake <- input_ldarake[!is.na(input_ldarake$term), ]
    input_ldarake <- input_ldarake[input_ldarake$term!="", ]
    
    # get document term frequency
    dtf_ldarake <- document_term_frequencies(input_ldarake, document = "document", term = "term")
    
    # create dtf matrix of keywords
    rake_dtf <- tibble(term = keywds_rake$keyword, rake = as.integer(keywds_rake$rake), rakefreq = keywds_rake$freq)
    
    # join two matrix by terms
    join_dtf <- left_join(dtf_ldarake, rake_dtf, by="term")
    
    # keep rake frequency or if null then document frequency
    join_dtf$freq <- ifelse((is.na(join_dtf$rakefreq)), join_dtf$freq, join_dtf$rakefreq)
    
    # final dtf matrix
    dtf_final <- as.data.frame(tibble(doc_id = join_dtf$doc_id, term = join_dtf$term, freq = join_dtf$freq))
    
    # create DTM
    dtm_ldarake <- document_term_matrix(dtf_final)
    
    # crete model
    set.seed(seed)
    model_v3 <- FitLdaModel(dtm = dtm_ldarake, 
                         k = num_topics,
                         iterations = param_iterations,
                         burnin = param_burnin, 
                         alpha = param_alpha,
                         beta = param_beta,
                         optimize_alpha = FALSE,
                         calc_likelihood = TRUE,
                         calc_coherence = TRUE,
                         calc_r2 = TRUE,
                         cpus = 2) 
    
  }
 
}
