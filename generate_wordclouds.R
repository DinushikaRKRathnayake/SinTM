# ===============================================================================================================
# Generate word clouds for three models
# ===============================================================================================================

text.clean = function(text_data){ 
  # remove HTML tags
  text_data = gsub(pattern = "<.*?>", text_data, replace=" ") 
  
  # remove numbers 
  text_data = gsub(pattern = "[[:digit:]]", text_data, replace=" ")  
  
  # remove english characters
  text_data = gsub(pattern = "[[:alpha:]]", text_data, replacement=" ") 
  
  # remove punctuations
  text_data = gsub(pattern = "[[:punct:]]", text_data, replacement=" ") 
  
  # remove extra whitespaces 
  text_data = stripWhitespace(text_data)     
  
  # remove leading and trailing white space
  text_data = gsub("^\\s+|\\s+$", "", text_data)  
  
  return(text_data)
}


createWordCloudLda <- function(raw_data, num_words, font_size ) {
  
  if(isTruthy(raw_data)){
    
    # input maximum number of words default setting
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # input font size default setting
    if (!is.numeric(font_size) || font_size > 1) {
      font_size <- 0.5
    }
    
    # pre processing data file
    if (is.character(raw_data)) {
      preprocess_data_lda <- text.clean(raw_data)
    }
    
    # create corpus
    corpus_lda <- Corpus(VectorSource(preprocess_data_lda))
    
    # create term document matrix
    tdm_lda <- TermDocumentMatrix(corpus_lda)
    
    # convert to matrix
    tdm_lda <- as.matrix(tdm_lda)
    
    result_lda <- sort(rowSums(tdm_lda), decreasing = TRUE)
    result_lda <- data.frame(word = names(result_lda), freq = as.numeric(result_lda))
    
    # grab the top n most common words
    result_lda <- head(result_lda, n = num_words)
    if (nrow(result_lda) == 0) {
      return(NULL)
    }
    
    wordcloud_v1 <- wordcloud2(result_lda, backgroundColor = "white", size = font_size )
    
  }
}

# ===============================================================================================================

createWordCloudLdaPos <- function(raw_data, remove_data, num_words, font_size ) {
  
  if(isTruthy(raw_data)){
    
    # input maximum number of words default setting
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # input font size default setting
    if (!is.numeric(font_size) || font_size > 1) {
      font_size <- 0.5
    }
    
    # pre processing data file
    if (is.character(raw_data)) {
      preprocess_data_ldapos <- text.clean(raw_data)
    }
    
    # create corpus
    corpus_ldapos <- Corpus(VectorSource(preprocess_data_ldapos))
    
    # create term document matrix
    tdm_ldapos <- TermDocumentMatrix(corpus_ldapos)
    
    # remove stop words
    if(isTruthy(remove_data)){
      tdm_ldapos <- remove_stopwords(tdm_ldapos, stopwords = remove_data)
    }
    
    # convert to matrix
    tdm_ldapos <- as.matrix(tdm_ldapos)
    
    result_ldapos <- sort(rowSums(tdm_ldapos), decreasing = TRUE)
    result_ldapos <- data.frame(word = names(result_ldapos), freq = as.numeric(result_ldapos))
    
    # grab the top n most common words
    result_ldapos <- head(result_ldapos, n = num_words)
    if (nrow(result_ldapos) == 0) {
      return(NULL)
    }
    
    wordcloud_v2 <- wordcloud2(result_ldapos, backgroundColor = "white", size = font_size )
    
  }
  
}

# ===============================================================================================================

createWordCloudLdaRake <- function(raw_data, num_words, font_size ) {
  
  if(isTruthy(raw_data)){
    
    # input maximum number of words default setting
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # input font size default setting
    if (!is.numeric(font_size) || font_size > 1) {
      font_size <- 0.5
    }
    
    # pre processing data file
    if (is.character(raw_data)) {
      
      # remove HTML tags
      preprocess_data = gsub(pattern = "<.*?>", raw_data, replace=" ") 
      
      # remove numbers 
      preprocess_data = gsub(pattern = "[[:digit:]]", preprocess_data, replace=" ") 
      
      # remove whitespaces 
      preprocess_data <- stripWhitespace(preprocess_data) 
      
      # remove leading and trailing white space
      preprocess_data = gsub("^\\s+|\\s+$", "", preprocess_data) 
      
    }
    
    # create tibble of document,text
    doc_tibble <- tibble(document = "doc1",text = preprocess_data )
    
    # split into words
    doc_words <- doc_tibble %>%
      unnest_tokens(word, text)
    
    # split word and pos tags
    doc_word_split <- str_split_fixed(doc_words$word, "_", 2)
    
    # create tibble of document, word, pos
    input<-tibble(document = doc_words$document, word = doc_word_split[,1], pos = doc_word_split[,2])
    
    # keyword extraction
    keyw_rake <- keywords_rake(x = input, 
                               term = "word", 
                               group = c("document"),
                               relevant = input$pos %in% c("nnm","nnn","nnpa","nnpi","nnf","vfm","vnf","vnn"),
                               ngram_max = 3, n_min = 1)
    
    # record terms to keyword
    input$term <- input$word
    input$term <- txt_recode_ngram(input$term, compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
    
    # Keep keyword or proper noun inanimate
    input$term <- ifelse(input$pos %in% "nnpi", input$word,
                         ifelse(input$term %in% c(keyw_rake$keyword), input$term, NA))
    
    # remove not applicable and empty terms
    input <- as.data.frame(input)
    input <- input[!is.na(input$term), ]
    input <- input[input$term!="", ]
    
    # get document term frequency
    dtf <- document_term_frequencies(input, document = "document", term = "term")
    
    # create dtf matrix of keywords
    rake_dtf <- tibble(term = keyw_rake$keyword, rake = as.integer(keyw_rake$rake), rakefreq = keyw_rake$freq)
    
    # join two matrix by terms
    join_dtf <- left_join(dtf, rake_dtf, by="term")
    
    # keep rake frequency or if null then document frequency
    join_dtf$freq <- ifelse((is.na(join_dtf$rakefreq)), join_dtf$freq, join_dtf$rakefreq)
    
    # final dtf matrix
    dtf_final <- as.data.frame(tibble(doc_id = join_dtf$doc_id, term = join_dtf$term, freq = join_dtf$freq))
    
    # final dtf matrix
    dtf_final <- as.data.frame(tibble(doc_id = join_dtf$doc_id, term = join_dtf$term, freq = join_dtf$freq))
    
    # get word frequency
    result_bwc <- data.frame(tibble(word = dtf_final$term, freq = as.numeric(dtf_final$freq)))
    
    # grab the top n most common words
    result_bwc <- head(result_bwc, n = num_words)
    if (nrow(result_bwc) == 0) {
      return(NULL)
    }
    
    wordcloud_v3 <- wordcloud2(result_bwc, backgroundColor = "white", size = font_size )
    
  }
  
}