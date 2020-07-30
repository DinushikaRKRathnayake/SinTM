# ===============================================================================================================
# Find best number of topics for SinTM
# ===============================================================================================================

BestNumTopics <- function(raw_data) {
  
  if(isTruthy(raw_data)){
    
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
    doc_tibble <- rowid_to_column(tibble(text = preprocess_data), var = "document")
    
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
    
    # create DTM
    dtm <- document_term_matrix(dtf_final)

    # range of k 
    k_list <- seq(2,20, by=1)
    
    # store fit models in temporary location
    model_dir <- paste0("models_", digest::digest(colnames(dtm), algo = "sha1"))
    
    # fit a bunch of LDA models
    source("init_parameters.R")
    
    model_list <- TmParallelApply(X = k_list, FUN = function(k){
      param_beta <- 0.1/k
      
      m <- FitLdaModel(dtm = dtm, 
                       k = k, 
                       iterations = 500, 
                       burnin = 180,
                       alpha = 0.1,
                       beta = 0.1/k,
                       optimize_alpha = FALSE,
                       calc_likelihood = FALSE,
                       calc_coherence = TRUE,
                       calc_r2 = FALSE,
                       cpus = 1)
      
      m$k <- k
      
      m
    }, export= ls(), # export only needed for Windows machines
    cpus = 2) 
    
    # get average coherence for each model
    coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                                coherence = sapply(model_list, function(x) mean(x$coherence)), 
                                stringsAsFactors = FALSE)
    
    
    # Plot the result
    coherence_matrix <- as.matrix(coherence_mat)
    coherence_df <- as.data.frame( tibble(topic = coherence_matrix[,1], coh = coherence_matrix[,2] ))
    
    ggplot(coherence_df, aes(x = topic, y = coh)) +
      geom_point(size=2) +
      geom_line(group = 1,size=1,color="#f2b21d")+
      theme_minimal() +
      scale_x_continuous(breaks = seq(1,20,1)) + 
      labs(x = "Number of Topics",
           y = "Coherence",
           title = "Coherence Score",
           subtitle = "Higher the coherence returns good topics")
    
  }
  
}

