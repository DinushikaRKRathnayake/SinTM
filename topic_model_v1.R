# ===============================================================================================================
# Generate LDA base model
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

CreateLdaModel <- function(raw_data, num_topics) {
  
  if(isTruthy(raw_data)){
    
    # input maximum number of words
    if(!isTruthy(num_topics)){
      num_topics <- 20
    }

    
    # pre processing data file
    if (is.character(raw_data)) {
      preprocess_data_lda <- text.clean(raw_data)
    }
    
    # create corpus
    corpus_tlda <- Corpus(VectorSource(preprocess_data_lda))
    
    # create document term matrix
    dtm_tlda <- DocumentTermMatrix(corpus_tlda)
    
    # cast to sparse Matrix of class "dgCMatrix"
    tf_tlda <- tidy(dtm_tlda)
    dtm_tlda_sparse <-cast_sparse(tf_tlda,document, term, count)
    
    # crete model
    param_beta <- param_alpha/num_topics
    
    set.seed(seed)
    model_v1 <- FitLdaModel(dtm = dtm_tlda_sparse, 
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
