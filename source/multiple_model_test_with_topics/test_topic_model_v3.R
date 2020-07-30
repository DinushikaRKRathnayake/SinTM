# ===============================================================================================================
# verion 03 testing: LDA model with RAKE keyword extraction
# ===============================================================================================================

# Initialize variables and inputs
# _______________________________________________________________________________________________________________

# initialize variables
source("E:\\R_Source\\Test_Scripts\\multiple_model_test_with_topics\\init.R")

# input file
file <- paste(c(location_path, filename), collapse = "")

# Base LDA model
# _______________________________________________________________________________________________________________

# read text data
doc <- readLines(file,encoding = "UTF-8")

# remove HTML tags
preprocess_data = gsub(pattern = "<.*?>", doc, replace=" ") 

# remove numbers 
preprocess_data <- gsub(pattern = "[[:digit:]]", preprocess_data, replace=" ")

# remove extra whitespaces 
preprocess_data <- stripWhitespace(preprocess_data) 

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
sparse_dtm <- document_term_matrix(dtf_final)

# store fit models in temporary location
model_dir <- paste0("models_", digest::digest(colnames(sparse_dtm), algo = "sha1"))

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  set.seed(seed)
  model <- FitLdaModel(dtm = sparse_dtm, 
                       k = k,
                       iterations = param_iterations,
                       burnin = param_burnin, 
                       alpha = param_alpha,
                       beta = param_beta,
                       optimize_alpha = FALSE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE,
                       cpus = 2) 
  
  model$k <- k
  model
  
}, export= ls(), cpus = 2)


# Testing
# _______________________________________________________________________________________________________________
# R-squared 
r2_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                     r2 = sapply(model_list, function(x) x$r2), 
                     stringsAsFactors = FALSE)
r2_matrix <- as.matrix(r2_mat)

# Maximum Log-likelihood
log_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                      loglikelihood = sapply(model_list, function(x) max((x$log_likelihood)$log_likelihood)), 
                      stringsAsFactors = FALSE)
log_matrix <- as.matrix(log_mat)


# Iterator at max log-likelihood
iter_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                       i = sapply(model_list, function(x) (x$log_likelihood)[which.max((x$log_likelihood)$log_likelihood),1 ]),
                       stringsAsFactors = FALSE)
iter_matrix <- as.matrix(iter_mat)

# Perplexity
perplexity_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                             perplexity = sapply(model_list, function(x) perplexity(sparse_dtm, x$phi, x$theta)), 
                             stringsAsFactors = FALSE)
perplexity_matrix <- as.matrix(perplexity_mat)

# coherence matrix
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
coherence_matrix <- as.matrix(coherence_mat)

# View results
# _______________________________________________________________________________________________________________

r2_matrix
log_matrix
iter_matrix
perplexity_matrix
coherence_matrix
