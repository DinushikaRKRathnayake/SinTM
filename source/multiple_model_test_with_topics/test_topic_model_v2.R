# ===============================================================================================================
# verion 02 testing: LDA model with stopwords removal
# ===============================================================================================================

# Initialize variables and inputs
# _______________________________________________________________________________________________________________

# initialize variables
source("E:\\R_Source\\Test_Scripts\\multiple_model_test_with_topics\\init.R")

# input files
file <- paste(c(location_path, filename), collapse = "")
remove_data <- readLines(paste(c(location_path, stopwordsfilename), collapse = ""), encoding = "UTF-8")

# Data preprocessing
# _______________________________________________________________________________________________________________

text.clean = function(text_data){ 
  text_data = gsub(pattern = "<.*?>", text_data, replace=" ")           # remove HTML tags
  text_data = gsub(pattern = "[[:digit:]]", text_data, replace=" ")     # remove numbers 
  text_data = gsub(pattern = "[[:alpha:]]", text_data, replacement=" ") # remove english characters
  text_data = gsub(pattern = "[[:punct:]]", text_data, replacement=" ") # remove punctuations
  text_data = stripWhitespace(text_data)                                # remove extra whitespaces 
  text_data = gsub("^\\s+|\\s+$", "", text_data)                        # remove leading and trailing white space
  return(text_data)
}

# Base LDA model
# _______________________________________________________________________________________________________________

# read text data
doc <- readLines(file,encoding = "UTF-8")

# process data
preprocess_data <- text.clean(doc)

# create corpus
corpus <- Corpus(VectorSource(preprocess_data))

# create document term matrix
dtm <- DocumentTermMatrix(corpus)

# remove empty documents
non_empty_docs = (apply(dtm, 1, sum) > 0)   
dtm = dtm[non_empty_docs,] 

# remove stop words
dtm <- remove_stopwords(dtm, stopwords = remove_data)

# cast to sparse Matrix of class "dgCMatrix"
tf_dtm <- tidy(dtm)
sparse_dtm <- cast_sparse(tf_dtm, document, term, count)

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

