# ===============================================================================================================
# verion 01 testing: Base LDA model
# ===============================================================================================================

# Initialize variables and inputs
# _______________________________________________________________________________________________________________

# initialize variables
source("E:\\R_Source\\Test_Scripts\\single_model_test\\init.R")

# input file
file <- paste(c(location_path, filename), collapse = "")

# Data preprocessing
# _______________________________________________________________________________________________________________

text.clean = function(text_data){ 
  text_data = gsub(pattern = "<.*?>", text_data, replace="")           # remove HTML tags
  text_data = gsub(pattern = "[[:digit:]]", text_data, replace="")     # remove numbers 
  text_data = gsub(pattern = "[[:alpha:]]", text_data, replacement="") # remove english characters
  text_data = gsub(pattern = "[[:punct:]]", text_data, replacement="") # remove punctuations
  text_data = stripWhitespace(text_data)                               # remove extra whitespaces 
  text_data = gsub("^\\s+|\\s+$", "", text_data)                       # remove leading and trailing white space
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

# cast to sparse Matrix of class "dgCMatrix"
tf_dtm <- tidy(dtm)
sparse_dtm <- cast_sparse(tf_dtm, document, term, count)

# crete model
set.seed(seed)
model <- FitLdaModel(dtm = sparse_dtm, 
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


# Testing
# _______________________________________________________________________________________________________________
# R-squared 
model$r2

# Log-likelihood
l <- model$log_likelihood
l[which(l$log_likelihood == max(l$log_likelihood)), ]

# Perplexity
perplexity(sparse_dtm, topic_word_distribution = model$phi, doc_topic_distribution = model$theta)

# coherence matrix
as.matrix(model$coherence)
# Mean coherence
mean(model$coherence)



