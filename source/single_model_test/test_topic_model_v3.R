# ===============================================================================================================
# verion 03 testing: LDA model with RAKE keyword extraction
# ===============================================================================================================

# Initialize variables and inputs
# _______________________________________________________________________________________________________________

# initialize variables
source("E:\\R_Source\\Test_Scripts\\single_model_test\\init.R")

# input file
file <- paste(c(location_path, filename), collapse = "")

# Base LDA model
# _______________________________________________________________________________________________________________

# read text data
doc <- readLines(file,encoding = "UTF-8")

# remove HTML tags
preprocess_data = gsub(pattern = "<.*?>", doc, replace="") 

# remove numbers 
preprocess_data <- gsub(pattern = "[[:digit:]]", preprocess_data, replace="")

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


