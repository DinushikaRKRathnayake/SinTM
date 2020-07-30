# ===============================================================================================================
# Generate top terms (topics)
# ===============================================================================================================

createTopics <- function(model, num_top_terms = 5 ){
  
  if(isTruthy(model) && isTruthy(num_top_terms)){
    
    top_terms <- GetTopTerms(phi = model$phi, M = num_top_terms)
    top_term_matrix <- as.data.frame(top_terms)
    top_term_matrix
    
  }
  
}

