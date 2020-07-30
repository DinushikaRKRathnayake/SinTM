# ===============================================================================================================
# Generate diagnosis: log-likelihood, coherence, pervelance
# ===============================================================================================================

#________________________________________________________________________________________________________________
# log-likelihood
#________________________________________________________________________________________________________________

Loglikelihood <- function(model){
  
  if(isTruthy(model)){
    
    log_matrix <- as.matrix(model$log_likelihood)
    log_tibble<-tibble(iteration=log_matrix[,1],log=log_matrix[,2] )
    log_df<- as.data.frame(log_tibble)
    
    ggplot(log_df, aes(x = iteration, y = log)) +
      geom_point(size=2) +
      geom_line(group = 1,color="#cc087e", size=1)+
      theme_minimal() +
      labs(x = "Iteration",
           y = "Loglikelihood",
           title = "Loglikelihood",
           subtitle = "How plausible model parameters are given the data")
    
  }
  
}

#________________________________________________________________________________________________________________
# coherence
#________________________________________________________________________________________________________________

Coherence <- function(model){
  
  if(isTruthy(model)){
    
    coherence_matrix <- as.matrix(model$coherence)
    coherence_topics <- matrix(1:nrow(model$phi))
    coherence_df <- as.data.frame( tibble(topic = coherence_topics, coh = coherence_matrix[,1] ))
    
    ggplot(coherence_df, aes(x = topic, y = coh)) +
      geom_point(size=2) +
      geom_line(group = 1,size=1,color="#0dd442")+
      theme_minimal() +
      scale_x_continuous(breaks = seq(1,20,1)) + 
      labs(x = "Topics",
           y = "Coherence",
           title = "Coherence Score",
           subtitle = "Probabilistic coherence of each topic.Higher the coherence returns more quality topics")
    
  }
  
}

#________________________________________________________________________________________________________________
# prevalence
#________________________________________________________________________________________________________________

Prevalence <- function(model){
  
  if(isTruthy(model)){
    
    # Get the prevalence of each topic
    prevalence <- colSums(model$theta) / sum(model$theta) * 100
    
    prevalence_df <- as.data.frame(prevalence)
    alpha_df <- as.data.frame(model$alpha)
    prevalence_out <- as.data.frame(tibble(topic = rownames(prevalence_df), p = prevalence_df[,1], a = alpha_df[,1]))
    
    # prevalence should be proportional to alpha
    ggplot(prevalence_out, aes(x = a, y = p, group = topic)) +
      geom_point(aes(color = as.factor(topic)), size = 5)+
      labs(x = "Alpha",
           y = "Prevalence",
           color ="Topics",
           title = "Prevalence",
           subtitle = "Topic contribution to the document")
    
  }
  
}



