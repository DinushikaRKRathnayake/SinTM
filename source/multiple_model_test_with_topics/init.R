# ============================================================
# Activate libraries
# ============================================================
library(stringr)
library(tm)
library(textmineR)
library(tidytext)
library(text2vec)
library(udpipe)
library(dplyr)
library(tibble)
library(gofastr)

# ============================================================
# Initialize parameters
# ============================================================

# model hyperparameters
param_iterations <- 500 
param_burnin <- 200
param_alpha <- 0.1
param_beta <- 0.01
seed <- 1234

# range of k 
k_list <- seq(2,20, by=2)

# path to documents
location_path <- "E:\\R_Source\\test_dataset\\"


# input document
filename <- "DBSM03.txt"

# input stopwords
stopwordsfilename <- "stop_words.txt"

