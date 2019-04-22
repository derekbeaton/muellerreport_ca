# packages ------------------------------------------------------------
library(tidytext)

# accoutrements ------------------------------------------------------------
load(here::here("data","cleaned_rda","tidy_mueller.rda"))
load(here::here("data","cleaned_rda","muellernets.rda"))

drop.rows.columns <- function(this_matrix, colSums_threshold, rowSums_threshold){
  
  dropped_rows <- c() -> dropped_columns
  while( any(rowSums(this_matrix) < rowSums_threshold) | any(colSums(this_matrix) < colSums_threshold) ){
    
    columns_below_threshold <- which(colSums(this_matrix) < colSums_threshold)
    if(length(columns_below_threshold)){
      dropped_columns <- c(dropped_columns, columns_below_threshold)
      this_matrix <- this_matrix[,-c(columns_below_threshold)]
    }
    
    rows_below_threshold <- which(rowSums(this_matrix) < rowSums_threshold)
    if(length(rows_below_threshold)){
      dropped_rows <- c(dropped_rows, rows_below_threshold)
      this_matrix <- this_matrix[-c(rows_below_threshold),]
    }
    
  }
  return(list(this_matrix = this_matrix, dropped_columns = dropped_columns, dropped_rows = dropped_rows)) 
}


# make base matrices ------------------------------------------------------------

page_by_word_counts <- cast_sparse(tidy_mueller, row = page, col = word, value = count)
  page_by_word_counts.matrix <- as.matrix(page_by_word_counts)
page_by_word <- cast_sparse(tidy_mueller, row = page, col = word)
  page_by_word.matrix <- as.matrix(page_by_word)
  
## quick test.
(sum((as.matrix(page_by_word_counts)!=0) - (as.matrix(page_by_word))))


page_by_lemma_counts <- cast_sparse(muellernets, row = page, col = lemma, value = count)
  page_by_lemma_counts.matrix <- as.matrix(page_by_lemma_counts)
page_by_lemma <- cast_sparse(muellernets, row = page, col = lemma)
  page_by_lemma.matrix <- as.matrix(page_by_lemma)

## quick test.
(sum((as.matrix(page_by_lemma_counts)!=0) - (as.matrix(page_by_lemma))))


# clean(er) matrices ------------------------------------------------------------
    ## something I should have done in the previous step...
  terms_to_drop <- c("blank","redact")
  page_by_word_counts.matrix <- page_by_word_counts.matrix[,-c(which(colnames(page_by_word_counts.matrix) %in% terms_to_drop))]
  page_by_word.matrix <- page_by_word.matrix[,-c(which(colnames(page_by_word.matrix) %in% terms_to_drop))]
  
# clean(er) matrices ------------------------------------------------------------
  ### some of these cleaning steps could have been done on the tbl/tidy objects, 
  ### but it is easier to clean on the matrices of interest 
  ### to find more suitable matrices for analyses


  ### we will use just the pre-adjacency matrices for cleaning:
    #### page_by_word & page_by_lemma

## super arbitrary
minimum_rowSum_counts <- 2 -> minimum_colSum_counts

page_by_word.drop_info <- drop.rows.columns(page_by_word.matrix, minimum_colSum_counts, minimum_rowSum_counts)
page_by_lemma.drop_info <- drop.rows.columns(page_by_lemma.matrix, minimum_colSum_counts, minimum_rowSum_counts)

page_by_word.matrix_for_analyses <- page_by_word.matrix[rownames(page_by_word.drop_info$this_matrix),colnames(page_by_word.drop_info$this_matrix)]
page_by_word_counts.matrix_for_analyses <- page_by_word_counts.matrix[rownames(page_by_word.drop_info$this_matrix),colnames(page_by_word.drop_info$this_matrix)]


page_by_lemma.matrix_for_analyses <- page_by_lemma.matrix[rownames(page_by_lemma.drop_info$this_matrix),colnames(page_by_lemma.drop_info$this_matrix)]
page_by_lemma_counts.matrix_for_analyses <- page_by_lemma_counts.matrix[rownames(page_by_lemma.drop_info$this_matrix),colnames(page_by_lemma.drop_info$this_matrix)]



## quick test.
(sum((page_by_word_counts.matrix_for_analyses!=0) - (page_by_word.matrix_for_analyses)))
(sum((page_by_lemma_counts.matrix_for_analyses!=0) - (page_by_lemma.matrix_for_analyses)))



write.csv(page_by_word_counts.matrix_for_analyses, file=here::here("data","for_analyses","page_by_word_counts.matrix_for_analyses.csv"))
write.csv(page_by_word.matrix_for_analyses, file=here::here("data","for_analyses","page_by_word.matrix_for_analyses.csv"))
write.csv(page_by_lemma_counts.matrix_for_analyses, file=here::here("data","for_analyses","page_by_lemma_counts.matrix_for_analyses.csv"))
write.csv(page_by_lemma.matrix_for_analyses, file=here::here("data","for_analyses","page_by_lemma.matrix_for_analyses.csv"))
