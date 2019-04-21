# packages ------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)
library(hunspell)
library(tm)
library(corpus)
library(pdftools)
library(textnets)

# accoutrements ------------------------------------------------------------
data(stop_words)


# tidy texting ------------------------------------------------------------
# copy of necessary code from fork of https://github.com/batpigandme/tidymueller

# this emphasizes single words
read_csv(here::here("data", "raw", "factbase_mueller_report.csv")) %>%
select(Page, Text)  %>%
rename_all(tolower) ->
  mueller_report

mueller_report %>%
unnest_tokens(word, text, to_lower = FALSE) ->
  tidy_mueller

tidy_mueller$word <- gsub('[[:punct:] ]+',"",tidy_mueller$word)
tidy_mueller$word <- unlist(text_tokens(tidy_mueller$word, stemmer="en"))

## unfortuately have to remove year...
tidy_mueller %>%
filter(!str_detect(word, "[0-9]")) ->
  tidy_mueller

# drop very short words.
tidy_mueller %>%
filter(nchar(word) >= 3) -> 
  tidy_mueller

tidy_mueller %>%
anti_join(stop_words) ->
  tidy_mueller

## stolen from textnets::PrepText
# textdata <- textdata %>%
#   group_by(lemma) %>%
#   count_(groupvar) %>%
#   rename(count = n)

tidy_mueller %>%
group_by(word) %>%
count(page) %>%
rename(count = n) ->
  tidy_mueller

tidy_mueller <- ungroup(tidy_mueller)

## save tidy_mueller as a .rda

save(tidy_mueller, file=here::here("data","cleaned_rda","tidy_mueller.rda"))

# page_by_word_counts <- cast_sparse(tidy_mueller, row = page, col = word, value = count)
# page_by_word <- cast_sparse(tidy_mueller, row = page, col = word)
# 
# ## quick test.
# (sum((as.matrix(page_by_word_counts)!=0) - (as.matrix(page_by_word))))

# ## so, so lazy and ugly. a better way is above.
# page_by_word_counts <- matrix(0, length(unique(tidy_mueller$page)), length(unique(tidy_mueller$word)))
# rownames(page_by_word_counts) <- unique(tidy_mueller$page)
# colnames(page_by_word_counts) <- unique(tidy_mueller$word)
#
# for(i in 1:nrow(tidy_mueller)){
#
#   page_by_word_counts[as.character(tidy_mueller[i,"page"]), as.character(tidy_mueller[i,"word"])] <- page_by_word_counts[as.character(tidy_mueller[i,"page"]), as.character(tidy_mueller[i,"word"])] + 1
#
#   if((i %% 1000)==0){
#     print(i)
#   }
# }
#
# page_by_word <- (page_by_word_counts>0)+0


# copy of necessary code from fork of https://github.com/cbail/mueller_report

report <- pdf_text(here::here("data", "raw", "Redacted-Mueller-Report.pdf"))
muellerreport <- as.data.frame(report)
muellerreport$page<-rownames(muellerreport)
  ## I believe this downloads extera data to the repo if you don't have it (i.e., the udpipe file)
muellernets <- PrepText(muellerreport, textvar = "report", groupvar="page", node_type="words", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

# out <- muellernets[muellernets$count>2,]
  ## from the inner workings of CreateTextnet
# # node_type <- ifelse(names(out)[1] == "lemma", "words", "groups")
# # nets_type <- ifelse(ncol(out) > 3, "signed", "regular")
# out <- ungroup(out)
# for_adjacency <- out %>% bind_tf_idf(page, lemma, count) %>% arrange(page)
# suppressWarnings(for_crossprod <- cast_sparse(for_adjacency, row = lemma, col = page, value = tf_idf))

## save muellernets as a .rda
save(muellernets, file=here::here("data","cleaned_rda","muellernets.rda"))


# page_by_lemma_counts <- cast_sparse(muellernets, row = page, col = lemma, value = count)
# page_by_lemma <- cast_sparse(muellernets, row = page, col = lemma)
# 
# (sum((as.matrix(page_by_lemma_counts)!=0) - (as.matrix(page_by_lemma))))



