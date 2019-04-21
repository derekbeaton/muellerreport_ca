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
tidy_mueller %>%
group_by(word) %>%
count(page) %>%
rename(count = n) ->
  tidy_mueller

tidy_mueller <- ungroup(tidy_mueller)

## save tidy_mueller as a .rda

save(tidy_mueller, file=here::here("data","cleaned_rda","tidy_mueller.rda"))

# copy of necessary code from fork of https://github.com/cbail/mueller_report
report <- pdf_text(here::here("data", "raw", "Redacted-Mueller-Report.pdf"))
muellerreport <- as.data.frame(report)
muellerreport$page<-rownames(muellerreport)
  ## I believe this downloads extera data to the repo if you don't have it (i.e., the udpipe file)
muellernets <- PrepText(muellerreport, textvar = "report", groupvar="page", node_type="words", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)


## save muellernets as a .rda
save(muellernets, file=here::here("data","cleaned_rda","muellernets.rda"))
