
library(textcat)
library(stringr)

clean.text <- function(text){
  # INPUT: Text to be "cleansed"
  # OUTPUT: Cleansed text
  # USAGE: clean.text(text) will return a string that has the punctuation removed
  #        lower case, and all other text cleaning operations done
  replace.links <- function(text){
    # extract urls from string, only works with t.co links, which all links in
    # twitter are nowadays
    return(str_replace_all(text,
                           ignore.case("http://[a-z0-9].[a-z]{2,3}/[a-z0-9]+"),
                           "urlextracted"))
  }
  remove.word <- function(string, starts.with.char){
    # INPUT:  string is a string to be edited,
    #         starts.with.char is a string or partial string to search and remove
    # OUTPUT: string with words removed
    # USAGE:  remove.word(string, "@") removes words starting with "@"
    #         remove.word(string, "RT") removes RT from string
    word.len <- nchar(starts.with.char)
    list.of.words <- strsplit(string, " ")[[1]]
    # remove ones that start with "starts.with.char"
    list.of.words <- list.of.words[!substring(list.of.words, 1,
                                              word.len)==starts.with.char]
    ret.string <- paste(list.of.words, collapse=" ")
    return(ret.string)
  }
  
  text.cleansed <- tolower(text)
  # remove the string "food poisoning" because every tweet has this in it...
  text.cleansed <- gsub("food poisoning", "", text.cleansed)
  text.cleansed <- replace.links(text.cleansed)
  text.cleansed <- remove.word(text.cleansed, "@")
  text.cleansed <- remove.word(text.cleansed, "rt")
  # replace non-letters with spaces
  text.cleansed <- gsub("[^[:alnum:]]", " ", text.cleansed)
  # remove leading and trailing spaces
  text.cleansed <- gsub("^\\s+|\\s+$", "", text.cleansed)
  # replace multiple spaces next to each other with single space
  text.cleansed <- gsub("\\s{2,}", " ", text.cleansed)
  return(text.cleansed)
}  

run <- function(text){
  load("/var/FastRWeb/web.R/rdata/fp_model.Rdata")
  # if it starts with RT or has " RT @" in it, it's junk to Raed
  if(grepl("^RT| RT @", text)){
    result <- 0
  }else{    
    text.cleansed <- clean.text(text)
    result <- textcat(text.cleansed, fp.model)
  }  
  if(is.na(result)){
    result.print <- "don't know..."
  }else{
    if(result=="1"){
      result.print <- "food poisoning tweet"
    }else{
      if(result=="0"){
        result.print <- "not a food poisoning tweet"
      }else{
        result.print <- "something is wrong..."
      }
    }
  }
  WebResult(result.print)
}
