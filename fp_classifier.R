
library(textcat)
library(stringr)

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
    result.print <- "dont_know"
  }else{
    if(result=="1"){
      result.print <- "food_poisoning"
    }else{
      if(result=="0"){
        result.print <- "not_food_poisoning"
      }else{
        result.print <- "classifier_failed"
      }
    }
  }
  WebResult(result.print)
}
