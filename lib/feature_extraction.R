library(dplyr)
library(tidytext)
library(janeaustenr)


find_feature1 <- function(word){
  return(nchar(word))
}
find_feature2 <- function(word){
  v <- "[a|e|i|o|u]"
  word <- tolower(word)
  if(nchar(word)==1){
    s <- word
  }
  else{
  s <- unlist(strsplit(word,split=NULL))
  }
  number  <- sum(grepl(v,s))
  return(number/length(s))
}
find_feature3 <- function(word){
  p <- "[^[:alnum:]]"
  if(nchar(word)==1){
    s <- word
  }
  else{
  s <- unlist(strsplit(word,split=NULL))
  }
  number <- sum(grepl(p,s))
  return(number/length(s))
}
find_feature4 <- function(word){
  n <- "[0-9]"
  if(nchar(word)==1){
    s <- word
  }
  else{
  s <- unlist(strsplit(word,split=NULL))
  }
  number <- sum(grepl(n,s))
  return(number/length(s))
}
find_feature5 <- function(word){
  u <- "[A-Z]"
  if(nchar(word)==1){
    s <- word
  }
  else{
  s <- unlist(strsplit(word,split=NULL))
  }
  number <- sum(grepl(u,s))
  return(number/length(s))
}
find_feature6 <- function(word){
if(nchar(word)<3){
  value=0
}
else{
  rr <- rle(strsplit(word,"")[[1]])
  maximum <- max(rr$lengths)
  if(maximum<3){
    value = 0
  }
  else{
    value = maximum/nchar(word)
  }
}
return(value)
} ##Need to modify for the letter 

find_feature7 <- function(word){
  p <- "[[:alnum:]]"
  if(nchar(word)==1){
    s <- word
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
  }
  l <- grepl(p,s)
  return(ifelse(sum(l)/nchar(word)>0.5,0,1)) 
}

find_feature8 <- function(word){
  p <- "[a|e|i|o|u|A|E|I|O|U]"
  if(nchar(word)<6){
    value=0
  }
  else{
  s <- unlist(strsplit(word,split=NULL))
  new_word  <- ifelse(grepl(p,s),0,1)
  rr <- rle(new_word)
  if(sum(new_word)==0){
    value=0
  }
  else{
  maximum <- max(rr$lengths[rr$values==1])
  if(maximum >=6){
    value=1
  }
  else{value=0}
  }
  }
  return(value)
}

find_feature9 <- function(word){
  if(nchar(word)<=3){
    value=0
  }
  else{
    p <- "[^[:alnum:]]"
    s <- unlist(strsplit(word,split=NULL))
    s <- s[-c(1,length(s))]
    l <- grepl(p,s)
    if(sum(l)>1){
      value = 1
    }
    else{value=0}
  }
  return(value)
}

find_tokens <- function(word){
  w <- strsplit(word, "", fixed = TRUE)[[1L]]
  return((vapply(ngrams(w, 2L), paste, "", collapse = " ")))
  
}


find_feature10 <- function(word,LB){
  if(nchar(word)==1){
    value = 0
  }
  else{
    t <-find_tokens(word)
    l <- length(t)
    count=rep(NA,l)
    for(i in 1:l){
      test <- t[i]
      count[i] = sum(test==LB)
    }
    value = sum(count)/(10000*l)
  }
  return(value)
}

find_feature11 <- function(word){
  if(nchar(word)==1){
    s <- word
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
  }
  i <- max(table(s))
  if(i<=2){
    value=0
  }
  else{
    value = i/nchar(word)
  }
  return(value)
}

find_feature12 <- function(word){
  if(nchar(word)==1){
    s <- word
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
  }
  p <- "[[:alpha:]]"
  l1 <- sum(grepl(p,s))
  l2 <- length(s)-l1
  return(l2/(l1+1))
}

find_feature13 <- function(word,LB){
  distance <- stringdist(word,LB)
  if(min(distance)>2){
    value = nchar(word)
  }
  else{
    index = which(distance==min(distance))
    target = LB[index]
    v = min(distance)
    logic = (nchar(word)==nchar(target))&&(v==1)
    if(logic){
      p = 1
    }
    else{ p = 2}
    value = (v+p/2+1)/nchar(word)
  }
  return(value)
}







p <- "[[:punct:]]"
