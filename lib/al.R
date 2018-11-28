##############################
## Garbage detection
## Ref: first three rules in the paper
##      'On Retrieving Legal Files: Shortening Documents and Weeding Out Garbage'
## Input: one word -- token
## Output: bool -- if the token is clean or not
##############################
f1 <- function(word){
  logic = FALSE
  p <- "[^[:alnum:]]"
  if(nchar(word)==1){
    s <- word
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
  }
  number <- sum(grepl(p,s))
  if((number/nchar(word))>0.5){
    logic = TRUE
  }
  return(logic)
}
f2 <- function(word){
  logic=FALSE
  if(nchar(word)<4){
    logic=FALSE
  }
  else{
    rr <- rle(strsplit(word,"")[[1]])
    maximum <- max(rr$lengths)
    if(maximum>=4){
      logic=TRUE
    }
  }
  return(logic)
}

f3 <- function(word){
  logic=FALSE
  if(nchar(word)>10){
  p <- "[[:alnum:]]"
  v <- "[a|e|i|o|u]"
  word <- tolower(word)
  s <- unlist(strsplit(word,split=NULL))
  l <- grepl(v,s)
  if(((nchar(word)-sum(l))>10*sum(l))||(nchar(word)-sum(l))<10*sum(l)){
    logic=TRUE
  }
  
  }
  return(logic)
}
f4 <- function(word){
  logic=FALSE
  if(nchar(word)<3){
    logic=FALSE
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
    u <- "[a-z]"
    m <- "[A-Z]"
    l <- sum(grepl(m,s[-c(1,nchar(word))]))
    if((grepl(u,s[1])==TRUE)&&(grepl(u,s[nchar(word)])==TRUE)&&(sum(l)>=1)){
      logic=TRUE
    }
  }
  return(logic)
}


ifClean_al <- function(cur_token){
  now <- 1
  if_clean <- TRUE
  
  ## in order to accelerate the computation, conduct ealy stopping
  rule_list <- c("length(unique(strsplit(gsub('[A-Za-z0-9]','',substr(cur_token, 2, nchar(cur_token)-1)),'')[[1]]))>1", #Ignoring the first and last characters in a string, if there are two or more different punctuation characters in thestring, it is garbage
                 "nchar(cur_token)>40","f1(cur_token)","f2(cur_token)","f3(cur_token)","f4(cur_token)") #A string composed of more than 40 symbols is garbage 
  while((if_clean == TRUE)&now<=length(rule_list)){
    if(eval(parse(text = rule_list[now]))){
      if_clean <- FALSE
    }
    now <- now + 1
  }
  return(if_clean)
}