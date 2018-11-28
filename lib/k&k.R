
f5 <- function(word){
  logic=FALSE
  if(nchar(word)<3){
    logic=FALSE
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
    u <- "[a-z]"
    m <- "[A-Z]"
    l <- sum(grepl(m,s[-c(1,nchar(word))]))
    if((grepl(u,s[1]))&&(grepl(u,s[nchar(word)]))&&(sum(l)>=1)){
      logic=TRUE
    }
  }
  return(logic)
}

f6 <- function(word){
  logic=FALSE
  if(nchar(word)<3){
    logic=FALSE
  }
  else{
    rr <- rle(strsplit(word,"")[[1]])
    maximum <- max(rr$lengths)
    if(maximum>=3){
      logic=TRUE
    }
  }
  return(logic)
}

f7 <- function(word){
  logic=FALSE
  if(nchar(word)==1){
    logic=FALSE
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
    u <- "[a-z]"
    m <- "[A-Z]"
    v <- "[a-zA-Z]"
    n1 <- grepl(u,s)
    n2 <- grepl(m,s)
    l <- (sum(n1)!=0)&(sum(n2)!=0)
    if((n2>n1)&&(l)){
      logic=TRUE
    }
  }
  return(logic)
}
f8 <- function(word){
  logic=FALSE
  if(nchar(word)>8){
    p <- "[[:alnum:]]"
    v <- "[a|e|i|o|u]"
    word <- tolower(word)
    s <- unlist(strsplit(word,split=NULL))
    l <- grepl(v,s)
    if(((nchar(word)-sum(l))>8*sum(l))||(nchar(word)-sum(l)<8*sum(l))){
      logic=TRUE
    }
    
  }
  return(logic)
}

f9 <- function(word){
  logic=FALSE
  if(nchar(word)<4){
    logic=FALSE
  }
  else{
    s <- unlist(strsplit(word,split=NULL))
    v <- "[a|e|i|o|u]"
    rr <- rle(strsplit(word,"")[[1]])
    maximum <- max(rr$lengths)
    index <- which.max(rr$lengths)
    value <- rr$values[[index]]
    l <- grepl(v,value)
    if((maximum>=4)&&(l)){
      logic=TRUE
    }
    if((maximum>=5)&&(!l)){
      logic =TRUE
    }
  }
  return(logic)
}

ifClean_kk <- function(cur_token){
  now <- 1
  if_clean <- TRUE
  
  ## in order to accelerate the computation, conduct ealy stopping
  rule_list <- c("length(unique(strsplit(gsub('[A-Za-z0-9]','',substr(cur_token, 2, nchar(cur_token)-1)),'')[[1]]))>1", #Ignoring the first and last characters in a string, if there are two or more different punctuation characters in thestring, it is garbage(Rule5)
                 "f5(cur_token)",# Rule6 
                 "nchar(cur_token)>20",#Rule7
                  "f6(cur_token)",#Rule8
                  "f7(cur_token)",#Rule9
                  "f8(cur_token)",#Rule10
                  "f9(cur_token)")#Rule11
  while((if_clean == TRUE)&now<=length(rule_list)){
    if(eval(parse(text = rule_list[now]))){
      if_clean <- FALSE
    }
    now <- now + 1
  }
  return(if_clean)
}