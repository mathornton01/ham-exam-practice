# General Exam HAM questions Testing Code 

setwd("C:/Users/matho/Desktop/HAM-Questions")

askOneQ <- function(QDF,probvec = rep(1/nrow(QDF),nrow(QDF))){
  qidx <- sample(1:nrow(QDF),1,prob=probvec); 
  cat(paste(as.character(QDF[qidx,1]),"|", as.character(QDF[qidx,2])),"\n");
  cat(paste("\t", as.character(QDF[qidx,3])),"\n");
  cat(paste("\t", as.character(QDF[qidx,4])),"\n");
  cat(paste("\t", as.character(QDF[qidx,5])),"\n");
  cat(paste("\t", as.character(QDF[qidx,6])),"\n");
  ans <- readline("Please Enter letter of answer (A-D) [Q to quit]: ")
  while(!(toupper(ans) %in% c("A","B","C","D","Q"))){
    ans <- readline("Please Enter letter of answer (A-D) [Q to quit]: ")
  }
  if(toupper(ans) == "Q"){
    return(list(qu=1,probvec))
  }
  corans <- grep(toupper(ans),QDF[qidx,7]);
  pvec <- probvec*nrow(QDF); 
  if (length(corans) != 1){
    cat("Sorry, that is incorrect, upweighting selection probability\n")
    pvec[qidx] <- pvec[qidx]+(0.5*pvec[qidx]);
  } else if (corans == 1){
    cat("Correct Answer, Downweighting selection probability\n"); 
    pvec[qidx] <- pvec[qidx]-(0.5*pvec[qidx]);
  }
  pvec <- pvec/sum(pvec);
  return(pvec)
} 

ContinuallyAskQs <- function(QDF,probvec = rep(1/nrow(QDF),nrow(QDF))){
  qpvec <- askOneQ(QDF,probvec); 
  while (!is.list(qpvec)){
    qpvec <- askOneQ(QDF,qpvec);
  }
  print("Most Missed Questions: ")
  print(as.character(QDF[order(qpvec[[2]]) %in% (nrow(QDF)-30):nrow(QDF),1]));
  return(qpvec[[2]]);
}

generalQs <- read.csv("genquest.csv",sep="|");

ContinuallyAskQs(QDF)