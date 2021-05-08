# General Exam HAM questions Testing Code 

setwd("C:/Users/matho/Desktop/HAM-Questions")

askOneQ <- function(QDF,probvec = rep(1/nrow(QDF),nrow(QDF))){
  qidx <- sample(1:nrow(QDF),1,prob=probvec); 
  
  qans1 <- substr(as.character(QDF[qidx,3]),3,nchar(as.character(QDF[qidx,3])))
  qans2 <- substr(as.character(QDF[qidx,4]),3,nchar(as.character(QDF[qidx,4])))
  qans3 <- substr(as.character(QDF[qidx,5]),3,nchar(as.character(QDF[qidx,5])))
  qans4 <- substr(as.character(QDF[qidx,6]),3,nchar(as.character(QDF[qidx,6])))
  
  qans <- c(qans1,qans2,qans3,qans4)
  qmix <- sample(qans)
  
  oldsolnval <- QDF[qidx,7];
  if (oldsolnval == "(A)"){oldsolnidx <- 1;}
  if (oldsolnval == "(B)"){oldsolnidx <- 2;}
  if (oldsolnval == "(C)"){oldsolnidx <- 3;}
  if (oldsolnval == "(D)"){oldsolnidx <- 4;}
  
  newsolnval <- c("(A)","(B)","(C)","(D)")[which(qans[oldsolnidx] == qmix)];
  
  cat(paste(as.character(QDF[qidx,1]),"|", as.character(QDF[qidx,2])),"\n");
  cat(paste("\t","A.",qmix[1],"\n"));
  cat(paste("\t","B.",qmix[2],"\n"));
  cat(paste("\t","C.",qmix[3],"\n"));
  cat(paste("\t","D.",qmix[4],"\n"));
  
  ans <- readline("Please Enter letter of answer (A-D) [Q to quit]: ")
  while(!(toupper(ans) %in% c("A","B","C","D","Q"))){
    ans <- readline("Please Enter letter of answer (A-D) [Q to quit]: ")
  }
  if(toupper(ans) == "Q"){
    return(list(qu=1,probvec))
  }
  corans <- grep(toupper(ans),newsolnval);
  pvec <- probvec*nrow(QDF); 
  if (length(corans) != 1){
    cat(paste("Sorry, that is incorrect, upweighting selection probability \n The correct solution was: ", newsolnval, qans[oldsolnidx],"\n"))
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
QDF <- generalQs;

numto <- 45; 
probvec <- c(rep(1,numto),rep(0,nrow(QDF)-numto))
probvec <- probvec/sum(probvec)

ContinuallyAskQs(QDF,probvec)