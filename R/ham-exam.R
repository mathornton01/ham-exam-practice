# General Exam HAM questions Testing Code 

#setwd("../data/HAM-Questions")

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
  return(list(pvec,qidx,length(corans)==1))
} 

ContinuallyAskQs <- function(QDF,probvec = rep(1/nrow(QDF),nrow(QDF))){
  qcors <- rep(0,nrow(QDF)); 
  qasks <- rep(0,nrow(QDF));
  qpret <- askOneQ(QDF,probvec); 
  if (length(qpret)!=3){return(probvec)}
  qpvec <- qpret[[1]];
  qcors[qpret[[2]]] <- qcors[qpret[[2]]]+qpret[[3]];
  qasks[qpret[[2]]] <- qasks[qpret[[2]]]+1;
  
  while (length(qpret)==3){
    qpret <- askOneQ(QDF,qpvec);
    if (length(qpret) != 3){break;}
    qpvec <- qpret[[1]]
    qcors[qpret[[2]]] <- qcors[qpret[[2]]]+qpret[[3]];
    qasks[qpret[[2]]] <- qasks[qpret[[2]]]+1;
  }
  cat(paste("Of a total of ", sum(qasks), " questions asked, you correctly answered ", sum(qcors), " (", sum(qcors)/sum(qasks)*100, "%)"))
  cat((paste("Here is the breakdown of the questions asked and your hit ratio:\n")))
  for (qasked in which(qasks!=0)){
    cat(paste(QDF[qasked,1]," | ",QDF[qasked,2]," | Ans: ",qans[oldsolnidx]," | Time(s) asked: ",qasks[qasked], " | Time(s) correct: ",qcors[qasked]," | Hit ratio: ", qcors[qasked]/qasks[qasked], "\n"));
  }
  return(qpvec);
}

generalQs <- read.csv("genquest.csv",sep="|");
QDF <- generalQs;

numto <- 60; 
probvec <- c(rep(1,numto),rep(0,nrow(QDF)-numto))
probvec <- probvec/sum(probvec)

numrange <- 40:60; 
probvec <- rep(0,nrow(QDF));
probvec[numrange] <- 1; 
probvec <- probvec/sum(probvec);

numto <- nrow(QDF); 
probvec <- c(rep(1,numto),rep(0,nrow(QDF)-numto))
probvec <- probvec/sum(probvec)

probvecsaved <- ContinuallyAskQs(QDF,probvec);