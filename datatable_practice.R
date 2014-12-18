install.packages("data.table")
install.packages("plyr")
library(plyr)
library(data.table)

set.seed(45)
DT <- data.table(rpois(1:20000, 10),V2=LETTERS[1:20],V3=round(rnorm(20000),4),
		     V4=1:20000)

DT

DT[V2=="A"]
DT[V2 %in% c("A","C")]

DT[,.(sum(V1),sd(V3))]

DT[,.(Sum = sum(V4)), by=.(V1,V2)]

DT[1:6,.(Sum = sum(V4)), by=.(V1,V2)]

DT[,.N,by=V2]

DT[,sum(V3),by=V2]
setkey(DT,V2)

DT["A"]
DT["T",mean(V3)]
DT["B"]
DT[.("B")]
DT[,lapply(.SD,mean),by=(V2)]

DT["A", mult="first"]
DT["A", mult="last"]

DT["D"]
DT["D", nomatch=0]

DT[c("A","C"),sum(V4)]

DT[c("A","C"),sum(V4)]

setkey(DT,V1,V2)

DT[.(2,"A")]

DT[,list(V1,V2)]
DT[.(2,c("A","B"))]

DT[,print(.SD)]

DT[,print(.SD), by=V1]

DT[,V4.Sum:=sum(V4),by=V1][V4.Sum > 40]
DT[,sum(V3), by=V2][V1 > 25]
DT[,V3.Sum:=sum(V3),by=V1][V3.Sum > 0][order(V3)]
DT[,V4.Sum:=sum(V4),by=V1][order(-V1)]
