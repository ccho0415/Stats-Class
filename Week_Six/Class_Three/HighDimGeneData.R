

# High dim gene expression level data 
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data


#Examine data (64 cell lines labeled with cancer type)
nci.labs[1:4]
table(nci.labs)

#Scale variables (genes) to have SD=1
pr.out=prcomp(nci.data, scale=TRUE)


#Assign distinct color to each cancer type (rainbow())
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}


#Plot principal component score vectors
par(mfrow=c(1,1))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
legend("bottomright",legend=levels(factor(nci.labs)), fill = 1:length(levels(factor(nci.labs))),cex = .5)


par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3")

#Note:cell lines from same cancer type tend 
#     to have similar gene expression levels




