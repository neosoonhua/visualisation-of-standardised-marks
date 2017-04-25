rm(list=ls(all=TRUE))	#remove all objects

data.raw = read.csv("Raw marks.csv", head=T)
tail(data.raw)
names(data.raw)

X = data.raw[,c(4:15)]

#are there any missing values?
#use apply on matrices to apply a function to columns
apply(is.na(X), 2, sum)

meanScore = apply(X, 2, mean, na.rm=TRUE)
meanScore
stdev = apply(X, 2, sd, na.rm=TRUE) 
#Note that sd uses n-1 and not n.
stdev

#To get min and max after standardising
nvar = dim(X)[2]	#No. of variables
nvar

for(i in 1:nvar) {
	X[,i] = (X[,i] - meanScore[i])/stdev[i]	#standardising
}

(least = min(X, na.rm=TRUE))
(most = max(X, na.rm=TRUE))

subjClasses = unique(data.raw$Subj.Class)
subjClassesCount = length(subjClasses)
subjClassesCount

for(j in 1:subjClassesCount) {
  subjClass = subjClasses[j]

perSubjClass = data.raw[which(data.raw$Subj.Class==subjClass),]	#Rows to use
columnsWithMarks = perSubjClass[,c(4:15)]

nvar = dim(columnsWithMarks)[2]	#No. of variables
nvar
nrow = dim(columnsWithMarks)[1]	#No. of rows
nrow
namesVar = names(columnsWithMarks)

#Standardising
for(i in 1:nvar) {
	columnsWithMarks[,i] = (columnsWithMarks[,i] - meanScore[i])/stdev[i]
}

pdf(paste(subjClass, ".pdf", sep=""))

#Plots for each student in this subject class
for(row in 1:nrow){
  plot(t(columnsWithMarks[row,]), main=perSubjClass[row,"Name"], 
	ylim = c(least, most),
	ylab = "Standardised marks obtained", 
	xlab = "Q1 to Q11. 12 is for the whole paper.",
	type="p")
  text(t(columnsWithMarks[row,]), namesVar, adj=c(0.5,1.5))
  abline(h=seq(-3,3), col="red")	#Red horizontal lines

  #par(ask = TRUE) #Pauses until "Enter" key is hit
}
dev.off()	#This signifies the end of outputting to pdf file.

} #End of for(j in 1:subjClassesCount)