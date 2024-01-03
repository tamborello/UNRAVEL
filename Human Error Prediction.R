# Author: Frank Tamborello
# CC-BY-SA 2014 Frank Tamborello
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of Creative Commons Attribute-ShareAlike 4.0 International License: http://creativecommons.org/licenses/by-sa/4.0/
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
# Description: Human Error Predictor Project - Import multiple data files, draw scatterplots, apply logistic regression, perform signal detection analysis on predicted logist given data.
#
# Acknowledgment: This work was performed at the U. S. Naval Research Laboratory, Washington, DC, with support from a National Research Council Research Associateship Award from the National Academy of Sciences.


# Perform analyses separately for each step because the context will be different at each step. Return probability of skip given interruption at that step.

require(Hmisc) 
require(ROCR)
source("model.helper.r") # unfortunately not available for release
newline <- function() {writeLines("\r")}

# Import Skip Data and run a logistic regression on it

seq_err.df <- read.delim(file="~/skip-data-99.txt", row.names=NULL, na.strings="nil", colClasses="character")
#=c("factor", "factor", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# Import multiple data files
se.df <- NULL
for (i in 0:7) {
		se.df <- rbind(se.df, read.delim(file=paste("~/skip-data-", i, ".txt", sep=""), row.names=NULL, na.strings="nil", colClasses="character"))	
	}
	
# Name correction
# Read.delim doesn't seem to behave as advertised, it inserts "row.names" into the beginning of the names vector and adds an empty column to the end of the data frame.
names(seq_err.df) <- names(seq_err.df[2:length(names(seq_err.df))])	

names(se.df) <- names(se.df)[2:length(names(se.df))]

# Remove the last column, which is empty
seq_err.df <- seq_err.df[,1:dim(seq_err.df)[2]-1]

se.df <- se.df[,1:dim(se.df)[2]-1]

for (v in 4:dim(seq_err.df)[2]) {seq_err.df[,v] <- as.numeric(seq_err.df[,v])}
for (v in 4:dim(se.df)[2]) {se.df[,v] <- as.numeric(se.df[,v])}

se.df.save <- se.df


# First, plot all data!
# x=ep0, y=ep1, point type=outcome
# Repeats
plot(x=NULL, y=NULL, xlab="ep0", ylab="Repeat", xlim=c(min(se.df$ep0, na.rm=T), max(se.df$ep0, na.rm=T)), ylim=c(min(se.df$Repeat, na.rm=T), max(se.df$ep1, na.rm=T)))

plot(x=NULL, y=NULL, xlab="ep0", ylab="ep1", xlim=c(min(se.df$ep0, na.rm=T), 2), ylim=c(min(se.df$ep1, na.rm=T), max(se.df$ep1, na.rm=T)))

points(se.df$ep0[se.df$Repeat==0], se.df$ep1[se.df$Repeat==0], pch=15, col="blue")
points(se.df$ep0[se.df$Repeat==1], se.df$ep1[se.df$Repeat==1], pch=16, col="red")

abline(a=-.571, b=-.198, lwd=2)
abline(a=NULL, b=0, v=.344, lty=1)
abline(a=NULL, b=0, v=2, lty=2)
legend(1.25, 65, legend=c("Rpt0", "Rpt1"), pch=c(15, 16), col=c("blue", "red"))
legend(1.25, 55, legend=c("AP LR", "AP Crit", "PH Crit"), lty=c(1, 1, 2), lwd=c(2, 1, 1))


# ep0 is the only predictor
plot(x=NULL, y=NULL, xlab="ep0", ylab="Perseveration", xlim=c(min(interrupt$EP0, na.rm=T), max(interrupt$EP0, na.rm=T)), ylim=c(0, 1))

plot(x=NULL, y=NULL, xlab="ep0", ylab="", xlim=c(min(se.df$ep0, na.rm=T), .8), ylim=c(0, 0))

points(interrupt$EP0[interrupt$REPEAT==0], rep(0, length(interrupt$EP0[interrupt$REPEAT==0])), pch=15, col="blue")
points(interrupt$EP0[interrupt$REPEAT==1], rep(0, length(interrupt$EP0[interrupt$REPEAT==1])), pch=16, col="red")

abline(a=-.571, b=-.198, lwd=2)
abline(a=NULL, b=0, v=.344, lty=1)
abline(a=NULL, b=0, v=, lty=2)
legend(1.5, 1, legend=c("Rpt0", "Rpt1"), pch=c(15, 16), col=c("blue", "red"))
legend(1.5, .5, legend=c("AP LR", "AP Crit", "PH Crit"), lty=c(1, 1, 2), lwd=c(2, 1, 1))




length(na.omit(se.df$ep0)[na.omit(se.df$ep0) < 5])


table(na.omit(se.df$Repeat[se.df$ep0<2]))

mean(c(median(na.omit(se.df$ep0)[na.omit(se.df$ep0) < 5]),
min(na.omit(se.df$ep0)[na.omit(se.df$ep0) > 5])))
# Suggests predictor should = 1 whenever 0.17 < ep0 < 8.06, such as 4.11. Were AUC & d' really any better with ep1 than using ep0 alone?






seq_err.df <- se.df


# Predictions

# Repeats
# Copy DV to a new "error" object because model.helper.r hard-codes that name
seq_err.df$error <- seq_err.df$REPEAT
se.df$error <- se.df$REPEAT + se.df$SKIP

interrupt <- seq_err.df[seq_err.df$IntType!="BSLN",]
interrupt <- se.df[se.df$IntType!="BSLN",]

dim(interrupt)
dim(interrupt[is.na(interrupt$EP0),])
dim(interrupt[!is.na(interrupt$EP0),])

interrupt <- interrupt[!is.na(interrupt$EP0),]

fit.glm <- glm(error~RESUMPTIONLATENCY, data=interrupt, family="binomial")
fit.glm <- glm(error~ep1, data=interrupt, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, interrupt, print.model = TRUE)

# Plot it
plot(performance(prediction(GetGLMPrediction(fit.glm, interrupt), interrupt$error), "tpr", "fpr"), xlab="FPR", ylab="TPR")
abline(a=0, b=1, lty=2)
GetROCInfo(GetGLMPrediction(fit.glm, interrupt), interrupt$error, "f")


# Try the cutoff value returned by GetROCInfo
pred <- rep(0, dim(interrupt)[1])
pred[-.0526 - (.268 * interrupt$ep0) > .418] <- 1 # f-best
pred[se.df$ep0 < .418] <- 1 # inter-ocular trauma test

perf <- performance(prediction(interrupt$RptPrd, interrupt$Repeat), "tpr", "fpr")
# str(perf)
plot(perf, xlab="FPR", ylab="TPR")
abline(a=0, b=1, lty=2)
GetROCInfo(interrupt$RptPrd, interrupt$Repeat, "f")


pRepeat <- 1 / (1 + exp(-1 * (-.0526 - (.268 * interrupt$ep0))))
pRepeat[interrupt$ep0 < 1]
pRepeat[interrupt$ep0 >=1]
PredRepeat <- pRepeat > .418
cbind(interrupt$ep0, pRepeat, PredRepeat)[PredRepeat==1,]


# Skips
# Copy DV to a new "error" object because model.helper.r hard-codes that name
seq_err.df$error <- seq_err.df$Skip
se.df$error <- se.df$Skip
interrupt <- seq_err.df[seq_err.df$IntType!="BSLN",]


dim(se.df[is.na(se.df$cp0sa),])

fit.glm <- glm(error~GSA, data=se.df, family="binomial")
fit.glm <- glm(error~cp0sa, data=se.df, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, se.df, print.model = TRUE)

pSkip <- 1 / (1 + exp(-1 * (.173 - (.748 * se.df$cp0sa) - (3.10 * se.df$cp0bla) + (2.86 * se.df$cp1bla))))
PredSkip <- pSkip > .246
cbind(se.df$cp0sa, se.df$cp0bla, se.df$cp1bla, pSkip, PredSkip)[PredSkip==1,]
se.df[se.df$cp0sa<3,] # Now that's interesting! Whenever cp0sa < 3, the model always committed either a repeat or a skip! All of these trial types were resumptions. However, there are only 38 such trials out of 10,527. I think these must be instances wherein the model retrieved some episode from the distant past, which makes this not interesting after all.
dim(se.df)
se.df[is.na(se.df$ep0) & se.df$IntType!="BSLN",]


plot(performance(prediction(GetGLMPrediction(fit.glm, se.df), se.df$Skip), "tpr", "fpr"), xlab="FPR", ylab="TPR")
abline(a=0, b=1, lty=2)
GetROCInfo(GetGLMPrediction(fit.glm, se.df), se.df$Skip, "f")


fit.glm <- glm(error~cp0sa+cp1sa+cp2sa+cp3sa+cp0bla+cp1bla+cp2bla+cp3bla, data=se.df, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, se.df, print.model = TRUE)

fit.glm <- glm(error~cp0sa+cp0bla, data=se.df, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, se.df, print.model = TRUE)

fit.glm <- glm(error~GSA, data=se.df, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, se.df, print.model = TRUE)




fit.glm <- glm(error~GSA+cp0sa+cp1sa+cp2sa, data=interrupt, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, interrupt, print.model = TRUE)

seq_err.df$SAdiff <- seq_err.df$cp0sa - seq_err.df$cp1sa
interrupt <- seq_err.df[seq_err.df$IntType!="BSLN",]
fit.glm <- glm(error~GSA, data=seq_err.df, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, seq_err.df, print.model = TRUE)
fit.glm <- glm(error~GSA, data=interrupt, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, interrupt, print.model = TRUE)


# Revise skip calculations, exclude cases wherein wrong episodic chunk is retrieved
revised.skip <- se.df[se.df$ep0>2 | is.na(se.df$ep0),]
revised.skip.rejects <- se.df[se.df$ep0<=2 & !is.na(se.df$ep0),]

revised.skip$error <- revised.skip$Skip
fit.glm <- glm(error~GSA, data=revised.skip, family="binomial")
summary(fit.glm)
ModelOutput(fit.glm, revised.skip, print.model = TRUE)



# Plot model's own generated predictions against its own performance (skips)
plot(performance(prediction(se.df$SkpPrd, se.df$Skip), "tpr", "fpr"), xlab="False Positive Rate", ylab="True Positive Rate")
abline(a=0, b=1, lty=2)
GetROCInfo(se.df$SkpPrd, se.df$Skip, "f")





# Optimal Cut Point
require(OptimalCutpoints)
optimal.cutpoints(Repeat ~ ep0, data=se.df, methods="Youden", tag.healthy="Repeat")
