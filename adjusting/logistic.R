library(randomForest)
library(beepr)
df<-read.csv("data/adult.txt",stringsAsFactors=T,sep=",")

# initial hypothesis and contexted data
atgt<-"income"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

# visualize
# plot(df.ctx[,atgt]~df.ctx[,acmp])
# plot(df.ctx[,acmp]~df.ctx[,atgt])

# context mining
# tgt.mod<-randomForest(df.ctx[,atgt]~.,data=subset(df.ctx,select=-occupation),importance=T)
# cmp.mod<-randomForest(df.ctx[,acmp]~.,data=subset(df.ctx,select=-income),importance=T)
# beep()

# let's just consider the known results
# use gender

# 2 steps:
# 1. adjust for the confounders using a linear model
# 2. generate new dataset: this is essentially getting the yhat's

# to adjust for confounding, use a linear model
# if atgt is numerical, use OLS
# if atgt is categorical, use logistic
# need to consider counts (Poisson)

log.mod<-glm(df.ctx[,atgt]~df.ctx[,acmp]+df.ctx$sex+df.ctx$education+df.ctx$workclass,
             family=binomial(link=logit))
yhat<-fitted(log.mod)

# convert probabilities to actual predicted classes
# that reflect adjusting, use Bayes Theorem

# first, we need the prior probabilities:
prior.pos<-table(df.ctx[,atgt])[1]/(sum(table(df.ctx[,atgt])))
prior.neg<-1-prior.pos
names(prior.pos)<-names(prior.neg)<-NULL

# next, the likelihood
# lh.pos is a vector of conditional probabilities
# for each sample to observe their logistic regression o/p,
# given their prior class
initially.pos.idx<-which(df.ctx[,atgt] == " >50K")
initially.neg.idx<-which(df.ctx[,atgt] == " <=50K")
# split the logistic regression o/p into two groups
yhat.pos<-yhat[initially.pos.idx]
yhat.neg<-yhat[initially.neg.idx]

# likelihood
# lh.pos is the vector of upper bounds of
# likelihoods of observing a particular sample,
# or a more extreme value
# lh.neg is similiarly defined
lh.pos<-NULL
lh.neg<-NULL

for(i in seq(length(yhat.pos))){
  lh.pos<-c(lh.pos,mean(yhat.pos[i] - yhat.pos >= 0))
  lh.pos<-lh.pos/mean(yhat.pos[i] - yhat == 0)
}
for(i in seq(length(yhat.neg))){
  lh.neg<-c(lh.neg,mean(yhat.neg[i] - yhat.neg <= 0))
  lh.neg<-lh.neg/mean(yhat.neg[i] - yhat == 0)
}

# posterior probabilities
pr.class.pos<-lh.pos*prior.pos # length of 1436
pr.class.neg<-lh.neg*prior.neg # length of 6433

# checking whether above 0.5
# if above, sample is classified as positive
# else, negative
# this is for pr.class.pos only
# converse for pr.class.neg

pos.to.pos<-which(pr.class.pos >= 0.5)
pos.to.neg<-which(pr.class.pos < 0.5)

neg.to.pos<-which(pr.class.neg < 0.5)
neg.to.neg<-which(pr.class.neg >= 0.5)

str(pos.to.pos)
str(pos.to.neg)
str(neg.to.pos)
str(neg.to.neg)

# use logistic regression for categorical Atgt to model
# probabilities, before and after addition of confounder



# pr.class.pos.pos<-lh.pos.pos*prior.pos
# pr.class.pos.neg<-lh.pos.neg*prior.neg
# pr.class.neg.pos<-lh.neg.pos*prior.pos
# pr.class.neg.neg<-lh.neg.neg*prior.neg
# 
# class.diff.pos<-pr.class.pos.pos-pr.class.pos.neg
# class.diff.neg<-pr.class.neg.pos-pr.class.neg.neg
# 
# hist(class.diff.pos)
# hist(class.diff.neg)
# 
# # bootstrap sample from the individual empirical distributions
# # to get a p-value of observing this value or a more extreme value
# # bootstrap.fun.pos<-function(obs,vec){
# #   l<-length(vec)
# #   extreme.counts<-0
# #   for(i in seq(1000)){
# #     draw<-vec[round(runif(n=1,min=1,max=l))]
# #     if(draw >= obs) extreme.counts<-extreme.counts+1
# #   }
# #   return(extreme.counts/1000)
# # }
# # bootstrap.fun.neg<-function(obs,vec){
# #   l<-length(vec)
# #   extreme.counts<-0
# #   for(i in seq(1000)){
# #     draw<-vec[round(runif(n=1,min=1,max=l))]
# #     if(draw < obs) extreme.counts<-extreme.counts+1
# #   }
# #   return(extreme.counts/1000)
# # }
# 
# # lh.pos<-sapply(yhat.pos,FUN=bootstrap.fun,vec=yhat.pos)
# # lh.neg<-sapply(yhat.neg,FUN=bootstrap.fun,vec=yhat.neg)
# 
# # pr.class.pos is a vector of posterior probabilities
# # UPPER BOUNDS for each sample to be positive
# # likewise for pr.class.neg
# pr.class.pos<-lh.pos*prior.pos
# pr.class.neg<-lh.pos*prior.neg
# # take difference: if < 0, sample belong in negative class
# # else, positive class
# class.diff<-pr.class.pos-pr.class.neg
# 
# 
# 
# # since yhat are the predicted probabilities of
# # a subject's income being high, need to convert
# # to actual prediction
# # naturally, use a threshold of 0.5
# 
# #yhat.actual<-sapply(yhat,FUN=function(x){if(x>=0.5) return(1); return(0)})
# # 
# # pred<-prediction(yhat,df.ctx[,atgt])
# # perf <- performance(pred,"tpr","fpr")
# # plot(perf,colorize=TRUE)
# # 
# # 
# # perf1 <- performance(pred, "prec", "rec")
# # plot(perf1,colorize=TRUE)
# # ## sensitivity/specificity curve (x-axis: specificity,
# # ## y-axis: sensitivity)
# # perf1 <- performance(pred, "sens", "spec")
# # plot(perf1,colorize=TRUE)
# 
# 
# true.class<-as.numeric(df.ctx[,atgt])-1
# perf = function(cut, fitted, true.class)
# {
#   yhat = (fitted>cut)
#   w = which(true.class==1)
#   sensitivity = mean( yhat[w] == 1 ) 
#   specificity = mean( yhat[-w] == 0 ) 
#   c.rate = mean( true.class==yhat ) 
#   d = cbind(sensitivity,specificity)-c(1,1)
#   d = sqrt( d[1]^2 + d[2]^2 ) 
#   out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
#   colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
#   return(out)
# }
# 
# s = seq(.01,.99,length=1000)
# OUT = matrix(0,1000,4)
# for(i in 1:1000) OUT[i,]=perf(s[i],yhat,true.class)
# plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
# axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# lines(s,OUT[,2],col="darkgreen",lwd=2)
# lines(s,OUT[,3],col=4,lwd=2)
# lines(s,OUT[,4],col="darkred",lwd=2)
# box()
# legend(0.5,.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))
# # 
# # plot.roc(df.ctx[, atgt], yhat, col="blue",
# #          lwd=3, print.auc=TRUE,print.auc.y = 0.3)
