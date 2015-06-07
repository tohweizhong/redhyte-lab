library(randomForest)
library(beepr)
df<-read.csv("data/adult.txt",stringsAsFactors=T,sep=",")

# categorical target attribute
atgt<-"income"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

par(mfrow=c(1,2))
# use null module to generate probabilities
null.mod<-glm(df.ctx[,atgt]~df.ctx[,acmp],
              family=binomial(link=logit))
yhat.null<-fitted(null.mod)

df.null<-data.frame(cbind(yhat.null,df.ctx))
df.null<-df.null[order(df.null$yhat.null),]
plot(df.null$yhat.null,col=as.numeric(df.ctx$occupation))  

# adjusting for confounders
fm<-"df.ctx[,atgt]~df.ctx[,acmp]+df.ctx$sex+df.ctx$education+df.ctx$workclass+df.ctx$relationship+df.ctx$education.num"
cf.mod<-glm(as.formula(fm),
             family=binomial(link=logit))
yhat.cf<-fitted(cf.mod)

df.adj<-data.frame(cbind(yhat.cf,df.null$occupation))
colnames(df.adj)<-c("yhat.cf","occupation")
df.adj<-df.adj[order(df.adj$yhat.cf),]


plot(df.adj$yhat.cf,col=as.numeric(df.adj$occupation))
  
# take difference in probabilities
diff<-df.adj$yhat.cf-df.null$yhat.null
# t-test 

#plot(yhat.cf~df.ctx[,acmp])

# tests
initial.t<-t.test(df.null$yhat.null~df.null$occupation)
adj.t<-t.test(df.adj$yhat.cf~df.adj$occupation)




# numerical target attribute
atgt<-"age"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx2<-df[rows,]
df.ctx2<-droplevels(df.ctx2)

# ===
# 200515
# sort samples by Atgt
# plot
par(mfrow=c(1,2))
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

df.ctx3<-df.ctx2
df.ctx3<-df.ctx3[order(df.ctx3$age),]
plot(df.ctx3$age,col=as.numeric(df.ctx3$occupation))

# get adjusted values
mod<-lm(data=df.ctx3,age~occupation+sex+education+workclass+relationship+education.num)
yhat<-fitted(mod)

df.adj<-data.frame(cbind(yhat,df.ctx3$occupation,df.ctx3$sex))
colnames(df.adj)<-c("yhat","occupation","sex")
df.adj<-df.adj[order(df.adj$yhat),]
plot(df.adj$yhat,col=as.numeric(df.adj$occupation))

# t-test
initial.t<-t.test(df.ctx3$age~df.ctx$occupation)
adj.t<-t.test(df.adj$yhat~df.adj$occupation)


# use bxp
plot(df.ctx3$age~df.ctx3$occupation)
plot(df.adj$yhat~factor(df.adj$occupation))
# t-test becomes test on lm coefficient
