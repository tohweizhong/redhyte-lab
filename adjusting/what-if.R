
# ================ #
# PROOF OF CONCEPT
# ================ #


library(beepr)
df<-read.csv("data/adult.txt",stringsAsFactors=T,sep=",")

# initial hypothesis
# Atgt: income
# Acmp: occupation, Adm-clerical vs. craft-repair
# Actx: -

# categorical target attribute
atgt<-"income"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

# # consider only Whites (as in example in thesis, Results section)
# rows<-which(df.ctx$race == " White")
# df.ctx<-df.ctx[rows,]

# drop unneccessary variables
df.ctx<-df.ctx[,c("income","occupation",
                  "sex","marital.status","relationship","education","workclass")]

# stepwise regression with logistic regression
# to select from the Actx*
primary.mod<-glm(income~.,data=df.ctx,
                 family=binomial(link=logit))
step.mod<-step(primary.mod,direction="backward")

# logistic model
# need to consider second order terms
# in order to observe Simpson's Reversals

# the following doesn't work
# fm<-"df.ctx$income~df.ctx$occupation+df.ctx$sex+df.ctx$education+df.ctx$workclass+df.ctx$relationship"
# mod<-glm(as.formula(fm),family=binomial(link=logit),data=df.ctx)
# mod<-glm(income ~ occupation + sex + marital.status + relationship + education + workclass,
#           family=binomial(link=logit),data=df.ctx)
source('~/R/redhyte-lab/adjusting/generate_itr_formula.R', echo=TRUE)
mod<-glm(itr.formula(vec=colnames(df.ctx)[-1],tgt=atgt),
         family=binomial(link=logit),data=df.ctx)
beep()
# ====

# "what if" analysis:
# consider certain combination of values for mined Actx

# newdata
newdata1<-data.frame(
  occupation=" Adm-clerical",
  sex=" Male",
  marital.status=" Married-civ-spouse",
  relationship=" Husband",
  education=" 7th-8th",
  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE)
newdata1<-rbind(newdata1,
                data.frame(
                  occupation=" Craft-repair",
                  sex=" Male",
                  marital.status=" Married-civ-spouse",
                  relationship=" Husband",
                  education=" 7th-8th",
                  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE))
newdata1$prob<-predict(mod,newdata1,type="response")

# additional sets of "what-if"s:
newdata2<-data.frame(
  occupation=" Adm-clerical",
  sex=" Male",
  marital.status=" Never-married",
  relationship=" Unmarried",
  education=" Bachelors",
  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE)
newdata2<-rbind(newdata2,
                data.frame(
                  occupation=" Craft-repair",
                  sex=" Male",
                  marital.status=" Never-married",
                  relationship=" Unmarried",
                  education=" Bachelors",
                  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE))
newdata2$prob<-predict(mod,newdata2,type="response")

newdata3<-data.frame(
  occupation=" Adm-clerical",
  sex=" Male",
  marital.status=" Never-married",
  relationship=" Unmarried",
  education=" HS-grad",
  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE)
newdata3<-rbind(newdata3,
                data.frame(
                  occupation=" Craft-repair",
                  sex=" Male",
                  marital.status=" Never-married",
                  relationship=" Unmarried",
                  education=" HS-grad",
                  workclass=" Self-emp-not-inc",stringsAsFactors=FALSE))
newdata3$prob<-predict(mod,newdata3,type="response")

# newdata4<-data.frame(
#   occupation=" Adm-clerical",
#   sex=" Male",
#   marital.status=" Married-AF-spouse",
#   relationship=" Husband",
#   education=" HS-grad",
#   workclass=" Private",stringsAsFactors=FALSE)
# newdata4<-rbind(newdata4,
#                 data.frame(
#                   occupation=" Craft-repair",
#                   sex=" Male",
#                   marital.status=" Married-AF-spouse",
#                   relationship=" Husband",
#                   education=" HS-grad",
#                   workclass=" Private",stringsAsFactors=FALSE))
# newdata4$prob<-predict(mod,newdata4,type="response")

# the predicted probabilities are for the non-first
# factor level in the response variable
# here, success is >50K

tab<-t(table(df.ctx$income,df.ctx$occupation))
initial.prob<-c(tab[1,2]/sum(tab[1,]),
                tab[2,2]/sum(tab[2,]))
names(initial.prob)<-c(" Adm-clerical"," Craft-repair")


# z-tests on proportions
sample.sz<-nrow(df.ctx)

# initial test
t0<-prop.test(initial.prob*sample.sz,c(sample.sz,sample.sz))
pv<-t0$p.value

# newdata
newdata1$counts<-round(newdata1$prob*sample.sz)
t1<-prop.test(newdata1$counts,c(sample.sz,sample.sz))
pv<-c(pv,t1$p.value)
newdata2$counts<-round(newdata2$prob*sample.sz)
t2<-prop.test(newdata2$counts,c(sample.sz,sample.sz))
pv<-c(pv,t2$p.value)
newdata3$counts<-round(newdata3$prob*sample.sz)
t3<-prop.test(newdata3$counts,c(sample.sz,sample.sz))
pv<-c(pv,t3$p.value)
# newdata4$counts<-round(newdata4$prob*sample.sz)
# t4<-prop.test(newdata4$counts,c(sample.sz,sample.sz))
# pv<-c(pv,t4$p.value)


# viz
plot.dat<-data.frame(Initial=initial.prob,
                     Whatif1=newdata1$prob,
                     Whatif2=newdata2$prob,
                     Whatif3=newdata3$prob)
                     #Whatif4=newdata4$prob)
barplot(as.matrix(plot.dat), main="Income ~ Occupation", ylab="Pr(Income >50K)", beside=TRUE, 
        col=terrain.colors(2),ylim=c(0,1))
legend("topright", c("Adm-clerical","Craft-repair"), cex=0.8, 
       fill=terrain.colors(2))
for(i in seq(4)){
  if(i == 1){incre<-1}
  else{incre<-incre+2}
  text(x=i+incre,y=max(plot.dat[,i])+0.1,paste("p = ",formatC(pv[i])))
}

# another plot, using *'s as notation for p-values
pv.star<-sapply(pv,FUN=function(pv){
  stars<-""
  if(pv<0.05)  stars<-paste(stars,"*",sep="")
  if(pv<0.01)  stars<-paste(stars,"*",sep="")
  if(pv<0.001) stars<-paste(stars,"*",sep="")
  stars
})
plot.dat<-data.frame(Initial=initial.prob,
                     Whatif1=newdata1$prob,
                     Whatif2=newdata2$prob,
                     Whatif3=newdata3$prob)
                     #Whatif4=newdata4$prob)
barplot(as.matrix(plot.dat), main="Income ~ Occupation", ylab="Pr(Income >50K)", beside=TRUE, 
        col=terrain.colors(2),ylim=c(0,1))
legend("topright", c("Adm-clerical","Craft-repair"), cex=0.8, 
       fill=terrain.colors(2))
for(i in seq(4)){
  if(i == 1){incre<-1}
  else{incre<-incre+2}
  text(x=i+incre,y=max(plot.dat[,i])+0.1,pv.star[i])
}



# length(unique(df.ctx$sex))
# length(unique(df.ctx$marital.status))
# length(unique(df.ctx$relationship))
# length(unique(df.ctx$education))
# length(unique(df.ctx$workclass))

# extract coefficients from adjustment model that is
# different from that of the comparing attribute

# Craft-repair: +ve coefficient
# extract negative coefficients
coe<-summary(mod)$coefficients[,1]
neg.coe<-coe[which(coe<0)]
plot(neg.coe)
