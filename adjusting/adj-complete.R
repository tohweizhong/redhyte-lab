# use model from what-if.R and
# combinations from combinations.R

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

cmb<-expand.grid(occupation=unique(df.ctx$occupation), # include also Acmp
                 sex=unique(df.ctx$sex),
                 marital.status=unique(df.ctx$marital.status),
                 relationship=unique(df.ctx$relationship),
                 education=unique(df.ctx$education),
                 workclass=unique(df.ctx$workclass))

cmb$prob<-predict(mod,cmb,type="response")

