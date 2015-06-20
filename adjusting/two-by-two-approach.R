source('~/R/redhyte-lab/adjusting/generate_itr_formula.R', echo=TRUE)

# categorical Atgt
# @ adjustments: probabilities
# @ test: z-test on proportions
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

# stepwise regression
primary.mod<-glm(income~.,data=df.ctx,
                 family=binomial(link=logit))
step.mod<-step(primary.mod,direction="backward")


### adjustments
# need a null model

mod0 <- glm(income ~ occupation, data = df.ctx,
            family = binomial(link = logit))

# now get the null probabilities
prob0 <- predict(mod0,
                 subset(df.ctx, select = -income),
                 type = "response")

hist(prob0)

mod1 <- glm(itr.formula(vec=colnames(df.ctx)[-1],tgt=atgt),
         family=binomial(link=logit),data=df.ctx)
beep()

prob1 <- predict(mod1,
                 subset(df.ctx, select = -income),
                 type = "response")

### test & what-if analysis: already documented in adjustments.html





# numerical target attribute
atgt<-"age"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

# # consider only Whites (as in example in thesis, Results section)
# rows<-which(df.ctx$race == " White")
# df.ctx<-df.ctx[rows,]

# drop unneccessary variables
df.ctx<-df.ctx[,c("age","occupation",
                  "sex","marital.status","relationship","education","workclass")]

# stepwise regression
primary.mod<-glm(income~.,data=df.ctx,
                 family=binomial(link=logit))
step.mod<-step(primary.mod,direction="backward") # all in

### adjustments

# no need null model: use original values in dataset

mod1 <- lm(age ~ ., data = df.ctx)

adj1 <- predict(mod1,
                subset(df.ctx, select = -age),
                se.fit = TRUE)

hist(df.ctx$age)
hist(adj1$fit)

# scatterplot don't really make any sense

# test

#t1 <- 

###
###
###