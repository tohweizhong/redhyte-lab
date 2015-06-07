# want a data.frame with all possible combinations of mined context items

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

# compute how many combinations are there
ctx.attr<-c("sex","marital.status","relationship","education","workclass")
num.ctx<-length(ctx.attr)

# total<-
#   length(unique(df.ctx$sex))*
#   length(unique(df.ctx$marital.status))*
#   length(unique(df.ctx$relationship))*
#   length(unique(df.ctx$education))*
#   length(unique(df.ctx$workclass))
# 
# # require a data.frame with nrows = total and ncols = 5
# cmb<-NULL
# for(i in seq(num.ctx)){
#   tmp<-NULL
#   num.class<-length(unique(df.ctx[,ctx.attr[i]]))
#   for(j in seq(total)){
#     for(k in unique(df.ctx[,ctx.attr[i]])){
#       tmp<-c(tmp,k)
#     }
#   }
#   cmb<-cbind(cmb,tmp)
# }

cmb<-expand.grid(unique(df.ctx$sex),
                 unique(df.ctx$marital.status),
                 unique(df.ctx$relationship),
                 unique(df.ctx$education),
                 unique(df.ctx$workclass))



