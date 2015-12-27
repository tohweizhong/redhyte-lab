

# testing.R

# Script to test various functions in packaging

source("pkg/MineCtx.R")
source("pkg/Discretize.R")

df <- read.csv("data/adult.txt", header = TRUE, stringsAsFactors = TRUE, strip.white = TRUE)

# ====

# Test Discretize()

df <- Discretize(df)

# ====

# Test MineCtx()

Atgt<-"income"
Acmp<-"occupation"
rows<-union(which(df[,Acmp] == "Adm-clerical"),
            which(df[,Acmp] == "Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

mods <- MineCtx(df = df.ctx, Atgt = "income", Acmp = "occupation")

# Variable importance plots
varImpPlot(mods[["mod_tgt"]]); varImpPlot(mods[["mod_cmp"]])

# ====