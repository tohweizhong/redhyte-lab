

# testing.R

# Script to test various functions in packaging

source("pkg/MineCtx.R")
source("pkg/Discretize.R")
source("pkg/Subset.R")
source("pkg/InitialTest.R")

df <- read.csv("data/adult.txt", header = TRUE, stringsAsFactors = TRUE, strip.white = TRUE)

Atgt <- "income"
Acmp <- "occupation"
Atgt_classes <- ""
Acmp_classes <- c("Adm-clerical", "Craft-repair")

# ====

# Test Discretize()

df <- Discretize(df)

# ====

# Test Subset()

df.ctx <- Subset(df, Atgt = "income", Acmp = "occupation", Acmp_classes = c("Adm-clerical", "Craft-repair"))

# ====

# Test MineCtx()

mods <- MineCtx(df = df.ctx, Atgt = "income", Acmp = "occupation")

# Variable importance plots
varImpPlot(mods[["mod_tgt"]]); varImpPlot(mods[["mod_cmp"]])

# ====


# Test InitialHypothesis() object

IH <- InitialHypothesis(Atgt = "income", Acmp = "occupation",
                        Atgt_classes = "", Acmp_classes = c("Adm-clerical", "Craft-repair"),
                        Actx_items = "", df = df)




