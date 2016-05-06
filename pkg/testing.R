

# testing.R
# Script to test various functions in packaging

# ====

# Test Discretize()

df <- Discretize(df)

# ====

# Test Subset()

df.ctx <- Subset(df, Atgt = "income", Acmp = "occupation", Acmp_classes = c("Adm-clerical", "Craft-repair"))

# ====

# Test MineCtx()

mods <- MineCtx(df = df.ctx, Atgt = "                                                                                                                                               ", Acmp = "occupation")

# Variable importance plots
varImpPlot(mods[["mod_tgt"]]); varImpPlot(mods[["mod_cmp"]])

# ====

# Test InitialHypothesis() object and initialTest

ih <- initialHy(df = df, Atgt = Atgt, Acmp = Acmp,
                Atgt_dist = Atgt_dist, Acmp_dist = Acmp_dist,
                Atgt_cl = Atgt_cl, Acmp_cl = Acmp_cl,
                Actx_items = "")
 
it <- initialTest(ih)


