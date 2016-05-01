

# testing.R
# Script to test various functions in packaging

# ====

# source all scripts for all functions
ok <- list.files("pkg") %>% sapply(., FUN = function(f){
        if(f != "testing.R"){
                paste0("pkg/", f) %>% source
        }
        return(TRUE)
})

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

ih <- initialHy(data = df, Atgt = "workclass", Acmp = "occupation",
                Atgt_cl = c("Federal-gov", "State-gov"), Acmp_cl = c("Adm-clerical", "Craft-repair"),
                Actx_items = "")


InitialTest(ih)


