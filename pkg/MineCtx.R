
library(randomForest)

df <- read.csv("data/adult.txt", header = TRUE, stringsAsFactors = TRUE, strip.white = TRUE)

# Consider more models for variable importance
# Eg. random forest, PLS, PCA, GLM
# Preferably models that
# @ Are able to accommodate categorical variables
# @ Do not require scaling of data
# @ Interpretable variable importances



MineCtx <- function(df, Atgt, Acmp, p = 0.7, numCtx = 5, metric = "auc"){
        
        # Load required packages
        if(!require(caret)) stop("caret pkg not installed.")
        if(!require(randomForest)) stop("randomForest pkg not installed.")
        if(!require(pROC)) stop("pROC pkg not installed.")
        
        # Check number of context attributes to mine for
        if(numCtx >= (ncol(df) - 2)) stop("numCtx >= (ncol(df) - 2)")
        
        # Preparation for cross-validation
        # Requires CV sets for both target and comparing models
        df_tgt <- subset(df, select = -Acmp)
        df_cmp <- subset(df, select = -Atgt)
        
        ytrain_tgt <- subset(df_tgt, select = Atgt)[,1]
        ytrain_cmp <- subset(df_cmp, select = Acmp)[,1]
        tgt_idx <- createDataPartition(ytrain_tgt, p = 0.7, list = FALSE)
        cmp_idx <- createDataPartition(ytrain_cmp, p = 0.7, list = FALSE)
        
        Xtrain_tgt <- df[tgt_idx,]
        Xtest_tgt <- df[-tgt_idx,]
        Xtrain_cmp <- df[cmp_idx,]
        Xtest_cmp <- df[-cmp_idx,]
        ytrain_tgt <- ytrain_tgt[tgt_idx]
        ytest_tgt <- ytrain_tgt[-tgt_idx]
        ytrain_cmp <- ytrain_cmp[cmp_idx]
        ytest_cmp <- ytrain_cmp[-cmp_idx]
        
        # Get the formulae
        predictors <- colnames(df)[-which(colnames(df) == Atgt)]
        predictors <- predictors[-which(predictors == Acmp)]
        frm_tgt <- paste(" ", predictors, sep = "", collapse = "+")
        frm_tgt <- as.formula(paste(Atgt, "~", frm_tgt))
        frm_cmp <- paste(" ", predictors, sep = "", collapse = "+")
        frm_cmp <- as.formula(paste(Acmp, "~", frm_cmp, sep = ""))
        
        # Construct target and comparing models
        print("Constructing target model...")
        mod_tgt <- randomForest(x = Xtrain_tgt, y = ytrain_tgt, importance = TRUE)
        print("Constructing comparing model...")
        mod_cmp <- randomForest(x = Xtrain_cmp, y = ytrain_cmp, importance = TRUE)
        
        # Variable importance plots
        varImpPlot(mod_tgt); varImpPlot(mod_cmp)
        
        # Make predictions on testing sets
        mod_tgt_pred_prob <- predict(mod_tgt, newdata = Xtest_tgt, type = "prob")[,1]
        mod_tgt_pred_class <- predict(mod_tgt, newdata = Xtest_tgt, type = "response")
        mod_cmp_pred_prob <- predict(mod_cmp, newdata = Xtest_cmp, type = "prob")[,1]
        mod_cmp_pred_class <- predict(mod_cmp, newdata = Xtest_cmp, type = "response")
        
        # Evaluate models
        if(metric == "auc"){
                tgt_auc <- auc(predictor = mod_tgt_pred_prob, response = ytest_tgt)
                cmp_auc <- auc(predictor = mod_cmp_pred_prob, response = ytest_cmp)
                print(tgt_auc); print(cmp_auc)
        }
        
        return(list(mod_tgt = mod_tgt, mod_cmp = mod_cmp))
}


# Example
foo <- MineCtx(df = df, Atgt = "income", Acmp = "occupation")
