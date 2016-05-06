
# initialHy.R
# @ df: df
# @ Atgt: target attribute
# @ Acmp: comparing attribute
# @ Atgt_cl: Atgt classes in hypothesis
# @ Acmp_cl: Acmp classes in hypothesis
# @ Atgt_dist: e.g. c(1,1,2); length(Atgt_dist) == length(Atgt_cl)
# @ Acmp_dist: e.g. c(1,1,2); length(Acmp_dist) == length(Acmp_cl)
# @ Actx_items: c("A1 = c1", "A2 = c2")

initialHy <- function(df, Atgt, Acmp,
                      Atgt_cl = NULL, Acmp_cl = NULL,
                      Atgt_dist = NULL, Acmp_dist = NULL,
                      Actx_items = ""){
        
        # length of Atgt_cl should be same as Atgt_dist
        if(length(Atgt_cl) != length(Atgt_dist)) stop("length(Atgt_cl) != length(Atgt_dist)")
        if(length(Acmp_cl) != length(Acmp_dist)) stop("length(Acmp_cl) != length(Acmp_dist)")
        
        require(magrittr)
        
        # hypothesis type
        Atgt_type   <- ifelse(is.factor(df[,Atgt]),"cate", "num")
        Atgt_num_cl <- ifelse(Atgt_cl %>% is.null, 0, length(Atgt_cl))
        Acmp_num_cl <- ifelse(Acmp_cl %>% is.null, 0, length(Acmp_cl))
        
        ht <- hyType(Atgt_type   = Atgt_type,
                     Atgt_num_cl = Atgt_num_cl,
                     Acmp_num_cl = Acmp_num_cl,
                     Atgt_dist   = Atgt_dist,
                     Acmp_dist   = Acmp_dist)
        
        # for numeric Atgt, get the mean first. Discretize later
        if(ht[["Atgt_type"]] == "num") mu <- mean(df[,Atgt])

        #if(Atgt_cl %>% length == 1 && Atgt_cl %>% nchar == 0){
                
        # Atgt
        if(ht[["Atgt_type"]] == "num"){
                idx_tgt <- 1:nrow(df)
        }
        else if(Atgt_cl %>% length > 1){
                idx_tgt <- NULL
                for(cl in Atgt_cl){
                        idx_tgt <- c(idx_tgt, which(df[,Atgt] == cl))
                }
        }
        
        # Acmp - must be categorical
        if(Acmp_cl %>% length > 1){
                idx_cmp <- NULL
                for(cl in Acmp_cl){
                        idx_cmp <- c(idx_cmp, which(df[,Acmp] == cl))
                }
        }
        
        # take intersect
        rows   <- intersect(idx_tgt, idx_cmp)
        df_ctx <- df[rows,]
        df_ctx <- droplevels(df_ctx)
        
        # now for Actx
        # Actx_items is the format of
        # c("A1 = c1", "A2 = c2", ...)
        if(Actx_items %>% nchar > 0){
                A       <- NULL
                cl      <- NULL
                idx_ctx <- NULL
                for(ii in Actx_items){
                        tmp  <- strsplit(ii, "=") %>% unlist
                        A    <- c(A,  tmp[1])
                        cl   <- c(cl, tmp[2])
                        
                        tmp_idx <- which(df_ctx[,A] == cl)
                        idx_ctx <- intersect(idx_ctx, tmp_idx)
                }
                df_ctx <- df_ctx[idx_ctx,]
        }
        
        # after subsetting, assign class for both Atgt and Acmp
        # tgt_cl and cmp_cl
        
        # function to AddClass tgt_cl and cmp_cl
        AddClass <- function(df, Atgt, Acmp,
                             Atgt_cl = NULL, Acmp_cl = NULL,
                             Atgt_dist = NULL, Acmp_dist = NULL){
                
                df$tgt_grp <- rep(0, nrow(df))
                df$cmp_grp <- rep(0, nrow(df))
                
                if(ht[["Atgt_type"]] == "num"){
                        df$tgt_grp <- sapply(df[,Atgt], FUN = function(x){
                                if(x >= mu) return(paste0(Atgt, "_above_equal_", round(mu, 2)))
                                else return(paste0(Atgt, "_below_", round(mu, 2)))
                        })
                }
                else if(ht[["Atgt_type"]] == "cate"){
                        
                        for(i in seq_along(Atgt_cl)){
                                
                                cl  <- Atgt_cl[i]
                                grp <- Atgt_dist[i]
                                
                                df$tgt_grp[which(df[,Atgt] == cl)] <- grp
                        }
                }
                
                for(i in seq_along(Acmp_cl)){
                        
                        cl  <- Acmp_cl[i]
                        grp <- Acmp_dist[i]
                        
                        df$cmp_grp[which(df[,Acmp] == cl)] <- grp
                }
                
                # convert to factor
                df$tgt_grp <- factor(df$tgt_grp)
                df$cmp_grp <- factor(df$cmp_grp)
                
                return(df)
        }
        
        df_ctx <- AddClass(df_ctx, Atgt = Atgt, Acmp = Acmp,
                           Atgt_dist = Atgt_dist, Acmp_dist = Acmp_dist,
                           Atgt_cl = Atgt_cl, Acmp_cl = Acmp_cl)
        
        # drop Atgt and Acmp from df_ctx
        #df_ctx <- df_ctx[, !(colnames(df_ctx) %in% c(eval(Atgt), eval(Acmp)))]
        
        # Discretize all other attributes
        df_ctx <- Discretize(df_ctx, Atgt = Atgt, Acmp = Acmp, by = "mean")
        
        str(df_ctx)
        
        
        return(list(Atgt = Atgt, Acmp = Acmp,
                    Atgt_cl = Atgt_cl,
                    Acmp_cl = Acmp_cl,
                    df_ctx = df_ctx,
                    hyType = ht))
}
