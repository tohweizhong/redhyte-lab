
# AddClass.R (internal)

AddClass <- function(df, Atgt, Acmp,
                     Atgt_cl = NULL, Acmp_cl = NULL,
                     Atgt_dist = NULL, Acmp_dist = NULL, mu){
        
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
        str(df)
        return(df)
}