
# Subset.R (internal)

Subset <- function(df, Atgt, Acmp, Atgt_cl = "", Acmp_cl = "", Actx_items = ""){
        
        require(magrittr)
        set.seed(123)
        
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
                idx_ctx <- df_ctx %>% nrow %>% seq
                for(ii in Actx_items){
                        tmp  <- strsplit(ii, " = ") %>% unlist
                        A    <- c(A,  tmp[1])
                        cl   <- c(cl, tmp[2])
                        print(A); print(cl)
                        
                        tmp_idx <- which(df_ctx[,A] == cl)
                        idx_ctx <- intersect(idx_ctx, tmp_idx)
                }
                df_ctx <- df_ctx[idx_ctx,]
        }
        
        return(df_ctx)
}
