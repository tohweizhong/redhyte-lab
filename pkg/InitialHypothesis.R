
# InitialHypothesis.R

InitialHypothesis <- function(df, Atgt, Acmp,
                              Atgt_classes = "", Acmp_classes = "",
                              Actx_items = ""){
        
        # Actx_items is the format of
        # c("A1 = c1", "A2 = c2", ...)
        
        require(magrittr)
        
        # Atgt
        if(Atgt_classes %>% length == 1 && Atgt_classes %>% nchar == 0)
                idx_tgt <- 1:nrow(df)
        else if(Atgt_classes %>% length > 1){
                idx_tgt <- NULL
                for(cl in Atgt_classes)
                        idx_tgt <- c(idx_tgt, which(df[,Atgt] == cl))
        }
        #else idx_tgt <- NULL
        
        # Acmp
        if(Acmp_classes %>% length == 1 && Acmp_classes %>% nchar == 0)
                idx_cmp <- 1:nrow(df)
        else if(Acmp_classes %>% length > 1){
                idx_cmp <- NULL
                for(cl in Acmp_classes)
                        idx_cmp <- c(idx_cmp, which(df[,Acmp] == cl))
        }
        #else idx_cmp <- NULL
        
        # take union
        rows   <- union(idx_tgt, idx_cmp)
        df.ctx <- df[rows,]
        df.ctx <- droplevels(df.ctx)
        
        # now for Actx
        if(Actx_items %>% nchar > 0){
                A       <- NULL
                cl      <- NULL
                idx_ctx <- NULL
                for(ii in Actx_items){
                        tmp  <- strsplit(ii, "=") %>% unlist
                        A    <- c(A,  tmp[1])
                        cl   <- c(cl, tmp[2])
                        
                        tmp_idx <- which(df.ctx[,A] == cl)
                        idx_ctx <- intersect(idx_ctx, tmp_idx)
                }
                df.ctx <- df.ctx[idx_ctx,]
        }
        return(list(Atgt = Atgt, Acmp = Acmp,
                    Atgt_classes = Atgt_classes, Acmp_classes = Acmp_classes,
                    df.ctx = df.ctx))
}
