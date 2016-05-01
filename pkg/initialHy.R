
# initialHy.R
# @ data: data
# @ Atgt: target attribute
# @ Acmp: comparing attribute
# @ Atgt_cl: Atgt classes in hypothesis
# @ Acmp_cl: Acmp classes in hypothesis
# @ Atgt_dist: e.g. c(1,1,2); length(Atgt_dist) == length(Atgt_cl)
# @ Acmp_dist: e.g. c(1,1,2); length(Acmp_dist) == length(Acmp_cl)
# @ Actx_items: c("A1 = c1", "A2 = c2")

initialHy <- function(data, Atgt, Acmp,
                      Atgt_cl = "", Acmp_cl = "",
                      Atgt_dist = 0, Acmp_dist = 0,
                      Actx_items = ""){
        
        # Actx_items is the format of
        # c("A1 = c1", "A2 = c2", ...)
        
        require(magrittr)
        
        # Atgt
        if(Atgt_cl %>% length == 1 && Atgt_cl %>% nchar == 0)
                idx_tgt <- 1:nrow(df)
        else if(Atgt_cl %>% length > 1){
                idx_tgt <- NULL
                for(cl in Atgt_cl)
                        idx_tgt <- c(idx_tgt, which(df[,Atgt] == cl))
        }
        #else idx_tgt <- NULL
        
        # Acmp
        if(Acmp_cl %>% length == 1 && Acmp_cl %>% nchar == 0)
                idx_cmp <- 1:nrow(df)
        else if(Acmp_cl %>% length > 1){
                idx_cmp <- NULL
                for(cl in Acmp_cl)
                        idx_cmp <- c(idx_cmp, which(df[,Acmp] == cl))
        }
        #else idx_cmp <- NULL
        
        # take union
        rows   <- intersect(idx_tgt, idx_cmp)
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
        
        # after subsetting, assign class for both Atgt and Acmp
        # tgt_cl and cmp_cl
        #Atgt_dist
        
        
        return(list(Atgt = Atgt, Acmp = Acmp,
                    Atgt_cl = Atgt_cl,
                    Acmp_cl = Acmp_cl,
                    df_ctx = df.ctx))
}
