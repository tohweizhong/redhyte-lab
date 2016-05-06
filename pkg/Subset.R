
# Subset.R - should be redundant

Subset <- function(df, Atgt, Acmp, Atgt_classes = "", Acmp_classes = ""){

        require(magrittr)
        set.seed(123)
        
        # check length of Atgt_classes and Acmp_classes
        if(Atgt_classes %>% length == 1)
                idx_tgt <- which(df[,Atgt] == Atgt_classes)
        else if(Atgt_classes %>% length > 1){
                idx_tgt <- NULL
                for(cl in Atgt_classes)
                        idx_tgt <- c(idx_tgt, which(df[,Atgt] == cl))
        }
        else idx_tgt <- NULL
        
        if(Acmp_classes %>% length == 1)
                idx_cmp <- which(df[,Acmp] == Acmp_classes)
        else if(Acmp_classes %>% length > 1){
                idx_cmp <- NULL
                for(cl in Acmp_classes)
                        idx_cmp <- c(idx_cmp, which(df[,Acmp] == cl))
        }
        else idx_cmp <- NULL
        
        # take union
        rows <- union(idx_tgt, idx_cmp)
        df.ctx<-df[rows,]
        df.ctx<-droplevels(df.ctx)
        
        return(df.ctx)
}

# Atgt<-"income"
# Acmp<-"occupation"
# rows<-union(which(df[,Acmp] == "Adm-clerical"),
#             which(df[,Acmp] == "Craft-repair"))
# df.ctx<-df[rows,]
# df.ctx<-droplevels(df.ctx)
