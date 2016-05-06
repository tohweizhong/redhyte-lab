
# AddClass.R - should be redundant

AddClass <- function(dat, Atgt, Acmp,
                     Atgt_cl = "", Acmp_cl = "",
                     Atgt_dist = 0, Acmp_dist = 0){
        
        if(length(Atgt_cl) != length(Atgt_dist)) stop("length(Atgt_cl) != length(Atgt_dist)")
        if(length(Acmp_cl) != length(Acmp_dist)) stop("length(Acmp_cl) != length(Acmp_dist)")
        
        dat$tgt_cl <- rep(0, nrow(dat))
        dat$cmp_cl <- rep(0, nrow(dat))
        
        for(i in seq_along(Atgt_cl)){
                
                cl  <- Atgt_cl[i];
                grp <- Atgt_dist[i];
                
                dat$tgt_cl[which(dat[,Atgt] == cl)] <- grp
        }
        
        for(i in seq_along(Acmp_cl)){
                
                cl  <- Acmp_cl[i]
                grp <- Acmp_dist[i]
                
                dat$cmp_cl[which(dat[,Acmp] == cl)] <- grp
        }
        
        return(dat)
}
