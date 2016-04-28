
# initial test

InitialTest <- function(df, Atgt, Acmp, Atgt_classes = "", Acmp_classes = ""){
        
        # get contexted data
        df.ctx <- Subset(df, Atgt = Atgt, Acmp = Acmp,
                         Atgt_classes = Atgt_classes, Acmp_classes = Acmp_classes)
        
        
        if(class(df.ctx[,Atgt]) == "factor"){
                tab <- table(df.ctx[,Atgt], df.ctx[,Acmp])
                test <- chisq.test(tab)
        }
        else{
             test <- t.test(df.ctx[,Atgt] ~ df.ctx[,Acmp])   
        }
        return(test)
        
}


