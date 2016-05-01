
# initial test

initialTest <- function(IH){
        
        Atgt <- IH[["Atgt"]]
        Acmp <- IH[["Acmp"]]
        Atgt_classes <- IH[["Atgt_classes"]]
        Acmp_classes <- IH[["Acmp_classes"]]
        df.ctx <- IH[["df.ctx"]]
        
        if(class(df.ctx[,Atgt]) == "factor"){
                tab <- table(df.ctx[,Atgt], df.ctx[,Acmp])
                test <- chisq.test(tab)
        }
        else{
             test <- t.test(df.ctx[,Atgt] ~ df.ctx[,Acmp])   
        }
        return(test)

}

