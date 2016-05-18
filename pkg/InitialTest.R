
# initialTest.R

initialTest <- function(ih){
        
        Atgt <- ih[["Atgt"]]
        Acmp <- ih[["Acmp"]]
        df_ctx <- ih[["df_ctx"]]
        ht <- ih[["hyType"]]
        Atgt_type <- ht[["Atgt_type"]]
        
        #str(df_ctx)
        
        if(Atgt_type == "cate"){
                tab <- table(df_ctx$cmp_grp, df_ctx$tgt_grp)
                print(tab)
                test <- chisq.test(tab)
                spineplot(df_ctx$tgt_grp ~ df_ctx$cmp_grp)
        }
        else if(ht[["Acmp_dist"]] %>% unique %>% length == 2){
                test <- t.test(df_ctx[,Atgt] ~ df_ctx$cmp_grp)
                boxplot(df_ctx[,Atgt] ~ df_ctx$cmp_grp)
        }
        else if(ht[["Acmp_dist"]] %>% unique %>% length > 2){
                test <- aov(df_ctx[,Atgt] ~ df_ctx$cmp_grp)
                boxplot(df_ctx[,Atgt] ~ df_ctx$cmp_grp)
        }
        return(test)
}
