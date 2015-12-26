
# Discretize.R

# function to convert all numeric variables by the dataset into categorical variables

Discretize <- function(df, ){
        
        ncols <- ncol(df)
        
        classes <- unlist(sapply(c(1:ncols), FUN = function(c){
                return(class(df[,c]))
        }))
        
        which_are_num <- union(which(classes == "integer"), which(classes = "numeric"))
        
        BinBy <- function(v, t){
                returnMe <- NULL
                for(i in seq(length(v))){
                        if(v[i] >= t) returnMe <- c(returnMe, paste0("above", t))
                        else if(v[i] < t) returnMe <- c(returnMe, paste0("below", t))
                }
                return(returnMe)
        }
        
        for(ii in which_are_num){
                
                m <- mean(df[,i])
                
        }
        
        return(classes)
}