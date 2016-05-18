
# load for testing

# ====

library(magrittr)
# source all scripts for all functions
ok <- list.files("pkg") %>% sapply(., FUN = function(f){
        if(f != "testing.R" && f != "load.R"){
                paste0("pkg/", f) %>% source
        }
        return(TRUE)
})

df <- read.csv("data/adult.txt", header = TRUE, stringsAsFactors = T, strip.white = TRUE)

Atgt <- "income"
Acmp <- "occupation"
Atgt_cl <- c("<=50K", ">50K") # have to enter the classes
Acmp_cl <- c("Adm-clerical", "Craft-repair")
Atgt_dist <- c(1,2)
Acmp_dist <- c(1,2)

Atgt <- "workclass"
Acmp <- "occupation"
Atgt_cl <- c("State-gov", "Federal-gov") # have to enter the classes
Acmp_cl <- c("Adm-clerical", "Craft-repair")
Atgt_dist <- c(1,2)
Acmp_dist <- c(1,2)
Actx_items <- ""

Atgt <- "age"
Acmp <- "native.country"
#Atgt_cl <- NULL
Acmp_cl <- c("?", "Vietnam")
#Atgt_dist <- NULL
Acmp_dist <- c(1,2)

Atgt <- "age"
Acmp <- "native.country"
Acmp_cl <- c("?", "China", "Vietnam")
Acmp_dist <- c(1,1,2)

Atgt <- "age"
Acmp <- "native.country"
Acmp_cl <- c("?", "China", "Vietnam")
Acmp_dist <- c(1,2,3)

Atgt <- "workclass"
Acmp <- "native.country"
Atgt_cl <- c("Federal-gov", "Local-gov", "State-gov")
Acmp_cl <- c("?", "Philippines", "United-States")
Atgt_dist <- c(1,1,2)
Acmp_dist <- c(1,1,2)
Actx_items <- c("education = Bachelors")
#"income = <=50K"

Atgt <- "age"
Acmp <- "native.country"
Atgt_cl <- NULL
Acmp_cl <- c("?", "China", "Vietnam")
Atgt_dist <- NULL
Acmp_dist <- c(1,2,2)
Actx_items <- ""

Atgt <- "education"
Acmp <- "native.country"
Atgt_cl <- c("Bachelors", "Masters")
Acmp_cl <- c("?", "China", "Vietnam")
Atgt_dist <- c(1,2)
Acmp_dist <- c(1,2,2)
Actx_items <- ""