# function to generate a formula with all pairwise 2-way interaction terms
# input:
#   @ vec: a vector of strings, representing all variable names,
#          excluding the target/response variable
#   @ tgt: name of the target/response variable
# output: a single formula

itr.formula<-function(vec,tgt){
  fm<-paste(tgt,paste0(vec,collapse="+"),sep="~")
  tmp<-outer(vec,
             vec,
             paste,sep="*")
  tmp<-tmp[lower.tri(tmp,diag=F)]
  tmp<-paste0(tmp,collapse="+")
  fm<-paste(fm,tmp,sep="+")
  return(as.formula(fm))
}