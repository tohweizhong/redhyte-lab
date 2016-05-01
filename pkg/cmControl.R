
# cmControl.R
# @ min_perf: minimum amt of performance cm models needs to achieve before shortlisting attrs
# @ metric: {"acc", "auc", "agm}
# @ num_mined: number of attrs to shortlist
# @ model: {"rf"}

cmControl <- function(min_perf = 0.7, metric = "acc", num_mined = 5, model = "rf"){
        
        return(list(min_perf = min_perf,
                    metric = metric,
                    num_mined = num_mined,
                    model = model))
}