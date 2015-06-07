install.packages("devtools") # you don't need to run this command if you already have the devtools package installed.

devtools::install_github("clickme", "nachocab")

library(clickme)

clickme("points", rnorm(100))
# try zooming in and out, click the Show names button, hover over points

data(microarray)
clickme("points", x = microarray$significance, y = microarray$logFC,
        color_groups = ifelse(microarray$adj.P.Val < 1e-4, "Significant", "Noise"),
        names = microarray$gene_name,
        x_title = "Significance (-log10)", y_title = "Fold-change (log2)",
        extra = list(Probe = microarray$probe_name))