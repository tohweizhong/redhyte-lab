df<-read.csv("data/adult.txt",stringsAsFactors=T,sep=",")

# set up initial hypothesis
# categorical target attribute
atgt<-"income"
acmp<-"occupation"
rows<-union(which(df[,acmp] == " Adm-clerical"),
            which(df[,acmp] == " Craft-repair"))
df.ctx<-df[rows,]
df.ctx<-droplevels(df.ctx)

# # consider only Whites (as in example in thesis, Results section)
# rows<-which(df.ctx$race == " White")
# df.ctx<-df.ctx[rows,]

# drop unneccessary variables
df.ctx<-df.ctx[,c("income","occupation",
                  "sex","marital.status","relationship","education","workclass")]


print(tab <- t(table(df.ctx[,c("income","occupation")])))
prop0 <- c(507/(507+3263), c(929/(929+3170)))
sup0 <- c(3263+507, 3170+929)
        
# sex = Female
rows.female <- which(df.ctx$sex == " Female")
df.ctx1 <- df.ctx[rows.female,]
print(tab <- t(table(df.ctx1[,c("income","occupation")])))
prop1 <- c(212/(212+2325),c(20/(202+20)))
sup1 <- c(2325+212, 202+20)

plot.dat <- data.frame(rbind(cbind(prop0,sup0,c("Admin","Craft"),c(0,0)),
                             cbind(prop1,sup1,c("Admin","Craft"),c(1,1))))
colnames(plot.dat) <- c("prop","sup","class","after")
plot.dat$class <- factor(plot.dat$class)
plot.dat$after <- factor(plot.dat$after)
plot.dat$prop <- as.numeric(levels(plot.dat$prop))
plot.dat$sup <- as.numeric(levels(plot.dat$sup))


str(plot.dat)


require(ggplot2)

th<-theme(
        plot.title = element_text(face="bold", color = "black", size=12),
        legend.position=c(1,1),
        legend.justification=c(1,1),
        axis.title.x=element_text(face="bold",color="black",size=11),
        axis.title.y=element_text(face="bold",color="black",size=11)
)

gg<-ggplot(plot.dat,aes(x=sup,y=prop))
gg<-gg+geom_point(size=5)
gg<-gg+ theme_bw()+th
gg
