library(ggplot2)
library(grid)

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


print(tab0 <- t(table(df.ctx[,c("income","occupation")])))
prop0 <- c(507/(507+3263), 929/(929+3170))
sup0 <- c(3263+507, 3170+929)
        
# education == Masters
rows.female <- which(df.ctx$education == " Masters")
df.ctx1 <- df.ctx[rows.female,]
print(tab1 <- t(table(df.ctx1[,c("income","occupation")])))
prop1 <- c(tab1[1,2]/sum(tab1[1,]),tab1[2,2]/sum(tab1[2,]))
sup1 <- c(sum(tab1[1,]), sum(tab1[2,]))

plot.dat<-cbind(prop0,sup0)
plot.dat<-data.frame(rbind(plot.dat,cbind(prop1,sup1)))

plot.dat<-cbind(plot.dat,
                c("Admin","Craft","Admin","Craft"),
                c(0,0,1,1))

colnames(plot.dat) <- c("prop","sup","cmp.class","after")
str(plot.dat)



th<-theme(
        plot.title = element_text(face="bold", color = "black", size=12),
        #legend.position=c(1,1),
        legend.justification=c(1,1),
        axis.title.x=element_text(face="bold",color="black",size=11),
        axis.title.y=element_text(face="bold",color="black",size=11)
)

gg<-ggplot(plot.dat,aes(x=sup,y=prop))
gg<-gg+geom_point(data=plot.dat[c(1:2),],aes(x=sup,y=prop,colour=cmp.class),size=5)
gg<-gg+geom_point(data=plot.dat[c(3:4),],aes(x=sup,y=prop,colour=cmp.class),size=5)

gg<-gg+geom_segment(aes(x=plot.dat$sup[1:2],
                        y=plot.dat$prop[1:2],
                        xend=plot.dat$sup[3:4],
                        yend=plot.dat$prop[3:4]),
                        arrow=arrow())

gg<-gg+ theme_bw()+th
gg

plot.dat
