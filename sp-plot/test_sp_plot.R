df<-read.csv("sp-plot/test_sp_plot.csv",stringsAsFactors=T)
df$tgt.class<-factor(df$tgt.class, levels=rev(levels(df$tgt.class)))

require(ggplot2)
th<-theme(
  plot.title = element_text(face="bold", color = "black", size=12),
  legend.position=c(1,1),
  legend.justification=c(1,1),
  axis.title.x=element_text(face="bold",color="black",size=11),
  axis.title.y=element_text(face="bold",color="black",size=11)
)

gg<-ggplot(df,aes(x=sup,y=tgt.class))
gg<-gg+geom_point(size=5,aes(color=factor(df$after),shape=factor(df$cmp.class)))
gg<-gg+ scale_color_manual(name ="cmp.class",labels=factor(df$cmp.class),values=c("red","blue"))
gg<-gg+ scale_shape_manual(name ="Engine",labels=factor(df$cmp.class),values=c(0,2))
gg<-gg+ theme_bw()+th
gg








#geom_segment(data=d, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), arrow=arrow(), size=2, color="blue") + 

df2<-read.csv("sp-plot/test_sp_plot2.csv",stringsAsFactors=T)

gg2<-ggplot(df2,aes(x=sup,y=tgt))
gg2<-gg2+geom_point(aes(
  size=5,
  color=factor(df2$after),
  shape=factor(df2$cmp.class)))
gg2

require(ggplot2)
th<-theme(
  axis.title.x=element_text(face="bold",color="black",size=11),
  axis.title.y=element_text(face="bold",color="black",size=11)
)
scoreplot<-ggplot(df,aes(x=sup,y=tgt.class))
scoreplot<-scoreplot+geom_point(color="red")
# scoreplot<-scoreplot+labs(x=paste("Principal Component",whichpc1),
#                           y=paste("Principal Component",whichpc2))

scoreplot<-scoreplot+theme_bw()+th
plot(scoreplot)

# names(scores)<-c(paste("PC",whichpc1,sep=""),
#                  paste("PC",whichpc2,sep=""))