require(gridExtra)
require(ggplot2)
require(ggthemes)
require(lattice)

wk <- as.Date(Sys.Date()) - 7*(51:0)
y <- rnorm(52, 0, 10) + c(rep(0, 26), rep(5, 26)) + 130
# plot(y)
# hist(y)
y[10] <- max(y)

pl <- barchart(y ~ as.POSIXct(wk), horizontal=FALSE, origin=0, 
               col=ifelse(y>0, "blue", "red"),
               border=ifelse(y>0, "blue", "red"), 
               box.width=1, 
               scales=list(draw=FALSE), ylab=NULL, xlab=NULL, main=NULL, sub=NULL)

df <- data.frame(wk, y)
df2 <- data.frame(wk, y=rev(y))
#   Variante 1
pg <- ggplot(df, aes( wk, y)) + geom_bar(stat="identity", position="dodge", 
                                         colour=ifelse(y>0, "blue", "red"),
                                         fill=ifelse(y>0, "blue", "red")) + 
  theme_tufte(ticks=FALSE) + 
  theme(title=element_blank(),
        axis.text=element_blank(), plot.margin=unit(c(0,0,0,0), "npc"))

#   Variante 2, Problem: Min. wird nicht rot, wenn größer Null
pg <- ggplot(df, aes( wk, y)) + geom_bar(stat="identity", position="dodge", 
                                         colour=NA,
                                         alpha=ifelse(y==max(y) | y==min(y), 1.0, 0.67),
                                         fill=ifelse(y>0, "blue", "red")) + 
  theme(title=element_blank(), panel.grid=element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank())

#   Variante 3, Min. wird rot, auch wenn größer Null
pg <- ggplot(df, aes( wk, y)) + geom_bar(stat="identity", position="dodge", 
                                         colour=NA, 
                                         alpha=ifelse(y==max(y) | y==min(y), 1.0, 0.67),
                                         fill=ifelse((y>0 | y==max(y)) & y>min(y), "blue", "red")) + 
  theme(title=element_blank(), panel.grid=element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank())

pgg <- ggplotGrob(pg)  

pg2 <- ggplot(df, aes( wk, y)) + 
#   geom_area(stat="identity", position="dodge", data=df2, alpha=0.33) +
  geom_line() + 
  geom_smooth(se=FALSE, linetype="dashed", color="grey50", size=rel(0.75)) + 
  annotate("point", x=df$wk[df$y==max(df$y)], y=max(df$y), colour="blue") + 
  annotate("point", x=df$wk[df$y==min(df$y)], y=min(df$y), colour="red") + 
  annotate("point", x=tail(df$wk, 1), y=tail(df$y, 1), colour="black") + 
#   theme_tufte(ticks=FALSE) + 
  theme(title=element_blank(), panel.grid=element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank())
pgg2 <- ggplotGrob(pg2)  

gl <- grid.layout(20,5)
grid.show.layout(gl)

grid.newpage()
pushViewport(viewport(layout=gl))
# showViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
# grid.rect()
print(pl, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=4))
# grid.rect()
print(pg, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=6))
# grid.rect()
grid.draw(pgg[3,4])
popViewport()
pushViewport(viewport(layout.pos.col=4, layout.pos.row=6))

# grid.rect()
ranges <- c(75, 85, 100)
measures <- 78
markers <- rev(c(38, 88))
grid.draw(ggplotGrob(bulletGraph(measures, markers, ranges))[3:4,4])
popViewport()

pushViewport(viewport(layout.pos.col=4, layout.pos.row=2, clip="off"))
# grid.rect()
grid.draw(pgg2[3,4])
popViewport()
pushViewport(viewport(layout.pos.col=5, layout.pos.row=2))
grid.rect()
ylbl <- sprintf("%3.1f", c(tail(y, 1), max(y), min(y)))
grid.text(ylbl, x=unit(c(0, 1, 1),"npc"), y=unit(c(0.5, 1, 0), "npc"), 
          hjust=c(0, 1, 1), vjust=c(0.5, 1, 0),
          gp=gpar(col=c("black","blue", "red"), cex=c(1, 0.8, 0.8)))
popViewport()
# grid.draw(pl)
