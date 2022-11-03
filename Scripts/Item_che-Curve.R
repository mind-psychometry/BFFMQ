
#ICC
item01 <- extract.item(observe.fit.2, 1, drop.zeros=TRUE)
theta <- matrix(seq(-6,6, by = .1))
P <-mirt::probtrace(x=item01 , Theta=theta )

plt2 <- data.frame(P = P, Theta = theta)
colnames(plt2) <- c(paste("P", 1:ncol(P), sep=''), "Theta")
plt2 <- reshape(plt2, direction='long', varying = paste("P", 1:ncol(P), sep=''), v.names = 'P',
                times = paste("P", 1:ncol(P), sep=''))

ggplot(plt2,aes(Theta,P, col =time))+geom_line()+xlab(expression(theta)) + 
  ylab(expression(I(theta)))+apatheme+scale_color_npg()





ggicc <- function(model, item, theta){
  Theta <- matrix(seq(-theta,theta, by = .1))
  iteminfo <- mirt::extract.item(model, item)
  P <-mirt::probtrace(x=iteminfo , Theta=Theta  )
  icc <- data.frame(P = P, Theta = Theta)
  colnames(icc) <- c(paste("P", 1:ncol(P), sep=''), "Theta")
  icc2<- reshape(icc, direction='long', varying = paste("P", 1:ncol(P), sep=''), v.names = 'P',
                  times = paste("P", 1:ncol(P), sep=''))
  plot <- ggplot2::ggplot(icc2,aes(Theta,P, col =time))+geom_line()+xlab(expression(theta)) + 
    ylab(expression(I(theta)))+ theme(legend.title=element_blank())
  return(plot)
  
}


ggicc(observe.fit.2,2,3)

#observe
item01.icc.legends <- ggicc(observe.fit.2,1,6)+ggtitle("Observe: item01")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))

item01.icc <- ggicc(observe.fit.2,1,6)+ggtitle("Observe: item01")+apatheme+scale_color_npg()+
scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item06.icc <- ggicc(observe.fit.2,2,6)+ggtitle("Observe: item06")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item11.icc <- ggicc(observe.fit.2,3,6)+ggtitle("Observe: item11")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")
item15.icc <- ggicc(observe.fit.2,4,6)+ggtitle("Observe: item15")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")
item20.icc <- ggicc(observe.fit.2,1,6)+ggtitle("Observe: item20")+apatheme+
  scale_color_npg()+scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item26.icc <- ggicc(observe.fit.2,5,6)+ggtitle("Observe: item261")+apatheme+
  scale_color_npg()+scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")
item31.icc <- ggicc(observe.fit.2,6,6)+ggtitle("Observe: item31")+apatheme+
  scale_color_npg()+scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")
item36.icc <- ggicc(observe.fit.2,7,6)+ggtitle("Observe: item36")+
  apatheme+scale_color_npg()+scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")



legend <- cowplot::get_legend(item01.icc.legends)

observe.iccplot1 <- cowplot::plot_grid(item01.icc,item06.icc ,item11.icc,item15.icc,item20.icc,item26.icc,
                                      item31.icc,item36.icc,
                               labels = "AUTO",
                               align="v",
                               ncol = 4,
                               label_size = 15,
                               label_fontfamily = "sans",
                               label_fontface = "plain")

observe.iccplot2 <- plot_grid(observe.iccplot1, legend, ncol = 2, rel_widths = c(1, .1))

ggsave("Figures/observe_iccplot.png",observe.iccplot2 , width = 12, height = 6, dpi = 1200, bg = "white")


###Describe
item07.icc <- ggicc(describe.fit.2,1,6)+ggtitle("Describe: item07")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item22.icc <- ggicc(describe.fit.2,2,6)+ggtitle("Describe: item022")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item27.icc <- ggicc(describe.fit.2,3,6)+ggtitle("Describe: item27")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item32.icc <- ggicc(describe.fit.2,4,6)+ggtitle("Describe: item32")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item37.icc <- ggicc(describe.fit.2,5,6)+ggtitle("Describe: item37")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

describe.iccplot1 <- cowplot::plot_grid(item07.icc,item22.icc ,item27.icc,item32.icc,item37.icc,
                                       labels = "AUTO",
                                       align="v",
                                       ncol = 3,
                                       label_size = 15,
                                       label_fontfamily = "sans",
                                       label_fontface = "plain")

describe.iccplot2 <- plot_grid(describe.iccplot1, legend, ncol = 2, rel_widths = c(1, .1))

ggsave("Figures/describe_iccplot.png",describe.iccplot2 , width = 12, height = 6, dpi = 1200, bg = "white")




iccplot1 <- cowplot::plot_grid(item01.icc,item06.icc ,item11.icc,item15.icc,item20.icc,item26.icc,
                                       item31.icc,item36.icc,item07.icc,item22.icc ,item27.icc,item32.icc,
                                     item37.icc,item05.icc,item08.icc,item13.icc ,item18.icc,item23.icc,
                                     item28.icc, item34.icc,
                                       labels = "AUTO",
                                       align="v",
                                       ncol = 4,
                                       label_size = 15,
                                       label_fontfamily = "sans",
                                       label_fontface = "plain")

iccplot2 <- plot_grid(iccplot1, legend, ncol = 2, rel_widths = c(1, .1))

ggsave("Figures/iccplot1.png",iccplot2  , width = 12, height = 12, dpi = 1200, bg = "white")





### ACTware
item05.icc <- ggicc(awareness.fit.2,1,6)+ggtitle("Actwar: item05")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item08.icc <- ggicc(awareness.fit.2,2,6)+ggtitle("Actwar: item08")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item13.icc <- ggicc(awareness.fit.2,3,6)+ggtitle("Actwar: item13")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item18.icc <- ggicc(awareness.fit.2,4,6)+ggtitle("Actwar: item18")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item23.icc <- ggicc(awareness.fit.2,5,6)+ggtitle("Actwar: item23")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item28.icc <- ggicc(awareness.fit.2,6,6)+ggtitle("Actwar: item28")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item34.icc <- ggicc(awareness.fit.2,7,6)+ggtitle("Actwar: item34")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")

item38.icc <- ggicc(awareness.fit.2,8,6)+ggtitle("Actwar: item38")+apatheme+scale_color_npg()+
  scale_y_continuous(breaks=c(0,.5,1), limits = c(0, 1))+theme(legend.position="none")





###Nonjudg
item03.icc <- iteminfo(item03, Theta)
item10.icc <- iteminfo(item10, Theta)
item17.icc <- iteminfo(item17, Theta)
item25.icc <- iteminfo(item25, Theta)
item30.icc <- iteminfo(item30, Theta)
item35.icc <- iteminfo(item35, Theta)
item39.icc <- iteminfo(item39, Theta)
##Nonreact
item04.icc <- iteminfo(item04, Theta)
item09.icc <- iteminfo(item09, Theta)
item19.icc <- iteminfo(item19, Theta)
item21.icc <- iteminfo(item21, Theta)
item24.icc <- iteminfo(item24, Theta)
item29.icc <- iteminfo(item29, Theta)
item33.icc <- iteminfo(item33, Theta)
  
  
  
  
