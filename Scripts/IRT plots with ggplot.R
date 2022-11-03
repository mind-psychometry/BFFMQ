apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text=element_text(family = "Helvetica"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_blank(),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))


#item information function
ggiteminfoplot <- function (model, item, theta){
  Theta <- matrix(seq(-theta,theta, by = .1))
  iteminfo <- mirt::extract.item(model, item)
  information <- mirt::iteminfo(iteminfo, Theta)
  data <- as.data.frame(cbind(Theta, information))
  plot <- ggplot(data, aes(x=Theta, y=information)) +
    geom_line()  +labs(y="Item Information")
 return(plot)
                        
}

library(mirt)


ggitemplot(vic_fit, 1, 6)

# Test Information Function

ggtestinfo <- function(dataframe,model){
  Theta <- matrix(seq(-6,6, by = .1))
  T1 <- 0
  for(i in 1:ncol(dataframe)){
    T1 <- T1 + mirt::iteminfo(extract.item(model, i), Theta)
  }
  
 data <- as.data.frame(cbind(Theta, T1))
 plot <-  ggplot(data, aes(x=Theta, y=T1)) +
    geom_line() +labs(y="Test Information")
  return(plot)
}


ggtestinfo(vic,vic_fit)



Theta <- matrix(seq(-6,6, by = .1))

##Item Extraction
observe.1 <- mirt::extract.item(vic_fit, 1)#item number
observe.2 <- extract.item(observe.fit.2, 2)
observe.3 <- extract.item(observe.fit.2, 3)
observe.4 <- extract.item(observe.fit.2, 4)
observe.5 <- extract.item(observe.fit.2, 5)
observe.6 <- extract.item(observe.fit.2, 6)
observe.7 <- extract.item(observe.fit.2, 7)
observe.8 <- extract.item(observe.fit.2, 8)

##Item Info

observe.item.info.1 <- mirt::iteminfo(observe.1, Theta)
observe.item.info.2 <- iteminfo(observe.2, Theta)
observe.item.info.3 <- iteminfo(observe.3, Theta)
observe.item.info.4 <- iteminfo(observe.4, Theta)
observe.item.info.5 <- iteminfo(observe.5, Theta)
observe.item.info.6 <- iteminfo(observe.6, Theta)
observe.item.info.7 <- iteminfo(observe.7, Theta)
observe.item.info.8 <- iteminfo(observe.8, Theta)

#Data frame

observe.item.info.data <- as.data.frame(cbind(Theta, observe.item.info.1, 
                                              observe.item.info.2,
                                              observe.item.info.3,
                            observe.item.info.4,
                            observe.item.info.5,
                            observe.item.info.6, 
                            observe.item.info.7,
                            observe.item.info.8))


## GGplot2 item-info

item1 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.1)) +
  geom_line(linetype = "dashed") +ylim(0,1.2) +apatheme +labs(y="Item Information")

item2 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.2)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme +labs(y="Item Information")

item3 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.3)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")

item4 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.4)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")

item5 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.5)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")

item6 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.6)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")

item7 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.7)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")

item8 <- ggplot(observe.item.info.data, aes(x=Theta, y=observe.item.info.8)) +
  geom_line(linetype = "dashed")+ylim(0,1.2)+apatheme+labs(y="Item Information")


library(ggpubr)

observe.itemfig<- ggarrange(item1, item2, item3,item4, item5,item6,item7,item8,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
observe.itemfig[[1]]
observe.itemfig[[2]]

#Test information
T1 <- 0
for(i in 1:ncol(observe.refit)){
  T1 <- T1 + iteminfo(extract.item(observe.fit.2, i), Theta)
}

observe.test.data <- as.data.frame(cbind(Theta, T1))
ggplot(observe.test.data, aes(x=Theta, y=T1)) +
  geom_line(linetype = "dashed")


ggexport(item1, item2, item3,item4, item5,item6,item7,item8, 
         labels = c("A", "B", "C", "D", "E","F", "G", "H"),
         filename = "test.png",
         nrow = 3, ncol = 2)



