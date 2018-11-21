


library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)

#################################################################################################
# Sample Data


x1 <- sample.int(100,10)
x2 <- sample.int(100,10)+20


y <- sample.int(100,10)


x3 <- sample.int(100,20)
x4 <- sample.int(100,20)+20
treatment <- append(rep("A",10),rep("B",10))

index = array(1:10)


xdf0 <- data.frame(x1,y)
xdf1 <- data.frame(x1,x2,y)
xdf2 <- data.frame(x1,x2)
xdf3 <- data.frame(x3,x4,treatment)
xdf4 <- data.frame(index,x1)
xdf5 <- data.frame(index,x1,x2)

write.csv(xdf0,'./xdf0.csv')
write.csv(xdf1,'./xdf1.csv')
write.csv(xdf2,'./xdf2.csv')
write.csv(xdf3,'./xdf3.csv')
write.csv(xdf4,'./xdf4.csv')
write.csv(xdf5,'./xdf5.csv')


x1label = 'Sample X Data'
x2label = 'Sample X2 Data'
ylabel = 'Sample Y Data'

################################################################################################


## ------------------------------------------------------------------------
# Sample scatter plot with one variable

# Input data ####################################
indata <- xdf0

fname <- 'Scatter.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################




out <- paste(fname)
pdf(out,width=7.5,height=5)

gg = ggplot(indata, aes_string(x=colnames(indata)[1], y = colnames(indata)[2])) + geom_point()

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12))


gg <- gg + scale_x_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,2,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg

dev.off()


## ------------------------------------------------------------------------
# Sample scatter plot with two variables

# Input data ####################################
indata <- xdf1 %>%
  gather(treatment,classval,-y) # specify column name of response variable=y
                                #specify column name of treatment variable=treatment


fname <- 'Scatter2.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################



out <- paste(fname)
pdf(out,width=7.5,height=5)

gg = ggplot(indata, aes_string(x = colnames(indata)[3], 
                               y = colnames(indata)[1],
                               color=colnames(indata)[2])) + 
                                geom_point()

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12)) +
            scale_color_grey(start=0.8, end=0.2)

gg <- gg + scale_x_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,0,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg

dev.off()


## ------------------------------------------------------------------------
# Sample box plot with one treatment

# Input data ####################################
indata <- xdf2 %>%
  gather(classname,classval)

fname <- 'Box.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################


out <- paste(fname)
pdf(out,width=7.5,height=5)

labels <- unique(indata[,1])
labeldf <- data.frame(classname=labels,"labelind"=array(1:length(labels)))

indata <- merge(indata,labeldf,by=colnames(indata)[1])

gg = ggplot(indata, aes(x = labelind, y = classval,group=labelind)) + geom_boxplot()

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12))



gg <- gg + scale_x_continuous(breaks=unique(indata$labelind),
                              labels=labels,
                              sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,1,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg


dev.off()


## ------------------------------------------------------------------------
# Sample box plot with two treatments

# Input data ####################################
indata <- xdf3 %>%
  gather(classname,classval,-treatment) 
treatmentname = "treatmemt" #Specify name of treatment column = "treatment"

fname <- 'Box2.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################

out <- paste(fname)
pdf(out,width=7.5,height=5)

labels <- unique(indata$classname)
labeldf <- data.frame("classname"=labels,"labelind"=array(1:length(labels)))
indata <- merge(indata,labeldf,by="classname")

gg = ggplot(data=indata, aes(x = labelind, y = classval,group=interaction(labelind,treatment),
                             fill=treatment)) + geom_boxplot(color='black')

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12)) +
            scale_fill_grey(start=0.8, end=0.2)




gg <- gg + scale_x_continuous(breaks=unique(indata$labelind),
                              labels=labels,
                              sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,0,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg <- gg + guides(fill=guide_legend(title=treatmentname))

gg

dev.off()


## ------------------------------------------------------------------------
# Sample bar chart with one treatment

# Input data ####################################
indata <- xdf2 %>%
  gather(classname,classval)


fname <- 'Bar.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################


meandf <- aggregate(indata$classval,list(indata$classname),mean)
meandf <- plyr::rename(meandf,c("Group.1"="classname","x"="classval"))

sddf <- aggregate(indata$classval,list(indata$classname),sd)
sddf <- plyr::rename(sddf,c("Group.1"="classname","x"="sd"))

indata <- merge(meandf,sddf,by="classname")

out <- paste(fname)
pdf(out,width=7.5,height=5)

labels <- unique(indata$classname)
labeldf <- data.frame("classname"=labels,"labelind"=array(1:length(labels)))
indata <- merge(indata,labeldf,by="classname")

gg = ggplot(indata, aes(x = labelind, y = classval)) + geom_bar(stat='identity') +
      geom_errorbar(aes(ymin=classval-sd,ymax=classval+sd),width=0.2)

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12))



gg <- gg + scale_x_continuous(breaks=unique(indata$labelind),
                              labels=labels,
                              sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,1,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg


dev.off()


## ------------------------------------------------------------------------
#Sample bar chart with two treatments

# Input data ####################################
indata <- xdf3 %>%
  gather(classname,classval,-treatment) 
treatmentname = "treatment" #Specify name of treatment column = treatment

fname <- 'Bar2.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################


meandf <- aggregate(classval~classname+treatment,indata,mean)


sddf <- aggregate(classval~classname+treatment,indata,sd)

indata <- merge(meandf,sddf,by=c("classname","treatment"))

indata <- plyr::rename(indata,c("classname"="classname",
                                "treatment"="treatment",
                                "classval.x"="classval",
                                "classval.y"="sd"))


out <- paste(fname)
pdf(out,width=7.5,height=5)

labels <- unique(indata$classname)
labeldf <- data.frame("classname"=labels,"labelind"=array(1:length(labels)))
indata <- merge(indata,labeldf,by="classname")

gg = ggplot(indata, aes(x = labelind, y = classval,
                             fill=treatment)) + 
      geom_bar(stat='identity',position=position_dodge()) +
      geom_errorbar(aes(ymin=classval-sd,ymax=classval+sd),width=0.2,
                    position=position_dodge(0.9))

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black")) +
          scale_fill_grey(start=0.8, end=0.2)

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12))



gg <- gg + scale_x_continuous(breaks=unique(indata$labelind),
                              labels=labels,
                              sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,0,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg <- gg + guides(fill=guide_legend(title=treatmentname))

gg


dev.off()


## ------------------------------------------------------------------------
# Sample line chart with one variable


# Input data ####################################
indata <- xdf4

fname <- 'Line.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################


out <- paste(fname)
pdf(out,width=7.5,height=5)

gg = ggplot(indata, aes_string(x=colnames(indata)[1],y=colnames(indata)[2])) + geom_line()

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12))


gg <- gg + scale_x_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,2,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg

dev.off()




## ------------------------------------------------------------------------
# Sample line chart with two variables

# Input data ####################################
indata <- xdf5 %>%
  gather(treatment,classval,-index) # specify column name of x variable=index
                                    #specify column name of treatment variable=treatment 


fname <- 'Line2.pdf'

xlabel <- 'Sample X Data'
ylabel <- 'Sample Y Data'

#################################################



out <- paste(fname)
pdf(out,width=7.5,height=5)

gg = ggplot(indata, aes_string(y=colnames(indata)[3],x=colnames(indata)[1],color=colnames(indata)[2])) + geom_line()

gg <- gg + theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"))

gg <- gg + theme(axis.ticks.length=unit(-0.25, "cm"), 
                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                 text=element_text(size=12)) + 
            scale_color_grey(start=0.8, end=0.2)


gg <- gg + scale_x_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))
gg <- gg + scale_y_continuous(sec.axis = dup_axis(labels=NULL,name = waiver()))

gg <- gg + theme(plot.margin=margin(1,0,0.5,0.5,"cm"))


gg <- gg + xlab(xlabel) + ylab(ylabel)


gg

dev.off()


