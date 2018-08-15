#Date: 2018-Aug-14
#Project: Example of functions in annotate
#Author: Donnelly West - DonnellyAWest at g mail dot com
#Version: 1.0
# purpose: showing the usefulness of functions in annotate

# Function to draw bargraph with standard error of the mean error bars:

plot.summary=function(x) {
  x=na.omit(x)
  y=mean(x)
  sem=sd(x)/sqrt(length(x))
  return(data.frame(
    y=y,
    ymin=y-sem,
    ymax=y+sem
  ))}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#install & load useful libraries ----
library(lme4)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5)) # for centered titles
library(dplyr)
library(cowplot)
library(ggpubr)
library(ggpmisc)
library(gridExtra)


# example data frame ----
data(iris)
head(iris)

# set wd for output images ----
setwd("~/Desktop/")

# add example column you'd like to annotate
iris_w_col <- iris %>% 
  group_by(Species) %>% 
  mutate(collector = "A")


# Graphing ----

# create theme ----

# bar plots 
a <- theme(legend.position = c(.12,.82),
      legend.direction="vertical",
      legend.title=element_blank(),
      legend.key.size = unit(.8, "cm"),
      legend.text = element_text(size = 25),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
      axis.title = element_text(size = 25, face = "bold"),
      axis.text = element_text(size = 25),
      axis.text.x = element_text(face = "italic"),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))



# example bar plot ----
 ggplot(iris_w_col, aes( x = Species, y = Sepal.Length, fill = Species))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text",
               aes( x = Species,
                    y = -.1,
                    label = collector),
               position = position_dodge(.6),
               color = "pink",
               size = 6)+
  a+
  ggtitle("Iris Sepal Lengths by Species")+
  ylab("Sepal Length (cm)")+
  xlab("Species")+
  annotate(geom = "text", color = "pink", label="Collector ID", size=6, x = length(levels(iris_w_col$Species))*.22, y = mean(iris_w_col$Sepal.Length)*1.27)

ggsave("Annotate_example.png", height = 10, width = 15) # save the png
