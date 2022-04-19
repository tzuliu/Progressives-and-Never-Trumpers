rm(list=ls(all=TRUE))

library(tidyverse)
library(ggrepel)
library(ggforce)
library(scPCA)
library(factoextra)
library(readit)
library(fastICA)
library(ggpubr)

## loading data for cal_voter
cal <- readRDS("/Users/tpliu/Dropbox/Mac/Desktop/Data/cal_voters.RDS")
#cal <- readRDS("C:/Users/sfmcr/Dropbox/cPCA_cMCA_Project/Data/cal_voters.RDS")

## recode the variable "party" into a binary one
cal$party[cal$party %in% 1:3] <- 1
cal$party[cal$party %in% 4] <- NA
cal$party[cal$party %in% 5:7] <- 2
cal$party <- factor(cal$party, labels = c("Democrat", "Republican"))
#cal$party <- factor(cal$party, levels = c("Republican", "Democrat"))
cal$education[cal$education %in% 6] <- NA
cal$favor_gunregs <- as.numeric(revalue(as.character(cal$favor_gunregs), c("2" = "3", "3" = "2")))
cal <- na.omit(cal)
cal.dem <- cal[cal$party == "Democrat",]
cal.rep <- cal[cal$party == "Republican",]
#cal <- cal %>% mutate(education = na_if(education, 6))

## new data without "party" "strength" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]

## Apply cPCA to the new data
cpca.cal.dem <- scPCA(target = cal2[cal$party == "Democrat",],
                      background = cal2[cal$party == "Republican",],
                      penalties = 0,
                      n_centers = 2)

## create loadings for plots---first method
real_loading <- cbind.data.frame(cpca.cal.dem$rotation, colnames(cal2))
colnames(real_loading) <- c("PC1", "PC2", "names")

## create loadings for plots---second method
## create correlation between variables and PCs, a.k.a. real "loadings"
## which is "correlations between variables and PCs"
########################################################################################
###  real_loading <- as.data.frame(cor(cal2.X[cal2$party == "Democrat",], c_score))  ###
###  colnames(real_loading) <- c("PC1", "PC2")                                       ###
########################################################################################

## Create the function for the coordinates of a unit circle
## The second way is by usgin "geom_circle"
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt) ## cos(theta) = y / r
  yy = center[1] + r * sin(tt) ## sin(theta) = x / r
  return(data.frame(x = xx, y = yy))
}

## Generate coordinates for a unit circle
corcir <- circle(c(0, 0), npoints = 100)

## Create coordinates for arrows of variable vectors
arrows <- data.frame(x1 = rep(0, 23), ## origins of x-axis
                     y1 = rep(0, 23), ## origins of y-axis
                     x2 = real_loading$PC1, ## ends of x-axis
                     y2 = real_loading$PC2) ## ends of y-axis

## plot variable loadings in a unit circle
f2.1 <- ggplot(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2, color=abs(x2))) + 
        geom_link(arrow = grid::arrow(length = grid::unit(0.2, 'cm')),size = 1) +
        geom_path(data = corcir, aes(x = x, y = y), inherit.aes = FALSE, size=0.2, colour = "gray45") +
        geom_text_repel(data = real_loading, aes(x = PC1, y = PC2, color=abs(PC1), label = names), inherit.aes = FALSE, size=4) +
        ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "gray") + 
        labs(title = "(a)",x="Dim1", y="Dim2", col="Contribution") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=20,hjust = 0.5), text = element_text(size=15))

## extract "scores" or "principal components"
c_score <- cpca.cal.dem$x

## create the dataset for individual plot on the contrstive space
cpca.df.cal.dem <- c_score %>% as_tibble() %>%
                   mutate(dem_approval = factor(cal$view_democrats[cal$party == "Democrat"], labels = c("Approve", "Disapprove")))
colnames(cpca.df.cal.dem) <- c("cPC1", "cPC2", "Dem_Approval")  
cpca.df.cal.dem$Dem_Approval <- factor(cpca.df.cal.dem$Dem_Approval, levels = c("Disapprove", "Approve"))

## indivual plot
f2.2 <- ggplot(cpca.df.cal.dem, aes(x=-cPC1, y=cPC2, colour = Dem_Approval, fill = Dem_Approval)) +
        geom_point() +
        stat_ellipse(geom = "polygon", alpha = 0, linetype = 1) +
        labs(title = "(b)", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") +
        scale_fill_manual(values = c("#FC4E07","#00AFBB")) + scale_shape_manual(values=c(20,20)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=20,hjust = 0.5), text = element_text(size=15))


figure2 <- ggarrange(f2.1, f2.2, 
                     #labels = c("(a)", "(b)"),
                     font.label=list(color="black",size=5),
                     nrow = 1)

figure2

#ggsave(f2.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f2.1.eps", device="eps")
#ggsave(f2.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f2.2.eps", device="eps")
ggsave(figure2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure2.v3.eps", device="eps",units="px",width = 3600,height = 2000)


#ggsave(figure2, file="C:/Users/sfmcr/Dropbox/cpca_plot/figure2.v2.eps", device="eps",units="px",width = 3736,height = 2180)

#This is the one we're using
ggsave(figure2, file="C:/Users/sfmcr/Dropbox/cpca_plot/figure2.v3.eps", device="eps",units="px",width = 3600,height = 2000)
