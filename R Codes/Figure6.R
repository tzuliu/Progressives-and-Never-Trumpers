rm(list=ls(all=TRUE))

library(tidyverse)
library(ggrepel)
library(ggforce)
library(scPCA)
library(factoextra)
library(readit)
library(fastICA)
library(lsa)
library(ggrepel)
library(basicspace)

## loading data for cal_voter
cal <- readRDS("/Users/tpliu/Dropbox/Mac/Desktop/Data/cal_voters.RDS")

## recode the variable "party" into a binary one
cal$party[cal$party %in% 1:3] <- 1
cal$party[cal$party %in% 4] <- NA
cal$party[cal$party %in% 5:7] <- 2
cal$party <- factor(cal$party, labels = c("Democrat", "Republican"))
cal$party <- factor(cal$party, levels = c("Republican", "Democrat"))

cal$education[cal$education %in% 6] <- NA
cal$favor_gunregs <- as.numeric(revalue(as.character(cal$favor_gunregs), c("2" = "3", "3" = "2")))
#cal <- cal %>% mutate(education = na_if(education, 6))
## omit missing variables
cal <- na.omit(cal)

## new data without "party" "strength" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]
cal2 <- as.matrix(cal2)


####################################
calbb <- blackbox(cal2, missing=99, dims=2, minscale=5, verbose=TRUE)
xx <- calbb$individuals[[2]][,1]
yy <- calbb$individuals[[2]][,2]

bbdt <- cbind.data.frame(xx, yy, cal$party, cal$view_democrats, cal$favor_borderwall)
colnames(bbdt) <- c("coord1D", "coord2D", "party", "view", "wall")

bbdt_n <- bbdt %>% filter(party == "Democrat" | party == "Republican")

bb.biplot_info <- calbb$stimuli[[2]]
bb.biplot_info$varname <- row.names(bb.biplot_info)
bb.biplot_info$l2norm <- sqrt(bb.biplot_info$w1 ^ 2 + bb.biplot_info$w2 ^ 2)
k = 9
bb.topk_biplot_info <- bb.biplot_info[rank(-bb.biplot_info$l2norm) <= k, ]
# add lrscale
bb.biplot_info.trump <- bb.topk_biplot_info [1, ]
#bb.topk_biplot_info <- rbind(bb.topk_biplot_info, bb.biplot_info.trump)



ggplot(data=bbdt_n, aes(x=coord1D, y=coord2D, color=party, fill=party)) +
  geom_point(alpha=0.4) +
  scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
  stat_ellipse(geom = "polygon", alpha = .3) +
  geom_segment(data = bb.topk_biplot_info,
               aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
               alpha = 1, color = "#000000",
               arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
  theme_minimal()

### Rotation---Aligning "Trump Approval" with the X-Axis
cos <- cosine(as.vector(c(1, 0)), as.vector(c(bb.biplot_info.trump$w1, bb.biplot_info.trump$w2)))
sin <- sqrt(1 - cos^2)
R <- as.matrix(cbind(c(cos, sin), c(-sin, cos)))
rotated_w <- t(R %*% t(as.matrix(cbind(bb.topk_biplot_info$w1, bb.topk_biplot_info$w2))))
bb.topk_rotated_biplot_info <- bb.topk_biplot_info
bb.topk_rotated_biplot_info$w1 <- rotated_w[, 1]
bb.topk_rotated_biplot_info$w2 <- rotated_w[, 2]
rotated_xy <- t(R %*% t(as.matrix(cbind(xx, yy))))
bbdt_n$coord1D.rotated <- rotated_xy[, 1]
bbdt_n$coord2D.rotated <- rotated_xy[, 2]
bbdt_n$coord2D.rotated <- -bbdt_n$coord2D.rotated

fa1.1 <- ggplot(data=bbdt_n, aes(x=coord1D.rotated, y=coord2D.rotated, color=party, fill=party)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "Individual Plot - BB All Californians", x="Dim1", y="Dim2", color="Party ID", fill="Party ID") + 
         #theme(plot.title = element_text(hjust = 0.5)) +
         geom_segment(data = bb.topk_rotated_biplot_info,
                      aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
                      alpha = 1, color = "#000000",
                      arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
         #ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
         geom_text_repel(data = bb.topk_rotated_biplot_info, aes(x = w1*0.1, y = w2*0.1, label = varname), inherit.aes = FALSE) +
         theme_minimal()

ggsave(fa1.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa1.1.eps", device="eps")

######################################
bbdt_n_dd <- bbdt %>% filter(party == "Democrat")
#bbdt_n_dd$view <- as.factor(bbdt_n_dd$view)
bbdt_n_dd$view <- factor(bbdt_n_dd$view, labels = c("Agree", "Disagree"))
bbdt_n_dd$view <- factor(bbdt_n_dd$view, levels=c("Disagree","Agree"))
#levels(bbdt_n_dd$view) <- c("Agree", "Disagree")

bb.biplot_info.view <- bb.biplot_info [16, ]
bb.topk_biplot_info <- rbind.data.frame(bb.topk_biplot_info, bb.biplot_info.view)

cos_dd <- cosine(as.vector(c(1, 0)), as.vector(c(bb.biplot_info.view$w1, bb.biplot_info.view$w2)))
sin_dd <- sqrt(1 - cos_dd^2)
R_dd <- as.matrix(cbind(c(cos_dd, sin_dd), c(-sin_dd, cos_dd)))
rotated_w_dd <- t(R_dd %*% t(as.matrix(cbind(bb.topk_biplot_info$w1, bb.topk_biplot_info$w2))))
bb.topk_rotated_biplot_info_dd <- bb.topk_biplot_info
bb.topk_rotated_biplot_info_dd$w1 <- rotated_w_dd[, 1]
bb.topk_rotated_biplot_info_dd$w2 <- rotated_w_dd[, 2]
rotated_xy_dd <- t(R_dd %*% t(as.matrix(cbind(bbdt_n_dd[,1], bbdt_n_dd[,2]))))

bbdt_n_dd$coord1D.rotated <- rotated_xy_dd[, 1]
bbdt_n_dd$coord2D.rotated <- rotated_xy_dd[, 2]
#bbdt_n_dd$coord2D.rotated <- -bbdt_n_dd$coord2D.rotated

fa1.2 <- ggplot(data=bbdt_n_dd, aes(x=coord1D.rotated, y=coord2D.rotated, color=view, fill=view)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "BB, Democrats Subset (Republicans Included in BB)", x="Dim1", y="Dim2", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") + 
         geom_segment(data = bb.topk_rotated_biplot_info_dd,
                      aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
                      color = "#000000",
                      arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
  #ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
         geom_text_repel(data = bb.topk_rotated_biplot_info_dd, aes(x = w1*0.1, y = w2*0.1, label = varname), inherit.aes = FALSE, size=2.5) +
         theme_minimal()
ggsave(fa1.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa1.2.eps", device="eps")

#################
cal_d <- cal %>% filter(party == "Democrat")

## new data without "party" "strength" and "vote_house"
cal2_d <- cal_d[, !colnames(cal_d) %in% c("party", "strength", "vote_house")]
cal2_d <- as.matrix(cal2_d)

calbb_d <- blackbox(cal2_d, missing=99, dims=2, minscale=5, verbose=TRUE)
xx_d <- calbb_d$individuals[[2]][,1]
yy_d <- calbb_d$individuals[[2]][,2]

bbdt_d <- cbind.data.frame(xx_d,yy_d,cal_d$view_democrats)
colnames(bbdt_d) <- c("coord1D", "coord2D", "View")

bb.biplot_info_d <- calbb_d$stimuli[[2]]
bb.biplot_info_d$varname <- row.names(bb.biplot_info_d)
bb.biplot_info_d$l2norm <- sqrt(bb.biplot_info_d$w1 ^ 2 + bb.biplot_info_d$w2 ^ 2)
bb.topk_biplot_info_d <- bb.biplot_info_d[rank(-bb.biplot_info_d$l2norm) <= k, ]
# add lrscale
bb.biplot_info_d.view <- bb.biplot_info_d [16, ]
bb.topk_biplot_info_d <- rbind.data.frame(bb.topk_biplot_info_d, bb.biplot_info_d.view)

cos_d <- cosine(as.vector(c(1, 0)), as.vector(c(bb.biplot_info_d.view$w1, bb.biplot_info_d.view$w2)))
sin_d <- sqrt(1 - cos_d^2)
R_d <- as.matrix(cbind(c(cos_d, sin_d), c(-sin_d, cos_d)))
rotated_w_d <- t(R_d %*% t(as.matrix(cbind(bb.topk_biplot_info_d$w1, bb.topk_biplot_info_d$w2))))
bb.topk_rotated_biplot_info_d <- bb.topk_biplot_info_d
bb.topk_rotated_biplot_info_d$w1 <- rotated_w_d[, 1]
bb.topk_rotated_biplot_info_d$w2 <- rotated_w_d[, 2]
rotated_xy_d <- t(R_d %*% t(as.matrix(cbind(xx_d, yy_d))))

bbdt_n_d <- bbdt_d %>% filter(View == 1 | View == 2)
bbdt_n_d$View <- factor(bbdt_n_d$View, labels = c("Agree", "Disagree"))
bbdt_n_d$View <- factor(bbdt_n_d$View, levels = c("Disagree", "Agree"))


bbdt_n_d$coord1D.rotated <- rotated_xy_d[, 1]
bbdt_n_d$coord2D.rotated <- rotated_xy_d[, 2]

fa1.3 <- ggplot(data=bbdt_n_d, aes(x=coord1D.rotated, y=coord2D.rotated, color=View, fill=View)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "BB, Democrats Only (Republicans Excluded in BB)", x="Dim1", y="Dim2", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") + 
         geom_segment(data = bb.topk_rotated_biplot_info_d,
                      aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
                      color = "#000000",
                      arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
         #ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
         geom_text_repel(data = bb.topk_rotated_biplot_info_d, aes(x = w1*0.1, y = w2*0.1, label = varname), inherit.aes = FALSE, size=2.5) +
         theme_minimal()
ggsave(fa1.3, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa1.3.eps", device="eps")

########################
bbdt_n_rr <- bbdt %>% filter(party == "Republican")
bbdt_n_rr$wall <- factor(bbdt_n_rr$wall, labels = c("Approval", "Disapproval"))
bbdt_n_rr$wall <- factor(bbdt_n_rr$wall, levels=c("Disapproval","Approval"))
#levels(bbdt_n_dd$view) <- c("Agree", "Disagree")

bb.biplot_info.wall <- bb.biplot_info [5, ]
#bb.topk_biplot_info <- rbind.data.frame(bb.topk_biplot_info, bb.biplot_info.view)

cos_rr <- cosine(as.vector(c(1, 0)), as.vector(c(bb.biplot_info.wall$w1, bb.biplot_info.wall$w2)))
sin_rr <- sqrt(1 - cos_rr^2)
R_rr <- as.matrix(cbind(c(cos_rr, sin_rr), c(-sin_rr, cos_rr)))
rotated_w_rr <- t(R_rr %*% t(as.matrix(cbind(bb.topk_biplot_info$w1, bb.topk_biplot_info$w2))))
bb.topk_rotated_biplot_info_rr <- bb.topk_biplot_info
bb.topk_rotated_biplot_info_rr$w1 <- rotated_w_rr[, 1]
bb.topk_rotated_biplot_info_rr$w2 <- rotated_w_rr[, 2]
rotated_xy_rr <- t(R_rr %*% t(as.matrix(cbind(bbdt_n_rr[,1], bbdt_n_rr[,2]))))

bbdt_n_rr$coord1D.rotated <- rotated_xy_rr[, 1]
bbdt_n_rr$coord2D.rotated <- rotated_xy_rr[, 2]
bbdt_n_rr$coord1D.rotated <- -bbdt_n_rr$coord1D.rotated#flipping the x-axis

fa1.4 <- ggplot(data=bbdt_n_rr, aes(x=coord1D.rotated, y=coord2D.rotated, color=wall, fill=wall)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "BB, Republicans Subset (Democrats Included in BB)", x="Dim1", y="Dim2", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") + 
         geom_segment(data = bb.topk_rotated_biplot_info_rr,
                      aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
                      color = "#000000",
                      arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
  #ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
         geom_text_repel(data = bb.topk_rotated_biplot_info_rr, aes(x = w1*0.1, y = w2*0.1, label = varname), inherit.aes = FALSE, size=2.5) +
         theme_minimal()
ggsave(fa1.4, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa1.4.eps", device="eps")

#################
cal_r <- cal %>% filter(party == "Republican")

## new data without "party" "strength" and "vote_house"
cal2_r <- cal_r[, !colnames(cal_r) %in% c("party", "strength", "vote_house")]
cal2_r <- as.matrix(cal2_r)

calbb_r <- blackbox(cal2_r, missing=99, dims=2, minscale=5, verbose=TRUE)
xx_r <- calbb_r$individuals[[2]][,1]
yy_r <- calbb_r$individuals[[2]][,2]

bbdt_r <- cbind.data.frame(xx_r,yy_r,cal_r$favor_borderwall)
colnames(bbdt_r) <- c("coord1D", "coord2D", "wall")

bb.biplot_info_r <- calbb_r$stimuli[[2]]
bb.biplot_info_r$varname <- row.names(bb.biplot_info_r)
bb.biplot_info_r$l2norm <- sqrt(bb.biplot_info_r$w1 ^ 2 + bb.biplot_info_r$w2 ^ 2)
bb.topk_biplot_info_r <- bb.biplot_info_r[rank(-bb.biplot_info_r$l2norm) <= k, ]
# add lrscale
bb.biplot_info_r.wall <- bb.biplot_info_r [12, ]
#bb.topk_biplot_info_d <- rbind.data.frame(bb.topk_biplot_info_d, bb.biplot_info_d.view)

cos_r <- cosine(as.vector(c(1, 0)), as.vector(c(bb.biplot_info_r.wall$w1, bb.biplot_info_r.wall$w2)))
sin_r <- sqrt(1 - cos_r^2)
R_r <- as.matrix(cbind(c(cos_r, sin_r), c(-sin_r, cos_r)))
rotated_w_r <- t(R_r %*% t(as.matrix(cbind(bb.topk_biplot_info_r$w1, bb.topk_biplot_info_r$w2))))
bb.topk_rotated_biplot_info_r <- bb.topk_biplot_info_r
bb.topk_rotated_biplot_info_r$w1 <- rotated_w_r[, 1]
bb.topk_rotated_biplot_info_r$w2 <- rotated_w_r[, 2]
rotated_xy_r <- t(R_r %*% t(as.matrix(cbind(xx_r, yy_r))))

bbdt_n_r <- bbdt_r %>% filter(wall == 1 | wall == 2)
bbdt_n_r$wall <- factor(bbdt_n_r$wall, labels = c("Approval", "Disapproval"))
bbdt_n_r$wall <- factor(bbdt_n_r$wall, levels = c("Disapproval", "Approval"))


bbdt_n_r$coord1D.rotated <- rotated_xy_r[, 1]
bbdt_n_r$coord2D.rotated <- rotated_xy_r[, 2]
bbdt_n_r$coord1D.rotated <- -bbdt_n_r$coord1D.rotated

fa1.5 <- ggplot(data=bbdt_n_r, aes(x=coord1D.rotated, y=coord2D.rotated, color=wall, fill=wall)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "BB, Republicans Only (Democrats Excluded in BB)", x="Dim1", y="Dim2", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") + 
         geom_segment(data = bb.topk_rotated_biplot_info_r,
                      aes(x = 0, y = 0, xend = w1*0.1, yend = w2*0.1),
                      color = "#000000",
                      arrow = arrow(length = unit(0.15, "cm")), arrow.fill = "#000000", inherit.aes = F) +
  #ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
         geom_text_repel(data = bb.topk_rotated_biplot_info_r, aes(x = w1*0.1, y = w2*0.1, label = varname), inherit.aes = FALSE, size=2.5) +
         theme_minimal()
ggsave(fa1.5, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa1.5.eps", device="eps")




