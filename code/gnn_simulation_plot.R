
##########################################################################################
## simulation plot
# library(ggplot2)
# library(plyr)
# library(ggrepel)
# 
# setwd("~/code/graph nnw/results/g3/revision_round_4/")
# # n.hypo = 6
# n.graph = 10^3
# n.sim = 10^6
# n.corr = 3
# n.scen = 4
# # corr.vec = c(0.5, 0.7, 0.9)
# options(scipen = 999)
# tol = 0
# options(digits=10)
# 
# data.plot = NULL
# for (scen.ind in 1:n.scen){
# 
#   if (scen.ind==1) {
#     pow.vec = c(0.7, 0.8, 0.9)
#     obj.vec = c(0.1, 0.3, 0.6)
#   } else if (scen.ind==2) {
#     pow.vec = c(0.6, 0.75, 0.9)
#     obj.vec = c(0.1, 0.3, 0.6)
#   } else if (scen.ind==3) {
#     pow.vec = c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
#     obj.vec = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
#   } else if (scen.ind==4) {
#     pow.vec = c(0.7, 0.7, 0.8, 0.8, 0.9, 0.9)
#     obj.vec = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
#   }
# 
#   n.hypo = length(pow.vec)
#   corr.vec = c(0, 0.3, 0.5)
# 
#   for (corr.ind in 1:n.corr){
#     corr = corr.vec[corr.ind]
# 
#     opt.csv = read.csv(file = paste0("fit pow ", paste(pow.vec, collapse = " "),
#                                      " obj ", paste(obj.vec, collapse = " "),
#                                      " corr.mat ", 1, " corr ", corr, " tol ", tol, " hypo ",
#                                      n.hypo,  ".csv"))
#     load(file = paste0("data pow ", paste(pow.vec, collapse = " "),
#                        " obj ", paste(obj.vec, collapse = " "),
#                        " corr.mat ", 1,  " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ",
#                        n.graph,  ".RData"))
# 
#     # target.power.in = as.numeric(sprintf("%.5f", round(target.power.in, 5)))
#     # opt.csv$opt_real_power = as.numeric(sprintf("%.5f",
#     #                         round(opt.csv$opt_real_power, 5)))
# 
#     target.power.in = sim.data.fit$data.matrix$target.power
# 
#     # sub.vec = (0:999)*10 + 1
#     sub.vec = 1:n.graph
# 
#     max.ind = which.max(target.power.in)[1]
#     sub.vec = c(sub.vec, max.ind)
#     sub.vec = unique(sub.vec)
#     sub.vec = sub.vec[order(sub.vec)]
#     # sub.vec = 1:n.graph
# 
#     data.plot.temp = data.frame(pow.vec =  paste0("Scenario ", scen.ind, ": ", paste(paste0(pow.vec*100, "%"), collapse = " ")),
#                                 corr = c(c(rep(corr- 0.035, n.graph/5), rep(corr+0.035,n.graph*4/5))[sub.vec],
#                                          corr),
#                                 obj = c(target.power.in[sub.vec], opt.csv$opt_real_power),
#                                 label = c(c(rep("Validation Dataset", n.graph/5), rep("Training Dataset",n.graph*4/5))[sub.vec],
#                                           "FNN-based Optimizer"),
#                                 text.label = ""
#                                 )
#     data.plot.temp$text.label = as.character(data.plot.temp$text.label)
#     data.plot.temp$text.label[data.plot.temp$label=="FNN-based Optimizer"] = paste0("FNN\n", paste0(sprintf("%.1f",opt.csv$opt_real_power*100), "%"))
#     data.plot.temp$text.label[which(data.plot.temp$obj==(max(target.power.in)))[1]] = paste0("BF\n", paste0(sprintf("%.1f", max(target.power.in*100)), "%"))
# 
#     # if (scen.ind==1) nudge_y_in = 0.02
#     # if (scen.ind==2) nudge_y_in = 0.1
#     # if (scen.ind==3) nudge_y_in = 0.04
#     # if (scen.ind==4) nudge_y_in = 0.01
#     # if (scen.ind==5) nudge_y_in = 0.02
#     # if (scen.ind==6) nudge_y_in = 0.02
#     #
#     # data.plot.temp$nudge_y = nudge_y_in
# 
# 
#     data.plot = rbind(data.plot, data.plot.temp)
#   }
# }
# #data.plot$corr = factor(data.plot$corr)
# 
# blank_data <- data.frame(pow.vec = rep(levels(data.plot$pow.vec), each = 2),
#                          corr = c(-0.15, 0.7, -0.15, 0.7, -0.15, 0.7, -0.15, 0.7),
#                          obj = c(0.65, 0.9, 0.60, 0.9, 0.70, 0.85, 0.65, 0.85),
#                          label = "",
#                          text.label = "")
# 
# png("~/code/graph nnw/results/sim_plot.png",  width = 2000, height = 1800)
# 
# ggplot.fit = ggplot(data.plot, aes(x=corr, y=obj, label = text.label)) +
#   geom_blank(data = blank_data, aes(x = corr, y = obj)) +
#   geom_point(data = data.plot, size=12, shape=16,
#              aes(col = label, alpha = label)) +
#   geom_text_repel(
#     data = subset(data.plot, label=="FNN-based Optimizer"),
#     nudge_y      =  0.03,
#     nudge_x      = 0.04,
#     direction    = "x",
#     angle        = 0,
#     vjust = 0.1,
#     hjust        = 0.3,
#     segment.size = 2,
#     segment.alpha = 0.4,
#     size = 14
#   ) +
#   geom_text_repel(
#     data = subset(data.plot, (!label=="FNN-based Optimizer")&(!text.label=="")),
#     nudge_y      = -0.05,
#     nudge_x      = 0.04,
#     direction    = "x",
#     angle        = 0.2,
#     vjust = 0.1,
#     hjust        = 0.25,
#     segment.size = 2,
#     segment.alpha = 0.4,
#     size = 14
#   ) +
#   scale_alpha_manual(values=c(1, 1, 1)) +
#   scale_x_continuous(breaks=c(0, 0.3, 0.5)) +
#   # scale_x_continuous(breaks=ifelse(as.numeric(data.plot$pow.vec)<=2,c(0, 0.3, 0.7),
#   #                                  c(0.5, 0.7, 0.9))) +
#   # scale_x_continuous(breaks=c(0.5, 0.7, 0.9), limits = c(0.5-0.06, 0.9+0.15)) +
#   scale_y_continuous(breaks = scales::pretty_breaks(7)) +
#   # labs(title = "Optimal graph has better performance than brute-force searching with six endpoints and strong correlation") +
#   ylab ("Objective Function") + xlab("Correlation") +
#   theme_bw() +
#   theme(text = element_text(size=45),plot.margin = unit(c(2,3,2,1),units="lines"),
#         axis.text.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="black",size=45,angle=0,hjust=1,vjust=0,face="plain"),
#         axis.title.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="black",size=45,angle=90,hjust=.5,vjust=.5,face="plain",
#                                     margin = margin(t = 0, r = 30, b = 0, l = 0)),
#         legend.text = element_text(colour="black", size = 42, face = "plain"),
#         legend.title = element_text(colour="black", size = 45, face = "plain"),
#         legend.key.size = unit(2,"line"),
#         legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
#   facet_wrap( ~pow.vec, scales = "free", ncol = 2)
# print(ggplot.fit)
# dev.off()









# ##########################################################################################
# ## simulation tables
# setwd("~/code/graph nnw/results/g3/revision_round_4/")
# library(ggplot2)
# library(plyr)
# library(ggrepel)
# 
# # n.hypo = 6
# n.graph = 10^3
# n.sim = 10^6
# n.corr = 3
# n.scen = 4
# # corr.vec = c(0.5, 0.7, 0.9)
# options(scipen = 999)
# tol = 0
# options(digits=10)
# sim.table = NULL
# 
# for (scen.ind in 1:n.scen){
# 
#   if (scen.ind==1) {
#     pow.vec = c(0.7, 0.8, 0.9)
#     obj.vec = c(0.1, 0.3, 0.6)
#   } else if (scen.ind==2) {
#     pow.vec = c(0.6, 0.75, 0.9)
#     obj.vec = c(0.1, 0.3, 0.6)
#   } else if (scen.ind==3) {
#     pow.vec = c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
#     obj.vec = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
#   } else if (scen.ind==4) {
#     pow.vec = c(0.7, 0.7, 0.8, 0.8, 0.9, 0.9)
#     obj.vec = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
#   }
# 
#   n.hypo = length(pow.vec)
#   corr.vec = c(0, 0.3, 0.5)
# 
#   for (corr.ind in 1:n.corr){
#     corr = corr.vec[corr.ind]
# 
#     naive.csv = read.csv(file = paste0("GF ", paste(pow.vec, collapse = " "),
#                                        " obj ", paste(obj.vec, collapse = " "),
#                                        " corr.mat ", 1, " corr ", corr,
#                                        " GF_n ", 1, " hypo ",
#                                        n.hypo,  ".csv"))
#     opt.csv = read.csv(file = paste0("fit pow ", paste(pow.vec, collapse = " "),
#                                      " obj ", paste(obj.vec, collapse = " "),
#                                      " corr.mat ", 1, " corr ", corr, " tol ", tol, " hypo ",
#                                      n.hypo,  ".csv"))
#     opt.cross.csv = read.csv(file = paste0("cross pow ", paste(pow.vec, collapse = " "),
#                                            " obj ", paste(obj.vec, collapse = " "),
#                                            " corr.mat ", 1, " corr ", corr, " tol ", tol, " hypo ",
#                                       n.hypo,  ".csv"))
#     load(file = paste0("data pow ", paste(pow.vec, collapse = " "),
#                        " obj ", paste(obj.vec, collapse = " "),
#                        " corr.mat ", 1,  " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ",
#                                            n.graph,  ".RData"))
# 
# 
#     if (corr.ind==1){
#       # scen.name.temp =
#       #   paste0(paste(paste0(pow.vec*100, "%"), collapse = " "))
#       scen.name.temp = scen.ind
#     } else {
#       scen.name.temp = ""
#     }
# 
#     opt.total.time = as.numeric(opt.csv$time+opt.cross.csv$time[1]+sim.data.fit$sim.data.time.diff)/60
#     naive.total.time = as.numeric(naive.csv$time)/60
# 
#     sim.table.temp = c(scen.name.temp,
#                        corr,
#                        paste0(sprintf("%.1f", opt.csv$opt_real_power*100), "%"),
#                        paste0(sprintf("%.1f", as.numeric(naive.csv$fit.power)*100), "%"),
#                        paste0(sprintf("%.1f", opt.csv$max_power*100), "%"),
#                        sprintf("%.1f", opt.total.time),
#                        sprintf("%.1f", naive.total.time),
#                        sprintf("%.1f", as.numeric(sim.data.fit$sim.data.time.diff/60))
#                        )
#     sim.table = rbind(sim.table, sim.table.temp)
# 
# 
#   }
# }
# colnames(sim.table) = c("scen", "corr", "FNN", "ISRES", "COBYLA", "naive","FNN_t",
#                         "ISRES_t", "COBYLA_t", "naive_t")
# sim.table = data.frame(sim.table, stringsAsFactors = FALSE)
# 
# library(xtable)
# sim.table$ISRES_t = "-"
# print(xtable(sim.table),include.rownames = FALSE)

##########################################################################################
## simulation tables (for revision, sensitivity analysis)
# library(ggplot2)
# library(plyr)
# library(ggrepel)
# 
# setwd("~/code/graph nnw/results/g6/revision_round_4/")
# 
# 
# # n.hypo = 6
# n.graph = 10^3
# n.sim = 10^6
# n.scen = 10
# n.hypo = 6
# n.corr = 1
# # corr.vec = c(0.5, 0.7, 0.9)
# options(scipen = 999)
# tol = 0
# options(digits=10)
# sim.table = NULL
# GF.fit.n = 1
# 
# for (scen.ind in 1:n.scen){
# 
#   if (scen.ind==1) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 1
#     pow.vec = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
#     M.name = "M4"
#     O.name = "O1"
#     S.name = "S1"
#     C.name = "C1"
#   } else if (scen.ind==2) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 1
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = "M5"
#     O.name = ""
#     S.name = ""
#     C.name = ""
#   } else if (scen.ind==3) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 1
#     pow.vec = c(0.8, 0.8, 0.8, 0.9, 0.9, 0.9)
#     M.name = "M6"
#     O.name = ""
#     S.name = ""
#     C.name = ""
#   } else if (scen.ind==4) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 5), 0.5)
#     corr.mat.ind = 1
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = "M5"
#     O.name = "O2"
#     S.name = "S1"
#     C.name = "C1"
#   } else if (scen.ind==5) {
#     corr.vec = 0.5
#     obj.weight = c(0.05, 0.05, 0.1, 0.1, 0.35, 0.35)
#     corr.mat.ind = 1
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = ""
#     O.name = "O3"
#     S.name = ""
#     C.name = ""
#   } else if (scen.ind==6) {
#     corr.vec = 0.5
#     obj.weight = rep(1, 6)
#     corr.mat.ind = 1
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = ""
#     O.name = "O4"
#     S.name = ""
#     C.name = ""
#   } else if (scen.ind==7) {
#     corr.vec = 0.3
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 2
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = "M5"
#     O.name = "O1"
#     S.name = "S2"
#     C.name = "C2"
#   } else if (scen.ind==8) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 2
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = ""
#     O.name = ""
#     S.name = ""
#     C.name = "C1"
#   } else if (scen.ind==9) {
#     corr.vec = 0.3
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 3
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = ""
#     O.name = ""
#     S.name = "S3"
#     C.name = "C2"
#   } else if (scen.ind==10) {
#     corr.vec = 0.5
#     obj.weight = c(rep(0.1, 4), 0.3, 0.3)
#     corr.mat.ind = 3
#     pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
#     M.name = ""
#     O.name = ""
#     S.name = ""
#     C.name = "C1"
#   }
# 
# 
# 
#   for (corr.ind in 1:n.corr){
#     corr = corr.vec[corr.ind]
# 
#     naive.csv = read.csv(file = paste0("GF ", paste(pow.vec, collapse = " "),
#                                        " obj ", paste(obj.weight, collapse = " "),
#                                        " corr.mat ", corr.mat.ind,
#                                        " corr ", corr, " GF_n ", GF.fit.n, " hypo ", n.hypo,  ".csv"))
#     opt.csv = read.csv(file = paste0("fit pow ", paste(pow.vec, collapse = " "),
#                                      " obj ", paste(obj.weight, collapse = " "),
#                                      " corr.mat ", corr.mat.ind,
#                                      " corr ", corr, " tol ", tol, " hypo ", n.hypo, ".csv"))
#     opt.cross.csv = read.csv(file = paste0("cross pow ", paste(pow.vec, collapse = " "),
#                                            " obj ", paste(obj.weight, collapse = " "),
#                                            " corr.mat ", corr.mat.ind,
#                                            " corr ", corr, " tol ", tol, " hypo ", n.hypo,   ".csv"))
#     load(file = paste0("data pow ", paste(pow.vec, collapse = " "),
#                        " obj ", paste(obj.weight, collapse = " "),
#                        " corr.mat ", corr.mat.ind,
#                        " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", n.graph,   ".RData"))
# 
# 
#     # if (corr.ind==1){
#     #   # scen.name.temp =
#     #   #   paste0(paste(paste0(pow.vec*100, "%"), collapse = " "))
#     #   scen.name.temp = scen.ind
#     # } else {
#     #   scen.name.temp = ""
#     # }
# 
#     opt.total.time = as.numeric(opt.csv$time+opt.cross.csv$time[1]+sim.data.fit$sim.data.time.diff)/60
#     naive.total.time = as.numeric(naive.csv$time)/60
# 
#     sim.table.temp = c(M.name,
#                        O.name,
#                        S.name,
#                        C.name,
#                        paste0(sprintf("%.1f", opt.csv$opt_real_power*100), "%"),
#                        paste0(sprintf("%.1f", as.numeric(naive.csv$fit.power)*100), "%"),
#                        paste0(sprintf("%.1f", opt.csv$max_power*100), "%"),
#                        sprintf("%.1f", opt.total.time),
#                        sprintf("%.1f", naive.total.time),
#                        sprintf("%.1f", as.numeric(sim.data.fit$sim.data.time.diff/60))
#     )
#     sim.table = rbind(sim.table, sim.table.temp)
# 
#     print(naive.csv$status)
#   }
# }
# colnames(sim.table) = c("M", "O", "S", "C","FNN", "ISRES", "COBYLA", "naive","FNN_t",
#                         "ISRES_t", "COBYLA_t", "naive_t")
# sim.table = data.frame(sim.table, stringsAsFactors = FALSE)
# 
# library(xtable)
# sim.table$ISRES_t = "-"
# print(xtable(sim.table),include.rownames = FALSE)

#########################################################################################################
## case study table and graphs
library(gMCP)
library(xtable)
setwd("~/code/graph nnw/results/gc3/revision_round_4/")
opt.in = read.csv("fit pow 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 tol 0 hypo 5.csv")
naive.in = read.csv("GF 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 GF_n 1 hypo 5.csv")
brute.in = read.csv("fit mat 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 tol 0 hypo 5.csv")
load("data pow 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 n.sim 6 n.graph 1000.RData")

case.study.table = matrix(NA, nrow = 4, ncol = 6)
for (i in 1:4){

  if (i==1) graph.file = opt.in
  if (i==2) graph.file = naive.in[1, ]
  if (i==3) graph.file = naive.in[2, ]
  if (i==4) graph.file = brute.in[which.max(brute.in$fit.power), ]

  alpha.in = c(1, 0, 0, 0, 0)
  w.in = matrix(c(0, graph.file$w1_2, graph.file$w1_3, graph.file$w1_4, 1-graph.file$w1_2-graph.file$w1_3-graph.file$w1_4,
                  0, 0, graph.file$w2_3, graph.file$w2_4, 1- graph.file$w2_3-graph.file$w2_4,
                  0, graph.file$w3_2, 0, graph.file$w3_4, 1- graph.file$w3_2 - graph.file$w3_4,
                  0, graph.file$w4_2, graph.file$w4_3, 0, 1-graph.file$w4_2-graph.file$w4_3,
                  0, graph.file$w5_2, graph.file$w5_3, 1-graph.file$w5_2 - graph.file$w5_3, 0
                  ),
                nrow = 5, ncol = 5, byrow = TRUE)

  for (j in 1:5){
    w.in[j, ] = abs(w.in[j,])/(0.00001+sum(w.in[j,]))
    w.in[j, ] = round(w.in[j, ], 3)
    w.in[j, ] = c(w.in[j, 1:4], 1-sum(w.in[j, 1:4]))
  }
  #w.in = round(w.in, 3)

  # w.in = round(w.in, 3)
  graph.in = matrix2graph(w.in, alpha.in)
  # graphGUI(graph.in)

  out_seq = graphTest(pvalues = t(sim.data.fit$pval.matrix), graph = graph.in, alpha = 0.025)
  out.power = apply(out_seq, 2, mean)
  case.study.table[i,] = paste0(sprintf("%.1f", c(sum(c(0, 0.6, 0.2, 0.1, 0.1)*out.power), out.power)*100), "%")
}

case.study.table = cbind(c("FNN", "ISRES", "COBYLA", "Brute-force"), case.study.table)
print(xtable(case.study.table),include.rownames = FALSE)

## case study SD
# library(gMCP)
# library(xtable)
# setwd("~/code/graph nnw/results/gc3/revision_round_4/sd/")
# 
# n.rep = 100
# sd.out = matrix(NA, nrow = n.rep, ncol = 4)
# for(rep.ind in 1:n.rep){
#   opt.in = read.csv(
#     paste0("fit pow 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 tol 0 hypo 5 rep.ind ",
#            rep.ind,".csv"))
#   naive.in = read.csv(
#     paste0("GF 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr.mat 1 corr 0.5 GF_n 1 hypo 5 rep.ind ",
#                              rep.ind,".csv"))
#   sd.out[rep.ind, ] = c(opt.in$opt_real_power, naive.in$fit.power, opt.in$max_power)
# 
# }
# print(apply(sd.out, 2, mean))
# print(apply(sd.out, 2, sd))
# print(apply(sd.out, 2, range))
# 
# #########################################################################
# ## Figure 1 plot
# alpha.in = c(0.5, 0, 0.5, 0)
# w.in = matrix(c(0, 0.8, 0.2, 0,
#                 0, 0, 1, 0,
#                 0.2, 0, 0, 0.8,
#                 1, 0, 0, 0
# ), 
# nrow = 4, ncol = 4, byrow = TRUE)
# graph.in = matrix2graph(w.in, alpha.in)
# graphGUI(graph.in)







































