

library(ggplot2)
library(plyr)
library(ggrepel)
library(xtable)
setwd("~/Dropbox/Research/AbbVie/graph nnw/R code/code_sharing_Github/results/main_results/")

###########################################################################################
n.scen = 9
sim.table = NULL
for (scen.ind in 1:n.scen){

  obj.weight = c(0.3, 0.3, 0.1, 0.1, 0.1, 0.1)
  n.hypo = length(obj.weight)
  fit.tol = 0
  n.graph = 1000
  
  if (scen.ind<=3){
    pow.vec = c(0.8, 0.8, 0.6, 0.6, 0.4, 0.4)
    corr.mat.ind = 1
    if (scen.ind==1) corr.vec = 0
    if (scen.ind==2) corr.vec = 0.3
    if (scen.ind==3) corr.vec = 0.5
  } else if (scen.ind<=6){
    pow.vec = c(0.9, 0.9, 0.8, 0.8, 0.6, 0.6)
    corr.vec = 0.3
    if (scen.ind==4) corr.mat.ind = 1
    if (scen.ind==5) corr.mat.ind = 2
    if (scen.ind==6) corr.mat.ind = 3
  } else if (scen.ind<=9){
    
    corr.vec = 0.3
    corr.mat.ind = 1
    
    if (scen.ind==7) pow.vec = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
    if (scen.ind==8) pow.vec = c(0.9, 0.9, 0.7, 0.7, 0.6, 0.6)
    if (scen.ind==9) pow.vec = c(0.95, 0.95, 0.8, 0.8, 0.6, 0.6)
    
  }

    naive.csv = read.csv(file = paste0("GF ", paste(pow.vec, collapse = " "),
                                       " obj ", paste(obj.weight, collapse = " "),
                                       " corr.mat ", corr.mat.ind, " corr ", corr.vec,
                                       " GF_n ", 5, " hypo ",
                                       n.hypo,  ".csv"))
    
    opt.csv = read.csv(file = paste0("fit pow ", paste(pow.vec, collapse = " "),
                                     " obj ", paste(obj.weight, collapse = " "),
                                     " corr.mat ", corr.mat.ind, " corr ", corr.vec, " tol ", fit.tol, 
                                     " hypo ",
                                     n.hypo,  ".csv"))
    opt.cross.csv = read.csv(file = paste0("cross pow ", paste(pow.vec, collapse = " "),
                                           " obj ", paste(obj.weight, collapse = " "),
                                           " corr.mat ", corr.mat.ind, " corr ", corr.vec, 
                                           " tol ", fit.tol,
                                           " hypo ",
                                      n.hypo,  ".csv"))
    load(file = paste0("data pow ", paste(pow.vec, collapse = " "),
                       " obj ", paste(obj.weight, collapse = " "),
                       " corr.mat ", corr.mat.ind,  " corr ", corr.vec, " n.sim ", 6, 
                       " n.graph ",
                                           n.graph,  ".RData"))

    print(opt.csv$opt_value_sd[1]) ## SD of FNN
    # print(c(naive.csv$opt_val_sd[1])) ## SD of COBYLA
    # print(c(naive.csv$opt_val_sd[6])) ## SD of ISRES

    opt.total.time = as.numeric(opt.csv$total_time[1])/60
    # opt.total.time = as.numeric(opt.csv$time+sim.data.fit$sim.data.time.diff)/60
    naive.total.time = c(as.numeric(mean(naive.csv$time[naive.csv$name=="NLOPT_LN_COBYLA"]))/60,
                         as.numeric(mean(naive.csv$time[naive.csv$name=="NLOPT_GN_ISRES"]))/60)
    naive.mean.power = c(as.numeric(mean(naive.csv$fit.power[naive.csv$name=="NLOPT_LN_COBYLA"])),
                         as.numeric(mean(naive.csv$fit.power[naive.csv$name=="NLOPT_GN_ISRES"])))

    sim.table.temp = c(paste0("L", scen.ind),
                       paste0(sprintf("%.1f", opt.csv$opt_value_mean[1]*100), "%"),
                       paste0(sprintf("%.1f", naive.mean.power*100), "%"),
                       paste0(sprintf("%.1f", opt.csv$max_power[1]*100), "%"),
                       sprintf("%.1f", opt.total.time),
                       sprintf("%.1f", naive.total.time),
                       sprintf("%.1f", as.numeric(sim.data.fit$sim.data.time.diff/60))
                       )
    sim.table = rbind(sim.table, sim.table.temp)
    

}
colnames(sim.table) = c("scen", "FNN", "COBYLA", "ISRES", "naive","FNN_t",
                        "COBYLA_t", "ISRES_t", "naive_t")
sim.table = data.frame(sim.table, stringsAsFactors = FALSE)

library(xtable)
sim.table$ISRES_t = "-"
print(xtable(sim.table),include.rownames = FALSE)
############################################################################################
## aggregate data
n.scen = 9
for (scen.ind in 1:n.scen){
  
  obj.weight = c(0.3, 0.3, 0.1, 0.1, 0.1, 0.1)
  n.hypo = length(obj.weight)
  fit.tol = 0
  
  if (scen.ind<=3){
    pow.vec = c(0.8, 0.8, 0.6, 0.6, 0.4, 0.4)
    corr.mat.ind = 1
    if (scen.ind==1) corr.vec = 0
    if (scen.ind==2) corr.vec = 0.3
    if (scen.ind==3) corr.vec = 0.5
  } else if (scen.ind<=6){
    pow.vec = c(0.9, 0.9, 0.8, 0.8, 0.6, 0.6)
    corr.vec = 0.3
    if (scen.ind==4) corr.mat.ind = 1
    if (scen.ind==5) corr.mat.ind = 2
    if (scen.ind==6) corr.mat.ind = 3
  } else if (scen.ind<=9){
    
    corr.vec = 0.3
    corr.mat.ind = 1
    
    if (scen.ind==7) pow.vec = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
    if (scen.ind==8) pow.vec = c(0.9, 0.9, 0.7, 0.7, 0.6, 0.6)
    if (scen.ind==9) pow.vec = c(0.95, 0.95, 0.8, 0.8, 0.6, 0.6)
    
  }
  
  data.in.name = paste0("fit mat ", paste(pow.vec, collapse = " "),
                        " obj ", paste(obj.weight, collapse = " "),
                        " corr.mat ", corr.mat.ind, 
                        " corr ", corr.vec, " tol ", fit.tol, " hypo ", n.hypo,
                        ".csv")
  FNN.fit.in.name = paste0("fit pow ", paste(pow.vec, collapse = " "),
                        " obj ", paste(obj.weight, collapse = " "),
                        " corr.mat ", corr.mat.ind, 
                        " corr ", corr.vec, " tol ", fit.tol, " hypo ", n.hypo,
                        ".csv")
  GF.fit.in.name = paste0("GF ", paste(pow.vec, collapse = " "),
                           " obj ", paste(obj.weight, collapse = " "),
                           " corr.mat ", corr.mat.ind, 
                           " corr ", corr.vec, " GF_n ", 5, " hypo ", n.hypo,
                           ".csv")
  data.in = read.csv(data.in.name)
  FNN.fit.in = read.csv(FNN.fit.in.name)
  GF.fit.in = read.csv(GF.fit.in.name)
  
  n.graph = dim(data.in)[1]
  
  data.train = data.in[1:(n.graph*4/5), ]
  data.train.order = data.train[order(data.train$target.power), ]
  
  data.val = data.in[((4*n.graph/5+1):n.graph), ]
  data.val.order = data.val[order(data.val$target.power), ]
  
  data.com.order = rbind(data.train.order, data.val.order)
  
  ##################################################################################
  n.select = n.graph
  select.ind = (1:n.select)*(n.graph/n.select)
  brute.ind = which.max(data.com.order$target.power)
  label.original.vec = 
    c(rep("Training dataset", n.select*0.8), rep("Validation dataset", n.select*0.2),
                "FNN-based optimizer", "ISRES", "COBYLA")
  label.vec = label.original.vec
  label.vec[brute.ind] = "SSM"
  
  size.vec = c(rep(10, n.select), rep(20, 3))
  size.vec[brute.ind] = 14
  
  data.plot.fit.temp = data.frame(power = c(data.com.order$target.power[select.ind], 
                                            FNN.fit.in$opt_value_mean[1], 
                                            GF.fit.in$opt_val_mean[6],
                                            GF.fit.in$opt_val_mean[1]),
                                  error = c(data.com.order$fit.target.power[select.ind]-
                                              data.com.order$target.power[select.ind],
                                            rep(0, 3)
                                            ),
                                  pos = c(rep(0, n.select), -0.5, 0, 1.5),
                                  label = factor(label.vec, levels = c("SSM", 
                "COBYLA", "FNN-based optimizer", "ISRES", 
                "Training dataset", "Validation dataset")),
                                  label_error = label.original.vec, 
                                  size = size.vec,
                                  ind = c(1:n.select, n.select*1.15, n.select*1.05, n.select*1.1),
                                  ind_error = c(sample(x=1:(n.select*4/5), (n.select*4/5)),
                                                sample(x=(n.select*4/5+1):n.select, (n.select/5)),
                                                rep(0, 3)
                                  )
                                  
                                  )
  
  if (scen.ind==1){
    data.plot.fit = data.plot.fit.temp
  } else {
    data.plot.fit = rbind(data.plot.fit, data.plot.fit.temp)
  }
  
}

data.plot.fit$plot_ind = rep(paste0("L", 1:9), each = (n.select+3))
data.plot.fit$plot_num_ind = rep(1:9, each = (n.select+3))

#######################################################################################
## generate plot
ggplot.fit = ggplot(data.plot.fit, aes(x=ind, y=power, label = label)) +
  geom_point(data = data.plot.fit, 
             aes(color = label, size = label, shape=label)) + 
  # geom_text(aes(label=text, vjust = pos),hjust=-0.1, size = 15) +
  scale_color_manual(values=c("#999999", "#999999", "#D55E00", "#999999", "#009E73", "#0072B2"))+
  scale_shape_manual(values=c(15,18,16,17,16,16))+
  scale_alpha_manual(values=c(1, 1, 0.3)) + 
  scale_size_manual(values=c(10, 10, 10, 10, 6, 6))+
  # scale_y_continuous(
  #   # breaks = c(0.35, 0.5, 0.6, 0.7, 0.8),
  # 
  #   )+
  scale_x_continuous(breaks = c(1, n.select/2, n.select),
                     limits =c(1, n.graph*1.15))+
  ylab ("Objective Function") + xlab("Index") +
  theme_bw() + 
  theme(text = element_text(size=45),plot.margin = unit(c(2,3,2,1),units="lines"),
        axis.text.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=45,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=45,angle=90,hjust=.5,vjust=.5,face="plain",
                                    margin = margin(t = 0, r = 30, b = 0, l = 0)),
        legend.text = element_text(colour="black", size = 42, face = "plain"),
        legend.title = element_text(colour="black", size = 45, face = "plain"),
        legend.key.size = unit(2,"line"),
        legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend(nrow=3,byrow=TRUE, keywidth=0.8,
                          keyheight=0.5,
                          default.unit="inch"))+
  facet_wrap( ~plot_ind, scales = "free", ncol = 3)

png("sim_fit_plot.png",  width = 2000, height = 1600)
print(ggplot.fit)
dev.off()

#######################################################################
## error plot
data.plot.error = data.plot.fit[data.plot.fit$ind<=n.select,]
ggplot.error = ggplot(data.plot.error, aes(x=ind_error, y=error, label = label_error)) +
  geom_point(data = data.plot.error, size=6, shape=16,
             aes(col = label_error)) +
  scale_alpha_manual(values=c(1, 1)) +
  scale_color_manual(values=c("#009E73", "#0072B2"))+
  scale_x_continuous(breaks = c(1, n.select/2, n.select))+
  ylab ("FNN Residuals") + xlab("Index") +
  theme_bw() +
  theme(text = element_text(size=45),plot.margin = unit(c(2,3,2,1),units="lines"),
        axis.text.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=45,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="black",size=45,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=45,angle=90,hjust=.5,vjust=.5,face="plain",
                                    margin = margin(t = 0, r = 30, b = 0, l = 0)),
        legend.text = element_text(colour="black", size = 42, face = "plain"),
        legend.title = element_text(colour="black", size = 45, face = "plain"),
        legend.key.size = unit(2,"line"),
        legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend(nrow=1,byrow=TRUE, keywidth=0.8,
                          keyheight=0.5,
                          default.unit="inch"))+
  facet_wrap( ~plot_ind, scales = "free", ncol = 3)

png("sim_error_plot.png",  width = 2000, height = 1200)
print(ggplot.error)
dev.off()













