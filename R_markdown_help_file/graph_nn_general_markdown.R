
### generalization with constraints and flexible objective functions. 
### R keras and tensorflow

## For mac
# setwd("~/Dropbox/Research/AbbVie/graph nnw/results/g6/")
source("~/Dropbox/Research/AbbVie/graph nnw/R code/general/mac/graph_nn_general_functions_keras.R")

## For windows
# setwd("~/code/graph nnw/results/gc3/")
# source("~/code/graph nnw/R code/general/mac/gmcp/graph_nn_general_functions_keras.R")

## For cluster
# setwd("~/neural_graph/results/g6/")
# source("~/neural_graph/graph_nn_general_functions_keras.R")
 
for (main.ind in c(1:2)){
 n.cluster = 3
 
 library(gMCP)
 library(doParallel)  
 library(MASS)
 library(nloptr)
 library(stringr)
 library(ANN2)
 library(CVTuningCov)
 
 ## Keras library
 # install.packages("devtools", dependencies = TRUE)
 # install.packages("keras", type = "source")
 library(keras)
 # install.packages("reticulate", dependencies = TRUE)
 library(reticulate)
 # conda_version()
 # install.packages("tensorflow", dependencies = TRUE)
 # devtools::install_github("rstudio/tensorflow")
 library(tensorflow)
 # install_tensorflow()
 #loading keras library
 library(keras)
 library(tibble)
 
 
 ### obtain scen.ind from script name
 
 # args = commandArgs(trailingOnly=TRUE)
 # main.ind <- args[1]
 # print(main.ind)
 
 if (main.ind==1){ # table 1
   
   # setwd("~/code/graph nnw/results/g3/")
   setwd("~/Dropbox/Research/AbbVie/graph nnw/results/g3/")
   # setwd("~/neural_graph/results/g3/")
   
   scen.vec = 1:4
   corr.vec = c(0, 0.3, 0.5)
   n.sim = 10^6 # number of simulation per graph
   
 } else if (main.ind==2){ # table 3 for sensitivity analysis
   
   # setwd("~/code/graph nnw/results/g6/")
   setwd("~/Dropbox/Research/AbbVie/graph nnw/results/g6/")
   # setwd("~/neural_graph/results/g6/")
   
   scen.vec = 1:10
   n.sim = 10^6 # number of simulation per graph
   
   n.hypo = 6
   alpha.const = rep(1, n.hypo)
   w.const = matrix(1, nrow = n.hypo, ncol = n.hypo)
   diag(w.const) = 0
   
 }  else if (main.ind==3){
   
   # setwd("~/code/graph nnw/results/gc3/")
   setwd("~/Dropbox/Research/AbbVie/graph nnw/results/gc3/")
   # setwd("~/neural_graph/results/g3/")
   
   scen.vec = 1
   corr.vec = c(0.5)
   n.sim = 10^6 # number of simulation per graph
   
   ## add the constraints
   obj.weight = c(0, 0.6, 0.2, 0.1, 0.1)
   n.hypo = length(obj.weight)
   alpha.const = c(1, rep(0, 4))
   
   w.const = matrix(1, nrow = n.hypo, ncol = n.hypo)
   diag(w.const) = 0
   w.const[,1] = 0
 }
 
 
 for (scen.ind in scen.vec){

  print(scen.ind)
  n.graph = (10^3)
  max.epoch = 10^4
  fit.tol = 0
  
  if (main.ind==1){
    
    if (scen.ind==1) {
      pow.vec = c(0.7, 0.8, 0.9)
      obj.weight = c(0.1, 0.3, 0.6)
    } else if (scen.ind==2) {
      pow.vec = c(0.6, 0.75, 0.9)
      obj.weight = c(0.1, 0.3, 0.6)
    } else if (scen.ind==3) {
      pow.vec = c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
      obj.weight = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
    } else if (scen.ind==4) {
      pow.vec = c(0.7, 0.7, 0.8, 0.8, 0.9, 0.9)
      obj.weight = c(0.1, 0.1, 0.15, 0.15, 0.3, 0.3)
    } 

    corr.mat.ind = 1
    n.hypo = length(pow.vec)    
    alpha.const = rep(1, n.hypo)
    w.const = matrix(1, nrow = n.hypo, ncol = n.hypo)
    diag(w.const) = 0
    
  } else if (main.ind==2){
    
    if (scen.ind==1) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 1
      pow.vec = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    } else if (scen.ind==2) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 1
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==3) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 1
      pow.vec = c(0.8, 0.8, 0.8, 0.9, 0.9, 0.9)
    } else if (scen.ind==4) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 5), 0.5)
      corr.mat.ind = 1
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==5) {
      corr.vec = 0.5
      obj.weight = c(0.05, 0.05, 0.1, 0.1, 0.35, 0.35)
      corr.mat.ind = 1
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==6) {
      corr.vec = 0.5
      obj.weight = rep(1, 6)
      corr.mat.ind = 1
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==7) {
      corr.vec = 0.3
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 2
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==8) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 2
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==9) {
      corr.vec = 0.3
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 3
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } else if (scen.ind==10) {
      corr.vec = 0.5
      obj.weight = c(rep(0.1, 4), 0.3, 0.3)
      corr.mat.ind = 3
      pow.vec = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8)
    } 
    
    
  } else if (main.ind==3){
    if (scen.ind==1) {
      corr.mat.ind = 1
      pow.vec = c(0.95, 0.9, 0.85, 0.65, 0.6)
    } 
  }
  
  

   
  print(pow.vec)
  trt.vec = 1.96-qnorm(pow.vec, lower.tail = FALSE)
  
for (corr.ind in 1:length(corr.vec)){
  
  corr = corr.vec[corr.ind]

  #############################################################
  set.seed(3)
  type.1.error = 0.025
  
  ###################################################
  
  alpha.fit = draw.alpha.fun(n.hypo, n.graph, alpha.const)
  w.fit = draw.w.fun(n.hypo, n.graph, w.const)
  
  #########################################################################################
  obtain.name.fit = obtain.name.func(alpha.const, w.const)
  name.free.space = obtain.name.fit$name.free.space
  name.free.plus = obtain.name.fit$name.free.plus
  name.free.comma = obtain.name.fit$name.free.comma
  
  if (corr.mat.ind==1){
    sigma.mat = matrix(corr, nrow = n.hypo, ncol = n.hypo)
    diag(sigma.mat) = 1
  } else if (corr.mat.ind==2){
    sigma.mat = AR1(n.hypo, rho=corr)
  } else if (corr.mat.ind==3){
    sigma.mat = Toeplitz(c(1, corr, rep(0, n.hypo-2)))
  } 
  
  if (!file.exists(paste0("data pow ", paste(pow.vec, collapse = " "), 
                          " obj ", paste(obj.weight, collapse = " "),
                          " corr.mat ", corr.mat.ind, 
                          " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                          n.graph, ".RData"))){
  
  sim.data.fit = sim.data.function(n.hypo.in = n.hypo,
                                   n.sim.in = n.sim,
                                   trt.vec.in = trt.vec,
                                   alpha.fit.in = alpha.fit,
                                   w.fit.in = w.fit,
                                   sigma.in = sigma.mat,
                                   corr.in = corr)
  
  save(sim.data.fit, file = paste0("data pow ", paste(pow.vec, collapse = " "), 
                                   " obj ", paste(obj.weight, collapse = " "),
                                   " corr.mat ", corr.mat.ind, 
                                      " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                                      n.graph, ".RData"))
  }
  
  load(file = paste0("data pow ", paste(pow.vec, collapse = " "), 
                     " obj ", paste(obj.weight, collapse = " "),
                     " corr.mat ", corr.mat.ind, 
                     " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                     n.graph, ".RData"), envir = )
  
  print("Done: data generalization")
  
  #################################################################################################
  # step 1: cross validation to obtain hidden and layer. 
  
  if (!file.exists( paste0("cross pow ", paste(pow.vec, collapse = " "),
                           " obj ", paste(obj.weight, collapse = " "),
                           " corr.mat ", corr.mat.ind, 
                           " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                           ".csv"))){

  cross.time = Sys.time()  
  n.nn = 6
  
  n.k = 5
  neu.fit.cross.par = matrix(NA, nrow = n.nn*n.k, ncol = 12 + length(name.free.space))
  neu.fit.cross = matrix(NA, nrow = n.nn, ncol = 12 + length(name.free.space))
  colnames(neu.fit.cross.par) = colnames(neu.fit.cross) =
    c("TD_MSE", "VD_MSE", "opt_fit_power", "opt_real_power", "opt_rank",
      name.free.space,
      "max_power", "hidden", "layer", "drop_rate", "time", "iters", "status")

  # cl <- makeCluster(n.cluster)
  # registerDoParallel(cl)
  # pred = foreach(n.fit.itt = 1:(n.nn*n.k)) %dopar% {
  for (n.fit.itt in 1:(n.nn*n.k)){

      n.nn.itt = (n.fit.itt-1)%/%n.k+1
      n.k.itt = n.fit.itt%%n.k
      if (n.k.itt==0) n.k.itt = 5

      if (n.nn.itt==1){n.node = 30; n.layer = 2; drop.rate = 0.3}
      if (n.nn.itt==2){n.node = 30; n.layer = 3; drop.rate = 0.3}
      if (n.nn.itt==3){n.node = 30; n.layer = 4; drop.rate = 0.3}
      
      if (n.nn.itt==4){n.node = 30; n.layer = 2; drop.rate = 0}
      if (n.nn.itt==5){n.node = 30; n.layer = 3; drop.rate = 0}
      if (n.nn.itt==6){n.node = 30; n.layer = 4; drop.rate = 0}
      
    

    # source("~/code/graph nnw/R code/general/mac/gmcp/graph_nn_general_functions_keras.R")
    # source("~/Dropbox/Research/AbbVie/graph nnw/R code/general/mac/graph_nn_general_functions_keras.R")
    # source("~/neural_graph/graph_nn_general_functions_keras.R")

    library(MASS)
    library(nloptr)
    library(stringr)
    library(ANN2)
    library(tensorflow)
    library(keras)
    library(reticulate)
    library(tibble)

    # print(paste("n.fit.itt", n.fit.itt))
    neu.func.fit = neu.function(n.node.in = n.node,
                                n.layer.in = n.layer,
                                k.indicator = FALSE,
                                k.itt.in = n.k.itt,
                                data.net.in = sim.data.fit$data.matrix,
                                fit.tol.in = fit.tol,
                                pval.sim.mat.in = sim.data.fit$pval.matrix,
                                parallel = FALSE,
                                obtain.name.fit = obtain.name.fit,
                                drop.rate.in = drop.rate,
                                max.epoch.in = 10^3,
                                df.fit.tol.in = 10^(-3),
                                df.max.n.in = 1,
                                df.max.t.in = -1)
    neu.fit.cross.par[n.fit.itt, ] = as.numeric(neu.func.fit$n.nodes.output)
  }

 

  for (n.nn.itt in 1:n.nn){
    print(paste("n.nn.itt", n.nn.itt))

    neu.fit.temp = neu.fit.cross.par[(1:n.k)+(n.nn.itt-1)*n.k, ]
    neu.fit.cross[n.nn.itt, ] = apply(neu.fit.temp, 2, function(x){mean(x, na.rm = TRUE)})
  }
  
  neu.fit.cross = data.frame(neu.fit.cross)
  neu.fit.cross$time = difftime(Sys.time(), cross.time, units="secs")

  write.csv(neu.fit.cross,
            paste0("cross pow ", paste(pow.vec, collapse = " "),
                   " obj ", paste(obj.weight, collapse = " "),
                   " corr.mat ", corr.mat.ind, 
                   " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                   ".csv"),  row.names=FALSE)

  }

  neu.fit.cross = read.csv(file =   paste0("cross pow ", paste(pow.vec, collapse = " "),
                                           " obj ", paste(obj.weight, collapse = " "),
                                           " corr.mat ", corr.mat.ind, 
                                           " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                                           ".csv"))

  neu.fit.cross = data.frame(neu.fit.cross)
  
  # opt.nn.ind = which.min(neu.fit.cross$VD_MSE)
  
  opt.nn.ind = which.max(neu.fit.cross$opt_real_power)
  opt.n.node = neu.fit.cross$hidden[opt.nn.ind]
  opt.n.layer = neu.fit.cross$layer[opt.nn.ind]
  opt.drop.rate = neu.fit.cross$drop_rate[opt.nn.ind]

  print("Done: cross validation")
  
  # if (main.ind==2){
  #   opt.n.node  = 40
  #   opt.n.layer = 3
  #   opt.drop.rate = 0
  # } else if (main.ind==4){
  #   opt.n.node  = 140
  #   opt.n.layer = 5
  #   opt.drop.rate = 0
  # }
  
  ######################################################################################
  ## nn fitting based on optimal one
  
  n.fit = 1
  
  if (!file.exists( paste0("fit pow ", paste(pow.vec, collapse = " "),
                           " obj ", paste(obj.weight, collapse = " "),
                           " corr.mat ", corr.mat.ind, 
                           " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                           ".csv"))){
  
  neu.fit.mat = array(NA, dim = c(n.fit, dim(sim.data.fit$data.matrix)[1], 
                                  dim(sim.data.fit$data.matrix)[2]+4))
  neu.fit.opt = matrix(NA, nrow = n.fit, ncol = 12 + length(name.free.space))
  colnames(neu.fit.opt) =
    c("TD_MSE", "VD_MSE", "opt_fit_power", "opt_real_power", "opt_rank",
      name.free.space,
      "max_power", "hidden", "layer", "drop_rate", "time", "iters", "status")

  for (n.fit.itt in 1:n.fit){

    print(paste("n.fit.itt", n.fit.itt))
    neu.func.fit = neu.function(n.node.in = opt.n.node,
                                n.layer.in = opt.n.layer,
                                k.indicator = FALSE,
                                k.itt.in = 1,
                                data.net.in = sim.data.fit$data.matrix,
                                fit.tol.in = fit.tol,
                                pval.sim.mat.in = sim.data.fit$pval.matrix,
                                parallel = FALSE,
                                obtain.name.fit = obtain.name.fit,
                                drop.rate.in = opt.drop.rate,
                                max.epoch.in = max.epoch,
                                df.fit.tol.in = 10^(-4),
                                df.max.n.in = 10^4,
                                df.max.t.in = -1)
    neu.fit.opt[n.fit.itt, ] = as.numeric(neu.func.fit$n.nodes.output)

    data.tv.com = rbind(neu.func.fit$data.train, neu.func.fit$data.val)
    neu.fit.mat[n.fit.itt, , ] = as.matrix(data.tv.com)

  }

  neu.fit.opt = data.frame(neu.fit.opt)
  neu.fit.opt.sum = neu.fit.opt[which.max(neu.fit.opt$opt_real_power), ]
  neu.fit.mat.sum = neu.fit.mat[which.max(neu.fit.opt$opt_real_power), , ]
  colnames(neu.fit.mat.sum) = colnames(data.tv.com)
  
  neu.fit.opt.sum$total_time = neu.fit.opt$time + 
    as.numeric(sim.data.fit$sim.data.time.diff)+
    neu.fit.cross$time[1]
  
  write.csv(neu.fit.opt.sum,
            paste0("fit pow ", paste(pow.vec, collapse = " "),
                   " obj ", paste(obj.weight, collapse = " "),
                   " corr.mat ", corr.mat.ind, 
                   " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                   ".csv"),  row.names=FALSE)
  write.csv(neu.fit.mat.sum,
            paste0("fit mat ", paste(pow.vec, collapse = " "),
                   " obj ", paste(obj.weight, collapse = " "),
                   " corr.mat ", corr.mat.ind, 
                   " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                   ".csv"),  row.names=FALSE)
  }
  
  neu.fit.opt.sum = read.csv(paste0("fit pow ", paste(pow.vec, collapse = " "),
                                    " obj ", paste(obj.weight, collapse = " "),
                                    " corr.mat ", corr.mat.ind, 
                                     " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                                     ".csv"))
  
  neu.fit.opt.sum = data.frame(neu.fit.opt.sum)
  
  print("Done: FNN fit")
  
  # ########################################################################################
  ## optmization based on several naive methods with no gradient information.
  # GF.name.vec = c("NLOPT_GN_ORIG_DIRECT", "NLOPT_GN_ORIG_DIRECT_L", "NLOPT_GN_ISRES", "NLOPT_LN_COBYLA")
  GF.name.vec = c("NLOPT_GN_ISRES", "NLOPT_LN_COBYLA")
  # GF.name.vec = c("NLOPT_LN_COBYLA")
  GF.fit.n = 1

  naive.fit.mat =  matrix(NA, nrow = length(GF.name.vec),
                          ncol = 4 + length(name.free.space))

  for (n.fit.itt in 1:length(GF.name.vec)){
    GF.fit = naive.opt.func(nloptr.func.name = GF.name.vec[n.fit.itt],
                            naive.opt.n = 1,
                            naive.tol = 10^(-4),
                            naive.max.n = -1,
                            naive.max.t = neu.fit.opt.sum$total_time*1.5,
                            pval.sim.mat.in = sim.data.fit$pval.matrix,
                            x0.given = NULL)
    naive.fit.mat[n.fit.itt, ] = c(GF.fit$naive.fit, GF.fit$solution,
                                   GF.fit$status, GF.fit$iters, GF.fit$time)
  }
  colnames(naive.fit.mat) = c("fit.power", name.free.space, "status", "iters", "time")
  naive.fit.mat = data.frame(naive.fit.mat)
  naive.fit.mat$name = GF.name.vec
  # stopCluster(cl)



  write.csv(naive.fit.mat,
            paste0("GF ", paste(pow.vec, collapse = " "),
                   " obj ", paste(obj.weight, collapse = " "),
                   " corr.mat ", corr.mat.ind,
                   " corr ", corr, " GF_n ", GF.fit.n, " hypo ", n.hypo,
                   ".csv"),  row.names=FALSE)

  

}
}
}




































