
## set working directory and source R function files
setwd("~")
source("graph_nn_general_functions_keras.R")

## load R packages
library(gMCP)
library(doParallel)  
library(MASS)
library(nloptr)
library(stringr)
library(ANN2)
library(CVTuningCov)
library(keras)
library(reticulate)
library(tensorflow)
library(keras)
library(tibble)
library(pracma)

n.cluster = 4 # number of clusters in parallel computing
path_name = "results/main_results/" # path name to write results
scen.vec = 1:9 # 9 scenarios in the Table 1
n.sim = 10^6 # number of simulation iterations "n" per graph
obj.weight = c(0.3, 0.3, 0.1, 0.1, 0.1, 0.1)  # vector v
 
for (scen.ind in scen.vec){

print(scen.ind)
n.graph = (10^3) # number of simulated graphs B
max.epoch = 10^3 # number of DNN fitting epochs 
fit.tol = 0 ## testing parameter

## specify 9 scenarios in Table 1
if (scen.ind<=3){
  pow.vec = c(0.8, 0.8, 0.6, 0.6, 0.4, 0.4) # marginal power vector
  corr.mat.ind = 1
  if (scen.ind==1) corr.vec = 0 # value of correlation
  if (scen.ind==2) corr.vec = 0.3
  if (scen.ind==3) corr.vec = 0.5
} else if (scen.ind<=6){
  pow.vec = c(0.9, 0.9, 0.8, 0.8, 0.6, 0.6)
  corr.vec = 0.3
  if (scen.ind==4) corr.mat.ind = 1 # compound symmetry
  if (scen.ind==5) corr.mat.ind = 2 # AR(1)
  if (scen.ind==6) corr.mat.ind = 3 # Banded Toeplitz
} else if (scen.ind<=9){
  corr.vec = 0.3
  corr.mat.ind = 1
  if (scen.ind==7) pow.vec = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  if (scen.ind==8) pow.vec = c(0.9, 0.9, 0.7, 0.7, 0.6, 0.6)
  if (scen.ind==9) pow.vec = c(0.95, 0.95, 0.8, 0.8, 0.6, 0.6)
}

n.hypo = length(pow.vec)  # number of hypothesis 
## constraints on alpha vector and transition matrix. 
## Non-zero values correspond to free parameters
alpha.const = rep(1, n.hypo) 
w.const = matrix(1, nrow = n.hypo, ncol = n.hypo)
diag(w.const) = 0

print(pow.vec)
type.1.error = 0.025 ## one sided type I error rate
## calculate treatment effect based on marginal power
trt.vec = qnorm(1-type.1.error)-qnorm(pow.vec, lower.tail = FALSE)

for (corr.ind in 1:length(corr.vec)){

corr = corr.vec[corr.ind] ## extract correlation value
## randomly simulate alpha vector
alpha.fit = draw.alpha.fun(n.hypo, n.graph, alpha.const)
## randomly simulate transition matrix 
w.fit = draw.w.fun(n.hypo, n.graph, w.const)

## obtain colname names
obtain.name.fit = obtain.name.func(alpha.const, w.const)
name.free.space = obtain.name.fit$name.free.space
name.free.plus = obtain.name.fit$name.free.plus
name.free.comma = obtain.name.fit$name.free.comma

## specify correlation strucutres
if (corr.mat.ind==1){
  sigma.mat = matrix(corr, nrow = n.hypo, ncol = n.hypo)
  diag(sigma.mat) = 1
} else if (corr.mat.ind==2){
  sigma.mat = AR1(n.hypo, rho=corr)
} else if (corr.mat.ind==3){
  sigma.mat = Toeplitz(c(1, corr, rep(0, n.hypo-2)))
} 

if (!file.exists(paste0(path_name, "data pow ", paste(pow.vec, collapse = " "), 
                        " obj ", paste(obj.weight, collapse = " "),
                        " corr.mat ", corr.mat.ind, 
                        " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                        n.graph, ".RData"))){

## set random seed
set.seed(123)
## training datasets
sim.data.fit = sim.data.function(n.hypo.in = n.hypo,
                                 n.sim.in = n.sim,
                                 trt.vec.in = trt.vec,
                                 alpha.fit.in = alpha.fit,
                                 w.fit.in = w.fit,
                                 sigma.in = sigma.mat,
                                 corr.in = corr,
                                 type.1.error.in = type.1.error,
                                 obj.weight.in = obj.weight)

## save training datasets to files
save(sim.data.fit, file = paste0(path_name,
                                 "data pow ", paste(pow.vec, collapse = " "), 
                                 " obj ", paste(obj.weight, collapse = " "),
                                 " corr.mat ", corr.mat.ind, 
                                    " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                                    n.graph, ".RData"))
}

## load files if exist
load(file = paste0(path_name,"data pow ", paste(pow.vec, collapse = " "), 
                   " obj ", paste(obj.weight, collapse = " "),
                   " corr.mat ", corr.mat.ind, 
                   " corr ", corr, " n.sim ", log(n.sim)/log(10), " n.graph ", 
                   n.graph, ".RData"), envir = )

print("Done: data generalization")

#################################################################################################
## cross validation to select DNN structure

if (!file.exists( paste0(path_name,
                         "cross pow ", paste(pow.vec, collapse = " "),
                         " obj ", paste(obj.weight, collapse = " "),
                         " corr.mat ", corr.mat.ind,
                         " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                         ".csv"))){

cross.time = Sys.time()
n.nn = 6 # number of structure candidates
n.k = 5 # number of K in cross-validation
## specify matrix to save results
neu.fit.cross.par = matrix(NA, nrow = n.nn*n.k, ncol = 12 + length(name.free.space))
neu.fit.cross = matrix(NA, nrow = n.nn, ncol = 12 + length(name.free.space))
colnames(neu.fit.cross.par) = colnames(neu.fit.cross) =
  c("TD_MSE", "VD_MSE", "opt_fit_power", "opt_real_power", "opt_rank",
    name.free.space,
    "max_power", "hidden", "layer", "drop_rate", "time", "iters", "status")

for (n.fit.itt in 1:(n.nn*n.k)){
  
    n.nn.itt = (n.fit.itt-1)%/%n.k+1
    n.k.itt = n.fit.itt%%n.k
    if (n.k.itt==0) n.k.itt = 5

    ## six candidate structures
    if (n.nn.itt==1){n.node = 30; n.layer = 2; drop.rate = 0.3}
    if (n.nn.itt==2){n.node = 30; n.layer = 3; drop.rate = 0.3}
    if (n.nn.itt==3){n.node = 30; n.layer = 4; drop.rate = 0.3}

    if (n.nn.itt==4){n.node = 30; n.layer = 2; drop.rate = 0}
    if (n.nn.itt==5){n.node = 30; n.layer = 3; drop.rate = 0}
    if (n.nn.itt==6){n.node = 30; n.layer = 4; drop.rate = 0}

  ## fit DNN with 80% as training and 20% as validation
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

## summary results on K cross-validations
for (n.nn.itt in 1:n.nn){
  print(paste("n.nn.itt", n.nn.itt))

  neu.fit.temp = neu.fit.cross.par[(1:n.k)+(n.nn.itt-1)*n.k, ]
  if (n.k==1){
    neu.fit.cross[n.nn.itt, ] = neu.fit.temp
  } else {
    neu.fit.cross[n.nn.itt, ] = apply(neu.fit.temp, 2, function(x){mean(x, na.rm = TRUE)})
  }


}

## add computing time
neu.fit.cross = data.frame(neu.fit.cross)
neu.fit.cross$time = difftime(Sys.time(), cross.time, units="secs")

## write results to files
write.csv(neu.fit.cross,
          paste0(path_name,
                 "cross pow ", paste(pow.vec, collapse = " "),
                 " obj ", paste(obj.weight, collapse = " "),
                 " corr.mat ", corr.mat.ind,
                 " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                 ".csv"),  row.names=FALSE)

}

neu.fit.cross = read.csv(file =   paste0(path_name,
                                         "cross pow ", paste(pow.vec, collapse = " "),
                                         " obj ", paste(obj.weight, collapse = " "),
                                         " corr.mat ", corr.mat.ind,
                                         " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                                         ".csv"))

neu.fit.cross = data.frame(neu.fit.cross)

## select the final DNN structure with the smallest training error
opt.nn.ind = which.min(neu.fit.cross$TD_MSE)
opt.n.node = neu.fit.cross$hidden[opt.nn.ind]
opt.n.layer = neu.fit.cross$layer[opt.nn.ind]
opt.drop.rate = neu.fit.cross$drop_rate[opt.nn.ind]

print("Done: cross validation")
######################################################################################
## DNN fitting with the selected structure
n.fit = 5 ## average of 5 validation fits in the simulations

if (!file.exists( paste0(path_name,
                         "fit pow ", paste(pow.vec, collapse = " "),
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
  ## fit DNN with k.itt.in = 1. 80% for training and 20% for validation.
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
neu.fit.opt.sum = neu.fit.opt[, ]
## select the best one to generate Figure 3 and Figure 4
neu.fit.mat.sum = neu.fit.mat[which.max(neu.fit.opt$opt_real_power), , ]
colnames(neu.fit.mat.sum) = colnames(data.tv.com)

## calculate computing time (averaging multiple optimizations)
neu.fit.opt.sum$total_time = mean(neu.fit.opt$time) +
  as.numeric(sim.data.fit$sim.data.time.diff)+
  neu.fit.cross$time[1]

## summary from all optimizations with means reported in Table 2
neu.fit.opt.sum$opt_value_mean = mean(neu.fit.opt$opt_real_power)
neu.fit.opt.sum$opt_value_sd = sd(neu.fit.opt$opt_real_power)
neu.fit.opt.sum$opt_value_max = max(neu.fit.opt$opt_real_power)
neu.fit.opt.sum$opt_value_min = min(neu.fit.opt$opt_real_power)

write.csv(neu.fit.opt.sum,
          paste0(path_name,
                 "fit pow ", paste(pow.vec, collapse = " "),
                 " obj ", paste(obj.weight, collapse = " "),
                 " corr.mat ", corr.mat.ind, 
                 " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                 ".csv"),  row.names=FALSE)
write.csv(neu.fit.mat.sum,
          paste0(path_name,
                 "fit mat ", paste(pow.vec, collapse = " "),
                 " obj ", paste(obj.weight, collapse = " "),
                 " corr.mat ", corr.mat.ind, 
                 " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                 ".csv"),  row.names=FALSE)
}

neu.fit.opt.sum = read.csv(paste0(path_name,
                                  "fit pow ", paste(pow.vec, collapse = " "),
                                  " obj ", paste(obj.weight, collapse = " "),
                                  " corr.mat ", corr.mat.ind, 
                                   " corr ", corr, " tol ", fit.tol, " hypo ", n.hypo,
                                   ".csv"))

neu.fit.opt.sum = data.frame(neu.fit.opt.sum)

print("Done: FNN fit")

# ########################################################################################
## optmization based on several naive methods with no gradient information.
GF.fit.n = 5 ## average of 5 validation fits in the simulations

GF.name.vec = c(rep("NLOPT_LN_COBYLA", GF.fit.n), 
                rep("NLOPT_GN_ISRES", GF.fit.n))

cl = makeCluster(n.cluster)
registerDoParallel(cl)
GF.fit.table = foreach(GF.ind = 1:(length(GF.name.vec))) %dopar% {
  library(gMCP)
  library(MASS)
  library(nloptr)
  library(stringr)
  library(ANN2)
  library(CVTuningCov)
  library(tibble)
  library(pracma)
  source("graph_nn_general_functions_keras.R")
  
  ## fit COBYLA and ISRES
  GF.fit = naive.opt.func(nloptr.func.name = GF.name.vec[GF.ind],
                          naive.opt.n = 1,
                          naive.tol = 10^(-4),
                          naive.max.n = -1,
                          naive.max.t = neu.fit.opt.sum$total_time*1.5,
                          # naive.max.t = 10,
                          pval.sim.mat.in = sim.data.fit$pval.matrix,
                          x0.given = NULL,
                          set.seed.in = GF.ind
  )
  return(c(GF.fit$naive.fit, GF.fit$solution,
                                 GF.fit$status, GF.fit$iters, GF.fit$time))
}
stopCluster(cl)

naive.fit.mat = matrix(unlist(GF.fit.table),
                       nrow = GF.fit.n*2,
                       ncol = 4 + length(name.free.space), byrow = TRUE)

colnames(naive.fit.mat) = c("fit.power", name.free.space, "status", "iters", "time")
naive.fit.mat = data.frame(naive.fit.mat)
naive.fit.mat$name = GF.name.vec

## caclulate mean, SD, min, max across all optimizations
naive.fit.mat$opt_val_mean = sapply(1:length(GF.name.vec),
                            function(naive.fit.mat.ind){
                              mean(naive.fit.mat$fit.power[GF.name.vec==
                                                    GF.name.vec[naive.fit.mat.ind]])
                            })
  
naive.fit.mat$opt_val_sd = sapply(1:length(GF.name.vec),
                                  function(naive.fit.mat.ind){
                                    sd(naive.fit.mat$fit.power[GF.name.vec==
                                      GF.name.vec[naive.fit.mat.ind]])
                                  })
naive.fit.mat$opt_val_max = sapply(1:length(GF.name.vec),
                                   function(naive.fit.mat.ind){
                                     max(naive.fit.mat$fit.power[GF.name.vec==
                                          GF.name.vec[naive.fit.mat.ind]])
                                   })
naive.fit.mat$opt_val_min = sapply(1:length(GF.name.vec),
                                   function(naive.fit.mat.ind){
                                     max(naive.fit.mat$fit.power[GF.name.vec==
                                      GF.name.vec[naive.fit.mat.ind]])
                                   })


write.csv(naive.fit.mat,
          paste0(path_name,
                 "GF ", paste(pow.vec, collapse = " "),
                 " obj ", paste(obj.weight, collapse = " "),
                 " corr.mat ", corr.mat.ind,
                 " corr ", corr, " GF_n ", GF.fit.n, " hypo ", n.hypo,
                 ".csv"),  row.names=FALSE)



}
}





































