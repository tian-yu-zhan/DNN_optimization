

sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
  
  # return(as.numeric(pmax(0, x)))
}
  
sig_der = function(x){
  return(sigmoid(x)*(1-sigmoid(x)))
  
  # return(as.numeric(sign(x)>=0))
}

### function to sample alpha vector
draw.alpha.fun = function(n.hypo, n.graph, alpha.const.in){
  temp = matrix(runif(n.hypo*n.graph, 0, 1), nrow = n.graph, ncol = n.hypo)
  temp[, alpha.const.in==0] = 0
  temp = temp/apply(temp, 1, sum)
  return(temp)
}

# a = draw.alpha.fun(n.hypo, n.graph, alpha.const.in = alpha.const)

### function to sample weight matrix
draw.w.fun = function(n.hypo, n.graph, w.const.in){
  temp = array(runif(n.hypo*n.hypo*n.graph, 0, 1), dim = c(n.hypo, n.hypo, n.graph))
  for (i in 1:n.graph){
    temp.in = temp[,,i]
    temp.in[w.const.in==0] = 0
    temp[,,i] = temp.in
  }
  norm = apply(temp, 3, rowSums)
  norm = array(rep(norm, each = n.hypo), dim = c(n.hypo, n.hypo, n.graph))
  norm = aperm(norm, c(2,1,3))
  
  temp = temp/ norm
  return(temp)
}

# a = draw.w.fun(n.hypo, n.graph, w.const.in = w.const)

### function to calculate graphical power
graph.power = function(alpha.in, w.in, type.1.error.in, pval.sim.mat.in){
  graph.in = matrix2graph(w.in, alpha.in)
  
  # w.out <<- w.in
  # alpha.out<<-alpha.in
  # graph.out <<- graph.in
  # print(graph.in)
  # graphGUI(graph.in)
  
  out_seq = graphTest(pvalues = t(pval.sim.mat.in), graph = graph.in, alpha = type.1.error.in)
  out.power = apply(out_seq, 2, mean)
  return(out.power)
}


obtain.name.func = function(alpha.const.in, w.const.in){
  n.hypo = length(alpha.const.in)
  
  name.free.space = head(paste0("a", which(!alpha.const==0)), -1)
  
  for (i in 1:dim(w.const.in)[1]){
    w.const.temp = w.const.in[i, ]
    name.free.space = c(name.free.space, head(paste0("w", i, "_",which(!w.const.temp==0)), -1))
  }
  
  name.free.plus = paste(name.free.space, collapse = "+")
  name.free.comma = paste(name.free.space, collapse = ",")
  
  newlist = list("name.free.space" = name.free.space,
                 "name.free.comma" = name.free.comma,
                 "name.free.plus" = name.free.plus)
  return(newlist)
}

neu.function = function(n.node.in, n.layer.in, k.indicator, k.itt.in, data.net.in,
                        pval.sim.mat.in, parallel, obtain.name.fit,
                        drop.rate.in, max.epoch.in, df.fit.tol.in, df.max.n.in, 
                        df.max.t.in){
  
  neu.time = Sys.time()
  name.free.space = obtain.name.fit$name.free.space
  name.free.plus = obtain.name.fit$name.free.plus
  name.free.comma = obtain.name.fit$name.free.comma
  
  n.nodes.output = matrix(NA, nrow = 1, ncol = 9 + length(name.free.space))
  #rownames(n.nodes.output) = n.nodes.vec
  colnames(n.nodes.output) = 
    c("TD_MSE", "VD_MSE", "opt_fit_power", "opt_real_power", "opt_rank",
      name.free.space, 
      "max_power", "hidden", "layer", "drop_rate")
  n.nodes.output = data.frame(n.nodes.output)
  
  hidden.in = rep(n.node.in, n.layer.in)
  n.graph = dim(data.net.in)[1]
  
  val.ind = (n.graph/5)*(k.itt.in-1) + 1:(n.graph/5)
  if (k.indicator){
    val.ind = sample(1:n.graph, 1)
  }
  
  train.ind = (1:n.graph)[-val.ind]
  
  data.train = data.net.in[train.ind,]
  data.val = data.net.in[val.ind,]
  
  ## Keras
  data.keras.train = subset(data.train, select=name.free.space)
  data.keras.val = subset(data.val, select=name.free.space)
  data.keras.train =  as_tibble(data.keras.train)
  data.keras.train.scale <- scale(data.keras.train) 
  
  col_means_train <- attr(data.keras.train.scale, "scaled:center")
  col_stddevs_train <- attr(data.keras.train.scale, "scaled:scale")
  data.keras.val.scale <- scale(data.keras.val, 
                                center = col_means_train, scale = col_stddevs_train)

  
  k_clear_session()
  #rm(model)
  build_model <- function() {
    
    model <- NULL
    
    ### with no dropout
    # model.text.1 = paste0("model <- keras_model_sequential() %>% layer_dense(units = n.node.in, activation =", 
    #                       shQuote("sigmoid"),
    #                       ",input_shape = dim(data.keras.train.scale)[2], kernel_regularizer = regularizer_l2(l = tol.reg.in)) %>% ")
    # 
    # model.text.2 = paste0(rep(paste0(" layer_dense(units = n.node.in, activation = ",
    #                       shQuote("sigmoid"),
    #                       ", kernel_regularizer = regularizer_l1(l = tol.reg.in)) %>%"),
    #                    (n.layer.in-1)), collapse ="")
    
    ### with dropout
    model.text.1 = paste0("model <- keras_model_sequential() %>% layer_dense(units = n.node.in, activation =",
                          shQuote("sigmoid"),
                  ",input_shape = dim(data.keras.train.scale)[2]) %>% layer_dropout(rate=", drop.rate.in, ")%>%")

    model.text.2 = paste0(rep(paste0(" layer_dense(units = n.node.in, activation = ",
                                     shQuote("sigmoid"),
                                     ") %>% layer_dropout(rate=", drop.rate.in, ")%>%"),
                              (n.layer.in-1)), collapse ="")
    
    ### model.text.3
    model.text.3 = paste0("layer_dense(units = 1, activation = ",
                          shQuote("sigmoid"), ")")
    
    eval(parse(text=paste0(model.text.1, model.text.2, model.text.3)))
    
    # model <- keras_model_sequential() %>%
    #   layer_dense(units = n.node.in, activation = "sigmoid",
    #               input_shape = dim(data.keras.train.scale)[2],
    #               kernel_regularizer = regularizer_l2(l = tol.reg.in)) %>%
    #   layer_dense(units = n.node.in, activation = "sigmoid",
    #               kernel_regularizer = regularizer_l2(l = tol.reg.in)) %>%
    #   layer_dense(units = 1)
    
    model %>% compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
    
    model
  }
  
  model <- build_model()
  model %>% summary()
  
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 80 == 0) cat("\n")
      cat(".")
    }
  )  
  
  label.keras.train %<-% data.train$target.power.norm
  # label.keras.train %<-% data.train$target.power
  
  history <- model %>% fit(
    data.keras.train.scale,
    label.keras.train,
    epochs = max.epoch.in,
    validation_split = 0,
    verbose = 0,
    callbacks = list(print_dot_callback),
    batch_size = 1000
  )
  
  net.train.result <- model %>% predict(data.keras.train.scale)
  net.val.result <- model %>% predict(data.keras.val.scale)
  
  w1.scale = get_weights(model)[[1]]
  b1.scale = as.matrix(get_weights(model)[[2]])
  w1 = t(w1.scale/matrix(rep(col_stddevs_train, dim(w1.scale)[2]), 
                       nrow = dim(w1.scale)[1], ncol = dim(w1.scale)[2]))
  b1 = b1.scale - t(w1.scale)%*%as.matrix(col_means_train/col_stddevs_train)
  
  for (wb.itt in 2:(n.layer.in+1)){
    w.text = paste0("w", wb.itt, "=t(get_weights(model)[[", wb.itt*2-1, "]])")
    b.text = paste0("b", wb.itt, "= as.matrix(get_weights(model)[[", wb.itt*2, "]])")
    
    eval(parse(text=w.text))
    eval(parse(text=b.text))
  }
  
  ######################################################################
  ## for sigmoid function
  eval_f_whole_text1 = paste0(
    "eval_f <- function( x ) {; x.mat = as.matrix(c(x)); w1x = (w1)%*%x.mat + b1;sw1x = as.matrix(c(sigmoid(w1x)))")
    
  eval_f_whole_text2 = NULL
    for (wb.itt in 2:(n.layer.in)){
      wx.text = paste0("w", wb.itt, "x = (w", wb.itt, ")%*%sw", wb.itt-1, 
                       "x + b", wb.itt)
      swx.text = paste0("sw", wb.itt, "x = as.matrix(c(sigmoid(w", wb.itt, "x)))")
      eval_f_whole_text2 = paste(eval_f_whole_text2, wx.text, swx.text, sep = ";")
    }
    
    wb.itt.final = n.layer.in + 1
    wx.text = paste0("w", wb.itt.final, "x = (w", wb.itt.final, ")%*%sw", wb.itt.final-1, 
                     "x + b", wb.itt.final)
    swx.text = paste0("sw",n.layer.in+1,"x =sigmoid(w", wb.itt.final, "x)")
    eval_f_whole_text2 = paste(eval_f_whole_text2, wx.text, swx.text, sep = ";")
    
    eval_f_whole_text3 = paste("der_f = function(i){;sw1x_der = as.matrix(as.vector(c((1-sigmoid(w1x))*sigmoid(w1x)))*as.vector(c(w1[, i])));w2x_der = (w2)%*%sw1x_der")
    
    if (n.layer.in>1){
      for (wb.itt in 2:(n.layer.in)){
        swx.text = paste0("sw", wb.itt, "x_der = as.matrix(as.vector(c(sig_der(w",
                         wb.itt, "x)))*as.vector(c(w", wb.itt, "x_der)))")
        
        wx.text = paste0("w", wb.itt+1, "x_der = (w", wb.itt+1, ")%*%sw",
                          wb.itt, "x_der")
        eval_f_whole_text3 = paste(eval_f_whole_text3, swx.text, wx.text, sep = ";")
      }
    }
    
    out.text = paste0("out = as.numeric(sig_der(w", n.layer.in+1,
                      "x)*w", n.layer.in+1, "x_der)")
    
    eval_f_whole_text3 = paste(eval_f_whole_text3, out.text,
                               "return(out); }", sep = ";")
    
    grad.text = paste("-der_f(", 1:(length(name.free.space)), ")", collapse = ",")
    return.text = paste0(" return( list( ", shQuote("objective"), " = -sw",
                         n.layer.in+1,"x,",
                         shQuote("gradient") , " = c(", grad.text, ") ) )")
    
    eval_f_whole_text = paste(eval_f_whole_text1, eval_f_whole_text2,";",
                              eval_f_whole_text3, ";", return.text,";", "}")
    
    eval(parse(text=eval_f_whole_text))

  
  #####################################################################################
  data.train$fit.power = as.vector(net.train.result)
    
  data.train$fit.target.power = (data.train$fit.power-0.3)/0.4*
    (max(data.net.in$target.power)-min(data.net.in$target.power))+
    min(data.net.in$target.power)
  
  data.train$rank.target.power = (rank(data.train$target.power)-1)/(dim(data.train)[1]-1)
  data.train$rank.fit.power = (rank(data.train$fit.power)-1)/(dim(data.train)[1]-1)
  
  data.val$fit.power = as.vector(net.val.result)
  
  data.val$fit.target.power = (data.val$fit.power-0.3)/0.4*
    (max(data.net.in$target.power)-min(data.net.in$target.power))+
    min(data.net.in$target.power)
  
  data.val$rank.target.power = (rank(data.val$target.power)-1)/(dim(data.val)[1]-1)
  data.val$rank.fit.power = (rank(data.val$fit.power)-1)/(dim(data.val)[1]-1)
  
  print( mean((data.train$rank.target.power-data.train$rank.fit.power)^2))
  print( mean((data.val$rank.target.power-data.val$rank.fit.power)^2))
  
  ###############################################################################
  
  ## optimization

  ###########################################################################################
  ## set several initial values for constrained optimization
  
  solve.opt.out = solve.opt.in = solve.opt.fit.out = NULL
  n.solve.opt = 1
  
  for (solve.opt.ind in 1:n.solve.opt){
    x0.in = NULL
    grad.mat = NULL
    const.text = ""
    alpha.free.ind = head(which(!alpha.const==0), -1)
    if (sum(alpha.const)>1){
      const.text = paste(const.text, paste("x[", 1:length(alpha.free.ind), "]", collapse = "+"), "-1,")
      grad.mat.temp = rep(0, length(name.free.space))
      grad.mat.temp[1:length(alpha.free.ind)] = 1
      grad.mat = rbind(grad.mat,  grad.mat.temp)
      
      x0.temp.in = abs(rnorm(length(alpha.free.ind)+1, 0, 1))
      x0.temp.in = x0.temp.in/sum(x0.temp.in)
      x0.temp.in = x0.temp.in[1:length(alpha.free.ind)]
      
      x0.in = c(x0.in, x0.temp.in)
    }
    const.end = length(alpha.free.ind)
    
    for (i in 1:dim(w.const)[1]){
      w.const.temp = w.const[i, ]
      if (sum(w.const.temp)<=1) next
      
      w.free.ind = head(which(!w.const.temp==0), -1)
      const.text = paste(const.text, paste("x[", const.end + 1:length(w.free.ind), "]", 
                                           collapse = "+"), "-1,")
      
      grad.mat.temp = rep(0, length(name.free.space))
      grad.mat.temp[const.end + 1:length(w.free.ind)] = 1
      grad.mat = rbind(grad.mat,  grad.mat.temp)
      
      x0.temp.in = abs(rnorm(length(w.free.ind)+1, 0, 1))
      x0.temp.in = x0.temp.in/sum(x0.temp.in)
      x0.temp.in = x0.temp.in[1:length(w.free.ind)]
      
      x0.in = c(x0.in, x0.temp.in)
      
      const.end = const.end + length(w.free.ind)
    }
    
    substr(const.text, str_length(const.text), str_length(const.text)) <- ")"
    const.text = paste("constr <- c(", const.text)
    
    # constraint functions
    # inequalities
    eval_g_ineq <- function( x ) {
      eval(parse(text=const.text))
      grad = grad.mat
      return( list( "constraints"=constr, "jacobian"=grad ) )
    }
    
    # lower and upper bounds of control
    lb <- rep(0, length(name.free.space))
    ub <- rep(1, length(name.free.space))
    
    # NLOPT_LD_AUGLAG NLOPT_LN_COBYLA
    local_opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                        "xtol_rel" = 1.0e-5 )
    opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                  "xtol_rel" = 1.0e-5,
                  "maxeval" = 10000,
                  "local_opts" = local_opts )
    
    res <- nloptr( x0=x0.in,
                   eval_f=eval_f,
                   lb=lb,
                   ub=ub,
                   eval_g_ineq=eval_g_ineq,
                   opts=opts)
    print(res)
    
    opt.input.temp = res$solution
    opt.data = as.tibble(t(as.matrix(opt.input.temp)))
    opt.data.scale <- scale(opt.data, 
                            center = col_means_train, scale = col_stddevs_train)
    opt.fit.power.temp = model %>% predict(opt.data.scale)
    
    opt.fit.power.real = -gfo.func(opt.input.temp)
    
    solve.opt.fit.out = c(solve.opt.fit.out, opt.fit.power.temp)
    solve.opt.out = c(solve.opt.out, opt.fit.power.real)
    solve.opt.in = rbind(solve.opt.in, opt.input.temp)
    
  }
  
  solve.opt.select.ind = which.max(solve.opt.out)
  opt.x0 = solve.opt.in[solve.opt.select.ind, ]
  opt.real.power = solve.opt.out[solve.opt.select.ind]
  
  print(opt.real.power)
  
  ## fine tune with COBYLA
  naive.opt.fit = naive.opt.func(nloptr.func.name = "NLOPT_LN_COBYLA", 
                 naive.opt.n = 1, 
                 naive.tol = df.fit.tol.in, 
                 naive.max.n = df.max.n.in,
                 naive.max.t = df.max.t.in, 
                 pval.sim.mat.in = pval.sim.mat.in, 
                 x0.given = opt.x0)
  
  n.nodes.output[1, name.free.space] = naive.opt.fit$solution
  
  n.nodes.output$TD_MSE[1] = mean((data.train$fit.target.power-data.train$target.power)^2)
  
  n.nodes.output$VD_MSE[1] = mean((data.val$fit.target.power-data.val$target.power)^2)
  
  n.nodes.output$opt_real_power[1] = as.numeric(naive.opt.fit$naive.fit)
  n.nodes.output$opt_fit_power[1] = as.numeric(solve.opt.fit.out[solve.opt.select.ind])
  n.nodes.output$opt_rank[1] = sum(data.net.in$target.power>naive.opt.fit$naive.fit)+1
  
  print(n.nodes.output$opt_real_power)
  print(n.nodes.output$opt_rank)
  
  n.nodes.output$max_power[1] = as.numeric(max(data.net.in$target.power))
  n.nodes.output$hidden[1] = n.node.in
  n.nodes.output$layer[1] = n.layer.in
  n.nodes.output$drop_rate[1] = drop.rate.in
  n.nodes.output$time[1] = difftime(Sys.time(), neu.time, units="secs")
  n.nodes.output$iters[1] = naive.opt.fit$iters
  n.nodes.output$status[1] = naive.opt.fit$status
  
  newlist = list("n.nodes.output" = n.nodes.output,
                 "data.train" = data.train,
                 "data.val" = data.val)
  
  return(newlist)
}

sim.data.function = function(n.hypo.in, n.sim.in, trt.vec.in, alpha.fit.in, w.fit.in,
                             sigma.in, corr.in){
  
  sim.data.time = Sys.time()
  #sigma.in = matrix(corr.in, nrow = n.hypo.in, ncol = n.hypo.in)
  #diag(sigma.in) = 1
  trt.sim.mat = t(mvrnorm(n = n.sim.in, trt.vec.in, Sigma = sigma.in))
  
  pval.sim.mat = pnorm(trt.sim.mat, lower.tail = FALSE)
  n.graph.in = dim(alpha.fit)[1]
  data.net = cbind(alpha.fit.in, matrix(aperm(w.fit.in, c(3,2,1)), nrow =  n.graph.in, 
                                        ncol = n.hypo.in*n.hypo.in))
  data.net = data.frame(data.net)
  colnames(data.net) = c(paste0("a", 1:n.hypo.in), 
                         paste0("w", as.vector(sapply(1:n.hypo.in, 
                                                      function(x){paste0(x,"_", 1:n.hypo.in)}))))
  
  pow.vec.in = pnorm(qnorm(1-qnorm(1-0.025)), mean = trt.vec.in, lower.tail = FALSE)
  
    target.power.in = rep(0, n.graph.in)
    for (i in 1:n.graph.in){
      # print(i)
      graph.power.fit = graph.power(as.vector(alpha.fit.in[i, ]), as.matrix(w.fit.in[,,i]),
                                    type.1.error, pval.sim.mat)
      
      target.power.in[i] = sum(graph.power.fit*obj.weight)/sum(obj.weight)
    }
    

  
  data.net$target.power = target.power.in
  
  assump.out = matrix(NA, nrow=2, ncol=length(trt.vec.in))
  assump.out[1, ] = pnorm(qnorm(1-type.1.error), mean=trt.vec.in, lower.tail = FALSE)
  assump.out[2, ] = apply(pval.sim.mat, 1, function(x){mean(x<=0.025)})
  rownames(assump.out) = c("true power", "sim power")
  
  data.net.all = data.net
  
  #final.ind = which(order(data.net$target.power)<=(10^4))
  #final.ind = which(rank(data.net$target.power)>(n.graph.in/2))
  #data.net = data.net.all[final.ind, ]
  
  #print(target.power.in)
  
  ## Finish data simulation
  ####################################################################################
  data.net$target.power.norm =
    (data.net$target.power-min(data.net$target.power))/(max(data.net$target.power)-min(data.net$target.power))
  data.net$target.power.norm = data.net$target.power.norm*0.4+0.3
  
  newlist = list("pval.matrix" = pval.sim.mat, "data.matrix" = data.net,
                 "data.matrix.all" = data.net.all, 
                 "sim.data.time.diff" = difftime(Sys.time(), sim.data.time, units="secs"))
  return(newlist)
}

############################################################
## optimization function with gradient information

## option:  NLOPT_GN_DIRECT, NLOPT_GN_DIRECT_L,  NLOPT_GN_CRS2_LM, NLOPT_GN_ISRES,
##  NLOPT_LN_BOBYQA,  NLOPT_LN_NEWUOA,  NLOPT_LN_PRAXIS, NLOPT_LN_NELDERMEAD,
## NLOPT_LN_SBPLX, 

## OK: NLOPT_GN_ORIG_DIRECT, NLOPT_GN_ORIG_DIRECT_L, NLOPT_GN_ISRES, NLOPT_LN_COBYLA
gfo.func = function(x.gfo){
  
  alpha.free.ind = head(which(!alpha.const==0), -1)
  alpha.in = as.vector(rep(0, length(alpha.const)))
  if (sum(alpha.const)==1){
    alpha.in = alpha.const
  } else {
    alpha.in[alpha.const==1] = c(x.gfo[1:length(alpha.free.ind)],
                                 1 - sum(x.gfo[1:length(alpha.free.ind)]))
  }
  
  const.end = length(alpha.free.ind)
  w.in = matrix(0, nrow=dim(w.const)[1], ncol=dim(w.const)[1])
  for (i in 1:dim(w.in)[1]){
    w.const.temp = w.const[i,]
    if (sum(w.const.temp)==1){
      w.in[i, ] = w.const.temp
    } else {
      
      w.free.ind = head(which(!w.const.temp==0), -1)
      w.in[i, w.const[i,]==1] = c(x.gfo[1:length(w.free.ind) + const.end],
                                  1 - sum(x.gfo[1:length(w.free.ind) + const.end]))
      const.end = const.end + length(w.free.ind)
      
    }
    
  }
  
  alpha.in = pmin(alpha.in, 1)
  alpha.in = pmax(alpha.in, 0)
  alpha.in = alpha.in / sum(alpha.in)
  
  w.in[w.in<0] = 0
  w.in[w.in>1] = 1
  w.in = t(apply(w.in, 1, function(x){x/(sum(x)+10^(-6))}))
  
  # print(w.in)
  
  graph.power.gfo = graph.power(as.vector(alpha.in), 
                                as.matrix(w.in),
                                type.1.error, sim.data.fit$pval.matrix)
  return(-sum(graph.power.gfo*obj.weight)/sum(obj.weight))
}

naive.opt.func = function(nloptr.func.name, naive.opt.n, naive.tol, naive.max.n,
                          naive.max.t, 
                          pval.sim.mat.in, x0.given){
  
  set.seed(seed.number)
  naive.opt.time = Sys.time()
  test.temp = rep(0, naive.opt.n)
  for (naive.opt.ind in 1:naive.opt.n){
    print(paste(nloptr.func.name, ":", naive.opt.ind, "/", naive.opt.n))
    const.text = ""
    x0.start = grad.mat = NULL
    alpha.free.ind = head(which(!alpha.const==0), -1)
    if (sum(alpha.const)>1){
      const.text = paste(const.text, paste("x[", 1:length(alpha.free.ind), "]", collapse = "+"), "-1,")
      grad.mat.temp = rep(0, length(name.free.space))
      grad.mat.temp[1:length(alpha.free.ind)] = 1
      grad.mat = rbind(grad.mat,  grad.mat.temp)
      
      x0.start.in = runif(n=length(alpha.free.ind)+1, 0, 1)
      x0.start.in = x0.start.in/sum(x0.start.in)
      x0.start.in = x0.start.in[1:length(alpha.free.ind)]
      
      x0.start = c(x0.start, x0.start.in)
    }
    const.end = length(alpha.free.ind)
    
    for (i in 1:dim(w.const)[1]){
      w.const.temp = w.const[i, ]
      if (sum(w.const.temp)<=1) next
      
      w.free.ind = head(which(!w.const.temp==0), -1)
      const.text = paste(const.text, paste("x[", const.end + 1:length(w.free.ind), "]",
                                           collapse = "+"), "-1,")
      
      grad.mat.temp = rep(0, length(name.free.space))
      grad.mat.temp[const.end + 1:length(w.free.ind)] = 1
      grad.mat = rbind(grad.mat,  grad.mat.temp)
      
      x0.start.in = runif(n=length(w.free.ind)+1, 0, 1)
      x0.start.in = x0.start.in/sum(x0.start.in)
      x0.start.in = x0.start.in[1:length(w.free.ind)]
      
      x0.start = c(x0.start, x0.start.in)
      
      const.end = const.end + length(w.free.ind)
    }
    
    substr(const.text, str_length(const.text), str_length(const.text)) <- ")"
    const.text = paste("constr <- c(", const.text)
    
    # constraint functions
    # inequalities
    eval_ineq <- function( x ) {
      eval(parse(text=const.text))
      grad = grad.mat
      return( list( "constraints"=constr, "jacobian"=grad ) )
    }
    
    local_opts <- list( "algorithm" = nloptr.func.name,
                        "xtol_rel" = naive.tol,
                        "ftol_rel" = 0,
                        "maxeval" = 100)
    opts <- list( "algorithm" = nloptr.func.name,
                  "xtol_rel" = naive.tol,
                  "ftol_rel" = 0,
                  "maxeval" = naive.max.n,
                  "maxtime" = naive.max.t, 
    "local_opts" = local_opts )
    
    if (is.null(x0.given)){
      x0.start.in = x0.start
    } else {
      x0.start.in = x0.given
    }
    
    res <- nloptr( x0=x0.start.in,
                   eval_f=gfo.func,
                   lb=rep(0, length(x0.start)),
                   ub=rep(1, length(x0.start)),
                   eval_g_ineq=eval_ineq,
                   opts=opts)
    print(res)
    test.temp[naive.opt.ind] = -res$objective
    
    naive.input.temp = res$solution
    # naive.data = as.tibble(t(as.matrix(naive.input.temp)))
    
    alpha.in = as.vector(rep(0, length(alpha.const)))
    if (sum(alpha.const)==1){
      alpha.in = alpha.const
    } else {
      alpha.in[alpha.const==1] = c(naive.input.temp[1:length(alpha.free.ind)],
                                   1 - sum(naive.input.temp[1:length(alpha.free.ind)]))
    }
    
    const.end = length(alpha.free.ind)
    w.in = matrix(0, nrow=dim(w.const)[1], ncol=dim(w.const)[1])
    for (i in 1:dim(w.in)[1]){
      w.const.temp = w.const[i,]
      if (sum(w.const.temp)==1){
        w.in[i, ] = w.const.temp
      } else {
        
        w.free.ind = head(which(!w.const.temp==0), -1)
        w.in[i, w.const[i,]==1] = c(naive.input.temp[1:length(w.free.ind) + const.end],
                          1 - sum(naive.input.temp[1:length(w.free.ind) + const.end]))
        const.end = const.end + length(w.free.ind)
        
      }
      
    }
    
    alpha.in = pmin(alpha.in, 1)
    alpha.in = pmax(alpha.in, 0)
    alpha.in = alpha.in / (10^(-6)+sum(alpha.in))
    
    w.in[w.in<0] = 0
    w.in[w.in>1] = 1
    w.in = t(apply(w.in, 1, function(x){x/(10^(-6)+sum(x))}))
    
    if (sum(alpha.in)>1) return(rep(NA, 9 + length(name.free.space)))
    if (sum(alpha.in<0)>0) return(rep(NA, 9 + length(name.free.space)))
    if (sum(alpha.in>1)>0) return(rep(NA, 9 + length(name.free.space)))
    if (sum(apply(w.in, 1, sum)>1)>0) return(rep(NA, 9 + length(name.free.space)))
    if (sum(w.in<0)>0) return(rep(NA, 9 + length(name.free.space)))
    if (sum(w.in>1)>0) return(rep(NA, 9 + length(name.free.space)))
    graph.in = matrix2graph(w.in, alpha.in)
    
    out_seq = graphTest(pvalues = t(pval.sim.mat.in), graph = graph.in, alpha = type.1.error)
    out.power = as.vector(apply(out_seq, 2, mean))
    # naive.fit.power.real = sum(out.power*obj.weight)/sum(obj.weight)

    
  }
  newlist = list("naive.fit" = test.temp,
                 "solution" = res$solution, 
                 "naive.alpha" = alpha.in,
                 "naive.w" = w.in,
                 "status" = res$status,
                 "iters" = res$iterations,
                 "time" = difftime(Sys.time(), naive.opt.time, units="secs"))
  
  return(newlist)
  
}
 






