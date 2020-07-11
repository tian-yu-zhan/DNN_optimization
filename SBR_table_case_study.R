
###################################################################################
## case study table and graphs
library(gMCP)
library(xtable)
setwd("~")
opt.in = read.csv("fit pow 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr 0.5 hypo 5.csv")
naive.in = read.csv("GF 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr 0.5 GF_n 1 hypo 5.csv")
brute.in = read.csv("fit mat 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr 0.5 hypo 5.csv")
load("data pow 0.95 0.9 0.85 0.65 0.6 obj 0 0.6 0.2 0.1 0.1 corr 0.5 n.sim 6 n.graph 1000.RData")

case.study.table = matrix(NA, nrow = 4, ncol = 6)
for (i in 1:4){
  
  if (i==1) graph.file = opt.in
  if (i==2) graph.file = naive.in[1, ]
  if (i==3) graph.file = naive.in[2, ]
  if (i==4) graph.file = brute.in[which.max(brute.in$target.power), ]
  
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
    if (j<=4){
      w.in[j, ] = c(w.in[j, 1:4], 1-sum(w.in[j, 1:4]))  
    } else {
      w.in[j, ] = c(w.in[j, 1:3], 1-sum(w.in[j, 1:3]), 0)  
    }
    
    # w.in[j, j] = 0
  }

  graph.in = matrix2graph(w.in, alpha.in)
  # graphGUI(graph.in)
  print(w.in)
  print(apply(w.in, 1, sum))
  
  out_seq = graphTest(pvalues = t(sim.data.fit$pval.matrix), graph = graph.in, alpha = 0.025)
  out.power = apply(out_seq, 2, mean)
  case.study.table[i,] = paste0(sprintf("%.1f", c(sum(c(0, 0.6, 0.2, 0.1, 0.1)*out.power), out.power)*100), "%")
}

case.study.table = cbind(c("FNN", "ISRES", "COBYLA", "Brute-force"), case.study.table)
print(xtable(case.study.table),include.rownames = FALSE)







