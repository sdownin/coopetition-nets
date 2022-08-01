##
## Test btergm parallel on HPC cluster slurm batch job
##
library(igraph)
library(btergm)
library(parallel)

dir_data <- '/data/sdr8y/coopetition_networks'
dir_data_results <- file.path(dir_data,'results')
dir_data_nets <- file.path(dir_data, 'nets_lists')

# periods <- 1:4
# n <- 100
cores <- 16
R <- 10000

filename <- 'multiplex_nets_list_TEST__Cymer Inc__d2_2011-2015.rds'
l <- readRDS(file.path(dir_data_nets, filename))
coop <- l$coop
comp <- l$comp

mod <- coop ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) +
  edgecov(comp) + nodecov('deg_coop') + #absdiff('deg_coop') +
  cycle(3) + cycle(4) + memory(type = "stability", lag = 1) #+ timecov(transform = function(t) t)
  
## RUN MODEL IN PARALLEL (using forking on non-Windows OS, or PSOCK otherwise[?])
#cl_type <- getClusterOption("type")
#cat(sprintf("\ncluster type = %s\n", cl_types))
#cl <- makeCluster(cores, type = getClusterOption("type")) ## types= ["FORK", "PSOCK"]
cl <- makeCluster(cores, type="FORK")
fit1 <- btergm::btergm(formula = mod, R = R,
                       parallel = 'snow', ncpus = cores, #cl = cl,
                       verbose = TRUE)
stopCluster(cl)

## Save output
resultsfile <- 'btergm_parallel_test.rds'
saveRDS(fit1, file=file.path(dir_data_results, resultsfile))

cat('Run completed successfully.')





#netlist <- list()
#for (i in 1:length(periods)) {
#  cat(i,'\n')
#  pd <- periods[i]
#  if (i == 1) {
#    mat <- matrix(rep(0, n^2, replace=T),nrow = n)
#    idx1 <- sample(1:(n^2), round(.1 * n^2), replace = F)
#    mat[idx1] <- 1
#  } else {
#    idx <- sample(1:(n^2), size = round(.02 * n^2), replace = FALSE)
#    mat <- network::as.matrix.network.adjacency( netlist[[i-1]] )
#    mat[idx] <- 1 - mat[idx] ## flip 25% of value 1->0, 0->1
#  }
#  netlist[[i]] <- network::as.network(mat, directed = FALSE)
#}



