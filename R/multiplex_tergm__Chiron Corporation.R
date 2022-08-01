##
##
## MULTIPLEX NETWORKS TERGM
## (competition + cooperation)
##
##
##
# ego_firms <- c(
#   'Cymer Inc', 
#   'Technical Olympic USA, Inc.', 
#   'Chiron Corporation', 
#   'Spartech Corporation', 
#   'Asyst Technologies Inc'
# )

##===================================
## Network sample parameters
##===================================
ego_firm <- 'Chiron Corporation'
samp_years <- 2003:2015
d <- 3   ## ego network sample order (degree of separation from seed node)
R <- 500 ## bootstrap resamples
cores <- 16 ## computing cores for parallelization
#####################################


library(btergm)
library(igraph)
library(intergraph)
library(parallel)
# library(snow)

## detect system for file paths or use between local and remote systems
getSysname <- function(){
  x <- Sys.info()
  sysname <- x[which(names(x)=='sysname')]
  return( unname(sysname) )
}

if (getSysname() == 'Windows') {
  ## Local PC (Windows)
  dir_data <- 'D:\\data\\Compete_cooperate_networks_data__Marat-Gideon'
  dir_data_nets <- dir_data
  dir_data_results <- dir_data
  # dir_proj <- dir_data
  # dir_proj_R <- dir_data
} else { 
  ## Remote HPC (Linux)
  dir_data <- '/data/sdr8y/coopetition_networks'
  dir_data_nets <- file.path(dir_data,'nets_lists')
  dir_data_results <- file.path(dir_data,'results')
  # dir_proj <- '/home/sdr8y/coopetition_networks'
  # dir_proj_R <- file.path(dir_proj, 'R')
}


# load data list
netsfile <- sprintf('multiplex_nets_list__%s__d%s_%s-%s.rds',
                    ego_firm, d, min(samp_years), max(samp_years))
l <- readRDS(file.path(dir_data_nets, netsfile))
comp <- l$comp
coop <- l$coop
years <- l$years

## Edgecov lists
comp_wt <- lapply(comp, function(net) network::as.matrix.network.adjacency(net, attrname = 'weight'))
comp_dist <- lapply(comp, function(net) distances(asIgraph(net)) )
# deg_comp <- lapply(comp, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )

## BTERGM MODEL
mod1 <- coop ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) +
  cycle(3) + cycle(4) +
  memory(type = "stability", lag = 1) + timecov(transform = function(t) t) +
  edgecov(comp_dist) + edgecov(comp_wt) + # edgecov(comp) +
  nodecov("deg_coop") + absdiff("deg_coop")

## RUN MODEL IN PARALLEL (using forking on non-Windows OS, or PSOCK otherwise[?])
#cl_type <- getClusterOption("type")
#cat(sprintf("\ncluster type = %s\n", cl_types))
#cl <- makeCluster(cores, type = getClusterOption("type")) ## types= ["FORK", "PSOCK"]
cl <- makeCluster(cores, type="FORK")
fit1 <- btergm::btergm(formula = mod1, R = R,
                       parallel = 'snow', ncpus = cores, #cl = cl,
                       verbose = TRUE)
stopCluster(cl)

## Save output
resultsfile <- sprintf('multiplex_TERGM_results__%s__d%s_%s-%s.rds',
                       ego_firm, d, min(samp_years), max(samp_years))
saveRDS(fit1, file=file.path(dir_data_results,resultsfile))

cat('Run completed successfully.')











# 
# 
# 
# ## make MMC nets list
# mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
# cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
# cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
# cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )
# cossim <- lapply(nets, function(net) as.matrix(net %n% 'cat_cos_sim'))
# centjoin <- lapply(nets, function(net) as.matrix(net %n% 'joint_cent_pow_n0_4'))
# centratio <- lapply(nets, function(net) as.matrix(net %n% 'cent_ratio_pow_n0_4'))
# shcomp <- lapply(nets, function(net) as.matrix(net %n% 'shared_competitor')) 
# shinv <- lapply(nets, function(net) as.matrix(net %n% 'shared_investor_nd'))
# 
# ####################### DEFINE MODELS ###################################
# 
# m4_1 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
#   nodematch("ipo_status", diff = F) + 
#   nodematch("state_code", diff = F) + 
#   nodecov("age") + absdiff("age") + 
#   nodecov("employee_na_catage") + ##nodecov("sales_na_0") +
#   ##edgecov(cossim) + ##edgecov(centjoin) + 
#   ##edgecov(shcomp) + ##edgecov(shinv) +   
#   edgecov(mmc) + 
#   ##edgecov(cpa) +
#   ##edgecov(cpc) + 
#   ##edgecov(cpp) +
#   memory(type = "stability", lag = 1) + 
#   timecov(transform = function(t) t) +
#   nodecov("genidx_multilevel") + 
#   nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
#   cycle(3) + cycle(4) + cycle(5) 
# 
# ################################ end models#######################
