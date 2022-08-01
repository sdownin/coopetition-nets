##
## Coopeartive & Competitive Relation TERGM
##
library(tidyr)
library(igraph)
library(btergm)
library(readxl)
library(stringr)
library(parallel)
library(network)
library(intergraph)


work_dir <- 'E:\\data\\Compete_cooperate_networks_data__Marat-Gideon'
coop_dir <- file.path(work_dir, 'Collaborative ties')
comp_dir <- file.path(work_dir, 'Competitive ties')

setwd(work_dir)

co <- readxl::read_xlsx('Company ID and Name.xlsx', col_types = c('text','text'))

coop_files <- dir(coop_dir)
comp_files <- dir(comp_dir)

coop_df <- data.frame()
for (i in 1:length(coop_files)) {
  year_str <- str_split(coop_files[i], '[.]', simplify = T)[1]
  cat(sprintf('i=%s, year=%s\n',i, year_str))
  .tmp_df <- read.csv(file.path(coop_dir,coop_files[i]), header = F)
  .tmp_df$year <- as.integer(year_str)
  coop_df <- rbind(coop_df, .tmp_df)
} 
coop_df$rel_type <- as.factor('cooperation')

comp_df <- data.frame()
for (i in 1:length(comp_files)) {
  year_str <- str_split(comp_files[i], '[.]', simplify = T)[1]
  cat(sprintf('i=%s, year=%s\n',i, year_str))
  .tmp_df <- read.csv(file.path(comp_dir,comp_files[i]), header = F)
  .tmp_df$year <- as.integer(year_str)
  comp_df <- rbind(comp_df, .tmp_df)
} 
comp_df$rel_type <- as.factor('competition')


samp_years <- 2003:2015

coop_df <- coop_df[which(coop_df$year %in% samp_years), ]
coop_IDs <- sort(unique(c(coop_df$V1,coop_df$V2 )))

comp_df <- comp_df[which(comp_df$year %in% samp_years), ]
comp_IDs <- sort(unique(c(comp_df$V1,comp_df$V2 )))


mult_IDs <- intersect(coop_IDs, comp_IDs)


## Multiplex graph
df_mult <- rbind(comp_df, coop_df)
vert_mult <- co[which(co$ID %in% c(df_mult$V1, df_mult$V2)), ]
df_mult_sub <- df_mult[which(df_mult$V1 %in% vert_mult$ID & df_mult$V2 %in% vert_mult$ID), ]
gmult <- graph.data.frame(df_mult_sub, directed = F, vertices = vert_mult)


ego_firm <- 'Asyst Technologies Inc'
d <- 3 

gmego <- igraph::make_ego_graph(gmult, order = d, nodes = which(V(gmult)$NAME==ego_firm))[[1]]

gmego.coop <- igraph::delete.edges(gmego, edges = which(E(gmego)$rel_type=='competition') ) ## remove other rel_type
gmego.comp <- igraph::delete.edges(gmego, edges = which(E(gmego)$rel_type=='cooperation') ) ## remove other rel_type


gmego.coop.dx <- igraph::make_ego_graph(gmego.coop, order = d, nodes = which(V(gmego.coop)$NAME==ego_firm))[[1]]
gmego.comp.dx <- igraph::make_ego_graph(gmego.comp, order = d, nodes = which(V(gmego.comp)$NAME==ego_firm))[[1]]


name.mult.dx <- intersect( V(gmego.coop.dx)$NAME, V(gmego.comp.dx)$NAME) 
gmdx <- induced_subgraph(gmego, vids = which(V(gmego)$NAME %in% name.mult.dx))

gmdx.coop <- igraph::delete.edges(gmdx, edges = which(E(gmdx)$rel_type=='competition') ) ## remove other rel_type
gmdx.comp <- igraph::delete.edges(gmdx, edges = which(E(gmdx)$rel_type=='cooperation') ) ## remove other rel_type

V(gmdx.coop)$deg_coop <- igraph::degree(gmdx.coop)
V(gmdx.comp)$deg_comp <- igraph::degree(gmdx.comp)

V(gmdx)$deg_coop <- V(gmdx.coop)$deg_coop
V(gmdx)$deg_comp <- V(gmdx.comp)$deg_comp

# V(gmego)$deg_coop <- igraph::degree(gmego)
# V(gmego)$deg_comp <- 
V(gmdx)$deg_mult <- igraph::degree(gmdx)

gx <- igraph::induced_subgraph(gmdx, v = which(V(gmdx)$deg_mult > 0 & V(gmdx)$deg_coop > 0 & V(gmdx)$deg_comp > 0))

g2 <- igraph::make_ego_graph(gx, order = 2, nodes = which(V(gx)$NAME==ego_firm))[[1]]

E(g2)$weight <- 1


nlcomp <- list()
nlcoop <- list()
nlmult <- list()
for (i in 1:length(samp_years)) { 
  cat(i)
  
  year <- samp_years[i]
  
  g2y <- igraph::delete.edges(g2, edges = which(E(g2)$year != year) ) 
  
  g2y.coop <- igraph::delete.edges(g2y, edges = which(E(g2y)$rel_type=='competition') ) ## remove other rel_type
  g2y.comp <- igraph::delete.edges(g2y, edges = which(E(g2y)$rel_type=='cooperation') ) ## remove other rel_type
  
  g2y.coop <- igraph::simplify(g2y.coop, remove.multiple = T, edge.attr.comb = list(weight='sum', year='min', rel_type='min'))
  g2y.comp <- igraph::simplify(g2y.comp, remove.multiple = T, edge.attr.comb = list(weight='sum', year='min', rel_type='min'))
  
  # coopmat <- igraph::as_adjacency_matrix(g2y.coop, names = T, sparse = F)
  # compmat <- igraph::as_adjacency_matrix(g2y.comp, names = T, sparse = F)
  # vdf.coop <- as.data.frame(igraph::vertex.attributes(g2y.coop))
  # vdf.comp <- as.data.frame(igraph::vertex.attributes(g2y.comp)) 
  
  nlmult[[i]] <- asNetwork(g2y)
  nlcoop[[i]] <- asNetwork(g2y.coop)
  nlcomp[[i]] <- asNetwork(g2y.comp)
  
}

names(nlmult) <- samp_years
names(nlcoop) <- samp_years
names(nlcomp) <- samp_years


## BTERGM
m1 <- nlcoop ~ edges + memory(type = "stability", lag = 1) + 
  edgecov(nlcomp) + 
  # timecov(onlytime = T) +
  # gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  cycle(3) + cycle(4) 

fit1 <- btergm::btergm(m1, R = 30, parallel = 'multicore', ncpus = detectCores())

# nodecov("genidx_multilevel") + 
#   nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
# nodematch("ipo_status", diff = F) + 
#   nodematch("state_code", diff = F) + 

# ## Cooperation graph
# coop_mult <- coop_df[which(coop_df$V1 %in% mult_IDs & coop_df$V2 %in% mult_IDs), ]
# vert_coop <- co[which(co$ID %in% c(coop_mult$V1, coop_mult$V2)), ]
# coop_mult_sub <- coop_mult[which(coop_mult$V1 %in% vert_coop$ID & coop_mult$V2 %in% vert_coop$ID), ]
# gcoop <- graph.data.frame(coop_mult_sub, directed = F, vertices = vert_coop)
# 
# ## Competition graph
# comp_mult <- comp_df[which(comp_df$V1 %in% mult_IDs & comp_df$V2 %in% mult_IDs), ]
# vert_comp <- co[which(co$ID %in% c(comp_mult$V1, comp_mult$V2)), ]
# comp_mult_sub <- comp_mult[which(comp_mult$V1 %in% vert_comp$ID & comp_mult$V2 %in% vert_comp$ID), ]
# gcomp <- graph.data.frame(comp_mult_sub, directed = F, vertices = vert_comp)




df <- rbind(comp_df, coop_df)

df2 <- df[which(df$year %in% 2008:2020), ]


# df <- merge.data.frame(x=, y=, by.x=, by.y=,  all.x=, all.y = ,  )

ego_firm <- 