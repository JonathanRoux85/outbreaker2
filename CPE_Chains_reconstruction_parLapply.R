#############################
#### Loading of packages ####
#############################
library(outbreaker2)
library(data.table)
library(fitdistrplus)
library(parallel)

###################################
#### Loading of simulated data ####
###################################
real_data <- readRDS("./Real_data_CPE.rds")

cpe <- real_data$cpe
transfer_matrix <- real_data$transfer

source("./Functions_chains_reconstruction.R")

###########################
#### Global parameters ####
###########################
cores = 20

n_iter_mcmc <- 50000
n_sample <- n_iter_mcmc*0.0001
burning <- n_iter_mcmc*0.1

# Coefficient of variation used for generation time #
variation_coef <- 0.5
variation_coef_label <- "05"
# Mean of generation interval time #
mean_generation <- seq(1,60,1)

# Compute or not priors for alpha (ancestors) #
prior_alpha <- TRUE

# Initialization of psi #
init_psi <- 1
move_psi <- TRUE

# Incubation time #
f_dens <- NULL

# Other parameters #
move_sigma <- TRUE
init_sigma <- 0.99
move_pi <- TRUE
init_pi <- 1
prior_pi <- c(1,1)

#############################
#### Preparation of data ####
#############################
# Dates of detection #
dates <- cpe[, date]
# IDs of hospitals infected or colonized #
ids <- cpe[, finess]
# Number of cases who can move in the network #
n_cases <- cpe[, as.numeric(no_cases)]

# Preparation of transfer matrix #
transfer_matrix <- transfer_matrix / rowSums(transfer_matrix)
# To take into account hospitals doing 0 transfer but admitting transferred
# patients
transfer_matrix[is.nan(transfer_matrix)] <- 0 

#################################################
#### Generating generation time distribution ####
#################################################
# w <- dgamma(1:70, shape = 7, rate = 1)

########################################
#### Parameters for parallelization ####
########################################
cl <- makeCluster(cores)
clusterExport(cl, c("dates", "n_cases", "transfer_matrix",
                    "cpe","ids",
                    "n_iter_mcmc", "n_sample", "burning",
                    "prior_alpha", "move_sigma", "init_sigma",
                    "move_pi", "init_pi", "init_psi", 
                    "move_psi", "prior_pi", "f_dens",
                    "variation_coef","mean_generation"))
clusterEvalQ(cl, library(outbreaker2))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, source("./Functions_chains_reconstruction.R"))

################################
#### Chains' reconstruction ####
################################
out <- parLapply(cl, 1/(mean_generation*variation_coef^2), function(i) {
  output <- RealChainsReconstruction(dates = dates, 
                                     w = dgamma(1:100, shape = 1/variation_coef^2, rate = i), 
                                     n_cases = n_cases, 
                                     transfers = transfer_matrix, 
                                     ids = ids,
                                     f_dens = f_dens,
                                     imported = cpe[, imported], 
                                     n_iter_mcmc = n_iter_mcmc, 
                                     n_sample = n_sample, 
                                     burning = burning,
                                     prior_alpha = prior_alpha,
                                     move_sigma = move_sigma,
                                     init_sigma = init_sigma,
                                     move_pi = move_pi,
                                     init_pi = init_pi,
                                     init_psi = init_psi, 
                                     move_psi = move_psi, 
                                     prior_pi = prior_pi)
  return(list(output = output,
              rate = i))
})

stopCluster(cl)

saveRDS(out, file=paste0("RealChainsReconstruction_vc",variation_coef_label,".rds"))

# ###################################
# #### Plot of resulting network ####
# ###################################
# plot_network(out[[1]]$output$res_consensus,
#              min_support = 0.06)


