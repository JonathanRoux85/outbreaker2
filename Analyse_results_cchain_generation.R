#############################
#### Loading of packages ####
#############################
library(outbreaker2)
library(data.table)
library(ggplot2)
library(cowplot)

#########################
#### Loading of data ####
#########################
results_01 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc01.rds")
results_02 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc02.rds")
results_05 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc05.rds")
results_1 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc1.rds")
results_15 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc15.rds")
results_2 <- readRDS("./results/Genouest/1-Cchain_n50000_b5000_t5_s1_generation_vc2.rds")

source("./Functions_chains_reconstruction.R")
source("./Functions_Rmd_1run.R")

min_support <- 0.079
variation_coefficient <- 1
max_episodes <- 848

# Burning period #
burning <- 5000
NewBurning <- FALSE
include_imported <- FALSE

# Complements if needed #
no_chain <- 1
complements <- ""
NotRetrieved <- FALSE
StudyKappa <- FALSE
prior_alpha <- TRUE

##########################################
#### Loading of chains_detect100_bind ####
##########################################
load(paste0("../StageCRENet/hackathon/outbreaker/data/data", no_chain, ".Rdata"))
load(paste0("../StageCRENet/hackathon/outbreaker/chains/chains", no_chain, ".Rdata"))

## Preparation of data ##
detect100 <- data$detect100
chains_detect100 <- chains$detect100

chains_detect100_bind <- rbindlist(chains_detect100)
setnames(chains_detect100_bind, 
         c("hospID","origin"),
         c("to","from"))

# Keeping in mind the chains #
counter <- 0
for(i in 1:chains_detect100_bind[,.N]){
  if(chains_detect100_bind[i, is.na(from)])
    counter <- counter + 1
  chains_detect100_bind[i, chain := counter]
}

# Length of chains #
lengths_chains <- chains_detect100_bind[, .N, by = "chain"]
chains_detect100_bind <- merge(chains_detect100_bind,
                               lengths_chains[, .(chain,
                                                  length_chain = N-1)],
                               by = "chain")

## Preparation of chains_detect100_bind to facilitate the computation of parameters ##
detect100[,num := seq_len(.N)]

chains_detect100_bind.2 <- merge(chains_detect100_bind,
                                 detect100[,.(hospID, t_descendant = t,
                                              t_detect_descendant = t_detect, 
                                              num)],
                                 by.x = c("to", "t_detect"),
                                 by.y = c("hospID","t_detect_descendant"))
chains_detect100_bind.2 <- merge(chains_detect100_bind.2,
                                 detect100[,.(hospID, 
                                              t_ancestor = t, 
                                              num)],
                                 by.x = c("from"),
                                 by.y = c("hospID"),
                                 all.x=TRUE)
chains_detect100_bind.2 <- chains_detect100_bind.2[t_ancestor<t | is.na(from)]
chains_detect100_bind.2 <- chains_detect100_bind.2[order(to, t, t_ancestor)]
chains_detect100_bind.2 <- chains_detect100_bind.2[,.SD[.N],
                                                   by = c("from", "to", "t")]
# chains_detect100_bind.2[, t.y := NULL]
setnames(chains_detect100_bind.2, c("num.x","num.y"), c("ids_to","ids_from"))

detect100[, num := NULL]

chains_detect100_bind <- chains_detect100_bind.2


###################################
#### Computation of parameters ####
###################################
gamma_rate <- unlist(lapply(results_1, 
                            function(r) {
                              r$rate
                            }
)
)

generation_time_mean <- 1 / variation_coefficient^2 / gamma_rate

output_01 <- rbindlist(lapply(results_01,
                             function(r) {
                               ParametersSynthesis_generation(r$output,
                                                              burning = burning,
                                                              min.support = min_support,
                                                              real_chains = chains_detect100_bind,
                                                              include_imported = include_imported)
                             }))

output_02 <- rbindlist(lapply(results_02,
                             function(r) {
                               ParametersSynthesis_generation(r$output,
                                                              burning = burning,
                                                              min.support = min_support,
                                                              real_chains = chains_detect100_bind,
                                                              include_imported = include_imported)
                             }))

output_05 <- rbindlist(lapply(results_05,
                             function(r) {
                               ParametersSynthesis_generation(r$output,
                                                              burning = burning,
                                                              min.support = min_support,
                                                              real_chains = chains_detect100_bind,
                                                              include_imported = include_imported)
                             }))

output_1 <- rbindlist(lapply(results_1,
                           function(r) {
                             ParametersSynthesis_generation(r$output,
                                                            burning = burning,
                                                            min.support = min_support,
                                                            real_chains = chains_detect100_bind,
                                                            include_imported = include_imported)
                             }))

output_15 <- rbindlist(lapply(results_15,
                             function(r) {
                               ParametersSynthesis_generation(r$output,
                                                              burning = burning,
                                                              min.support = min_support,
                                                              real_chains = chains_detect100_bind,
                                                              include_imported = include_imported)
                             }))

output_2 <- rbindlist(lapply(results_2,
                              function(r) {
                                ParametersSynthesis_generation(r$output,
                                                               burning = burning,
                                                               min.support = min_support,
                                                               real_chains = chains_detect100_bind,
                                                               include_imported = include_imported)
                              }))

output_01 <- cbind(generation_time_mean, output_01)
output_02 <- cbind(generation_time_mean, output_02)
output_05 <- cbind(generation_time_mean, output_05)
output_1 <- cbind(generation_time_mean, output_1)
output_15 <- cbind(generation_time_mean, output_15)
output_2 <- cbind(generation_time_mean, output_2)

#########################
#### Plot of results ####
#########################
FigPlot.no_links <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "01"), size = 1.3, 
            data = output_01) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "01"), size = 2.5, 
             data = output_01) + 
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "02"), size = 1.3, 
            data = output_02) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "02"), size = 2.5, 
             data = output_02) + 
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "05"), size = 1.3, 
            data = output_05) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "05"), size = 2.5, 
             data = output_05) + 
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "1"), size = 2.5, 
             data = output_1) + 
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "15"), size = 1.3, 
            data = output_15) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "15"), size = 2.5, 
             data = output_15) +
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "2"), size = 1.3, 
            data = output_2) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "2"), size = 2.5, 
             data = output_2) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# true positive links identified") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("01"="#0000CD",'02'="#8B0000","05"="#DAA520",
                               "1"="#228B22","15"="#FF4500","2"="#9370DB"), 
                      labels=c("01"="0.1","02"="0.2","05"="0.5","1"="1","15"="1.5", "2"="2"))
FigPlot.no_links

FigPlot.no_links_1 <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "1"), size = 2.5, 
             data = output_1) + 
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "15"), size = 1.3, 
            data = output_15) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "15"), size = 2.5, 
             data = output_15) +
  geom_line(aes(x = generation_time_mean, y = tp_links.aa, color = "2"), size = 1.3, 
            data = output_2) +
  geom_point(aes(x = generation_time_mean, y = tp_links.aa, color = "2"), size = 2.5, 
             data = output_2) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# true positive links identified") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("01"="#0000CD",'02'="#8B0000","05"="#DAA520",
                               "1"="#228B22","15"="#FF4500","2"="#9370DB"), 
                      labels=c("01"="0.1","02"="0.2","05"="0.5","1"="1","15"="1.5", "2"="2"))
FigPlot.no_links_1


##################################
#### Focus on generation time ####
##################################
# We use resuts with variation coefficient equal to 0.5 #

chains_05 <- cbind(gamma_mean = generation_time_mean,
                   rbindlist(lapply(results_05,
                                    function(r) {
                                      merged <- merge(r$output$res_aa,
                                                      chains_detect100_bind[, .(from, to, 
                                                                                ids_from, ids_to,
                                                                                t_descendant,
                                                                                t_ancestor,
                                                                                chain)],
                                                      by.x = c("from","to"),
                                                      by.y = c("ids_from","ids_to"),
                                                      all = TRUE)
                                      
                                      # We only keep the links in real_data #
                                      merged <- merged[to %in% chains_detect100_bind[,ids_to]]
                                      # We exclude imported episodes #
                                      merged <- merged[!(is.na(init_alpha) & !is.na(t_descendant)),]
                                      # We only keep true identified chains with minimal support #
                                      merged <- merged[support >= min_support & 
                                                         !is.na(t_descendant), ]
                                      # Computation of generation time #
                                      merged[,generation_time := t_descendant - t_ancestor]

                                      return(merged[,.(mean = mean(generation_time),
                                                       median = median(generation_time),
                                                       min = min(generation_time),
                                                       max = max(generation_time),
                                                       q1 = quantile(generation_time,0.25),
                                                       q3 = quantile(generation_time,0.75)),])
                                      })))


