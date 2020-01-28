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
source("./Functions_chains_reconstruction.R")
facilites_distances <- readRDS("./Facilities_distances.rds")
facilites_distances <- facilites_distances$real

min_support <- 0.1122
variation_coefficient <- 0.5

# Burning period #
burning <- 5000
NewBurning <- FALSE


####################################################################################
####################################################################################
#### Modification of standard deviation of the generation interval distribution ####
####################################################################################
####################################################################################

###################################
#### Computation of parameters ####
###################################
## Variation coefficient = 0.5 ##
results_05 <- readRDS("./results/Genouest/RealChainsReconstruction_vc05.rds")

gamma_rate <- unlist(lapply(results_05, 
                            function(r) {
                              r$rate
                            }
                            )
                     )

generation_time_mean <- 1 / variation_coefficient^2 / gamma_rate

output_05 <- rbindlist(lapply(results_05,
                              function(r) {
                                ParametersSynthesis_CPEgeneration(r$output,
                                                                  burning = burning,
                                                                  min.support = min_support,
                                                                  distances = facilites_distances)
                              }))
rm(results_05)

results_05_suite <- readRDS("./results/Genouest/RealChainsReconstruction_vc05_suite.rds")

gamma_rate <- unlist(lapply(results_05_suite, 
                            function(r) {
                              r$rate
                            }
                            )
                     )

generation_time_mean_suite <- 1 / variation_coefficient^2 / gamma_rate

output_05_suite <- rbindlist(lapply(results_05_suite,
                              function(r) {
                                ParametersSynthesis_CPEgeneration(r$output,
                                                                  burning = burning,
                                                                  min.support = min_support,
                                                                  distances = facilites_distances)
                              }))
rm(results_05_suite)

## Variation coefficient = 0.75 ##
results_075 <- readRDS("./results/Genouest/RealChainsReconstruction_vc075.rds")
output_075 <- rbindlist(lapply(results_075,
                              function(r) {
                                ParametersSynthesis_CPEgeneration(r$output,
                                                                  burning = burning,
                                                                  min.support = min_support,
                                                                  distances = facilites_distances)
                              }))
rm(results_075)

results_075_suite <- readRDS("./results/Genouest/RealChainsReconstruction_vc075_suite.rds")
output_075_suite <- rbindlist(lapply(results_075_suite,
                               function(r) {
                                 ParametersSynthesis_CPEgeneration(r$output,
                                                                   burning = burning,
                                                                   min.support = min_support,
                                                                   distances = facilites_distances)
                               }))
rm(results_075_suite)

## Variation coefficient = 1 ##
results_1 <- readRDS("./results/Genouest/RealChainsReconstruction_vc1.rds")
output_1 <- rbindlist(lapply(results_1,
                              function(r) {
                                ParametersSynthesis_CPEgeneration(r$output,
                                                                  burning = burning,
                                                                  min.support = min_support,
                                                                  distances = facilites_distances)
                              }))
rm(results_1)

results_1_suite <- readRDS("./results/Genouest/RealChainsReconstruction_vc1_suite.rds")
output_1_suite <- rbindlist(lapply(results_1_suite,
                             function(r) {
                               ParametersSynthesis_CPEgeneration(r$output,
                                                                 burning = burning,
                                                                 min.support = min_support,
                                                                 distances = facilites_distances)
                             }))
rm(results_1_suite)

output_05 <- cbind(generation_time_mean, output_05)
output_075 <- cbind(generation_time_mean, output_075)
output_1 <- cbind(generation_time_mean, output_1)
output_05_suite <- cbind(generation_time_mean_suite, output_05_suite)
output_075_suite <- cbind(generation_time_mean_suite, output_075_suite)
output_1_suite <- cbind(generation_time_mean_suite, output_1_suite)

#########################
#### Plot of results ####
#########################
# No links identified #
FigPlot.no_links <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = no_links_identified, color = "05"), size = 1.3, 
            data = output_05) +
  geom_point(aes(x = generation_time_mean, y = no_links_identified, color = "05"), size = 2.5, 
             data = output_05) + 
  geom_line(aes(x = generation_time_mean, y = no_links_identified, color = "075"), size = 1.3, 
            data = output_075) +
  geom_point(aes(x = generation_time_mean, y = no_links_identified, color = "075"), size = 2.5, 
             data = output_075) + 
  geom_line(aes(x = generation_time_mean, y = no_links_identified, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = no_links_identified, color = "1"), size = 2.5, 
             data = output_1) + 
  geom_line(aes(x = generation_time_mean_suite, y = no_links_identified, color = "05"), size = 1.3, 
            data = output_05_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = no_links_identified, color = "05"), size = 2.5, 
             data = output_05_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = no_links_identified, color = "075"), size = 1.3, 
            data = output_075_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = no_links_identified, color = "075"), size = 2.5, 
             data = output_075_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = no_links_identified, color = "1"), size = 1.3, 
            data = output_1_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = no_links_identified, color = "1"), size = 2.5, 
             data = output_1_suite) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# links identified") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("05"="#DAA520",
                               "1"="#228B22","075"="#FF4500"), 
                      labels=c("05"="0.5","1"="1","075"="0.75"))
FigPlot.no_links

# No community episodes #
FigPlot.no_community <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = no_community_episodes, color = "05"), size = 1.3, 
            data = output_05) +
  geom_point(aes(x = generation_time_mean, y = no_community_episodes, color = "05"), size = 2.5, 
             data = output_05) + 
  geom_line(aes(x = generation_time_mean, y = no_community_episodes, color = "075"), size = 1.3, 
            data = output_075) +
  geom_point(aes(x = generation_time_mean, y = no_community_episodes, color = "075"), size = 2.5, 
             data = output_075) + 
  geom_line(aes(x = generation_time_mean, y = no_community_episodes, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = no_community_episodes, color = "1"), size = 2.5, 
             data = output_1) + 
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# community links") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("05"="#DAA520",
                               "1"="#228B22","075"="#FF4500"), 
                      labels=c("05"="0.5","1"="1","075"="0.75"))
FigPlot.no_community

FigPlot.mean_distance <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified, color = "05"), size = 1.3, 
            data = output_05) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified, color = "05"), size = 2.5, 
             data = output_05) + 
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified, color = "075"), size = 1.3, 
            data = output_075) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified, color = "075"), size = 2.5, 
             data = output_075) + 
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified, color = "1"), size = 2.5, 
             data = output_1) + 
  geom_line(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "05"), size = 1.3, 
            data = output_05_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "05"), size = 2.5, 
             data = output_05_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "075"), size = 1.3, 
            data = output_075_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "075"), size = 2.5, 
             data = output_075_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "1"), size = 1.3, 
            data = output_1_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = mean_distance_identified, color = "1"), size = 2.5, 
             data = output_1_suite) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("Mean geographic distance") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("05"="#DAA520",
                               "1"="#228B22","075"="#FF4500"), 
                      labels=c("05"="0.5","1"="1","075"="0.75")) +
  ylim(25,75)

FigPlot.mean_distance


FigPlot.median_distance <- ggplot() +
  geom_line(aes(x = generation_time_mean, y = median_distance_identified, color = "05"), size = 1.3, 
            data = output_05) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified, color = "05"), size = 2.5, 
             data = output_05) + 
  geom_line(aes(x = generation_time_mean, y = median_distance_identified, color = "075"), size = 1.3, 
            data = output_075) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified, color = "075"), size = 2.5, 
             data = output_075) + 
  geom_line(aes(x = generation_time_mean, y = median_distance_identified, color = "1"), size = 1.3, 
            data = output_1) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified, color = "1"), size = 2.5, 
             data = output_1) + 
  geom_line(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "05"), size = 1.3, 
            data = output_05_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "05"), size = 2.5, 
             data = output_05_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "075"), size = 1.3, 
            data = output_075_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "075"), size = 2.5, 
             data = output_075_suite) + 
  geom_line(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "1"), size = 1.3, 
            data = output_1_suite) +
  geom_point(aes(x = generation_time_mean_suite, y = median_distance_identified, color = "1"), size = 2.5, 
             data = output_1_suite) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("Median geographic distance") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  scale_colour_manual(name="Variation coefficient", 
                      values=c("05"="#DAA520",
                               "1"="#228B22","075"="#FF4500"), 
                      labels=c("05"="0.5","1"="1","075"="0.75")) +
  ylim(0,25)

FigPlot.median_distance


######################################################################
######################################################################
#### Modification of mean of the generation interval distribution ####
######################################################################
######################################################################

###################################
#### Computation of parameters ####
###################################
## Standard deviation = 5 ##
results_part1 <- readRDS("./results/Genouest/RealChainsReconstruction_sd5.rds")

generation_time_mean <- unlist(lapply(results_part1, 
                                      function(r) {
                                        r$shape / r$rate
                                        }
                                      )
                               )

output_part1 <- rbindlist(lapply(results_part1,
                              function(r) {
                                ParametersSynthesis_CPEgeneration(r$output,
                                                                  burning = burning,
                                                                  min.support = min_support,
                                                                  distances = facilites_distances)
                              }))
rm(results_part1)
output_part1 <- cbind(generation_time_mean, output_part1)


results_part2 <- readRDS("./results/Genouest/RealChainsReconstruction_sd5_part2.rds")

generation_time_mean <- unlist(lapply(results_part2, 
                                      function(r) {
                                        r$shape / r$rate
                                      }
                                      )
                               )

output_part2 <- rbindlist(lapply(results_part2,
                                 function(r) {
                                   ParametersSynthesis_CPEgeneration(r$output,
                                                                     burning = burning,
                                                                     min.support = min_support,
                                                                     distances = facilites_distances)
                                 }))
rm(results_part2)
output_part2 <- cbind(generation_time_mean, output_part2)


results_part3 <- readRDS("./results/Genouest/RealChainsReconstruction_sd5_part3.rds")

generation_time_mean <- unlist(lapply(results_part3, 
                                      function(r) {
                                        r$shape / r$rate
                                      }
                                      )
                               )

output_part3 <- rbindlist(lapply(results_part3,
                                 function(r) {
                                   ParametersSynthesis_CPEgeneration(r$output,
                                                                     burning = burning,
                                                                     min.support = min_support,
                                                                     distances = facilites_distances)
                                 }))
rm(results_part3)
output_part3 <- cbind(generation_time_mean, output_part3)


## Merge of all parts of results ##
output <- rbind(output_part1,
                output_part2,
                output_part3)

#########################
#### Plot of results ####
#########################
# No links identified #
FigPlot.no_links <- ggplot(data = output) +
  geom_line(aes(x = generation_time_mean, y = no_links_identified), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = no_links_identified), size = 2.5) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# links identified") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
FigPlot.no_links

# No community episodes #
FigPlot.no_community <- ggplot(data = output) +
  geom_line(aes(x = generation_time_mean, y = no_community_episodes), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = no_community_episodes), size = 2.5) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("# community links") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
FigPlot.no_community

# Mean and median geographic distances between episodes #
FigPlot.distance <- ggplot(data = output) +
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified, color = "mean"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified, color = "mean"), size = 2.5) +
  geom_line(aes(x = generation_time_mean, y = median_distance_identified, color = "median"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified, color = "median"), size = 2.5) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("Geographic distance") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  ylim(5,50) +
  scale_colour_manual(name="Geographic distance", 
                      values=c("mean"="#0000CD",'median'="#8B0000"), 
                      labels=c("mean"="Mean","median"="Median"))
FigPlot.distance

# Using moving average #
output[, mean_distance_identified_lag1 := c(NA,mean_distance_identified[1:(.N-1)])]
output[, mean_distance_identified_lag := c(mean_distance_identified[2:.N],NA)]
output[, mean_distance_identified_ma := rowSums(.SD)/3,
       .SDcols = c("mean_distance_identified",
                   "mean_distance_identified_lag1",
                   "mean_distance_identified_lag")]

output[, median_distance_identified_lag1 := c(NA,median_distance_identified[1:(.N-1)])]
output[, median_distance_identified_lag := c(median_distance_identified[2:.N],NA)]
output[, median_distance_identified_ma := rowSums(.SD)/3,
       .SDcols = c("median_distance_identified",
                   "median_distance_identified_lag1",
                   "median_distance_identified_lag")]

FigPlot.distance <- ggplot(data = output) +
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified_ma, color = "mean"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified_ma, color = "mean"), size = 2.5) +
  geom_line(aes(x = generation_time_mean, y = median_distance_identified_ma, color = "median"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified_ma, color = "median"), size = 2.5) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("Geographic distance") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  ylim(5,50) +
  scale_colour_manual(name="Geographic distance", 
                      values=c("mean"="#0000CD",'median'="#8B0000"), 
                      labels=c("mean"="Mean","median"="Median"))
FigPlot.distance

FigPlot.distance <- ggplot(data = output) +
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified, color = "mean_real", linetype = "real"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = mean_distance_identified, color = "mean_real"), size = 2.5) +
  geom_line(aes(x = generation_time_mean, y = median_distance_identified, color = "median_real", linetype = "real"), size = 1.3) +
  geom_point(aes(x = generation_time_mean, y = median_distance_identified, color = "median_real"), size = 2.5) +
  geom_line(aes(x = generation_time_mean, y = mean_distance_identified_ma, color = "mean_ma", linetype = "ma"), size = 1.3) +
  geom_line(aes(x = generation_time_mean, y = median_distance_identified_ma, color = "median_ma", linetype = "ma"), size = 1.3) +
  theme_minimal() +
  xlab("Mean of CPE generation interval") + 
  ylab("Geographic distance") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  ylim(5,50) +
  scale_colour_manual(name="Geographic distance", 
                      values=c("mean_real"="#00BFFF",'median_real'="#FF6347",
                               "mean_ma"="#0000CD",'median_ma'="#8B0000"), 
                      labels=c("mean_real"="Mean","median_real"="Median")) +
  scale_linetype_manual(name="Geographic distance (km)", 
                        values=c("real"=1,'ma'=3), 
                        labels=c("real"="Real values","ma"="Moving average"))
FigPlot.distance

plot_grid(FigPlot.no_links,
        FigPlot.distance, nrow = 2)
