# Part 2: Bootstrapping to find the Limits and Changepoints:
#-----------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("customFunctions.R")
angles = readRDS("../Outputs/angles.rds")

first_n_angles = lapply(angles, head, n=400L)
BootstrapSamples = lapply(first_n_angles, boot.sampling, B=10000)

BootstrapQuantiles = vector(mode = "list", length = 15L)
thresholds = vector(mode = "numeric", length = 15L)
signal_locations = vector(mode = "list", length = 15L)


for (j in 1:15) {
  BootstrapQuantiles[[j]] = apply(BootstrapSamples[[j]], MARGIN=2, FUN=quantile, prob=0.999)
  thresholds[j] = quantile(BootstrapQuantiles[[j]], probs = 0.95)
  
  n = length(first_n_angles[[j]]) + 1
  N = length(angles[[j]])
  
  signal_locations[[j]] = which(angles[[j]][n:N] > thresholds[j])
  
  png(file= paste0("../Figures/ChangePoints_Subject",j,".png"), width= 6.5, height = 4, units="in", res=600)
  plot(angles[[j]][n:N], main= paste("Signals based on Angle Changes for Subject",j), cex.axis=1.5, cex.main= 1.5, cex.lab= 1.5,
       ylab = "Angles in Degrees", xlab= paste0("Observation No. (Starting from ", n, ")"),
       pch=16, cex=0.75)
  abline(h=thresholds[j], col = "red", lwd = 2)
  points(x=(signal_locations[[j]]), y=angles[[j]][signal_locations[[j]]+n-1], col = "red", pch=15, cex=0.75)
  dev.off()
}

rm(list = setdiff(ls(), c('BootstrapQuantiles', 'signal_locations', 'thresholds')))
saveRDS(BootstrapQuantiles, "../Outputs/bootstrapQuantiles.rds")
saveRDS(signal_locations, "../Outputs/signalLocations.rds")
saveRDS(thresholds, "../Outputs/thresholds.rds")