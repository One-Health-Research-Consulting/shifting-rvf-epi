#Uganda seroprev estimates
#Data is a subset of data provided in supplementary information for
#Tumusiime D, Isingoma E, Tashoroora OB, et al. Mapping the risk of Rift Valley
    #fever in Uganda using national seroprevalence data from cattle, sheep and goats.
    #PLOS Neglected Tropical Diseases 2023; 17(5): e0010482.

#Libraries
library(dplyr)

#Data
dat <- read.csv("data/Uganda_seroprev_dat.csv")

#Estimate seroprevalence with 95% CI
tab <- data.frame(matrix(nrow = 0, ncol = 7))

for(i in 1:nrow(dat)){
seroprev <- prop.test(x = dat$NO_POSITIVE[i], n = dat$ALL_LIVESTOCK[i], correct=FALSE, conf.level = .95)

ci_hi <- seroprev$conf.int[2]
ci_lo <- seroprev$conf.int[1]

tab[i,1] <- dat$District[i]
tab[i,2] <- dat$Category[i]
tab[i,3] <- dat$Cattle.corridor[i]
tab[i,4] <- dat$ALL_LIVESTOCK[i]
tab[i,5] <- round(seroprev$estimate,3)*100
tab[i,6] <- round(ci_hi,3)*100
tab[i,7] <- round(ci_lo,3)*100

}

colnames(tab) <- c("District", "Category", "Cattle_corridor", "n", "Mean_Seroprevalence", "CI_high", "CI_low")

tab_fin <- tab%>%
  mutate("CI95" = paste(CI_low, CI_high, sep = "-"))%>%
  dplyr::select(-CI_high, -CI_low)

write.csv(tab_fin, "output/Seroprevalence estimates for select sites in Uganda.csv", row.names = FALSE)


#### Quick estimate of confidence interval of KZN IgM positives for Reviewer 3's response.

kzn_igM <- prop.test(x = 5, n = 114, correct=FALSE, conf.level = .95)
