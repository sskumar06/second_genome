setwd("C:\\Users\\sk\\Documents\\Data_Science\\Second_Genome\\")

halo_tolerant <- read.csv("halotolerant_input.out", stringsAsFactors = F)

halo_sensitive <- read.csv("halo_sensitive_input.out", stringsAsFactors = F)

fresh <- read.csv("fresh_input.out", stringsAsFactors = F)

# To remove unwanted rows
# grep("^tr|K8CLK0|K8CLK0_CROSK Corresponds", fresh$Feature)
# fresh[165, 1]
# fresh[164, 1]
# fresh[159, 1]
# fresh[158, 1]

fresh <- fresh[-c(158, 159, 164, 165), ]
fresh <- na.omit(fresh)
non_halo_tolerant <- rbind(fresh, halo_sensitive)

halo_tolerant$y <- 1
non_halo_tolerant$y <- 0

View(halo_tolerant[duplicated(halo_tolerant),])
View(non_halo_tolerant[duplicated(non_halo_tolerant),])


