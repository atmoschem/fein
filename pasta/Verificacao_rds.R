library(DataExplorer)
list.files("inventory/", pattern = ".rds", full.names = T) -> f

# Refinarias ####
dat <- readRDS(f[6])
## Refinarias: 1.A.1.b (6) 1a1b (10) 1.B.2.a.iv (8) 1B2aiv(12)
plot_str(dat)
#1.A.1.b (6) - 312176 obs. of 17 variables
#1a1b (10) - 102816 obs. of 10 variables
#1.B.2.a.iv (8) - 329953 obs. of 16 variables
#1B2aiv(12) - 331687 obs. of 16 variables
introduce(dat)
plot_intro(dat)
plot_missing(dat)
profile_missing(dat)
plot_histogram(dat)
plot_bar(dat)
plot_qq(dat)
plot_correlation(dat)
plot_prcomp(dat)

