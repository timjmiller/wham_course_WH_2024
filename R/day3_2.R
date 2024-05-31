# Day 3 Session 2: environmental covariates

# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
wham.dir <- find.package("wham")
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex1_SNEMAYT.dat"))
env.dat <- read.csv(file.path(path_to_examples,"GSI.csv"), header=T)
tmp.dir <- tempdir(check=TRUE)


NAA_re <- list(
	sigma = "rec+1", 
	cor = "iid", 
	recruit_model = 3)# Bev Holt recruitment

ecov <- list(
  label = "GSI",
  mean = as.matrix(env.dat$GSI),
  logsigma = 'est_1', # estimate obs sigma, 1 value shared across years
  year = env.dat$year,
  use_obs = matrix(1, ncol=1, nrow=dim(env.dat)[1]), # use all obs (=1)
  process_model = 'ar1', # "rw" or "ar1"
  recruitment_how = matrix("none")) # n_Ecov x n_stocks

selectivity=list(
	model=rep("age-specific",3), re=c("none","none","none"), 
  initial_pars=list(c(0.1,0.5,0.5,1,1,1),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,0.5,1,1,1,1)), 
  fix_pars=list(4:6,4,3:6))

selectivity <- list(model = rep("logistic", 3))
selectivity$initial_pars <- list(
  c(3, 0.2), 
  c(3, 0.2), 
  c(3, 0.2)) 

input_0 <- prepare_wham_input(asap3, recruit_model = 3, 
  NAA_re = NAA_re, 
  selectivity = selectivity,
  ecov=ecov,
  age_comp = "logistic-normal-miss0") # logistic normal, treat 0 obs as missing

fit_0 <- fit_wham(input_0, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_0 <- do_sdreport(fit_0)
fit_0$peels <- retro(fit_0)
fit_0 <- make_osa_residuals(fit_0)
saveRDS(fit_0, file.path("temp", "day_3_2_fit_0.RDS"))

ecov_1 <- ecov
ecov_1$recruitment_how <- matrix("limiting-lag-1-linear")

input_1 <- set_ecov(input_0, ecov_1)
fit_1 <- fit_wham(input_1, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_1 <- do_sdreport(fit_1)
fit_1$peels <- retro(fit_1)
fit_1 <- make_osa_residuals(fit_1)
saveRDS(fit_1, file.path("temp", "day_3_2_fit_1.RDS"))

# M

ecov_2 <- list(
  label = "GSI",
  mean = as.matrix(env.dat$GSI),
  logsigma = 'est_1', # estimate obs sigma, 1 value shared across years
  year = env.dat$year,
  use_obs = matrix(1, ncol=1, nrow=dim(env.dat)[1]), # use all obs (=1)
  process_model = "ar1", # "rw" or "ar1"
  M_how = array("none",c(1,1,6,1))) # n_Ecov x n_stocks x n_ages x n_regions

M <- list(
  initial_means = array(0.2, c(1,1,6)), 
  re_model = matrix("iid_ay", 1,1),
  means_map = array(NA, c(1,1,6))
)

input_2 <- set_ecov(input_0, ecov_2)
input_2 <- set_M(input_2, M)
fit_2 <- fit_wham(input_2, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_2 <- do_sdreport(fit_2)
fit_2$peels <- retro(fit_2)
fit_2 <- make_osa_residuals(fit_2)
saveRDS(fit_2, file.path("temp", "day_3_2_fit_2.RDS"))

ecov_3 <- ecov_2
ecov_3$M_how[] <- "lag-0-linear"
input_3 <- set_ecov(input_2, ecov_3)
fit_3 <- fit_wham(input_3, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_3 <- do_sdreport(fit_3)
fit_3$peels <- retro(fit_3)
fit_3 <- make_osa_residuals(fit_3)
saveRDS(fit_3, file.path("temp", "day_3_2_fit_3.RDS"))



