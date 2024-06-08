# BRPs, projections: Day 4 Session 2:

# check spr based brps
# changing initial values

# checking selectivity parameters
# 	check selAA[[]]



# Annual reference points:

#Taken from ex3_projections
# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")

library("wham", lib.loc = "c:/work/wham/old_packages/lab")
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex2_SNEMAYT.dat"))
env.dat <- read.csv(file.path(path_to_examples, "CPI.csv"), header=T)

# specify model: AR1 CPI, limiting effect on Bev-Holt
env <- list(
  label = "CPI",
  mean = as.matrix(env.dat$CPI), # CPI observations
  logsigma = as.matrix(log(env.dat$CPI_sigma)), # CPI standard error is given/fixed as data
  year = env.dat$Year,
  use_obs = matrix(1, ncol=1, nrow=dim(env.dat)[1]), # use all obs (=1)
  process_model = "ar1", # fit CPI as AR1 process
  recruitment_how = matrix("limiting-lag-1-linear")) # limiting (carrying capacity), CPI in year t affects recruitment in year t+1

basic_info <- list(
  XSPR_R_avg_yrs = tail(1:asap3[[1]]$dat$n_years, 10),
  percentSPR = 50,
  XSPR_input_average_years = tail(1:asap3[[1]]$dat$n_years, 10),
  XSPR_R_opt = 1 # use annual R estimates for annual SSB_XSPR
)
input_1 <- prepare_wham_input(asap3, recruit_model = 3,
  ecov = env,
  NAA_re = list(sigma="rec+1", cor="iid"),
  age_comp = "logistic-normal-pool0", basic_info = basic_info) # logistic normal pool 0 obs
input_1$par$logit_selpars[1:4,7:8] <- 0 # original code started selpars at 0 (last 2 rows are fixed)

input_1$data$percentSPR
input_1$data$avg_years_ind
input_1$data$XSPR_R_avg_yrs
tail(1:length(input_1$years),10)-1 #C++ starts at 0
input_1$data$XSPR_R_opt


mod_1 <- fit_wham(input_1, do.sdrep = FALSE, do.osa = FALSE, do.retro = FALSE) 
temp <- retro(mod_1, use.mle = TRUE)
temp2 <- retro(mod_1, use.mle = FALSE)



mod_1$rep$R_XSPR

# temp <- input_1
# temp$par <- mod_1$parList
# mod_1 <- fit_wham(temp, do.fit = FALSE)
saveRDS(mod_1, file.path("temp", "day_4_2_mod_1.RDS"))

cbind(mod_1$rep$NAA[1,1,,1], mod_1$rep$R_XSPR)
cbind(exp(mod_1$rep$log_SSB_FXSPR[,1]), mod_1$rep$NAA[1,1,,1]*exp(mod_1$rep$log_SPR_FXSPR[,1]))

exp(mod_1$rep$log_SSB_FXSPR_static[1])
exp(mod_1$rep$log_SSB_FXSPR_static[1])/exp(mod_1$rep$log_SPR_FXSPR_static[1])
mean(mod_1$rep$R_XSPR[tail(1:input_1$data$n_years_model,10),1])
mod_1$rep$R_XSPR[input_1$data$n_years_model,1]

mod_1$rep$R_XSPR[input_1$data$n_years_model,1]*exp(mod_1$rep$log_SPR_FXSPR_static[1])

input_2 <- input_1
input_2$par <- mod_1$parList
input_2$data$XSPR_R_opt = 2 # use average R estimates for annual SSB_XSPR (the default value)

mod_2 <- fit_wham(input_2, do.fit = FALSE)
saveRDS(mod_2, file.path("temp", "day_4_2_mod_2.RDS"))

mod_2$rep$R_XSPR
mean(mod_2$rep$NAA[1,1,input_1$data$XSPR_R_avg_yrs+1,1])

cbind(exp(mod_2$rep$log_SSB_FXSPR[,1]), mean(mod_2$rep$NAA[1,1,tail(1:length(temp$years), 10),1])*exp(mod_2$rep$log_SPR_FXSPR[,1]))

input_3 <- input_2
input_3$data$XSPR_R_opt = 3 # use annual expected R for annual SSB_XSPR
mod_3 <- fit_wham(input_3, do.fit = FALSE)
saveRDS(mod_3, file.path("temp", "day_4_2_mod_3.RDS"))

cbind(mod_3$rep$R_XSPR, mod_3$rep$pred_NAA[1,1,,1])

input_4 <- input_2
input_4$data$XSPR_R_opt = 4 # use average expected R estimates for annual SSB_XSPR
mod_4 <- fit_wham(input_4, do.fit = FALSE)
saveRDS(mod_4, file.path("temp", "day_4_2_mod_4.RDS"))

cbind(mod_4$rep$R_XSPR, mod_4$rep$pred_NAA[1,1,,1])

mod_4$rep$R_XSPR
mean(mod_4$rep$pred_NAA[1,1,input_4$data$XSPR_R_avg_yrs+1,1])


#static reference points:

stat_nms <- grep("static", names(mod_4$rep), value = TRUE)

#need to be careful for XSPR_R_opt = 1 or 3
exp(mod_1$rep$log_SSB_FXSPR_static[1])
#not the same because not specified to use average R
exp(mod_1$rep$log_SPR_FXSPR_static[1]) * mean(mod_1$rep$R_XSPR[input_1$data$XSPR_R_avg_yrs+1])
#uses terminal year R
exp(mod_1$rep$log_SPR_FXSPR_static[1]) * tail(mod_1$rep$R_XSPR[,1],1)

exp(mod_3$rep$log_SSB_FXSPR_static[1])
#not the same because not specified to use average R
exp(mod_3$rep$log_SPR_FXSPR_static[1]) * mean(mod_3$rep$R_XSPR[input_3$data$XSPR_R_avg_yrs+1])
#uses terminal year R
exp(mod_3$rep$log_SPR_FXSPR_static[1]) * tail(mod_3$rep$R_XSPR[,1],1)

exp(mod_2$rep$log_SSB_FXSPR_static[1])
exp(mod_2$rep$log_SPR_FXSPR_static[1]) * mean(mod_2$rep$R_XSPR[input_2$data$XSPR_R_avg_yrs+1])
#all R_XSPR are the same
exp(mod_2$rep$log_SPR_FXSPR_static[1]) * tail(mod_2$rep$R_XSPR[,1],1)

exp(mod_4$rep$log_SSB_FXSPR_static[1])
exp(mod_4$rep$log_SPR_FXSPR_static[1]) * mean(mod_4$rep$R_XSPR[input_4$data$XSPR_R_avg_yrs+1])
#all R_XSPR are the same
exp(mod_4$rep$log_SPR_FXSPR_static[1]) * tail(mod_4$rep$R_XSPR[,1],1)


# Projections

mod_2 <- fit_wham(input_2, do.retro = FALSE, do.osa = FALSE)
mod_proj <- list()
proj_opts <- list()

# default settings: 3 years, use last F, continue ecov
proj_opts[[1]] <-list(n.yrs=3, use.last.F=TRUE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)
# 5 years, use last F, average ecov 1992-1996
proj_opts[[2]] <- list(n.yrs=5, use.last.F=TRUE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=FALSE, use.last.ecov=FALSE, avg.ecov.yrs=1992:1996, proj.ecov=NULL)

# 5 years, use last F, use last ecov
proj_opts[[3]] <- list(n.yrs=5, use.last.F=TRUE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=FALSE, use.last.ecov=TRUE, avg.ecov.yrs=NULL, proj.ecov=NULL)

# 5 years, use last F, specify high CPI ~ 0.5
proj_opts[[4]] <- list(n.yrs=5, use.last.F=TRUE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=FALSE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=matrix(c(0.5,0.7,0.4,0.5,0.55),ncol=1))

# 5 years, use last F, specify low CPI ~ -1.5
proj_opts[[5]] <- list(n.yrs=5, use.last.F=TRUE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=FALSE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=matrix(c(-1.6,-1.3,-1,-1.2,-1.25),ncol=1))

# specify catch, 5 years
proj_opts[[6]] <- list(n.yrs=5, use.last.F=FALSE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=c(10, 2000, 1000, 3000, 20), avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

# specify F, 5 years
proj_opts[[7]] <- list(n.yrs=5, use.last.F=FALSE, use.avg.F=FALSE,
              use.FXSPR=FALSE, proj.F=c(0.001, 1, 0.5, .1, .2), proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

# use FXSPR (avg.yrs defaults to last 5 years, 2007-2011), 5 years
proj_opts[[8]] <- list(n.yrs=5, use.last.F=FALSE, use.avg.F=FALSE,
              use.FXSPR=TRUE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

# specify catch first year then FXSPR last 4 years
proj_opts[[9]] <- list(n.yrs=5, proj_F_opt =c(5,3,3,3,3), proj_Fcatch=c(300,10,10,10,10),
              cont.ecov=TRUE, use.last.ecov=FALSE)

# use avg F 1992-1996, 10 years
proj_opts[[10]] <- list(n.yrs=10, use.last.F=FALSE, use.avg.F=TRUE,
              use.FXSPR=FALSE, proj.F=NULL, proj.catch=NULL, avg.yrs=1992:1996,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

# use FMSY (avg.yrs defaults to last 5 years, 2007-2011), 5 years
proj_opts[[11]] <- list(n.yrs=5, use.last.F=FALSE, use.avg.F=FALSE,
              use.FMSY=TRUE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

tmp.dir <- tempdir(check=TRUE)
for(i in 1:length(proj_opts)){
  mod_proj[[i]] <- project_wham(mod_2, proj.opts=proj_opts[[i]])
  plot_wham_output(mod_proj[[i]], dir.main=file.path(tmp.dir,paste0("proj_",i)))
}
  mod_proj[[i]] <- project_wham(mod_2, proj.opts=proj_opts[[i]], do.sdrep = FALSE)
saveRDS(mod_proj, file.path("temp", "day_4_2_mod_proj.RDS"))

for(i in c(1,9,10,11)){
  file.copy(file.path(tmp.dir, paste0("proj_",i), "plots_png", "results", "SSB_F_trend.png"), 
    file.path("slides", paste0("day_4_2_proj_", i, "_SSB_F.png")))
}

i = 9
  file.copy(file.path(tmp.dir, paste0("proj_",i), "plots_png", "ref_points", "Kobe_status.png"), 
    file.path("slides", paste0("day_4_2_proj_", i, "_kobe_F50.png")))
  file.copy(file.path(tmp.dir, paste0("proj_",i), "plots_png", "ref_points", "Kobe_msy_status.png"), 
    file.path("slides", paste0("day_4_2_proj_", i, "_kobe_msy.png")))
  file.copy(file.path(tmp.dir, paste0("proj_",i), "plots_png", "ref_points", "FSPR_relative.png"), 
    file.path("slides", paste0("day_4_2_proj_", i, "_annual_status.png")))


nll_proj <-  sapply(mod_proj, function(x) x$opt$obj)
mod_2$opt$obj
round(nll_proj - mod_2$opt$obj, 6) # difference between original and projected models' NLL

