# Day 2 Session 2: NAA, M, q

# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
wham.dir <- find.package("wham")
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex1_SNEMAYT.dat"))
tmp.dir <- tempdir(check=TRUE)


#NAA:

# specify models:
#    Model NAA_cor NAA_sigma
# 2     m2     iid       rec
# 3     m3   ar1_y       rec
# 4     m4     iid     rec+1
# 5     m5   ar1_a     rec+1
# 6     m6   ar1_y     rec+1
# 7     m7   2dar1     rec+1

selectivity <- list(
	model=rep("age-specific",3),
  initial_pars=list(c(0.5,0.5,0.5,1,1,1),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,0.5,1,1,1,1)), 
  fix_pars=list(4:6,4,3:6))

selectivity <- list(
	model=rep("logistic",3),
  initial_pars= list(c(3, 0.2), c(3, 0.2), c(3, 0.2)))

#initial NAA

input <- prepare_wham_input(asap3)
input <- set_selectivity(input, selectivity)
fit_0 <- fit_wham(input, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_0, dir.main = tmp.dir)

NAA_list <- list(N1_model = "equilibrium")
input_1 <- set_NAA(input, NAA_list)
fit_1 <- fit_wham(input_1, do.retro=FALSE, do.osa=FALSE)

NAA_list <- list(N1_model = "iid-re")
input_2 <- set_NAA(input, NAA_list)
fit_2 <- fit_wham(input_2, do.retro=FALSE, do.osa=FALSE)

NAA_list <- list(N1_model = "ar1-re")
input_3 <- set_NAA(input, NAA_list)
fit_3 <- fit_wham(input_3, do.retro=FALSE, do.osa=FALSE)

N1 <- sapply(0:3, function(x) get(paste0("fit_", x))$rep$NAA[1,1,1,])
SSB <- sapply(0:3, function(x) get(paste0("fit_", x))$rep$SSB)
# saveRDS(N1, file = file.path("temp", "day_2_2_init_NAA.RDS"))
# saveRDS(SSB, file = file.path("temp", "day_2_2_init_NAA_SSB.RDS"))

# matplot(N1, type = 'o', lty = 1, ylab = "Initial Abundance", xlab = "Age")
# matplot(SSB, type = 'l', lty = 1, ylab = "SSB", xlab = "Year", lwd = 2)

# RE model structure
NAA_list <- list(
	sigma = "rec",
	cor = "iid")

input_4 <- set_NAA(input, NAA_list)
nofit_4 <- fit_wham(input_4, do.fit=FALSE)

fit_4 <- fit_wham(input_4, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_4, file.path("temp", "day_2_2_fit_4.RDS"))
plot_wham_output(fit_4, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_4_NAA_devs.png"))


NAA_list <- list(
	sigma = "rec",
	cor = "ar1_y")
input_5 <- set_NAA(input, NAA_list)
fit_5 <- fit_wham(input_5, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_5, file.path("temp", "day_2_2_fit_5.RDS"))
plot_wham_output(fit_5, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_5_NAA_devs.png"))

NAA_list <- list(
	sigma = "rec+1",
	cor = "iid")
input_6 <- set_NAA(input, NAA_list)
fit_6 <- fit_wham(input_6, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_6, file.path("temp", "day_2_2_fit_6.RDS"))
plot_wham_output(fit_6, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_6_NAA_devs.png"))

NAA_list <- list(
	sigma = "rec+1",
	cor = "ar1_a")
input_7 <- set_NAA(input, NAA_list)
fit_7 <- fit_wham(input_7, do.retro=FALSE, do.osa=FALSE)
#doesn't converge well due to logistic selectivity parameters
# saveRDS(fit_7, file.path("temp", "day_2_2_fit_7.RDS"))
plot_wham_output(fit_7, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_7_NAA_devs.png"))

NAA_list <- list(
	sigma = "rec+1",
	cor = "ar1_y")
input_8 <- set_NAA(input, NAA_list)
fit_8 <- fit_wham(input_8, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_8, file.path("temp", "day_2_2_fit_8.RDS"))
plot_wham_output(fit_8, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_8_NAA_devs.png"))

NAA_list <- list(
	sigma = "rec+1",
	cor = "2dar1")
input_9 <- set_NAA(input, NAA_list)
fit_9 <- fit_wham(input_9, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_9, file.path("temp", "day_2_2_fit_9.RDS"))
plot_wham_output(fit_9, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_9_NAA_devs.png"))

#recruitment models

NAA_list <- list(recruit_model = 3,
	sigma = "rec+1",
	cor = "ar1_y")
input_10 <- set_NAA(input, NAA_list)
fit_10 <- fit_wham(input_10, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_10, file.path("temp", "day_2_2_fit_10.RDS"))
plot_wham_output(fit_10, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SSB_Rec_stock_1_fit.png"), file.path(getwd(),"slides", "day_2_2_fit_10_BH_SR.png"))

NAA_list <- list(recruit_model = 4,
	sigma = "rec+1",
	cor = "ar1_y")
input_11 <- set_NAA(input, NAA_list)
fit_11 <- fit_wham(input_11, do.retro=FALSE, do.osa=FALSE)
# saveRDS(fit_11, file.path("temp", "day_2_2_fit_11.RDS"))
plot_wham_output(fit_11, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SSB_Rec_stock_1_fit.png"), file.path(getwd(),"slides", "day_2_2_fit_11_Ricker_SR.png"))


#M:

# specify models:
# Model M_model       M_re  
# m1    ---           --- 
# m2    constant      ---   
# m3    age-specific  ---   
# m7    waa           ---   
# m4    ---           ar1_a 
# m5    ---           ar1_y 
# m6    ---           2dar1 


M <- list(
	mean_model = "estimate-M",
	means_map = array(1, dim = c(1,1,6)))
input_12 <- set_M(input, M)
fit_12 <- fit_wham(input_12, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_12, file.path("temp", "day_2_2_fit_12.RDS"))
plot_wham_output(fit_12, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_12_M.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "M_at_age_stock_1_.png"), file.path(getwd(),"slides", "day_2_2_fit_12_M_const.png"))

M <- list(
	mean_model = "estimate-M",
	means_map = array(c(1,1,1,2,2,2), dim = c(1,1,6)))
input_13 <- set_M(input, M)
fit_13 <- fit_wham(input_13, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_13, file.path("temp", "day_2_2_fit_13.RDS"))
plot_wham_output(fit_13, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_13_M.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "M_at_age_stock_1_.png"), file.path(getwd(),"slides", "day_2_2_fit_13_M_const.png"))

M <- list(
	mean_model = "weight-at-age",
	b_prior = TRUE
	)
input_14 <- set_M(input, M)
input_14$data$waa[4,44,1] <- 0.1 #a 0 at age 1 for weight.
fit_14 <- fit_wham(input_14, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_14, file.path("temp", "day_2_2_fit_14.RDS"))
plot_wham_output(fit_14, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_14_M.png"))

M <- list(
	initial_means = array(0.2, dim = c(1,1,6)), 
	means_map = array(NA, dim = c(1,1,6)),
	re_model = matrix("ar1_a",1,1)
)
input_15 <- set_M(input, M)
fit_15 <- fit_wham(input_15, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_15, file.path("temp", "day_2_2_fit_15.RDS"))
#some bad selectivity pars
plot_wham_output(fit_15, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_15_M.png"))

M <- list(
	initial_means = array(0.2, dim = c(1,1,6)), 
	means_map = array(NA, dim = c(1,1,6)),
	re_model = matrix("ar1_y",1,1)
)
input_16 <- set_M(input, M)
fit_16 <- fit_wham(input_16, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_16, file.path("temp", "day_2_2_fit_16.RDS"))
#scale is bad for this fit
plot_wham_output(fit_16, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_16_M.png"))

M <- list(
	initial_means = array(0.2, dim = c(1,1,6)), 
	means_map = array(NA, dim = c(1,1,6)),
	re_model = matrix("iid_ay",1,1)
)
input_17 <- set_M(input, M)
fit_17 <- fit_wham(input_17, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_17, file.path("temp", "day_2_2_fit_17.RDS"))
plot_wham_output(fit_17, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_2_2_fit_17_M.png"))

#q:

# specify models:
# Model q_model       q_re  
# m1    constant      ---
# m2    constant      iid
# m3    constant      ar1

catchability <- list(
	re = c("iid", "none")
)
input_18 <- set_q(input, catchability)
fit_18 <- fit_wham(input_18, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_18, file.path("temp", "day_2_2_fit_18.RDS"))
plot_wham_output(fit_18, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "q_time_series.png"), file.path(getwd(),"slides", "day_2_2_fit_18_q.png"))

catchability <- list(
	re = c("ar1", "ar1")
)
input_19 <- set_q(input, catchability)
fit_19 <- fit_wham(input_19, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_19, file.path("temp", "day_2_2_fit_19.RDS"))
plot_wham_output(fit_19, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "q_time_series.png"), file.path(getwd(),"slides", "day_2_2_fit_19_q.png"))

catchability <- list(
	re = c("ar1", "ar1"),
	q_upper = rep(1,2)
)

input_20 <- set_q(input, catchability)
fit_20 <- fit_wham(input_20, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_20, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "q_time_series.png"), file.path(getwd(),"slides", "day_2_2_fit_20_q.png"))
