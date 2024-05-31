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

# selectivity=list(
# 	model=rep("age-specific",3), re=c("none","none","none"), 
#   initial_pars=list(c(0.1,0.5,0.5,1,1,1),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,0.5,1,1,1,1)), 
#   fix_pars=list(4:6,4,3:6))

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
# saveRDS(fit_0, file.path("temp", "day_3_2_fit_0.RDS"))
plot_wham_output(fit_0, dir.main = tmp.dir)
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_0_GSI_diagnostic.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_0_GSI_osa.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_0_NAA_devs.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_0_MAA.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_0_GSI.png"))
file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_msy_status.png"), file.path(getwd(),"slides", "day_3_2_fit_0_Kobe_msy.png"))
file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_status.png"), file.path(getwd(),"slides", "day_3_2_fit_0_Kobe_F40.png"))

ecov_1 <- ecov
ecov_1$recruitment_how <- matrix("limiting-lag-1-linear")

input_1 <- set_ecov(input_0, ecov_1)
fit_1 <- fit_wham(input_1, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_1 <- do_sdreport(fit_1)
fit_1$peels <- retro(fit_1)
fit_1 <- make_osa_residuals(fit_1)
plot_wham_output(fit_1, dir.main = tmp.dir)
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_1_GSI_diagnostic.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_1_GSI_osa.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_1_NAA_devs.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_1_MAA.png"))
file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_1_GSI.png"))
file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_msy_status.png"), file.path(getwd(),"slides", "day_3_2_fit_1_Kobe_msy.png"))
file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_status.png"), file.path(getwd(),"slides", "day_3_2_fit_1_Kobe_F40.png"))

mypalette = function(n){
  palette.fn <- colorRampPalette(c("dodgerblue","green","red"), space = "Lab")
  palette.fn(n)
}

# saveRDS(fit_1, file.path("temp", "day_3_2_fit_1.RDS"))

SSB <- seq(1,max(fit_1$rep$SSB),100)
ab <- exp(cbind(fit_1$rep$log_SR_a,fit_1$rep$log_SR_b))
Ecov <- fit_1$rep$Ecov_out_R[1,,]
ord <- order(Ecov)
pal <- mypalette(length(ord))
years <- 1:length(ord)
R <- sapply(1:NROW(ab), function(x) ab[x,1]*SSB/(1 + ab[x,2]*SSB))
# Rmax <- ab[,1]*max(SSB)/(1 + ab[,2]*max(SSB))
# matplot(SSB, R, type = 'l', col = pal, lty = 1, lwd = 2)

df <- cbind.data.frame(year = rep(years,each = length(SSB)), SSB = SSB, R = c(R), GSI = rep(Ecov, each = length(SSB)), group = 1)
df <- rbind.data.frame(df, cbind.data.frame(year = years, SSB = fit_1$rep$SSB, R = fit_1$rep$NAA[1,1,,1], GSI = Ecov, group = 2))
plt <- ggplot(df, aes(SSB, R, colour = GSI, group = factor(year))) +
  geom_line(data = subset(df, group == 1), size = 1.2) +
  geom_point(data = subset(df, group == 2)) + 
  scale_colour_gradientn(colours = pal)
ggsave(file.path("slides", "day_3_2_fit_1_SR.png"), plt)


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
plot_wham_output(fit_2, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_2_GSI_diagnostic.png"))
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_2_GSI_osa.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_2_NAA_devs.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_2_MAA.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_2_GSI.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_msy_status.png"), file.path(getwd(),"slides", "day_3_2_fit_2_Kobe_msy.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_status.png"), file.path(getwd(),"slides", "day_3_2_fit_2_Kobe_F40.png"))


ecov_3 <- ecov_2
ecov_3$M_how[] <- "lag-0-linear"

input_3 <- set_ecov(input_2, ecov_3)
fit_3 <- fit_wham(input_3, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_3 <- do_sdreport(fit_3)
fit_3$peels <- retro(fit_3)
fit_3 <- make_osa_residuals(fit_3)
saveRDS(fit_3, file.path("temp", "day_3_2_fit_3.RDS"))
plot_wham_output(fit_3, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_3_GSI_diagnostic.png"))
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_3_GSI_osa.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_3_NAA_devs.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_3_MAA.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_3_GSI.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_msy_status.png"), file.path(getwd(),"slides", "day_3_2_fit_3_Kobe_msy.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_status.png"), file.path(getwd(),"slides", "day_3_2_fit_3_Kobe_F40.png"))

Ecov <- fit_3$rep$Ecov_out_M[1,1,1,,]
M_out <- fit_3$rep$MAA[1,1,,1]
ord <- order(Ecov)

Ecov_plt <- seq(min(Ecov),max(Ecov),0.01)
M <- exp(fit_3$parList$Mpars[1,1,1] + fit_3$parList$Ecov_beta_M[1,1,1,1,1] * Ecov_plt)
pal <- mypalette(length(ord))

# plot(Ecov_plt, M, type = 'l', lwd = 2, ylim = c(0, max(M_out)))
# points(Ecov[ord], M_out[ord], col = pal, pch = 19)

df <- cbind.data.frame(M=M, GSI = Ecov_plt, group = 1)
df <- rbind(df, cbind.data.frame(M = M_out, GSI = Ecov, group = 2))
plt <- ggplot(df, aes(GSI, M, colour = GSI)) +
  geom_line(data = subset(df, group == 1), size = 1.2) +
  geom_point(data = subset(df, group == 2)) + 
  scale_colour_gradientn(colours = pal)
ggsave(file.path("slides", "day_3_2_fit_3_M.png"), plt)


ecov_4 <- ecov_3
ecov_4$recruitment_how <- matrix("limiting-lag-1-linear")

input_4 <- set_ecov(input_3, ecov_4)


fit_4 <- fit_wham(input_4, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
max(abs(fit_4$gr()))
saveRDS(fit_4, file.path("temp", "day_3_2_fit_4_no_conv.RDS"))

#a rough check that all parameter elements have the same size.
all(sort(sapply(input_3$par, length)) == sort(sapply(input_4$par, length)))

#start at good initial values
input_4$par <- fit_3$parList
fit_4 <- fit_wham(input_4, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
max(abs(fit_4$gr()))

fit_4 <- do_sdreport(fit_4)
fit_4$peels <- retro(fit_4)
fit_4 <- make_osa_residuals(fit_4)
saveRDS(fit_4, file.path("temp", "day_3_2_fit_4.RDS"))
plot_wham_output(fit_4, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_4_GSI_diagnostic.png"))
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_4_GSI_osa.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "NAA_dev_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_4_NAA_devs.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "MAA_tile.png"), file.path(getwd(),"slides", "day_3_2_fit_4_MAA.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_4_GSI.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_msy_status.png"), file.path(getwd(),"slides", "day_3_2_fit_4_Kobe_msy.png"))
# file.copy(file.path(tmp.dir, "plots_png", "ref_points", "Kobe_status.png"), file.path(getwd(),"slides", "day_3_2_fit_4_Kobe_F40.png"))

SSB <- seq(1,max(fit_4$rep$SSB),100)
ab <- exp(cbind(fit_4$rep$log_SR_a,fit_4$rep$log_SR_b))
Ecov <- fit_4$rep$Ecov_out_R[1,,]
ord <- order(Ecov)
pal <- mypalette(length(ord))
years <- 1:length(ord)
R <- sapply(1:NROW(ab), function(x) ab[x,1]*SSB/(1 + ab[x,2]*SSB))
# Rmax <- ab[,1]*max(SSB)/(1 + ab[,2]*max(SSB))
# matplot(SSB, R, type = 'l', col = pal, lty = 1, lwd = 2)

df <- cbind.data.frame(year = rep(years,each = length(SSB)), SSB = SSB, R = c(R), GSI = rep(Ecov, each = length(SSB)), group = 1)
df <- rbind.data.frame(df, cbind.data.frame(year = years, SSB = fit_4$rep$SSB, R = fit_4$rep$NAA[1,1,,1], GSI = Ecov, group = 2))
plt <- ggplot(df, aes(SSB, R, colour = GSI, group = factor(year))) +
  geom_line(data = subset(df, group == 1), size = 1.2) +
  geom_point(data = subset(df, group == 2)) + 
  scale_colour_gradientn(colours = pal)
ggsave(file.path("slides", "day_3_2_fit_4_SR.png"), plt)

Ecov <- fit_4$rep$Ecov_out_M[1,1,1,,]
M_out <- fit_4$rep$MAA[1,1,,1]
ord <- order(Ecov)

Ecov_plt <- seq(min(Ecov),max(Ecov),0.01)
M <- exp(fit_4$parList$Mpars[1,1,1] + fit_4$parList$Ecov_beta_M[1,1,1,1,1] * Ecov_plt)
pal <- mypalette(length(ord))

# plot(Ecov_plt, M, type = 'l', lwd = 2, ylim = c(0, max(M_out)))
# points(Ecov[ord], M_out[ord], col = pal, pch = 19)

df <- cbind.data.frame(M=M, GSI = Ecov_plt, group = 1)
df <- rbind(df, cbind.data.frame(M = M_out, GSI = Ecov, group = 2))
plt <- ggplot(df, aes(GSI, M, colour = GSI)) +
  geom_line(data = subset(df, group == 1), size = 1.2) +
  geom_point(data = subset(df, group == 2)) + 
  scale_colour_gradientn(colours = pal)
ggsave(file.path("slides", "day_3_2_fit_4_M.png"), plt)


comp_mods <- compare_wham_models(list(fit_0,fit_1,fit_2,fit_3,fit_4), fdir = tmp.dir)
saveRDS(comp_mods, file.path("temp","day_3_2_comp_mods_1.RDS"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_sel_tile.png"), file.path(getwd(),"slides", "day_3_2_compare_sel.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_SSB_F_R.png"), file.path(getwd(),"slides", "day_3_2_compare_SSB_F_R.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_rel_status_timeseries.png"), file.path(getwd(),"slides", "day_3_2_compare_annual_status.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_ref_pts.png"), file.path(getwd(),"slides", "day_3_2_compare_annual_ref_pts.png"))

# fit_0 <- readRDS(file.path("temp", "day_3_2_fit_0.RDS"))
# fit_1 <- readRDS(file.path("temp", "day_3_2_fit_1.RDS"))
# fit_2 <- readRDS(file.path("temp", "day_3_2_fit_2.RDS"))
# fit_3 <- readRDS(file.path("temp", "day_3_2_fit_3.RDS"))
# fit_4 <- readRDS(file.path("temp", "day_3_2_fit_4.RDS"))

catchability <- list(
  re = c("iid", "none")
)
input_5 <- set_q(input_0, catchability)
fit_5 <- fit_wham(input_5, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
fit_5 <- do_sdreport(fit_5)
fit_5$peels <- retro(fit_5)
fit_5 <- make_osa_residuals(fit_5)
saveRDS(fit_5, file.path("temp", "day_3_2_fit_5.RDS"))
plot_wham_output(fit_5, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_5_GSI_diagnostic.png"))
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_5_GSI_osa.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_5_GSI.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "q_time_series.png"), file.path(getwd(),"slides", "day_3_2_fit_5_q.png"))

ecov_5 <- ecov
ecov_5$q_how  <- matrix(c("lag-0-linear", "none"),1,2)
input_6 <- set_ecov(input_5, ecov_5)

fit_6 <- fit_wham(input_6, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
max(abs(fit_6$final_gradient))
fit_6 <- do_sdreport(fit_6)
fit_6$peels <- retro(fit_6)
fit_6 <- make_osa_residuals(fit_6)
saveRDS(fit_6, file.path("temp", "day_3_2_fit_6.RDS"))
plot_wham_output(fit_6, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "GSI_diagnostic.png"), file.path(getwd(),"slides", "day_3_2_fit_6_GSI_diagnostic.png"))
# file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_ecov_4panel_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_6_GSI_osa.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Ecov_1_GSI.png"), file.path(getwd(),"slides", "day_3_2_fit_6_GSI.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "q_time_series.png"), file.path(getwd(),"slides", "day_3_2_fit_6_q.png"))

comp_mods <- compare_wham_models(list(fit_5,fit_6), fdir = tmp.dir)
saveRDS(comp_mods, file.path("temp","day_3_2_comp_mods_2.RDS"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_SSB_F_R.png"), file.path(getwd(),"slides", "day_3_2_compare_SSB_F_R_2.png"))
