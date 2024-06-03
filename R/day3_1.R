# Day 3 Session 1: Comparing models

# Use compare_wham_models function
# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
tmp.dir <- tempdir(check=TRUE)

fit_4 <- readRDS(file.path("temp", "day_2_2_fit_4.RDS"))
fit_5 <- readRDS(file.path("temp", "day_2_2_fit_5.RDS"))
fit_6 <- readRDS(file.path("temp", "day_2_2_fit_6.RDS"))
fit_4$peels <- retro(fit_4)
fit_5$peels <- retro(fit_5)
fit_6$peels <- retro(fit_6)

fit_4 <- make_osa_residuals(fit_4)
fit_5 <- make_osa_residuals(fit_5)
fit_6 <- make_osa_residuals(fit_6)
saveRDS(fit_4, file.path("temp", "day_3_1_fit_4.RDS"))
saveRDS(fit_5, file.path("temp", "day_3_1_fit_5.RDS"))
saveRDS(fit_6, file.path("temp", "day_3_1_fit_6.RDS"))
# fit_4 <- readRDS(file.path("temp", "day_3_1_fit_4.RDS"))
# fit_5 <- readRDS(file.path("temp", "day_3_1_fit_5.RDS"))
# fit_6 <- readRDS(file.path("temp", "day_3_1_fit_6.RDS"))

res <- compare_wham_models(list(fit_4,fit_5,fit_6), fdir = tmp.dir)
# file.copy(file.path(tmp.dir, "compare_png", "compare_sel_tile.png"), file.path(getwd(),"slides", "day_3_1_compare_sel.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_SSB_F_R.png"), file.path(getwd(),"slides", "day_3_1_compare_SSB_F_R.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_rel_status_timeseries.png"), file.path(getwd(),"slides", "day_3_1_compare_annual_status.png"))
# file.copy(file.path(tmp.dir, "compare_png", "compare_ref_pts.png"), file.path(getwd(),"slides", "day_3_1_compare_annual_ref_pts.png"))

plot_wham_output(fit_4, dir.main = tmp.dir)
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "likelihood.png"), file.path(getwd(),"slides", "day_3_1_fit_4_jll.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "catch_age_comp_fleet_1_region_1_a.png"), file.path(getwd(),"slides", "day_3_1_fit_4_acomp_obs_pred.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_catch_4panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_4_catch_osa_4plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_paa_6panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_4_acomp_osa_6plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_4_acomp_resid.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_osa_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_4_acomp_osa.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro.png"), file.path(getwd(),"slides", "day_3_1_fit_4_SSB_retro.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro_relative.png"), file.path(getwd(),"slides", "day_3_1_fit_4_SSB_retro_rel.png"))

plot_wham_output(fit_5, dir.main = tmp.dir)
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "likelihood.png"), file.path(getwd(),"slides", "day_3_1_fit_5_jll.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "catch_age_comp_fleet_1_region_1_a.png"), file.path(getwd(),"slides", "day_3_1_fit_5_acomp_obs_pred.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_catch_4panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_5_catch_osa_4plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_paa_6panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_5_acomp_osa_6plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_5_acomp_resid.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_osa_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_5_acomp_osa.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro.png"), file.path(getwd(),"slides", "day_3_1_fit_5_SSB_retro.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro_relative.png"), file.path(getwd(),"slides", "day_3_1_fit_5_SSB_retro_rel.png"))

plot_wham_output(fit_6, dir.main = tmp.dir)
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "likelihood.png"), file.path(getwd(),"slides", "day_3_1_fit_6_jll.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "catch_age_comp_fleet_1_region_1_a.png"), file.path(getwd(),"slides", "day_3_1_fit_6_acomp_obs_pred.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_catch_4panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_6_catch_osa_4plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "OSA_resid_paa_6panel_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_6_acomp_osa_6plot.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_6_acomp_resid.png"))
file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "Catch_age_comp_osa_resids_fleet_1.png"), file.path(getwd(),"slides", "day_3_1_fit_6_acomp_osa.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro.png"), file.path(getwd(),"slides", "day_3_1_fit_6_SSB_retro.png"))
file.copy(file.path(tmp.dir, "plots_png", "retro", "stock_1_SSB_retro_relative.png"), file.path(getwd(),"slides", "day_3_1_fit_6_SSB_retro_rel.png"))

# Use different age composition and selectivity assumptions to demonstrates OSA residuals

# Use jitter_wham function

# jitter_res <- jitter_wham(fit_RDS = file.path("temp", "day_3_1_fit_6.RDS"), n_jitter = 10, res_dir = "temp", do_parallel = TRUE, wham_location = "c:/work/wham/old_packages/lab")
jitter_res <- jitter_wham(fit_RDS = file.path("temp", "day_3_1_fit_6.RDS"), n_jitter = 10, res_dir = "temp", do_parallel = FALSE, wham_location = "c:/work/wham/old_packages/lab")
# saveRDS(jitter_res, file.path("temp", "day_3_1_jitter_res.RDS"))

plot(1:10, sapply(x[[1]], function(x) x$obj) - fit_6$opt$obj, xlab = "Jitter #", ylab = "NLL Difference")

# Use self-test function?

self_test_res <- self_test(fit_RDS = file.path("temp", "day_3_1_fit_6.RDS"), n = 10, res_dir = "temp", test_dir = "c:/work/wham/wham", do_parallel = FALSE)
# saveRDS(self_test_res, file.path("temp", "day_3_1_self_test_res.RDS"))

true = fit_6$rep$SSB
est = sapply(self_test_res[[1]], function(x) return(x$SSB))
SSB_rel_resid = apply(est,2, function(x) x/true - 1)
resid_cis = apply(SSB_rel_resid,1,mean) + apply(SSB_rel_resid,1,sd)*qnorm(0.975)*t(matrix(c(-1,1),2,length(true)))/sqrt(10)

plot(fit_6$years, apply(SSB_rel_resid,1,mean), ylab = "Estimated Relative Bias SSB", xlab = "Year")
lines(fit_6$years, resid_cis[,1])
lines(fit_6$years, resid_cis[,2])
abline(h = 0, lty = 2)



