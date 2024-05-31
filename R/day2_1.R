# Day 2 Session 1: Selectivity

# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
wham.dir <- find.package("wham")
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex1_SNEMAYT.dat"))

input <- prepare_wham_input(asap3)
nofit <- fit_wham(input, do.fit = FALSE)

tmp.dir <- tempdir(check=TRUE)
plot_wham_output(nofit, dir.main = tmp.dir)
#file.copy(file.path(tmp.dir, "plots_png", "diagnostics", "summary_text.png"), file.path(getwd(),"slides", "day_2_1_nofit_summary.png"))

#2 = logistic, 1 = age-specific
input$data$selblock_models


# alternative mean models
selectivity <- list(model = c("double-logistic", "decreasing-logistic", "logistic"))
selectivity$initial_pars <- list(
	c(2,0.2,5,0.2), 
	c(3, 0.2), 
	c(3, 0.2))
selectivity$fix_pars <- list(NULL, NULL, NULL)
input_0 <- set_selectivity(input, selectivity = selectivity)
nofit_0 <- fit_wham(input_0, do.fit = FALSE)
plot_wham_output(nofit_0, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_fleet_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_nofit_0_sel_fleet_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_nofit_0_sel_index_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_2_region_1.png"), file.path(getwd(),"slides", "day_2_1_nofit_0_sel_index_2.png"))


# Suppose age-specific selectivity for fleet

selectivity <- list(model = c("age-specific", "logistic", "logistic"))
selectivity$initial_pars <- list(
	rep(0.5, 6), 
	c(3, 0.2), 
	c(3, 0.2))
selectivity$fix_pars <- list(NULL, NULL, NULL)

input_1 <- set_selectivity(input, selectivity = selectivity)

#check that all ages are estimated
temp <- input_1$par$logit_selpars
temp[] <- input_1$map$logit_selpars
temp

fit_1 <- fit_wham(input_1, do.sdrep = FALSE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_1, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_fleet_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_1_sel_fleet_1.png"))

lapply(fit_1$rep$selpars, function(x) x[1,])


# Assume all are age specific

selectivity <- list(model = rep("age-specific", 3))
selectivity$initial_pars <- list(
	rep(0.5, 6), 
	rep(0.5, 6), 
	rep(0.5, 6)) 

input_2 <- set_selectivity(input, selectivity = selectivity)
temp <- input_2$par$logit_selpars
temp[] <- input_2$map$logit_selpars
temp

fit_2 <- fit_wham(input_2, do.sdrep = FALSE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_2, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_fleet_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_2_sel_fleet_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_2_sel_index_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_2_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_2_sel_index_2.png"))


lapply(fit_2$rep$selpars, function(x) x[1,])

fit_2$parList$logit_selpars

#now estimate model
selectivity <- list(model = rep("age-specific", 3))
selectivity$initial_pars <- list(
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$fix_pars <- list(4,4,4)
input_3 <- set_selectivity(input, selectivity = selectivity)
temp <- input_3$par$logit_selpars
temp[] <- input_3$map$logit_selpars
temp

fit_3 <- fit_wham(input_3, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_3, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_fleet_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_3_sel_fleet_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_1_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_3_sel_index_1.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "Selectivity_index_2_region_1.png"), file.path(getwd(),"slides", "day_2_1_fit_3_sel_index_2.png"))


#age and time-varying random effects on fleet

F_opts <- list(
	F = cbind(rep(5,input$data$n_years_model)),
	map_F = cbind(rep(NA, input$data$n_years_model)))

selectivity <- list(model = rep("age-specific", 3))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	rep(0.1,6),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,1,1),c(1:3,NA,4:5), c(1:3,NA,4:5))
input_4 <- set_selectivity(input, selectivity = selectivity)
input_4 <- set_F(input_4, F_opts)
temp <- input_4$par$selpars_re
temp[] <- input_4$map$selpars_re
temp[1,,]
temp <- input_4$par$logit_selpars
temp[] <- input_4$map$logit_selpars
temp
# nofit_4 <- fit_wham(input_4, do.fit = FALSE)
# This one takes a while
fit_4 <- fit_wham(input_4, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_4, file.path("temp", "day_2_1_fit_4.RDS"))
plot_wham_output(fit_4, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_4_selAA.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "F_byfleet.png"), file.path(getwd(),"slides", "day_2_1_fit_4_full_F.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "CV_SSB_Rec_F.png"), file.path(getwd(),"slides", "day_2_1_fit_4_CV.png"))

#doesn't work to estimate age-specific means
# selectivity$re <- c("iid", "none", "none")
# selectivity$fix_pars <- NULL
# selectivity$initial_pars[[1]] <- c(rep(0.5,4),1,0.5)
# selectivity$map_pars <- list(c(1:4,NA,5),c(1:3,NA,4:5), c(1:3,NA,4:5))
# input_5 <- set_selectivity(input, selectivity = selectivity)
# fit_5 <- fit_wham(input_5, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
# check_convergence(fit_5)
# fit_5$sdrep

#now fix age 5 as fully selected
selectivity <- list(model = rep("age-specific", 3))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	c(rep(0.5,4),1,0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(1:3,NA,4:5), c(1:3,NA,4:5))
input_5 <- set_selectivity(input, selectivity = selectivity)

temp <- input_5$par$logit_selpars
temp[] <- input_5$map$logit_selpars
temp

temp <- input_5$par$selpars_re
temp[] <- input_5$map$selpars_re
temp[1,,]
nofit_5 <- fit_wham(input_5, do.fit = FALSE)
nofit_5$rep$selAA[[1]]

fit_5 <- fit_wham(input_5, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_5, file.path("temp", "day_2_1_fit_5.RDS"))
plot_wham_output(fit_5, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_5_selAA.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "F_byfleet.png"), file.path(getwd(),"slides", "day_2_1_fit_5_full_F.png"))
# file.copy(file.path(tmp.dir, "plots_png", "results", "CV_SSB_Rec_F.png"), file.path(getwd(),"slides", "day_2_1_fit_5_CV.png"))

#ar1(year) RE

selectivity <- list(model = rep("age-specific", 3))
selectivity$re <- c("ar1_y", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	c(rep(0.5,4),1,0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1:4,NA,5),c(1:3,NA,4:5), c(1:3,NA,4:5))
input_6 <- set_selectivity(input, selectivity = selectivity)
# nofit_6 <- fit_wham(input_6, do.fit = FALSE)

temp <- input_6$par$selpars_re
temp[] <- input_6$map$selpars_re
temp[1,,]

fit_6 <- fit_wham(input_6, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_6, file.path("temp", "day_2_1_fit_6.RDS"))
plot_wham_output(fit_6, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_6_selAA.png"))

#ar1(age) RE
selectivity$re <- c("ar1", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	c(rep(0.5,4),1,0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(1:3,NA,4:5), c(1:3,NA,4:5))
input_7 <- set_selectivity(input, selectivity = selectivity)
fit_7 <- fit_wham(input_7, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_7, file.path("temp", "day_2_1_fit_7.RDS"))
plot_wham_output(fit_7, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_7_selAA.png"))

# 2dAR1 RE
selectivity$re <- c("2dar1", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	c(rep(0.5,4),1,0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5),
	c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(1:3,NA,4:5), c(1:3,NA,4:5))
input_8 <- set_selectivity(input, selectivity = selectivity)
fit_8 <- fit_wham(input_8, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_8, file.path("temp", "day_2_1_fit_8.RDS"))
plot_wham_output(fit_8, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_8_selAA.png"))


#iid RE (logistic)

selectivity <- list(model = rep("logistic", 3))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
	c(3, 0.2), 
	c(3, 0.2), 
	c(3, 0.2)) 
input_9 <- set_selectivity(input, selectivity = selectivity)
fit_9 <- fit_wham(input_9, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
saveRDS(fit_9, file.path("temp", "day_2_1_fit_9.RDS"))
plot_wham_output(fit_9, dir.main = tmp.dir)
# file.copy(file.path(tmp.dir, "plots_png", "results", "SelAA_tile.png"), file.path(getwd(),"slides", "day_2_1_fit_9_selAA.png"))

