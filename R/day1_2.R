# Day 1 Session 2: Exploring output

# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex2_SNEMAYT.dat"))
input_asap <- prepare_wham_input(asap3) 
tmp.dir <- tempdir(check=TRUE)

# res_dir <- file.path(getwd(),"temp")
# fit_RDS <- file.path(res_dir,"fit_asap.RDS")
# fit_asap <- readRDS(fit_RDS)
input_asap
fit_asap <- fit_wham(input_asap, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE, do.brps = FALSE)
fit_asap <- do_sdreport(fit_asap)
fit_asap <- do_reference_points(fit_asap, do.sdrep = TRUE)
fit_asap$peels <- retro(fit_asap)
fit_asap <- make_osa_residuals(fit_asap)
saveRDS(fit_asap, file.path("temp", "fit_asap_full.RDS"))
input_asap <- fit_asap$input
nofit_asap <- fit_wham(input_asap, do.fit = FALSE)

plot_wham_output(nofit_asap, out.type = "pdf", dir.main = tmp.dir)

plot_wham_output(fit_asap, dir.main = tmp.dir)





