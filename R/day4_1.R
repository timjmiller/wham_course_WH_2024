# Debugging: Day 4 Session 1: Debugging

# using grep to check component nlls for NAs

# checking selectivity parameters
# 	check selAA[[]]
fit_8 from day 2 session 1 has a high selpar estimated.
fit_0 from day 2 session 2 has a hig selpar


# checking variance parameters/random effects
sort(mod$opt$par)
mod$parList

# age comp and index cv inputs needs to be 
Dirichlet multinomial input Neff should be an upper bound
Index CVs from ASAP3 file are often adjusted from the CVs from the bottom trawl surveys 



# internal calculation of reference points needs a different intial value
round(fit$rep$log_SPR0 + log(0.4) - fit$rep$log_SPR_FXSPR, 4)
fit$rep$log_SPR0_static + log(0.4) - fit$rep$log_SPR_FXSPR_static
exp(fit$rep$log_FXSPR)
input$data$FXSPR_static_init
nofit$env$data$FXSPR_static_init

grep("static", names(nofit_0$rep), value = TRUE)
plot_wham_output(nofit_0, dir.main = tmp.dir)


#no features for estimation in phases 

#have to do this by hand

#using parList from previous fit
#most important for fixed effects