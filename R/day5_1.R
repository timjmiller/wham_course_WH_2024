# Simulation: Day 5 Session 1: Simulation

# pkgbuild::compile_dll("c:/work/wham/wham", debug = FALSE)
# pkgload::load_all("c:/work/wham/wham")
library("wham", lib.loc = "c:/work/wham/old_packages/lab")
tmp.dir <- tempdir(check=TRUE)

wham.dir <- find.package("wham")
asap3 <- read_asap3_dat(file.path(wham.dir,"extdata","ex1_SNEMAYT.dat"))

selectivity=list(
  model=rep("age-specific",3), #define selectivity model
  re=rep("none",3), #define selectivity random effects model
  #define initial/fixed values for age-specific selectivity
  initial_pars=list(c(0.5,0.5,0.5,1,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,1,1,0.5,0.5)),
  fix_pars=list(4:5,4,2:4)) #which ages to not estimate age-specfic selectivity parameters

# selectivity <- list(model = rep("logistic", 3))
# selectivity$initial_pars <- list(
#   c(3, 0.2), 
#   c(3, 0.2), 
#   c(3, 0.2)) 

NAA_re = list(
  sigma="rec", #random effects for recruitment only
  cor="iid", #random effects are independent
  recruit_model = 2) #mean recruitment is the only fixed effect parameter

input <- prepare_wham_input(
  asap3, 
  selectivity = selectivity,
  NAA_re = NAA_re) 

fit_1 <- fit_wham(input, do.sdrep = FALSE, do.osa = FALSE, do.retro=FALSE) # don't do retro peels and OSA residuals, don't show output during optimization
saveRDS(fit_1, file.path("temp", "day_5_1_fit_1.RDS"))

sim_fn = function(om, input, do.fit = FALSE){
  obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")
  om_data = om$simulate(complete = TRUE)
  input$data[obs_names] = om_data[obs_names]
  input <- set_osa_obs(input) #to transform catch and index paa appropriately
  if(do.fit) {
    fit = fit_wham(input, do.sdrep = FALSE, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = TRUE)
    return(list(fit, om_data))
  } else return(list(input,om_data))
}

#alternative age comp assumption

input_em <- prepare_wham_input(
  asap3, 
  selectivity=selectivity,
  NAA_re = NAA_re,
  age_comp = "logistic-normal-miss0") #the only difference from original input

set.seed(123)
sim = sim_fn(fit_1, input_em, do.fit = FALSE)
sim[[1]]$data$obs[1:10,] #data.frame of observations and identifying information
sim[[2]]$obsvec[4:9] #showing obsvec that is simulated (multinomial frequencies)
sim[[1]]$data$obsvec[4:9] #showing obsvec that is used (MVN = logistic normal)
paa <- sim[[2]]$index_paa[1,1,]
paa #multinomial proportions : naa/sum(naa)
sim[[2]]$index_Neff[1] * paa # = simulated obsvec (multinomial frequencies)
log(paa[-6]) - log(paa[6]) # = inverse additive logistic transform for obsvec that is used

set.seed(123)
simfit <- sim_fn(fit_1, input_em, do.fit = TRUE)
saveRDS(simfit, file.path("temp", "day_5_1_simfit_1.RDS"))


plot(fit_1$years, simfit[[2]]$SSB, type = 'l', ylab = "SSB", xlab = "Year")
lines(fit_1$years, simfit[[1]]$rep$SSB, lty = 2, col = 'red')

# alternative selectivity assumption

selectivity_em = list(
  model = c(rep("logistic", input$data$n_fleets),rep("logistic", input$data$n_indices)),
  initial_pars = rep(list(c(5,1)), input$data$n_fleets + input$data$n_indices)) #fleet, index

input_em_2 <- prepare_wham_input(
    asap3, 
    selectivity = selectivity_em,
    NAA_re = NAA_re,
    model_name="Ex 1: SNEMA Yellowtail Flounder") 

set.seed(123)
simfit_2 = sim_fn(fit_1, input_em_2, do.fit = TRUE)
saveRDS(simfit_2, file.path("temp", "day_5_1_simfit_2.RDS"))

plot(fit_1$years, simfit_2[[2]]$SSB, type = 'l', ylab = "SSB", xlab = "Year")
lines(fit_1$years, simfit_2[[1]]$rep$SSB, lty = 2, col = 'red')

# Operating model without ASAP

make_info <- function(base_years = 1982:2021, ages = 1:10, Fhist = "updown", n_feedback_years = 0) { #changed years
    info <- list()
    info$ages <- ages
    info$years <- as.integer(base_years[1] - 1 + 1:(length(base_years) + n_feedback_years))
    na <- length(info$ages)
    ny <- length(info$years)
    info$n_regions <- 1L
    info$n_stocks <- 1L
    nby <- length(base_years)
    mid <- floor(nby/2)
    #up then down

    catch_info <- list()
    catch_info$n_fleets <- 1L
    catch_info$catch_cv <- matrix(0.1, ny, catch_info$n_fleets)
    catch_info$catch_Neff <- matrix(200, ny, catch_info$n_fleets)

    F_info <- list()
    if(Fhist == "updown") F_info$F <- matrix(0.2 + c(seq(0,0.4,length.out = mid),seq(0.4,0,length.out=nby-mid)),nby, catch_info$n_fleets)

    #down then up
    if(Fhist == "downup") F_info$F <- matrix(0.2 + c(seq(0.4,0,length.out = mid),seq(0,0.4,length.out=nby-mid)),nby, catch_info$n_fleets)

    if(n_feedback_years>0) F_info$F <- rbind(F_info$F, F_info$F[rep(nby, n_feedback_years),, drop = F]) #same F as terminal year for feedback period

    index_info <- list()
    index_info$n_indices <- 1L
    index_info$index_cv <- matrix(0.3, ny, index_info$n_indices)
    index_info$index_Neff <- matrix(100, ny, index_info$n_indices)
    index_info$fracyr_indices <- matrix(0.5, ny, index_info$n_indices)
    index_info$index_units <- rep(1, length(index_info$n_indices)) #biomass
    index_info$index_paa_units <- rep(2, length(index_info$n_indices)) #abundance
    index_info$q <- rep(0.3, index_info$n_indices)
    
    info$maturity <- array(t(matrix(1/(1 + exp(-1*(1:na - na/2))), na, ny)), dim = c(1, ny, na))

    L <- 100*(1-exp(-0.3*(1:na - 0)))
    W <- exp(-11)*L^3
    nwaa <- index_info$n_indices + catch_info$n_fleets + 2
    info$waa <- array(NA, dim = c(nwaa, ny, na))
    for(i in 1:nwaa) info$waa[i,,] <- t(matrix(W, na, ny))

    info$fracyr_SSB <- cbind(rep(0.25,ny))

    catch_info$selblock_pointer_fleets <- t(matrix(1:catch_info$n_fleets, catch_info$n_fleets, ny))
    index_info$selblock_pointer_indices <- t(matrix(catch_info$n_fleets + 1:index_info$n_indices, index_info$n_indices, ny))
    return(list(basic_info = info, catch_info = catch_info, index_info = index_info, F = F_info))
}

stock_om_info <- make_info()
basic_info <- stock_om_info$basic_info
catch_info <- stock_om_info$catch_info
index_info <- stock_om_info$index_info
F_info <- stock_om_info$F


selectivity_om <- list(
  model = c(rep("logistic", catch_info$n_fleets),rep("logistic", index_info$n_indices)),
  initial_pars = rep(list(c(5,1)), catch_info$n_fleets + index_info$n_indices)
) #fleet, index

M_om <- list(initial_means = array(0.2, dim = c(1,1, length(basic_info$ages))))

NAA_re_om <- list(
  N1_pars = array(exp(10)*exp(-(0:(length(basic_info$ages)-1))*M_om$initial_means[1]), dim = c(1,1,length(basic_info$ages))),
  sigma = "rec", #random about mean
  cor="iid", #random effects are independent
  recruit_model = 2, #random effects with a constant mean
  recruit_pars = list(exp(10)),
  sigma_vals = array(0.5, c(1,1,length(basic_info$ages))) #sigma_R, only the value in the first age class is used.
)


stock_om_input <- prepare_wham_input(
  basic_info = basic_info, 
  selectivity = selectivity_om, 
  NAA_re = NAA_re_om, 
  catch_info = catch_info,
  index_info = index_info,
  M = M_om,
  F = F_info
)
stock_om = fit_wham(stock_om_input, do.fit = FALSE, MakeADFun.silent = TRUE)
saveRDS(stock_om, file.path("temp", "day_5_1_stock_om_1.RDS"))

#incorrect M assumed in EM

M_em = list(
  initial_means = array(0.3, dim = c(1,1, length(basic_info$ages)))
)

em_input = prepare_wham_input(
  basic_info = basic_info, 
  selectivity = selectivity_om, 
  NAA_re = NAA_re_om, 
  M = M_em, 
  catch_info = catch_info,
  index_info = index_info,
  F = F_info
)

set.seed(123)
simfit_3 = sim_fn(stock_om, em_input, do.fit = TRUE)
saveRDS(simfit_3, file.path("temp", "day_5_1_simfit_3.RDS"))

res <- cbind(simfit_3[[2]]$SSB,simfit_3[[1]]$rep$SSB)
plot(stock_om$years, res[,1], type = 'l', ylab = "SSB", xlab = "Year", ylim = c(0, max(res)))
lines(stock_om$years, res[,2], lty = 2, col = 'red')

get_F_from_catch <- function(om, year, catch, Finit = 0.1, maxF = 10){
    rep = om$rep
    naa = rep$NAA[1,1,year,]
    Maa = rep$MAA[1,1,year,]
    sel_tot = rbind(rep$FAA[,year,]/max(exp(rep$log_FAA_tot[year,]))) #matrix nfleets x n_ages 
    waa = rbind(om$input$data$waa[om$input$data$waa_pointer_fleets, year,]) #matrix nfleets x n_ages 
    nfleets <- length(om$input$data$waa_pointer_fleets)
    get_catch = function(log_F, naa, sel, waa, Maa){
        Faa = exp(log_F) * sel_tot
        Zaa = Maa + apply(Faa,2,sum)
        Catch = 0
        for(a  in 1:length(naa)) for(f in 1:nfleets) Catch = Catch + waa[f,a] * naa[a] * Faa[f,a] *(1 - exp(-Zaa[a]))/Zaa[a];
        return(Catch)
    }
    obj = function(log_F) (catch - get_catch(log_F, naa, sel_tot, waa, Maa))^2
    opt = try(nlminb(log(Finit), obj))
    if(!is.character(opt)) Fsolve = exp(opt$par)[1] else Fsolve = maxF
    if(Fsolve>10) Fsolve = maxF
    print(paste0("Fsolve: ", Fsolve))
    return(Fsolve)
}

update_om_F = function(om, year, catch){
    rep = om$rep #generate the reported values given the parameters
    year_ind = which(om$years == year) #index corresponding to year
    Fsolve = get_F_from_catch(om, year_ind, catch) #find the F for the catch advice

    #have to be careful if more than one fleet
    FAA = rbind(rep$FAA[,year_ind,]) #n_fleets x n_ages
    FAA_tot <- apply(FAA,2,sum)
    age_ind <- which(FAA_tot == max(FAA_tot))[1]
    selAA = FAA/FAA_tot[age_ind] #sum(sel_all[i,]) = 1
    FAA_catch = Fsolve * selAA
    F_fleet = apply(FAA_catch, 1, max) #full F for each fleet
    if(om$input$data$F_config==1) {
      if(year_ind>1) om$input$par$F_pars[year_ind-1,] <- log(F_fleet) - log(apply(rbind(rep$FAA[,year_ind-1,]),1,max)) #change the F_dev to produce the right full F
      else om$input$par$log_F1[] <- log(F_fleet) #if year is the first year of the model, change F in year 1
    } else{ #alternative configuration of F_pars
      om$input$par$F_pars[year_ind,] <- log(F_fleet)
    }
    om <- fit_wham(om$input, do.fit = FALSE, MakeADFun.silent = TRUE)
    return(om)
}

update_om_fn = function(om, seed = 123, interval.info = NULL, random = "log_NAA"){
  obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")

  if(!is.null(interval.info)){ #iteratively update F over assessment interval for the given catch advice
    for(y in interval.info$years){
      om = update_om_F(om, year = y, catch = interval.info$catch) #put in the right F values
      set.seed(seed)
      om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
      om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
      om$input$par[om$input$random] = om_sim[om$input$random] #update any simulated random effects
      # reset the om
      om <- fit_wham(om$input, do.fit = FALSE, MakeADFun.silent = TRUE)
    }
  } else { #otherwise just (re)generate the population
    set.seed(seed)
    om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
    om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
    om$input$par[random] = om_sim[random] #update any simulated random effects
    # reset the om
    om <- fit_wham(om$input, do.fit = FALSE, MakeADFun.silent = TRUE)
  }
  # Loop through feedback period

  return(om)
}

make_em_input = function(M_em, sel_em, NAA_em, om_data, em_years){
  info = make_info(base_years = em_years) #update the terminal year for the estimation model
  ind_em = 1:length(em_years) #year indices
  #fill in the data from the operating model simulation
  info$catch_info$agg_catch = om_data$agg_catch[ind_em,, drop = FALSE]
  info$index_info$agg_indices = om_data$agg_indices[ind_em,, drop = FALSE]
  info$catch_info$catch_paa = om_data$catch_paa[,ind_em,, drop = FALSE]
  info$index_info$index_paa = om_data$index_paa[,ind_em,, drop = FALSE]
  em_input <- prepare_wham_input(
    basic_info = info$basic_info, 
    selectivity = sel_em, 
    NAA_re = NAA_em, 
    M = M_em, 
    catch_info = info$catch_info,
    index_info = info$index_info)
  return(em_input)
}

advice_fn = function(em){
  #make 5 year projections using F40. Use average SSB/R and YPR inputs over most recent 5 years
  proj_opts = list(n.yrs=5, use.FXSPR=TRUE, avg.yrs=tail(em$years,5)) 
  em_proj = project_wham(em, do.sdrep = FALSE, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
  advice = mean(apply(em_proj$rep$pred_catch[length(em_proj$years) + 1:5,,drop = FALSE],1,sum)) #mean of the projected catch over the next 5 years fishing at F40
  print(advice)
  return(advice)
}

loop_through_fn = function(om, M_em, selectivity_em, NAA_re_em, assess_years, assess_interval = assess.interval, base_years){
  catches = numeric() #save the catch advice
  for(i in assess_years){
    print(i)
    #make the input for the estimation model

    em_input = make_em_input(M_em, selectivity_em, NAA_re_em, om_data = om$input$data, em_years = base_years[1]:i)
    #fit the estimation model
    em = fit_wham(em_input, do.sdrep = FALSE, do.retro = FALSE, do.osa=FALSE, MakeADFun.silent = TRUE) #no feedback period yet
    #make the catch advice
    advice = advice_fn(em)
    catches = c(catches, advice)
    #set the catch for the next assess_interval years
    interval.info = list(catch = advice, years = i + 1:assess_interval)
    #update the operating model with the right Fs and resimulate the data given those Fs
    om = update_om_fn(om, seed = 123, interval.info = interval.info)  
  }
  return(list(om = om, em  = em, catches = catches))
}

stock_om_info <- make_info(base_years = 1982:2021, n_feedback_years = 40)
basic_info <- stock_om_info$basic_info
catch_info <- stock_om_info$catch_info
index_info <- stock_om_info$index_info
F_info <- stock_om_info$F

stock_om_input = prepare_wham_input(
  basic_info = basic_info, 
  selectivity = selectivity_om, 
  NAA_re = NAA_re_om, 
  M = M_om, 
  catch_info = catch_info,
  index_info = index_info,
  F = F_info
)
stock_om_input$random <- NULL #so inner optimization won't change simulated RE
stock_om_fb <- fit_wham(stock_om_input, do.fit = FALSE, MakeADFun.silent = TRUE)
saveRDS(stock_om_fb, file.path("temp", "day_5_1_stock_om_fb.RDS"))

assess.interval = 4
base.years = make_info()$basic_info$years #no feedback period yet
first.year = head(base.years,1)
terminal.year = tail(base.years,1)
assess.years = seq(terminal.year, tail(stock_om_fb$years,1)-assess.interval,by = assess.interval)

stock_om_fb = update_om_fn(stock_om_fb)

looped_res = loop_through_fn(stock_om_fb, M_em = M_em, selectivity_em = selectivity_om, NAA_re_em = NAA_re, assess_years = assess.years, base_years = base.years)
saveRDS(looped_res, file.path("temp", "day_5_1_looped_res.RDS"))

looped_rep <- looped_res$om$rep

res <- cbind(stock_om_fb$rep$SSB, looped_rep$SSB)
plot(stock_om_fb$years, res[,1], type = "l", ylim = c(0,max(res)), ylab = "SSB", xlab = "Year")
lines(stock_om_fb$years, res[,2], col = "red")

res <- cbind(looped_rep$SSB[1:76]/exp(looped_rep$log_SSB_FXSPR[1:76,1]), looped_res$em$rep$SSB[1:76]/exp(looped_res$em$rep$log_SSB_FXSPR[1:76,1]))
plot(stock_om_fb$years[1:76], res[,1], type = "l", ylim = c(0,max(res)), ylab = "SSB/SSB40", xlab = "Year")
lines(stock_om_fb$years[1:76], res[,2], col = "red")

