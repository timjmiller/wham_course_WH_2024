# Simulation: Day 5 Session 1:

#########################################################
#statistical catch at age model

input <- prepare_wham_input()

om <- fit_wham(input, do.fit = FALSE)

set.seed(123)
simdat <- om$simulate(complete=TRUE)

sim_input <- input
sim_input$data <- simdat

sim_fit <- fit_wham(sim_input, do.osa=FALSE, do.retro = FALSE)

#########################################################
#recruitment and survival random effects
naa_re_input_1 <- set_NAA(input, NAA_re = list(sigma = "rec+1", cor = "2dar1"))
naa_re_om <- fit_wham(naa_re_input_1, do.fit = FALSE)

set.seed(123)
naa_re_sim_1 <- om$simulate(complete=TRUE)

sim_input <- input
sim_input$data <- simdat

sim_fit <- fit_wham(sim_input, do.osa=FALSE, do.retro = FALSE)

