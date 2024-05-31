# BRPs, projections: Day 4 Session 2:

# check spr based brps
# changing initial values

# checking selectivity parameters
# 	check selAA[[]]


# Annual reference points:

# Use results from GSI effects on R and M

fit_4 <- readRDS(file.path("temp", "day_3_2_fit_4.RDS"))
df <- cbind.data.frame(SSBmsy = exp(fit_4$rep$log_SSB_MSY[,1]), Fmsy = exp(fit_4$rep$log_FMSY), GSI = Ecov)
plt <- ggplot(df, aes(GSI, Fmsy, colour = GSI)) +
  geom_point() + 
  scale_colour_gradientn(colours = pal)
plt <- ggplot(df, aes(GSI, SSBmsy, colour = GSI)) +
  geom_point() + 
  scale_colour_gradientn(colours = pal)
