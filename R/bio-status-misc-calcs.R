# Read in model fits and wrangle ---- 
quantilesDF<-readRDS(here("Results/Model-fits/2025COSR3B.tr1_lalpha_MCMCDF.rds"))
mcmc_names<-readRDS(here("Results/Model-fits/2025mcmc_names.rds"))

n.years<-length(seq(1980,2020,1))
n.pops<-max(SR.dat$pop_no)

yrs<-matrix(nrow=n.years,ncol=n.pops,NA) 
# pulling out the parameters
spawners_impute<-quantilesDF[grep("spawners",mcmc_names),]
pop_alpha_mu<-quantilesDF[grep("ln_alpha.mu",mcmc_names),]
pop_betas3B<-quantilesDF[grep("beta",mcmc_names),]
pop_alphas3B<-quantilesDF[grep("\\blalpha",mcmc_names),]
mu_alphaHG<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,1]",mcmc_names),] # grab Group 1
mu_alphaNass<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,2]",mcmc_names),] # grab Group 2
mu_alphaSkeena<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,3]",mcmc_names),] # grab Group 3
mu_alphaHec<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,4]",mcmc_names),] # grab Group 4
mu_alphaNC<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,5]",mcmc_names),] # grab Group 5
mu_alphaCC<-quantilesDF[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,6]",mcmc_names),] # grab Group 6



library(lamW)

# Function to compute U_MSY from intrinsic productivity (p)
U_msy_ricker <- function(p) {
  # p is the intrinsic productivity parameter (a)
  W_term <- lambertW0(exp(1 - p))    # principal branch
  U_msy  <- 1 - (W_term )
  return(U_msy)
}

## ---------------------------------------------------------
## Example usage
## ---------------------------------------------------------

p <- mean(mu_alphaHec[38:41,3])   # intrinsic productivity (a)
U_msy <- U_msy_ricker(p)


U_msy_ricker(mu_alphaHec[38:41,3])   # intrinsic productivity (a)

U_msy_ricker(mean(mu_alphaHec[,3]))
U_msy_ricker(mean(mu_alphaCC[,3]))
U_msy_ricker(mean(mu_alphaNC[,3]))
U_msy_ricker(mean(c(mu_alphaNC[,3],mu_alphaCC[,3],mu_alphaHec[,3])))



er.2er <- er |>
  filter(Fishery != "Canada (English 2018)",
         Year > 2020) |>
  group_by(CU, Year) |>
  summarize(total_harv = sum(er)) |>
  group_by(CU) |>
  summarize(avg_harv = mean(total_harv)) 
  
