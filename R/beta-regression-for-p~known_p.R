#######
# Fit beta regression for p ~ known_p.
# See ../survey_analysis.md for an analysis of the results of this model.
#######

source("R/load-survey-data.R")

library(tidybayes)
library(metabayes)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
memory.limit(8000)

#MODEL SPEC
final_model = TRUE
m.metastan = metastan(
    data = {
        n : int(lower=1)
        restricted_p : vector(lower=0,upper=1)[n]
        known_p : vector[n]
        
        n_viz : int(lower=1)
        viz[n] : int(lower=1, upper=n_viz)
        n_layout : int(lower=1)
        layout[n] : int(lower=1, upper=n_layout)
        n_gender : int(lower=1)
        gender[n] : int(lower=1, upper=n_gender)
        n_participant : int(lower=1)
        participant[n] : int(lower=1, upper=n_participant)
        n_pquestion : int(lower=1)
        pquestion[n] : int(lower=1, upper=n_pquestion)
    },
    transformed_data = {
        layout_coding : vector[2]
        gender_coding : vector[2]
        logit_known_p : vector[n]

        #use simple coding for layout and gender so that the coefficients for them
        #correspond to the differences between the two levels, and
        #other factors are estimated with respect to the mean of the
        #two levels
        layout_coding[1] = -0.5
        layout_coding[2] = 0.5
        gender_coding[1] = -0.5
        gender_coding[2] = 0.5
        
        #pre-compute logit to save a bit on inner loop
        for (i in 1:n) {
            logit_known_p[i] = logit(known_p[i])
        }
    },
    parameters = {
        b[2] : vector[n_viz]
        d_viz : vector[n_viz]
        d_layout : real
        d_gender : real
        d_participant : vector[n_participant]
        d_participant_tau : real 
        d_viz_participant[n_viz] : vector[n_participant]
        d_viz_participant_tau : real 
        d_pquestion : vector[n_pquestion]
        d_pquestion_tau : real 
    },
    model = {
        d_participant_sigma : real 
        d_viz_participant_sigma : real 
        d_pquestion_sigma : real 
        mu : vector[n]
        phi : vector[n]
        alpha : vector[n]
        beta : vector[n]

        #REGRESSION MODEL
        for (i in 1:n) {
            #regression submodels
            #mean
            mu[i] <- inv_logit(b[1,viz[i]] + b[2,viz[i]] * logit_known_p[i])
            #dispersion
            #negation here makes coefficients represent dispersion (instead of precision) to ease interpretation
            #see Smithson (2006)
            phi[i] <- exp(-(
                    d_viz[viz[i]]
                    + d_layout * layout_coding[layout[i]]
                    + d_gender * gender_coding[gender[i]]
                    + d_participant[participant[i]] 
                    + d_viz_participant[viz[i], participant[i]] 
                    + d_pquestion[pquestion[i]]
                    ))

            #beta-distributed observations
            #reparameterized beta distribution in terms of mu (mean) and phi (precision)
            #See Smithson M, Verkuilen J (2006). “A Better Lemon Squeezer? Maximum-Likelihood Regression
            #with Beta-Distributed Dependent Variables.” Psychological Methods, 11(1), 54–71.
            alpha[i] <- mu[i] * phi[i]
            beta[i]  <- (1 - mu[i]) * phi[i]
        }
        restricted_p ~ beta(alpha, beta)
        
        #prior is slope 0, intercept 1
        #informed priors based on HOPS
        b[1] ~ normal(0, 0.4)
        b[2] ~ normal(1, 0.3)
        
        #informed priors based on HOPS
        d_viz ~ normal(-2.6, 0.6)

        #difference in layout is on the order of what we expect for dispersion from HOPS model, centered at 0
        d_layout ~ normal(0, 0.6)

        #difference in gender is on the order of what we expect for dispersion from HOPS model, centered at 0
        d_gender ~ normal(0, 0.6)

        #random effect for participant: some participants are better/worse at this task
        #informed priors based on HOPS
        d_participant_tau ~ gamma(58.23573, 122.9211)
        d_participant_sigma <- 1/sqrt(d_participant_tau) 
        d_participant ~ normal(0, d_participant_sigma)
        
        #random effect for participant*viz: some participants are better/worse at some vizes
        d_viz_participant_tau ~ gamma(58.23573, 122.9211)
        d_viz_participant_sigma <- 1/sqrt(d_viz_participant_tau)
        for (v in 1:n_viz) {
            d_viz_participant[v] ~ normal(0, d_viz_participant_sigma)
        }
    
        #random effect for pquestion: some questions are harder/easier
        d_pquestion_tau ~ gamma(58.23573, 122.9211)
        d_pquestion_sigma <- 1/sqrt(d_pquestion_tau) 
        d_pquestion ~ normal(0, d_pquestion_sigma)
    })
m.stan = stan_model(model_code = as.character(m.metastan))

#DATA
data_list = dfl %>%
    compose_data() %>%
    #drop variables not actually used in the model so stan doesn't complain
    {.[intersect(names(.), variables(m.metastan))]}

#RUN MODEL
if (final_model) {
    iterations = 20000
    thin = 8
    chains = 16
} else {
    iterations = 1000
    thin = 2
    chains = 2
}

#RUN THE SAMPLER
fit = sampling(m.stan, 
    data = data_list, seed = NA,
    iter = iterations, thin = thin,
    chains = chains)

#SAVE THE RESULTS
save(fit, file = "fits/fit-p~known_p.RData")
