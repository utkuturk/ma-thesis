set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "brms", "rstan")
lapply(packages, require, character.only = TRUE)

## Save compiled models:
rstan_options(auto_write = TRUE)
rstan_options(silent = TRUE, open_progress=FALSE,show_messages=FALSE)
rstan_options(javascript = FALSE)
## Use all the cores:
options(mc.cores = parallel::detectCores())

theme_set(theme_bw(base_family = "Times"))

# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp3$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")
options(brms.file_refit = config$refit)
## MODELS -----

### POST-AMLAP MODELS ====
exp3.dfModel <- exp3.clean
exp3.dfModel %<>% subset(experiment != "filler")


exp3.dfModel %<>% within(., {
    c_exp <- ifelse(biasManipulation == "gram", .5, -.5)
    c_ung <- ifelse(grammatical == "ungram", .5, -.5)
    c_att <- ifelse(attractor_num == "pl", .5, -.5)
    c_mis <- ifelse(attractor_num == "pl", .5, -.5)
    l_trial <- log(trial_no)
})

#***********************************


exp3.formula <- response_yes ~ bias * c_mis + l_trial +
                            (bias * c_mis + 1 | subject) +
                            (c_mis + l_trial + 1 | item)


exp3.priors <- c(
    set_prior("student_t(3,0,2.5)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b"),
    set_prior("cauchy(0,1)", class = "sd"),
    set_prior("lkj(2)", class = "cor")
)


exp3.mresp.g <- brm(
    formula = exp3.formula,
    data = filter(exp3.dfModel, grammatical == "gram"),
    family = bernoulli("probit"), 
    prior = exp3.priors,
    chains = config$exp3$model$chains, 
    cores = config$exp3$model$cores,
    iter = config$exp3$model$iter, 
    warmup = config$exp3$model$warmup, 
    init_r = .1,
    file = paste0(config$exp3$model$path, "exp3.mresp.g")
)

exp3.coefs <- c(
    "bias" = "Ungram. Bias",
    "c_mis" = "Plural Attactor",
    "l_trial" = "Trial No (log)",
    "bias:c_mis" = "Ungram. Bias * Plural Attactor"
)

exp3.pmresp.g <- create_model_coefs_plot(
    exp3.mresp.g,
    plot_stats = T, map_names = exp3.coefs,
    expand_right = 2.5, expand_top = 4,
    x_stat_adjust = 0.2, x_breaks = c(-1.5,-1,-0.5,0,0.5)
) + annotate(
    x = -1.5, xend = 0.5, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp3.mresp.u <- brm(
    formula = exp3.formula,
    data = filter(exp3.dfModel, grammatical == "ungram"),
    family = bernoulli("probit"), 
    prior = exp3.priors,
    chains = config$exp3$model$chains, 
    cores = config$exp3$model$cores,
    iter = config$exp3$model$iter, 
    warmup = config$exp3$model$warmup, 
    init_r = .1,
    file = paste0(config$exp3$model$path, "exp3.mresp.u")
)

exp3.pmresp.u <- create_model_coefs_plot(
    exp3.mresp.u,
    plot_stats = T, map_names = exp3.coefs,
    expand_right = 2.5, expand_top = 4,
    x_stat_adjust = 0.7, x_breaks = seq(-2,1,0.5)
) + annotate(
    x = -2, xend = 1, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp3.mdf.g <- fixef(exp3.mresp.g) %>% as.data.frame()
count = 1
for (i in row.names(exp3.mdf.g)) {
    temp <- print_estimate_with_ci(exp3.mresp.g, i)
    exp3.mdf.g$text[count] = temp
    count = count + 1
}

exp3.mdf.u <- fixef(exp3.mresp.u) %>% as.data.frame()
count = 1
for (i in row.names(exp3.mdf.u)) {
    temp <- print_estimate_with_ci(exp3.mresp.u, i)
    exp3.mdf.u$text[count] = temp
    count = count + 1
}


exp3.models <- list(
    gramModel = exp3.mresp.g,
    dfGramModel = exp3.mdf.g,
    gramPlot = exp3.pmresp.g,
    ungramModel = exp3.mresp.u,
    dfUngramModel = exp3.mdf.u,
    ungramPlot = exp3.pmresp.u
)

# saveRDS(exp3.models, file = config$exp3$out$model)
