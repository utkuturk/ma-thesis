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
# load(config$exp1$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")
options(brms.file_refit = config$refit)
## MODELS -----

### POST-AMLAP MODELS ====
exp1.dfModel <- exp1.merged
exp1.dfModel %<>% subset(match != "filler")


exp1.dfModel %<>% within(., {
    c_cons <- ifelse(experiment != "Experiment 1", .5, -.5)
    c_ung <- ifelse(grammatical == "ungrammatical", .5, -.5)
    c_att <- ifelse(attractor_num == "plural", .5, -.5)
    c_mis <- ifelse(attractor_num == "plural", .5, -.5)
    l_trial <- log(trial_no)
})

#***********************************

exp1.formula <- response_yes ~ c_cons * c_ung * c_mis + l_trial + 
                            (c_ung * c_mis + 1 | subject) +
                            (c_ung * c_mis + l_trial + 1 | item)


exp1.priors <- c(
    set_prior("student_t(3,0,2.5)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b"),
    set_prior("cauchy(0,1)", class = "sd"),
    set_prior("lkj(2)", class = "cor")
)


exp1.mresp <- brm(
    formula = exp1.formula,
    data = exp1.dfModel,
    family = bernoulli("probit"),
    prior = exp1.priors,
    chains = config$exp1$model$chains,
    cores = config$exp1$model$cores,
    iter = config$exp1$model$iter,
    warmup = config$exp1$model$warmup,
    init_r = .1,
    file = paste0(config$exp1$model$path, "exp1.mresp")
)

exp1.coefs <- c(
    "c_ung" = "Ungrammaticality",
    "c_mis" = "Plural Attactor",
    "c_ung:c_mis" = "Ungrammaticality * Plural Attractor",
    "c_cons" = "Ambiguity",
    "l_trial" = "Trial No (log)",
    "c_cons:c_ung" = "Ambiguity * Ungrammaticality",
    "c_cons:c_mis" = "Ambiguity * Plural Attractor",
    "c_cons:c_ung:c_mis" = "Ambiguity * Grammaticality Illusion \n (Ambiguity * Ungram. * Pl. Attractor)"
)

exp1.prior.table <- brms::prior_summary(exp1.mresp) %>% as.data.frame() %>% dplyr::select(-resp, -dpar, -nlpar, -bound, -source)

exp1.prior.table %<>% kableExtra::kbl(format = "latex", caption="caption", col.names = c("Prior", "Class", "Coefficient", "Group"))

exp1.pmresp <- create_model_coefs_plot(
    exp1.mresp,
    plot_stats = T, map_names = exp1.coefs,
    expand_right = 1, expand_top = 1.5,
    x_stat_adjust = 0.5, x_breaks = -4:3
) + annotate(
    x = -4, xend = 3, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)") 

exp1.formula.u <- response_yes ~ c_cons * c_mis + l_trial +
                            (c_mis + 1 | subject) +
                            (c_mis + l_trial + 1 | item)

exp1.mresp.u <- brm(
    formula = exp1.formula.u,
    data = filter(exp1.dfModel, grammatical != "grammatical"),
    family = bernoulli("probit"),
    prior = exp1.priors,
    chains = config$exp1$model$chains,
    cores = config$exp1$model$cores,
    iter = config$exp1$model$iter,
    warmup = config$exp1$model$warmup,
    init_r = .1,
    file = paste0(config$exp1$model$path, "exp1.mresp.u")
)

exp1.coefs.u <- c(
    "c_mis" = "Plural Attactor",
    "c_cons" = "Ambiguity",
    "l_trial" = "Trial No (log)",
    "c_cons:c_mis" = "Ambiguity * Plural Attractor"
)

exp1.pmresp.u <- create_model_coefs_plot(
    exp1.mresp.u,
    plot_stats = T, map_names = exp1.coefs.u,
    expand_right = 1, expand_top = 2.6,
    x_stat_adjust = 1.25, x_breaks = -4:3
) + annotate(
    x = -4, xend = 3, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)") 

exp1.mdf <- fixef(exp1.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp1.mdf)) {
    temp <- print_estimate_with_ci(exp1.mresp, i)
    exp1.mdf$text[count] = temp
    count = count + 1
}

exp1.mdf.u <- fixef(exp1.mresp.u) %>% as.data.frame()
count = 1
for (i in row.names(exp1.mdf.u)) {
    temp <- print_estimate_with_ci(exp1.mresp.u, i)
    exp1.mdf.u$text[count] = temp
    count = count + 1
}


exp1.models <- list(
    Model = exp1.mresp,
    dfModel = exp1.mdf,
    Plot = exp1.pmresp,
    ungramModel = exp1.mresp.u,
    dfUngramModel = exp1.mdf.u,
    ungramPlot = exp1.pmresp.u
)

#saveRDS(exp1.models, file = config$exp1$out$model)
