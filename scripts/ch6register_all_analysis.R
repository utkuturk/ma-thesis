# config = yaml::read_yaml("scripts/config.yml")

library(readxl)
library(janitor)
exp4 <- read_excel(config$exp4$data)
colnames(exp4) %<>%  make_clean_names()
exp4 %<>% subset(logged_in_as_experiment_owner_if_known != "yes") 

subject_id <- with(exp4, { as.integer(as.factor(paste(time, hash))) })
exp4$item_number[exp4$type == "intro" | exp4$type == "practice"] <- 0
exp4$item_number <- as.integer(exp4$item_number)
exp4$subject <- sprintf("S[%d]", subject_id + 7000)
exp4$item <- sprintf("I[%d]", exp4$item_number + 7000)
exp4.forms <- exp4 %>% subset(controller_name != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
exp4 %<>% subset(controller_name == "DashedAcceptabilityJudgment")
age <- exp4.forms %>% dplyr::filter(field_name == "age") %>% 
    dplyr::select(subject, age = field_value)
age$age %<>% asi()
age %<>% subset(!is.na(age))
natturk <- exp4.forms %>% dplyr::filter(field_name == "natturk") %>% 
    dplyr::select(subject, natturk = field_name) %T>% 
    { .$natturk %<>% recode(male ="nat_turk", female = "nat_non_turk") } 
exp4.forms <- dplyr::left_join(age, natturk, by = "subject")

stopifnot( nrow(exp4) %% 2 == 0 )
rows_stim <- exp4[seq_len(nrow(exp4)) %% 2 == 1, ]
rows_resp <- exp4[seq_len(nrow(exp4)) %% 2 == 0, ]
stopifnot( all(is.na( rows_stim$time_taken_to_answer )) )
exp4 <- rows_resp %>% left_join(exp4.forms, by = "subject")


#practice %>% subset(whether_or_not_answer_was_correct_null_if_n_a == "NULL")
exp4.practice <- exp4 %>% subset(type == "practice" & !is.na(question_null_if_none))
exp4.practice$whether_or_not_answer_was_correct_null_if_n_a[exp4.practice$whether_or_not_answer_was_correct_null_if_n_a == "NULL"] <- 0
exp4.practice$whether_or_not_answer_was_correct_null_if_n_a %<>%  as.integer()
exp4.bad_subjects_by_practice <- exp4.practice %>% group_by(subject) %>% 
  summarize(p_yes = mean(whether_or_not_answer_was_correct_null_if_n_a)) %>% subset(p_yes <=0.5) %>% .$subject
exp4 <- exp4 %>% subset(grepl("utku", type)) %>% subset(!is.na(question_null_if_none))
exp4 %<>% subset(!subject %in% exp4.bad_subjects_by_practice)

exp4.deletion <- exp4.bad_subjects_by_practice %>% length()


exp4 %<>% group_by(subject) %>% mutate(trial_no = seq(subject)) %>% ungroup()
exp4 %<>% mutate( late_response = (answer == "NULL"), answer = ifelse(late_response, NA, as.character(answer)) )
responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
exp4$answer %<>% as.character() %>% enc2native()
stopifnot( all(exp4$answer %in% responses | is.na(exp4$answer) ) )

exp4$response_yes <- ifelse(grepl("P'ye",exp4$answer) , T, ifelse(grepl("Q'ya",exp4$answer) , F, NA))
print( with(exp4, table(answer, response_yes)) )
exp4 %<>% select(-answer)

exp4 %<>% select(-time, -counter, -hash, -logged_in_as_experiment_owner_if_known,
                    -element_number, -field_name, -field_value, -sentence_or_sentence_md5,
                    -question_null_if_none, -whether_or_not_answer_was_correct_null_if_n_a, -controller_name, -results_index, -item_number)

exp4.conditions <- data.frame(
  type = c("filler_utku_informal_sgpl", "filler_utku_informal_sgsg", "filler_utku_formal_sgpl", "filler_utku_formal_sgsg", "filler_utku_informal_plsg", "filler_utku_informal_plpl", "filler_utku_formal_plsg", "filler_utku_formal_plpl"),
  experiment =    rep("AgrAttr", 8),
  condition =     c("informal_c", "informal_d", "formal_c", "formal_d", "informal_b", "informal_a", "formal_b", "formal_a"),
  grammatical =   c("ungram", "gram", "ungram", "gram", "gram", "ungram", "gram", "ungram"),
  verb_num =      c("pl", "sg", "pl", "sg", "sg", "pl", "sg", "pl"),
  attractor_num = c( rep("sg",4), rep("pl",4)),
  match =         c("mismatch", "match", "mismatch", "match", "mismatch", "match", "mismatch", "match"),
  register = c(rep(c(rep("informal", 2), rep("formal", 2)), 2) ),
  stringsAsFactors = T
)

exp4 %<>% left_join(exp4.conditions, by = "type")

exp4 %<>% rename(RT = time_taken_to_answer)

# exp4.clean <- exclude_bad_subjects_8_formal(
#   exp4,
#   accuracy_threshold = config$exp4$thresholds$accuracy,
#   rt_below = config$exp4$thresholds$rt_below,
#   rt_upper = config$exp4$thresholds$rt_upper
# )

exp4 %<>% subset(!is.na(response_yes))

exp4 %<>% subset(!(RT > config$exp4$thresholds$rt_upper | RT < config$exp4$thresholds$rt_below)) 
exp4 %<>% mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

exp4.avg <- get_averages(
    exp4, 
    grouping = c('register', 'grammatical', 'attractor_num')
) %>% as.data.table()

## Text Inputs ======
exp4.nsubj <- quantify_subjects(exp4)
exp4.meanage <- mean(asi(subset(exp4$age, !is.na(exp4$age)))) %>% round()
exp4.maxage <- max(asi(subset(exp4$age, !is.na(exp4$age))))
exp4.minage <- min(asi(subset(exp4$age, !is.na(exp4$age))))

# (G)ram or (U)ngram / (P)lural or (S)ingular Att / (F)ormal or (I)nformal / exp(4) 
GPFM4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.register == "formal"]$resp.M,2)
GPFSE4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.register == "formal"]$resp.SE,2)

GPIM4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.register == "informal"]$resp.M,2)
GPISE4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.register == "informal"]$resp.SE,2)

GSFM4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.register == "formal"]$resp.M,2)
GSFSE4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.register == "formal"]$resp.SE,2)

GSIM4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.register == "informal"]$resp.M,2)
GSISE4 <- round(exp4.avg[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.register == "informal"]$resp.SE,2)

UPFM4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.register == "formal"]$resp.M,2)
UPFSE4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.register == "formal"]$resp.SE,2)

UPIM4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.register == "informal"]$resp.M,2)
UPISE4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.register == "informal"]$resp.SE,2)

USFM4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.register == "formal"]$resp.M,2)
USFSE4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.register == "formal"]$resp.SE,2)

USIM4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.register == "informal"]$resp.M,2)
USISE4 <- round(exp4.avg[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.register == "informal"]$resp.SE,2)

## Plots =====
set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "patchwork", "ggstatsplot")
lapply(packages, require, character.only = TRUE)

theme_set(theme_bw(base_family = "Times"))

exp4.plots <- list()

exp4.gram.label <- c(
    gram = "Grammatical\n(Singular Verb)",
    ungram = "Ungrammatical\n(Plural Verb)"
)

exp4.resp.avg.plot <- exp4.avg %>%
    ggplot(aes(resp.register, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical,
                labeller = labeller(resp.grammatical = exp4.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Register") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp4.plots$avgResp = exp4.resp.avg.plot

exp4.rt.avg.plot <- exp4.avg %>%
    ggplot(aes(resp.register, rt_correct.M,
        linetype = rt_correct.attractor_num,
        group = rt_correct.attractor_num
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~rt_correct.grammatical,
        labeller = labeller(rt_correct.grammatical = exp4.gram.label)
    ) +
    geom_errorbar(aes(
        ymin = rt_correct.M - 1.96 * rt_correct.SE,
        ymax = rt_correct.M + 1.96 * rt_correct.SE
    ),
    width = 0.1
    ) +
    theme(strip.background = element_rect(fill="white")) +
    xlab("Response Bias") + 
    ylab("Response Times")  + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp4.plots$avgRT = exp4.rt.avg.plot

## Models =====
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
options(brms.file_refit = config$refit)

exp4.dfModel <- exp4

exp4.dfModel %<>% within(., {
    c_formal <- ifelse(register == "formal", .5, -.5)
    c_ung <- ifelse(grammatical == "ungram", .5, -.5)
    c_att <- ifelse(attractor_num == "pl", .5, -.5)
    l_trial <- log(trial_no)
})

exp4.formula <- response_yes ~ c_formal * c_ung * c_att + l_trial + 
                            (c_formal * c_ung * c_att + 1 | subject) +
                            (c_formal * c_ung * c_att + l_trial + 1 | item)

exp4.priors <- c(
    set_prior("student_t(3,0,2.5)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b"),
    set_prior("cauchy(0,1)", class = "sd"),
    set_prior("lkj(2)", class = "cor")
)

exp4.mresp <- brm(
    formula = exp4.formula,
    data = exp4.dfModel,
    family = bernoulli("probit"),
    prior = exp4.priors,
    chains = config$exp4$model$chains,
    cores = config$exp4$model$cores,
    iter = config$exp4$model$iter,
    warmup = config$exp4$model$warmup,
    init_r = .1,
    file = paste0(config$exp4$model$path, "exp4.mresp")
)

exp4.coefs <- c(
    "c_formal" = "Formal Reg.",  
    "c_ung" = "Ungrammaticality", 
    "c_att" = "Plural Attactor", 
    "l_trial" = "Trial No (log)",
    "c_formal:c_ung" = "Reg. * Ung.", 
    "c_formal:c_att" = "Reg. * Pl. Att.",
    "c_ung:c_att" = "Ung. * Pl. Att.",
    "c_formal:c_ung:c_att" = "Reg. * Gram. Illusion \n       (Ung. * Pl. Att.)"
)

exp4.pmresp <- create_model_coefs_plot(
    exp4.mresp,
    plot_stats = T, map_names = exp4.coefs,
    expand_right = 1, expand_top = 1.5,
    x_stat_adjust = 0.5, x_breaks = -3:1
) + annotate(
    x = -3, xend = 1, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")

exp4.mdf <- fixef(exp4.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp4.mdf)) {
    temp <- print_estimate_with_ci(exp4.mresp, i)
    exp4.mdf$text[count] = temp
    count = count + 1
}

exp4.models <- list(
    Model = exp4.mresp,
    dfModel = exp4.mdf,
    Plot = exp4.pmresp
)

