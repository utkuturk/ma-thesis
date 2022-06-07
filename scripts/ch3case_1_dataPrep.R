config = yaml::read_yaml("scripts/config.yml")

# DATA -------

exp1 = read_experimental_data(config$exp1$data, subj_offset = 1000, item_offset = 1000) 

exp1 %<>% mutate(exp_condition = case_when(
  exp_condition == "filler" & item_num <= 120 ~ "filler_g",
  exp_condition == "filler" & item_num >= 121 ~ "filler_ung",
  exp_condition == "practice" ~ "practice",
  exp_condition == "condition_b" ~ "condition_b",
  exp_condition == "condition_a" ~ "condition_a",
  exp_condition == "condition_c" ~ "condition_c",
  exp_condition == "condition_d" ~ "condition_d"
))


exp1.conditions <- data.frame(
    exp_condition = c("practice", "condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
    experiment =    c("practice", "AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
    condition =     c("practice", "a",           "b",           "c",           "d",           "filler_ung", "filler_g"),
    grammatical =   c("practice", "ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
    verb_num =      c("practice", "pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
    attractor_num = c("practice", "pl",          "pl",          "sg",          "sg",          'filler',     'filler'),
    match =         c("practice", "mismatch",    "mismatch",    "match",       "match",       'filler',     'filler'),
    stringsAsFactors = T
)


exp1 %<>% left_join(exp1.conditions, by = "exp_condition")

exp1.no.practice <- exp1 %>%
                        subset(exp_condition != "practice")

exp1.clean <- exclude_bad_subjects(
    exp1,
    accuracy_threshold = config$exp3$thresholds$accuracy,
    rt_below = config$exp3$thresholds$rt_below,
    rt_upper = config$exp3$thresholds$rt_upper
)

exp1.clean %<>% no_null_no_practice(.)

stopifnot(exp1.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)

# DIFF DATA =====
exp1.diff <- dplyr::anti_join(exp1, exp1.clean) %>%
    filter(exp_condition != "practice") %>%
    filter(natturk != "nat_turk")

exp1.clean$isGram <- ifelse(exp1.clean$grammatical == "ungram", F, T)
exp1.clean$p_acc <- with(exp1.clean, response_yes & isGram)
exp1.clean %<>% 
    mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

exp1.clean$age %<>% gsub("on sekiz", "18", .) %>% as.integer()



# Lit review data


lago = read.csv(config$litRev$lago, encoding = "UTF-8", as.is = T) %>% 
        subset(Group == "monolingual") %>% 
            select(-Accuracy, -L1:-Group, -List:-SelfRateGerman)

with(lago, stopifnot( is.na(Grammatical) == (Experiment == "offline") ))

lago.unp = lago %>% 
                subset(is.na(Grammatical)) %>%
                    select(-Grammatical:-Label)

lago.attr = lago %>% 
                subset(!is.na(Grammatical)) %>%
                    select(-Distance:-NewCond) %>% 
                        mutate(response_yes = (Response == "yes") ) %>% 
                            select(-Response) %>% ungroup() %>%
                                select(
                                    grammatical=Grammatical,
                                    attractor_num=Attractor,
                                    experiment=Experiment,
                                    lagoetal_condition=Condition, 
                                    subject=Participant, 
                                    item=Item,
                                    response_yes,
                                    RT
                                )

lago.attr %<>% group_by(subject) %>% mutate(trial_no = seq(subject))

# map to our condition labels
lago.mapping <- data.frame(
    condition = c("a", "b", "c", "d"),
    lagoetal_condition = c("d", "b", "c", "a"), 
    stringsAsFactors = F
)
lago.attr %<>% left_join( lago.mapping, by = "lagoetal_condition" )
lago.attr %<>% mutate(
    verb_num = case_when(
        grammatical == "ungrammatical" ~ "pl",
        grammatical == "gramatical" ~ "sg"
    ),
    experiment = case_when(
        experiment == "online" ~ "AgrAttr"
    ),
    exp_condition = case_when(
        condition == "a" ~ "condition_a",
        condition == "b" ~ "condition_b",
        condition == "c" ~ "condition_c",
        condition == "d" ~ "condition_d"
    ),
    match = case_when(
        attractor_num == "plural" ~ "mismatch",
        attractor_num == "singular" ~ "match",
    )
)

lago.clean <- exclude_bad_subjects(
    lago.attr,
    accuracy_threshold = config$exp3$thresholds$accuracy,
    rt_below = config$exp3$thresholds$rt_below,
    rt_upper = config$exp3$thresholds$rt_upper
)

lago.clean %<>% no_null_no_practice(.)

stopifnot(lago.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)

# DIFF DATA =====
lago.diff <- dplyr::anti_join(lago.attr, lago.clean) %>%
    filter(exp_condition != "practice")

lago.clean$isGram <- ifelse(lago.clean$grammatical == "ungram", F, T)
lago.clean$p_acc <- with(lago.clean, response_yes & isGram)
lago.clean %<>% 
    mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# MERGE

# merge both datasets
df_merge_exp1 <- exp1.clean %>% ungroup() %>% 
                      dplyr::select(source=experiment, 
                                    grammatical, 
                                    attractor_num,
                                    match,
                                    # condition,
                                    subject, 
                                    trial_no,
                                    item,
                                    response_yes, 
                                    RT,
                                    ResponseCorrect)
df_merge_exp1$experiment <- "Experiment 1"
df_merge_exp1$grammatical %<>% dplyr::recode(gram="grammatical", ungram="ungrammatical")
df_merge_exp1$attractor_num %<>% dplyr::recode(pl="plural", sg="singular")

df_merge_exp1$item %<>% as.factor()
df_merge_exp1$subject %<>% as.character()



df_merge_lago <- lago.clean %>%
                      ungroup() %>% 
                      dplyr::select(grammatical, attractor_num, match,
                                    subject, item, response_yes, RT, ResponseCorrect)
df_merge_lago$experiment <- "Lago et al. (2019)" 
df_merge_lago$source <- NA

## word frequencies: 

# word_freq <- readxl::read_excel(config$exp1$freqData, sheet = 1)
# word_freq$freq_percentage %<>% as.numeric()
# word_freq %<>% dplyr::select(-freq_standardized, -freq_percentage, -word)
# word_freq %<>% tidyr::spread(place, freq_count)

# word_freq %<>% dplyr::group_by(exp) %>% 
#                dplyr::mutate( freq_cat_n1 = ifelse(n1 > median(n1), "high", "low"),
#                               freq_cat_n2 = ifelse(n2 > median(n2), "high", "low"),
#                               freqlog_n1 = scale(log(n1)),
#                               freqlog_n2 = scale(log(n2)),
#                               freqlog_n1n2 = log(n1) - log(n2)
#                             ) %>% 
#                 ungroup()

# word_freq_exp1 <- word_freq %>% subset(exp == "exp1") %>% dplyr::select(-exp)
# df_merge_exp1 %<>% left_join(word_freq_exp1, by = "item")

# word_freq_lago <- word_freq %>% subset(exp == "lagoetal") %>% dplyr::select(-exp)
# df_merge_lago %<>% left_join(word_freq_lago, by = "item")
df_merge_lago$item_num <- as.integer(df_merge_lago$item)
  df_merge_lago$item <- sprintf("I[%d]", df_merge_lago$item_num + 10000)
df_merge_lago %<>% select(-item_num)

exp1.merged <- dplyr::bind_rows(df_merge_exp1, df_merge_lago)
exp1.merged$subject %<>% as.factor()
exp1.merged$item %<>% as.factor()

# Averages


exp1.avg <- get_averages(
    subset(exp1.merged, experiment == "Experiment 1"), 
    grouping = c('experiment', 'grammatical', 'attractor_num', 'match')
) %>% as.data.table()

lago.avg <- get_averages(
    subset(exp1.merged, experiment == "Lago et al. (2019)"), 
    grouping = c('experiment', 'grammatical', 'attractor_num', 'match')
) %>% as.data.table()

exp1.avgs <- bind_rows(
  subset(exp1.avg, resp.attractor_num != "filler"), 
  subset(lago.avg, resp.attractor_num != "filler")
)

#avg_g_resp <- avg_g[, .SD, .SDcols = names(avg_g) %like% "resp"]
#avg_u_resp <- avg_u[, .SD, .SDcols = names(avg_u) %like% "resp"]

#avg_g_rt <- avg_g[, .SD, .SDcols = names(avg_g) %like% "rt_correct"]
#avg_u_rt <- avg_u[, .SD, .SDcols = names(avg_u) %like% "rt_correct"]


# FILLER AVERAGES =====

exp1.f.avgs <- bind_rows(
  subset(exp1.avg, resp.attractor_num == "filler"), 
  subset(lago.avg, resp.attractor_num == "filler")
)

# I want accuracy, not the response yes
exp1.f.avgs[resp.grammatical == "ungrammatical"]$resp.M %<>% "-"(1,.)


# Text Inputs
exp1.nsubj <- quantify_subjects(exp1)
lago.nsubj <- quantify_subjects(lago.attr)

exp1.nsubj.nontr <- exp1 %>%
  subset(natturk == "nat_non_turk") %>%
  .$subject %>%
  unique() %>%
  length()

exp1.nsubj.threshold <- 10
lago.nsubj.threshold <- 1

exp1.deletion <- quantify_preprocessing(exp1.no.practice , exp1.clean)
lago.deletion <- quantify_preprocessing(lago.attr , lago.clean)

exp1.meanage <- mean(asi(exp1.clean$age)) %>% round()
exp1.maxage <- max(asi(exp1.clean$age))
exp1.minage <- min(asi(exp1.clean$age))

# FILLER AVERAGES
FGM1 <- exp1.f.avgs[resp.grammatical == "grammatical"]$resp.M %>% round(2)
FUM1 <- exp1.f.avgs[resp.grammatical == "ungrammatical"]$resp.M %>% round(2)
FGSE1 <- exp1.f.avgs[resp.grammatical == "grammatical"]$resp.SE %>% round(2)
FUSE1 <- exp1.f.avgs[resp.grammatical == "ungrammatical"]$resp.SE %>% round(2)



# EXP AVERAGES
# SOURCE - GRAM - ATTNUM - mean or se
GPM1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.M,2)

GPSE1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.SE,2)

GSM1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.M,2)

GSSE1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.SE,2)

GPML <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.M,2)

GPSEL <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.SE,2)


GSML <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.M,2)

GSSEL <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.SE,2)


UPM1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.M,2)

UPSE1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.SE,2)

USM1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.M,2)

USSE1 <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.SE,2)

UPML <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.M,2)

UPSEL <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.SE,2)


USML <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.M,2)

USSEL <- round(exp1.avgs[resp.experiment == "Lago et al. (2019)" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.SE,2)


## RT ----

USME1RT <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$rt_correct.M,2)

USSE1RT <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$rt_correct.SE,2)

UPME1RT <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$rt_correct.M,2)

UPSE1RT <- round(exp1.avgs[resp.experiment == "Experiment 1" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$rt_correct.SE,2)

#save.image(file = config$exp1$out$dataPrep)

