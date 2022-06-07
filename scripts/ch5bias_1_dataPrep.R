# config = yaml::read_yaml("scripts/config.yml")

# DATA -------

exp3.gram <- read_experimental_data(config$exp3$gramData) %>% mutate(biasManipulation = "gram")
exp3.ungram <- read_experimental_data(config$exp3$ungramData, 
                                    subj_offset = 100) %>% mutate(biasManipulation = "ungram")

exp3.conditions <- data.frame(
  exp_condition = c("practice", "condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
  experiment =    c("practice", "AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
  condition =     c("practice", "a",           "b",           "c",           "d",           "filler_ung", "filler_g"),
  grammatical =   c("practice", "ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
  verb_num =      c("practice", "pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
  attractor_num = c("practice", "pl",          "pl",          "sg",          "sg",          'filler',     'filler'),
  match =         c("practice", "mismatch",    "mismatch",    "match",       "match",       'filler',     'filler'),
  stringsAsFactors = T
)

exp3.df <- dplyr::bind_rows(exp3.gram, exp3.ungram)

exp3.df %<>% left_join(exp3.conditions, by = "exp_condition")

exp3.no.practice <- exp3.df %>%
                          subset(exp_condition != "practice")

exp3.clean <- exclude_bad_subjects(
  exp3.df,
  accuracy_threshold = config$exp3$thresholds$accuracy,
  rt_below = config$exp3$thresholds$rt_below,
  rt_upper = config$exp3$thresholds$rt_upper
)

exp3.clean %<>% no_null_no_practice(.)

stopifnot(exp3.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)


# DIFF DATA =====
exp3.diff <- dplyr::anti_join(exp3.df, exp3.clean) %>%
  filter(exp_condition != "practice") %>%
  filter(natturk != "nat_turk")

exp3.clean$isGram <- ifelse(exp3.clean$grammatical == "ungram", F, T)
exp3.clean$p_acc <- with(exp3.clean, response_yes & isGram)
exp3.clean %<>% 
  mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# NORMING =====

exp3.pilot <- read_experimental_data(config$exp3$pilotData)

exp3.pilot %<>% mutate(exp_condition = case_when(
  exp_condition == "filler" & item <= 130 ~ "filler_g",
  exp_condition == "filler" & item >= 131 ~ "filler_ung",
  exp_condition == "practice" ~ "practice",
  exp_condition == "condition_b" ~ "condition_b",
  exp_condition == "condition_a" ~ "condition_a",
  exp_condition == "condition_c" ~ "condition_c",
  exp_condition == "condition_d" ~ "condition_d"
))

exp3.pilot %<>% left_join(exp3.conditions, by = "exp_condition")

exp3.pilot %<>% exclude_bad_subjects(., 
                  accuracy_threshold = config$exp3$thresholds$accuracy,
                  rt_below = config$exp3$thresholds$rt_below,
                  rt_upper = config$exp3$thresholds$rt_upper)
exp3.pilot %<>% no_null_no_practice(.)

exp3.pilot$isGram <- ifelse(exp3.pilot$grammatical == "ungram", F, T)
exp3.pilot$p_acc <- with(exp3.pilot, response_yes & isGram)
exp3.pilot %<>%
  mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

exp3.avg.pilot <- get_averages(exp3.pilot,
                            grouping = c('experiment', 'grammatical', 'attractor_num', 'match')) %>% 
  as.data.table()

# LIT REVIEW DATA ===== 
## HAMMERLY DATA ======

hammerlydata1 = read.csv(config$litRev$hammerly1)
hammerlydata3 = read.csv(config$litRev$hammerly3)
hammerlydata1$exp = '1'
hammerlydata3$exp = '3'
hammerlydata3$subj %<>%  + 100

hammerlydata = bind_rows(hammerlydata1, hammerlydata3)
hammerlydata %<>% filter(response != "NULL")

# BIAS ======

## OUR BIAS =======
exp3.fillers <- exp3.clean %>% subset(!is.na(response_yes) & experiment == "filler")
exp3.fillers %<>% select(item, RT, subject, response_yes, grammatical, ResponseCorrect, biasManipulation)

exp3.bias.filler <- exp3.fillers %>% 
  group_by(subject, grammatical, biasManipulation) %>%
  mutate(correct = sum(ResponseCorrect) +.5 ) %>% 
  mutate(numItem = n_distinct(item)+1) %>%  
  mutate(mean = correct/numItem) %>% 
  group_by(subject, grammatical, biasManipulation) %>%
  summarise(average = mean(mean)) %>%
  spread(grammatical, average) %>%
  mutate(FA = 1 - ungram, hit = gram) %>%  
  group_by(subject, biasManipulation) %>%
  summarise(bias = -.5*(qnorm(hit)+qnorm(FA)))

# find a better way to distract this data
bar_jitter_bias <-
  ggplot(exp3.bias.filler) +
  stat_summary(
    mapping = aes(x = biasManipulation, y = bias, fill = biasManipulation),
    geom = "col", fun.y = "mean", color = "black"
  ) +
  geom_point(
    mapping = aes(x = biasManipulation, y = bias, fill = biasManipulation),
    shape = 21, size = 3, position = position_jitter(width = 0.2, height = 0)
  ) +
  stat_summary(
    mapping = aes(x = biasManipulation, y = bias),
    geom = "errorbar", fun.data = "mean_se",
    color = "black", size = 2, width =  0
  )

exp3.bias.plot.avgs = layer_data(bar_jitter_bias, 3)


exp3.bias <- exp3.bias.filler %>% select(-biasManipulation)

exp3.clean %<>% left_join(., exp3.bias, by = 'subject') 

exp3.clean %<>% mutate(c_valueFiller = ifelse(bias < 0, "negative", "positive" ))

## HAMMERLY BIAS ======

hammerlybias_exp <- 
  hammerlydata %>% filter(., stimulustype != 'Filler') %>% 
  hammerly_bias_calc()


hammerlybias_fill <- 
  hammerlydata %>% filter(., stimulustype == 'Filler') %>% 
  hammerly_bias_calc()

# SENSITIVITY =====
# By-item Sensitivity calculation using singular attractor conditions


# df_sensitivity <- exp3.clean %>% subset(!is.na(response_yes) & attractor_num == "sg")
# df_sensitivity %<>% select(item, RT, subject, response_yes, grammatical, ResponseCorrect, biasManipulation)


# calculate_d <- function(data) {
#   data %>%
#   group_by(subj, Grammaticality, exp) %>%
#   mutate(correct = sum(Accuracy) +.5 ) %>% 
#   mutate(numItem = n_distinct(item)+1) %>%  
#   mutate(mean = correct/numItem) %>% 
#   group_by(subj, Grammaticality, exp) %>%
#   summarise(average = mean(mean)) %>%
#   spread(Grammaticality, average) %>%
#   mutate(FA = 1 - Ungrammatical, hit = Grammatical) %>%  
#   group_by(subj, exp) %>%
#   summarise(bias = -.5*(qnorm(hit)+qnorm(FA)))
# }

# AVERAGES =====

exp3.avg.g <- get_averages(
  subset(exp3.clean, c_valueFiller == "negative"), 
  grouping = c('experiment', 'grammatical', 'attractor_num', 'match')
) %>% as.data.table()

exp3.avg.u <- get_averages(
  subset(exp3.clean, c_valueFiller == "positive"), 
  grouping = c('experiment', 'grammatical', 'attractor_num', 'match')
) %>% as.data.table()

exp3.avg.u$bias <- "ungrammatical"
exp3.avg.g$bias <- "grammatical"


exp3.avgs <- bind_rows(
  subset(exp3.avg.u, resp.experiment != "filler"), 
  subset(exp3.avg.g, resp.experiment != "filler")
)

#avg_g_resp <- avg_g[, .SD, .SDcols = names(avg_g) %like% "resp"]
#avg_u_resp <- avg_u[, .SD, .SDcols = names(avg_u) %like% "resp"]

#avg_g_rt <- avg_g[, .SD, .SDcols = names(avg_g) %like% "rt_correct"]
#avg_u_rt <- avg_u[, .SD, .SDcols = names(avg_u) %like% "rt_correct"]


# FILLER AVERAGES =====

exp3.f.avgs <- bind_rows(
  subset(exp3.avg.u, resp.experiment == "filler"), 
  subset(exp3.avg.g, resp.experiment == "filler")
)
exp3.f.avgs[resp.grammatical == "ungram" & bias == "ungrammatical"]$resp.M %<>% "-"(1,.)
exp3.f.avgs[resp.grammatical == "ungram" & bias == "grammatical"]$resp.M %<>% "-"(1,.)


# TEXT INPUTS ========

exp3.nsubj <- quantify_subjects(exp3.df)

exp3.nsubj.nontr <- exp3.df %>%
  subset(natturk == "nat_non_turk") %>%
  .$subject %>%
  unique() %>%
  length()

exp3.nsubj.threshold <- 6

exp3.deletion <- quantify_preprocessing(exp3.no.practice , exp3.clean)

exp3.meanage <- mean(asi(exp3.clean$age)) %>% round()
exp3.maxage <- max(asi(exp3.clean$age))
exp3.minage <- min(asi(exp3.clean$age))

exp3.nsubj.pilot <- quantify_subjects(exp3.pilot)

exp3.gramMean.pilot <- sprintf("%0.2f", exp3.avg.pilot[resp.grammatical == "gram" & resp.attractor_num == "sg"]$resp.M)
exp3.gramSE.pilot <- sprintf("%0.2f", exp3.avg.pilot[resp.grammatical == "gram" & resp.attractor_num == "sg"]$resp.SE)

# BIAS AVERAGES
OGM3 <- sprintf("%0.3f", exp3.bias.plot.avgs[1,]$y)
OGSE3 <- sprintf("%0.3f", (exp3.bias.plot.avgs[1,]$ymax - exp3.bias.plot.avgs[1,]$ymin)/2)
OUM3 <- sprintf("%0.3f", exp3.bias.plot.avgs[2,]$y)
OUSE3 <- sprintf("%0.3f", (exp3.bias.plot.avgs[2,]$ymax - exp3.bias.plot.avgs[2,]$ymin)/2)


# FILLER AVERAGES
FGM3 <- mean(exp3.f.avgs[bias == "grammatical"]$resp.M) %>% round(2)
FGSE3 <- mean(exp3.f.avgs[bias == "grammatical"]$resp.SE) %>% round(2)
FUM3 <- mean(exp3.f.avgs[bias == "ungrammatical"]$resp.M) %>% round(2)
FUSE3 <- mean(exp3.f.avgs[bias == "ungrammatical"]$resp.SE) %>% round(2)


# EXP AVERAGES
# BIAS - GRAM - ATTNUM - mean or se
UGPM3 <- round(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "gram" & resp.attractor_num == "pl"]$resp.M,2)

UGPSE3 <- round(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "gram" & resp.attractor_num == "pl"]$resp.SE,2)

UGSM3 <- round(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "gram" & resp.attractor_num == "sg"]$resp.M,2)

UGSSE3 <- round(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "gram" & resp.attractor_num == "sg"]$resp.SE,2)

GUM3 <- round(mean(exp3.avgs[bias == "grammatical" & resp.grammatical == "ungram"]$resp.M),2)
GUSE3 <- round(mean(exp3.avgs[bias == "grammatical" & resp.grammatical == "ungram"]$resp.SE),2)
UUM3 <- round(mean(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "ungram"]$resp.M),2)
UUSE3 <- round(mean(exp3.avgs[bias == "ungrammatical" & resp.grammatical == "ungram"]$resp.SE),2)

# save.image(file = config$exp3$out$dataPrep)
