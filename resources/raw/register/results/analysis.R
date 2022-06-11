# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get the csv
# consider using rPraat next time? 
library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(janitor)
library(brms)
library(ggpubr)
library(cowplot)
library(bayesplot)

df <- read_excel("results_2021-01-27T15_03_57_033Z_SA_AAF.xlsx")
colnames(df) %<>%  make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes") 
practice <- df %>% subset(type == "practice" & !is.na(question_null_if_none))

practice %>% subset(whether_or_not_answer_was_correct_null_if_n_a == "NULL")
practice$whether_or_not_answer_was_correct_null_if_n_a[practice$whether_or_not_answer_was_correct_null_if_n_a == "NULL"] <- 0
practice$whether_or_not_answer_was_correct_null_if_n_a %<>%  as.integer()
bad_subjects_by_practice <- practice %>% group_by(results_index) %>% 
  summarize(p_yes = mean(whether_or_not_answer_was_correct_null_if_n_a)) %>% subset(p_yes <=0.5) %>% .$results_index
results <- df %>% subset(type == "condition_datve" | type == "condition_ve" | type == "condition_yada" | type == "condition_datyada" ) %>% subset(!is.na(question_null_if_none))
results %<>% subset(results_index != 22)
results %<>% subset(results_index != 44)


results %<>% select(-time, -counter, -hash, -logged_in_as_experiment_owner_if_known,
                    -element_number, -field_name, -field_value, -sentence_or_sentence_md5,
                    -question_null_if_none, -whether_or_not_answer_was_correct_null_if_n_a)


results$response_yes <- ifelse(grepl("P'ye",results$answer) , T, 
                               ifelse(grepl("Q'ya",results$answer) , F, NA))

results %<>% select(-answer) 
results %<>% subset(!is.na(response_yes))

results$grammatical <- ifelse(results$type == "condition_datve" | results$type == "condition_datyada", T, F)
results$dat <- ifelse(results$type == "condition_datve" | results$type == "condition_datyada", T, F)
results$conj <- ifelse(results$type == "condition_datve" | results$type == "condition_ve", T, F)

results %<>% mutate(ResponseCorrect = (response_yes == (grammatical == T) ) )
results %<>% subset(!(time_taken_to_answer > 2000 | time_taken_to_answer < 400)) 

avg_clean <- list()
avg_clean$resp <- results %>% 
  plyr::ddply(c("type"), function(df) {
    df %>% se_cousineau(n_conditions = 4, results_index, DV = response_yes, 
                        group = c("dat", "conj"), 
                        is_proportion = TRUE)
  })

avg_clean$rt <- results %>%
  plyr::ddply(c("type"), function(df) {
    df %>% se_cousineau(n_conditions = 4, results_index, DV = time_taken_to_answer, 
                        group = c( "dat", "conj"), 
                        is_proportion = FALSE)
  })

avg_clean$rt_correct <- results %>% subset(ResponseCorrect) %>%
  plyr::ddply(c("type"), function(df) {
    df %>% se_cousineau(n_conditions = 4, results_index, DV = time_taken_to_answer, 
                        group = c("dat", "conj"), 
                        is_proportion = FALSE)
  })

pd <- position_dodge(0.1)
p_avg_resp <- avg_clean$resp %>%
  ggplot(aes(dat, M, #linetype = attractor_num, 
             color = conj, group = conj)) + 
  geom_point(position = pd) + geom_line(position = pd)

p_avg_resp <- p_avg_resp + geom_errorbar(aes(ymin = M - 1.96*SE, ymax = M + 1.96*SE), width = 0.1, position = pd)

# p_avg_resp <- p_avg_resp + geom_line(data = avg_fillers) + 
#                             geom_point(data = avg_fillers) + 

p_avg_resp <- p_avg_resp + theme( strip.background = element_rect(fill="white") ) +
  theme_bw() + xlab("") + ylab("Percentage 'acceptable'")
p_avg_resp <- p_avg_resp + scale_y_continuous(labels=scales::percent)#, breaks = c(0, .25, .5, .75, 1))
p_avg_resp <- p_avg_resp + theme_bw()
p_avg_resp <- p_avg_resp + scale_color_discrete(name = "Conjoiner", labels = c("ya da", "ve")) + scale_x_discrete(labels = c("Suspended Dative", "No Suspended Dative")) + theme(text = element_text(size = 30))


avg_by_item <- results %>%
  group_by(group, type, dat, conj) %>%
  summarize(avRT = mean(time_taken_to_answer), 
            p_yes = mean(response_yes, na.rm = T), 
            N = sum(!is.na(response_yes))  )

# reformat by-subject averages to a wide format
avg_by_item_wide <- avg_by_item %>% 
  ungroup() %>%
  dplyr::select(-avRT, -N,-conj, -dat) %>%
  tidyr::spread(type, p_yes) %>% 
  mutate(delta_yada = condition_datyada - condition_yada,
         delta_ve = condition_datve - condition_ve,
         delta_dat = condition_datve - condition_datyada,
         delta_nosa = condition_ve - condition_yada)

results$c_dat <- ifelse(results$dat == 0, .5, -.5)
results$c_ve <- ifelse(results$conj == 0, .5, -.5)

model <- brm(
  response_yes ~ c_dat*c_ve + (c_dat*c_ve | results_index) + (c_dat*c_ve | group),
  family = bernoulli("logit"),
  data = results,
  core = 4,
  file = "./model2.rds"
)

summary(model)
color_scheme_set("red")
bayesplot_theme_set(theme_default(base_size = 30, base_family = "Fira Sans"))
model.coef.plot <-
  mcmc_areas(
    model,
    pars = c("b_c_dat", "b_c_ve", "b_c_dat:c_ve"),
    point_est = "median",
    prob = 0.8, prob_outer = 0.95
  ) 
model.coef.plot <- model.coef.plot + 
  scale_y_discrete(labels = c(
    "SA",
    "or",
    "Interaction\n SA * or"))
model.coef.plot <- model.coef.plot + vline_0(color = "blue", linetype = 2)
