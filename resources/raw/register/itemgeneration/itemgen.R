#setwd active document
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#packages
library(magrittr)
library(janitor)
library(googledrive)
library(readxl)
library(tidyr)
library(tidyverse)

#read file
drive_download("Suspended-Affixation-experiment2", type = "xlsx", overwrite = T)
stimFile <- "Suspended-Affixation-experiment2.xlsx"
experimental <- read_excel(stimFile, sheet = "SA_Items")
filler_utku <- read_excel(stimFile, sheet = "Formal_AgreementAtt")
filler_furkan <- read_excel(stimFile, sheet = "Furkan_filler")

experimental %<>% .[1:40,]
filler_utku %<>% .[1:40,]


# prepare conditions
experimental$condition_datve <- with(experimental, paste(adj, datve, pnoun, verb, sep = " "))
experimental$condition_ve <- with(experimental, paste(adj, ve, pnoun, verb, sep = " "))
experimental$condition_datyada <- with(experimental, paste(adj, datyada, pnoun, verb, sep = " "))
experimental$condition_yada <-with(experimental, paste(adj, yada, pnoun, verb, sep = " "))

filler_utku$filler_utku_formal_sgsg <- with(filler_utku, paste(dp_gen_sg, noun_poss, adjunct, mv_sg, formal, sep =" "))
filler_utku$filler_utku_formal_sgpl <- with(filler_utku, paste(dp_gen_sg, noun_poss, adjunct, mv_pl, formal, sep =" "))
filler_utku$filler_utku_formal_plsg <- with(filler_utku, paste(dp_gen_pl, noun_poss, adjunct, mv_sg, formal, sep =" "))
filler_utku$filler_utku_formal_plpl <- with(filler_utku, paste(dp_gen_pl, noun_poss, adjunct, mv_pl, formal, sep =" "))
filler_utku$filler_utku_informal_sgsg <- with(filler_utku, paste(dp_gen_sg, noun_poss, adjunct, mv_sg, informal, sep =" "))
filler_utku$filler_utku_informal_sgpl <- with(filler_utku, paste(dp_gen_sg, noun_poss, adjunct, mv_pl, informal, sep =" "))
filler_utku$filler_utku_informal_plsg <- with(filler_utku, paste(dp_gen_pl, noun_poss, adjunct, mv_sg, informal, sep =" "))
filler_utku$filler_utku_informal_plpl <- with(filler_utku, paste(dp_gen_pl, noun_poss, adjunct, mv_pl, informal, sep =" "))
#extract
## exp items

stim_exp <- experimental %>% select(item, condition_datve:condition_yada)
stim_exp %<>% gather(condition, sentence, condition_datve:condition_yada)
stim_exp %<>% arrange(item,condition)
stim_exp$ibex <- with(stim_exp, sprintf('[["%s", %d], "DashedAcceptabilityJudgment", {s: "%s"}]', condition, item, sentence
))

stim_filler_utku <- filler_utku %>% select(item, filler_utku_formal_sgsg:filler_utku_informal_plpl)
stim_filler_utku %<>% gather(condition, sentence, filler_utku_formal_sgsg:filler_utku_informal_plpl)
stim_filler_utku %<>% arrange(item,condition)
stim_filler_utku$ibex <- with(stim_filler_utku, sprintf('[["%s", %d], "DashedAcceptabilityJudgment", {s: "%s"}]', condition, item, sentence
))

stim_filler_furkan <- filler_furkan
stim_filler_furkan %<>% arrange(item,condition)
stim_filler_furkan$ibex <- with(stim_filler_furkan, sprintf('[["%s", %d], "DashedAcceptabilityJudgment", {s: "%s"}]', condition, item, sentence
))

# as a string
stim_exp_string <- paste(stim_exp$ibex, collapse = ",\n")
stim_filler_utku_string <- paste(stim_filler_utku$ibex, collapse = ",\n")
stim_filler_furkan_string <- paste(stim_filler_furkan$ibex, collapse = ",\n")
#stim_exp_string %<>% utf8::utf8_encode()

#stim_fill_string <- paste(stim_fill$ibex, collapse = ",\n")
#stim_fill_string %<>% utf8::utf8_encode()

# create a link for the file
exp_js_file <- "../data_includes/experiment.js"
# remove the existing file.
if (file.exists(exp_js_file)) {file.remove(exp_js_file)}

# put everything together
file.copy("js_top", exp_js_file)
cat(stim_exp_string, file = exp_js_file, append = T )
cat(", \n", file = exp_js_file, append = T)
cat(stim_filler_utku_string, file = exp_js_file, append = T )
cat(", \n", file = exp_js_file, append = T)
cat(stim_filler_furkan_string, file = exp_js_file, append = T )
cat(readLines("js_foot", encoding = "utf-8"), 
    file = exp_js_file, append = T)

