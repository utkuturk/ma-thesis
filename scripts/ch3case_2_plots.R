set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "patchwork", "ggstatsplot")
lapply(packages, require, character.only = TRUE)

theme_set(theme_bw(base_family = "Times"))

#config <- yaml::read_yaml("scripts/config.yml")
#load(config$exp1$out$dataPrep)
#config <- yaml::read_yaml("scripts/config.yml")

## PLOTS -------
exp1.plots <- list()


exp1.gram.label <- c(
    grammatical = "Grammatical\n(Singular Verb)",
    ungrammatical = "Ungrammatical\n(Plural Verb)"
)


# responses 

exp1.resp.avg.plot <- exp1.avgs %>%
    ggplot(aes(resp.experiment, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp1.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Experiment") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp1.plots$avgResp = exp1.resp.avg.plot



# rt_correct
# 
exp1.rt.avg.plot <- exp1.avgs %>%
    ggplot(aes(resp.experiment, rt_correct.M,
        linetype = rt_correct.attractor_num,
        group = rt_correct.attractor_num
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~rt_correct.grammatical,
        labeller = labeller(rt_correct.grammatical = exp1.gram.label)
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

exp1.plots$avgRT = exp1.rt.avg.plot



####!!!! FILLER GRAPH

exp1.filler.gHistData <- exp1 %>%
    group_by(subject, experiment, condition, 
             grammatical, verb_num, attractor_num) %>%
    summarize(avRT = mean(RT), 
              p_yes = mean(response_yes, na.rm = T), 
              N = sum(!is.na(response_yes))  ) %>% 
    mutate(expcond = paste(experiment, condition, sep="_")) %>% 
    ungroup() %>%
    dplyr::select(-experiment, -condition, -avRT, -N,
                  -grammatical, -verb_num, -attractor_num) %>%
    tidyr::spread(expcond, p_yes) %>% 
    mutate(delta_dc = AgrAttr_d - AgrAttr_c) %>% 
    .$filler_filler_g

exp1.filler.gHist <- ggplot(data = NULL, aes(exp1.filler.gHistData)) + geom_histogram(bins=15) + 
    theme(strip.background = element_rect(fill="white")) +
    ylab("Count") + 
    xlab("Proportion of Accuracy")  +  
    theme_minimal(base_family = "Times") 

exp1.plots$fgHist = exp1.filler.gHist

#saveRDS(exp1.plots, file = config$exp1$out$plot)
