set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "patchwork", "ggstatsplot")
lapply(packages, require, character.only = TRUE)

theme_set(theme_bw(base_family = "Times"))

# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp3$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")

## PLOTS -------
exp3.plots <- list()


exp3.gram.label <- c(
    gram = "Grammatical\n(Singular Verb)",
    ungram = "Ungrammatical\n(Plural Verb)"
)


# responses 

exp3.resp.avg.plot <- exp3.avgs %>%
    ggplot(aes(bias, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp3.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Response Bias") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    scale_x_discrete(labels = c("Grammaticality", "Ungrammaticality")) +
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp3.plots$avgResp = exp3.resp.avg.plot



# rt_correct
# 
exp3.rt.avg.plot <- exp3.avgs %>%
    ggplot(aes(bias, rt_correct.M,
        linetype = rt_correct.attractor_num,
        group = rt_correct.attractor_num
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~rt_correct.grammatical,
        labeller = labeller(rt_correct.grammatical = exp3.gram.label)
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
    scale_x_discrete(labels = c("Grammaticality", "Ungrammaticality")) +
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp3.plots$avgRT = exp3.rt.avg.plot



# missing data
exp3.bias.label <- c(
    gram="Grammaticality Bias",
    ungram="Ungrammaticality Bias"
)

exp3.miss.plot <-
exp3.diff %>%
    group_by(exp_condition, grammatical,
            attractor_num, experiment, biasManipulation) %>%
    summarize(n = n(), perc = n() / nrow(exp3.no.practice)) %>%
    ggplot(., aes(y = perc, x = attractor_num,
                fill = grammatical, group = grammatical)) +
    geom_bar(stat = "identity", 
            position = position_dodge2(width = 0.0)) +
    facet_wrap(~biasManipulation, ncol =  2,
                labeller = labeller(biasManipulation = exp3.bias.label)) +
    theme(strip.background = element_rect(fill = "white")) +
    xlab("") +
    ylab("Proportion of Missing Values") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name = "Grammaticality",
                        labels = c("Grammatical", "Ungrammatical")) +
    scale_x_discrete(labels = c("Filler", 
                                "Experimental\nPL Attractor",
                                "Experimental\nSG Attractor")) +
    theme_minimal(base_family = "Times") +
    theme(legend.position = 'bottom')

exp3.plots$missPlot = exp3.miss.plot


exp3.fresp.avg.plot <- exp3.f.avgs %>%
    ggplot(aes(bias, resp.M, linetype = resp.grammatical, group = resp.grammatical)) + 
    geom_point() + geom_line() +
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    facet_wrap(~resp.experiment, 
                labeller = labeller(resp.experiment = c(filler = "Fillers"))) +
    theme(strip.background = element_rect(fill="white")) +
    xlab("Response Bias") + 
    ylab("Percentage Correct") + 
    scale_y_continuous(labels=scales::percent) +
    scale_linetype_discrete(name = "Grammaticality", 
                        labels = c("Grammatical", "Ungrammatical")) +
    scale_x_discrete(labels = c("Grammaticality", "Ungrammaticality")) + 
    theme_minimal(base_family = "Times") +
    theme(legend.position = 'bottom')

exp3.plots$avgFill = exp3.fresp.avg.plot



# BIAS PLOTS


exp3.bf.bias.sub <- 
    ggstatsplot::ggbetweenstats(
        data = exp3.bias.filler,
        x = biasManipulation,
        y = bias,
        type = "bayes",
        plot.type = "box",
        bf.prior =  "ultrawide",
    ) %>% ggstatsplot::extract_stats() %>% .$subtitle_data %>% .$expression %>% .[[1]]

exp3.plots$bfBiasSub = exp3.bf.bias.sub

exp3.bias.plot <- 
    ggstatsplot::ggbetweenstats(
        data = exp3.bias.filler,
        x = biasManipulation,
        y = bias,
        xlab = "Bias Manipulation Condition",
        ylab = "Estimated Bias",
        type = "bayes",
        centrality.plotting = FALSE,
        plot.type = "box",
        bf.prior =  "ultrawide",
        results.subtitle = F,
        #package = "wesanderson",
        #palette = "GrandBudapest1"
    ) + 
    scale_x_discrete(
        labels = c("gram" = "Grammaticality", "ungram" = "Ungrammaticality")
    ) +
    scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) + 
    theme_minimal(base_family = "Times") +
    theme(legend.position = 'none')

exp3.plots$bfOur = exp3.bias.plot


# hammerly

sub_hammerlybias_exp <- extract_subtitle(hammerlybias_exp)
exp3.plots$bfHExp = sub_hammerlybias_exp
p_hammerlybias_exp <- hammerly_bias_plot(hammerlybias_exp) + ggtitle("A")

sub_hammerlybias_fill <- extract_subtitle(hammerlybias_fill)
exp3.plots$bfHFill = sub_hammerlybias_fill
p_hammerlybias_filler <- hammerly_bias_plot(hammerlybias_fill) + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    ggtitle("B")

p_hammerlybias <- p_hammerlybias_exp + p_hammerlybias_filler
exp3.plots$bfHammerly = p_hammerlybias


# saveRDS(exp3.plots, file = config$exp3$out$plot)
