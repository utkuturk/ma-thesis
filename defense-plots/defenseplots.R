# EXP1=======

library(ggsci)

smp <- function(name, w=10, h=5) {
  ggsave(name, plot=last_plot(), device="png", dpi="retina", width=w, height=h)
}



p_exp1_avgs <- 
  exp1.avgs %>%
  ggplot(aes(resp.experiment, resp.M, 
             color = resp.attractor_num, 
             group = resp.attractor_num)) + 
  geom_point() + geom_line() + 
  facet_wrap(~resp.grammatical, 
             labeller = labeller(resp.grammatical = exp1.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
  xlab("Experiment") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')

smp("exp1avgs.png")


exp1fit <- as.matrix(exp1.mresp)
exp1tbl <- model_summary_(exp1.mresp)
exp1tbl$xmax <- with(exp1tbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
exp1coefnames <- c(
  "c_cons:c_mis" = "b_c_cons:c_mis",
  "c_cons:c_ung" = "b_c_cons:c_ung",
  "c_ung:c_mis" = "b_c_ung:c_mis",
  "c_cons:c_ung:c_mis" = "b_c_cons:c_ung:c_mis",
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "c_ung" = "b_c_ung",
  "c_cons" = "b_c_cons"
)
for (i in seq_along(exp1coefnames)) {
  idx <- which(exp1tbl$coef == names(exp1coefnames)[i])
  if (length(idx) > 0) {
    if (exp1coefnames[i] == "") {
      exp1tbl <- exp1tbl[-idx,]
    } else {
      exp1tbl$coef[idx] <- exp1coefnames[i]
    }
  }
}

mcmc_intervals(exp1fit, pars = exp1coefnames, prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(
    labels = c(
      "b_c_cons:c_ung:c_mis" = "Overt Case x   \nGram. Illusion",
      "b_c_ung:c_mis" = "Ungrammatical x   \nPlural Attractor\n(Gram. Illusion)",
      "b_c_cons:c_ung" = "Overt Case x   \nUngrammatical",
      "b_c_cons:c_mis" = "Overt Case x   \nPlural Attractor", 
      "b_l_trial" = "Trial No",
      "b_c_mis" = "Plural Attractor",
      "b_c_cons" = "Overt Case",
      "b_c_ung" = "Ungrammatical"
    ),
    expand = expand_scale(mult = c(.05, .15*1.3), 
                          add = c(0, 0)
    )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp1tbl$xmax, y = exp1tbl$coef,#_idx, 
                label = sprintf("[%s]", exp1tbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp1tbl$xmax[1]+0.2, y = length(unique(exp1tbl$coef))+0.3, 
            label = label,
            family = "mono" , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp1M1.png",w=12,h=8)

exp1fit <- as.matrix(exp1.mresp.u)
exp1tbl <- model_summary_(exp1.mresp.u)
exp1tbl$xmax <- with(exp1tbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' < - 0.1)'))")
#exp1tbl %<>% subset(!coef %in% c("c_cons:c_ung", "c_cons:c_mis"))
exp1coefnames <- c(
  "c_cons:c_mis" = "b_c_cons:c_mis", 
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "c_cons" = "b_c_cons"
)
for (i in seq_along(exp1coefnames)) {
  idx <- which(exp1tbl$coef == names(exp1coefnames)[i])
  if (length(idx) > 0) {
    if (exp1coefnames[i] == "") {
      exp1tbl <- exp1tbl[-idx,]
    } else {
      exp1tbl$coef[idx] <- exp1coefnames[i]
    }
  }
}

mcmc_intervals(exp1fit, pars = c("b_c_cons", "b_c_mis","b_l_trial", "b_c_cons:c_mis"), prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(limits=rev,
                   labels = c(
                     "b_c_cons:c_mis" = "Overt Case\n    x   \nPlural Attractor", 
                     "b_l_trial" = "Trial No",
                     "b_c_mis" = "Plural Attractor",
                     "b_c_cons" = "Overt Case"
                   ),
                   expand = expand_scale(mult = c(.05, .15*1.3), 
                                         add = c(0, 0)
                   )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp1tbl$xmax, y = exp1tbl$coef,#_idx, 
                label = sprintf("[%s]", exp1tbl$PBelowZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp1tbl$xmax[1]+0.2, y = length(unique(exp1tbl$coef))+0.3, 
            label = label,
            family = "mono" , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp1model.png",w=10,h=4)


# EXP 2 =======

p_exp2A_avgs <- 
  exp2.avgs %>% subset(resp.experiment == "Experiment 2" | resp.experiment == "Experiment 1") %>%
  ggplot(aes(resp.experiment, resp.M, 
             color = resp.attractor_num, 
             group = resp.attractor_num)) + 
  geom_point() + geom_line() + 
  facet_wrap(~resp.grammatical, 
             labeller = labeller(resp.grammatical = exp1.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
  xlab("Experiment") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')

smp("exp2Aavgs.png")




exp2fit <- as.matrix(exp2.mresp)
exp2tbl <- model_summary_(exp2.mresp)
exp2tbl$xmax <- with(exp2tbl, max(c(Estimate, Q2.5, Q97.5))) + 0.5
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
exp2tbl %<>% subset(!coef %in% c("c_cons:c_ung", "c_cons:c_mis"))
exp2coefnames <- c(
  "c_ung:c_mis" = "b_c_ung:c_mis", 
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "c_ung" = "b_c_ung" 
)
for (i in seq_along(exp2coefnames)) {
  idx <- which(exp2tbl$coef == names(exp2coefnames)[i])
  if (length(idx) > 0) {
    if (exp2coefnames[i] == "") {
      exp2tbl <- exp2tbl[-idx,]
    } else {
      exp2tbl$coef[idx] <- exp2coefnames[i]
    }
  }
}

mcmc_intervals(exp2fit, pars = c("b_c_ung", "b_c_mis","b_l_trial", "b_c_ung:c_mis"), prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(limits=rev,
                   labels = c(
                     "b_c_ung:c_mis" = "Ungrammaticality\n    x   \nPlural Attractor\n(Grammaticality Illusion)", 
                     "b_l_trial" = "Trial No",
                     "b_c_mis" = "Plural Attractor",
                     "b_c_ung" = "Ungrammaticality" 
                   ),
                   expand = expand_scale(mult = c(.05, .15*1.5), 
                                         add = c(0, 0)
                   )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp2tbl$xmax, y = exp2tbl$coef,#_idx, 
                label = sprintf("[%s]", exp2tbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = 1, y = length(unique(exp2tbl$coef))+0.3, 
            label = label,
            family = "mono", size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp2Amodel.png",w=10,h=4)

# EXP 2B =========


p_exp2B_avgs <- 
  exp2ab.avgs %>% subset(resp.experiment == "Experiment 2B") %>%
  ggplot(aes(resp.att_type, resp.M, 
             color = resp.attractor_num, 
             group = resp.attractor_num)) + 
  geom_point() + geom_line() + 
  facet_wrap(~resp.grammatical, 
             labeller = labeller(resp.grammatical = exp1.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
  xlab("Experiment") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_x_discrete(name = "Attractor Type", 
                   labels = c("Nominal\n(Genitive)", "Verbal\n(RC)")) +
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')

smp("exp2Bavgs.png")



exp2bfit <- as.matrix(exp2b.mresp)
exp2btbl <- model_summary_(exp2b.mresp)
exp2btbl$xmax <- with(exp2btbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
#exp2btbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp2bcoefnames <- c(
  "c_form:c_ung:c_mis" = "b_c_form:c_ung:c_mis",
  "c_ung:c_mis" = "b_c_ung:c_mis", 
  "c_form:c_mis" = "b_c_form:c_mis",
  "c_form:c_ung" = "b_c_form:c_ung",
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "c_form" = "b_c_form",
  "c_ung" = "b_c_ung" 
)
for (i in seq_along(exp2bcoefnames)) {
  idx <- which(exp2btbl$coef == names(exp2bcoefnames)[i])
  if (length(idx) > 0) {
    if (exp2bcoefnames[i] == "") {
      exp2btbl <- exp2btbl[-idx,]
    } else {
      exp2btbl$coef[idx] <- exp2bcoefnames[i]
    }
  }
}

mcmc_intervals(exp2bfit, exp2bcoefnames, prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(
    labels = c(
      "b_c_form:c_ung:c_mis" = "Nominal x \nGrammaticality Illusion",
      "b_c_form:c_mis" = "Nominal x   \nPlural Attractor",
      "b_c_form:c_ung" = "Nominal x \nUngrammaticality",
      "b_c_ung:c_mis" = "Ungrammaticality x   \nPlural Attractor\n(Grammaticality Illusion)", 
      "b_c_form" = "Nominal",
      "b_l_trial" = "Trial No",
      "b_c_mis" = "Plural Attractor",
      "b_c_ung" = "Ungrammaticality" 
    ),
    expand = expand_scale(mult = c(.05, .15*1.3), 
                          add = c(0, 0)
    )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp2btbl$xmax+0.3, y = exp2btbl$coef,#_idx, 
                label = sprintf("[%s]", exp2btbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp2btbl$xmax[1]+0.5, y = length(unique(exp2btbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp2BM1.png",w=12,h=8)


exp2bfit <- as.matrix(exp2bRC.mresp)
exp2btbl <- model_summary_(exp2bRC.mresp)
exp2btbl$xmax <- with(exp2btbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
#exp2btbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp2bcoefnames <- c(
  "c_ung:c_mis" = "b_c_ung:c_mis", 
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "c_ung" = "b_c_ung" 
)
for (i in seq_along(exp2bcoefnames)) {
  idx <- which(exp2btbl$coef == names(exp2bcoefnames)[i])
  if (length(idx) > 0) {
    if (exp2bcoefnames[i] == "") {
      exp2btbl <- exp2btbl[-idx,]
    } else {
      exp2btbl$coef[idx] <- exp2bcoefnames[i]
    }
  }
}

mcmc_intervals(exp2bfit, pars = c("b_c_ung", "b_c_mis","b_l_trial", "b_c_ung:c_mis"), prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(limits=rev,
                   labels = c(
                     "b_c_ung:c_mis" = "Ungrammaticality\n    x   \nPlural Attractor\n(Grammaticality Illusion)", 
                     "b_l_trial" = "Trial No",
                     "b_c_mis" = "Plural Attractor",
                     "b_c_ung" = "Ungrammaticality" 
                   ),
                   expand = expand_scale(mult = c(.05, .15*1.3), 
                                         add = c(0, 0)
                   )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp2btbl$xmax+0.3, y = exp2btbl$coef,#_idx, 
                label = sprintf("[%s]", exp2btbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp2btbl$xmax[1]+0.5, y = length(unique(exp2btbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp2Bmodel.png",w=10,h=4)


# Hammerly =======
hammerlydata$response_yes <- ifelse(grepl("f",hammerlydata$response) , T, 
                                    ifelse(grepl("j",hammerlydata$response) , F, NA))
hammerlydata$subject <- hammerlydata$subj
hammerly_avg <- list()
hammerly_avg$resp <- hammerlydata %>% 
  plyr::ddply(c("stimulustype"), function(df) {
    df %>% se_cousineau(n_conditions = 4, subject, DV = response_yes, 
                        group = c("exp", "Grammaticality", "Attractor", "stimulustype"), 
                        is_proportion = TRUE)
  })


ham.gram.label <- c(
  Grammatical = "Grammatical\n(Singular Verb)",
  Ungrammatical = "Ungrammatical\n(Plural Verb)"
)

ham_avgs <- 
  hammerly_avg$resp %>% filter(stimulustype == "Exp") %>%
  ggplot(aes(exp, M, 
             color = Attractor, 
             group = Attractor)) + 
  geom_point() + geom_line() + 
  facet_wrap(~Grammaticality, 
             labeller = labeller(Grammaticality = ham.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = M - 1.96*SE, 
                    ymax = M + 1.96*SE), 
                width = 0.1) + 
  xlab("Experiment") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_x_discrete(name = "Bias", 
                   labels = c("Towards\nGrammaticality", "Neutralized")) +
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')

smp("hamavgs.png")

# Exp 3 =====


exp3.gram.label <- c(
  gram = "Grammatical\n(Singular Verb)",
  ungram = "Ungrammatical\n(Plural Verb)"
)


p_exp3_avgs <- 
  exp3.avgs %>% 
  ggplot(aes(bias, resp.M, 
             color = resp.attractor_num, 
             group = resp.attractor_num)) + 
  geom_point() + geom_line() + 
  facet_wrap(~resp.grammatical, 
             labeller = labeller(resp.grammatical = exp3.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
  xlab("Experiment") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_x_discrete(name = "Bias", 
                   labels = c("Towards\nGrammaticality", "Towards\nUngrammaticality")) +
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')

smp("exp3avgs.png")


#exp3.mresp.u


exp3ufit <- as.matrix(exp3.mresp.u)
exp3utbl <- model_summary_(exp3.mresp.u)
exp3utbl$xmax <- with(exp3utbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
#exp3utbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp3ucoefnames <- c(
  "bias:c_mis" = "b_bias:c_mis", 
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "bias" = "b_bias" 
)
for (i in seq_along(exp3ucoefnames)) {
  idx <- which(exp3utbl$coef == names(exp3ucoefnames)[i])
  if (length(idx) > 0) {
    if (exp3ucoefnames[i] == "") {
      exp3utbl <- exp3utbl[-idx,]
    } else {
      exp3utbl$coef[idx] <- exp3ucoefnames[i]
    }
  }
}

mcmc_intervals(exp3ufit, pars = c("b_bias", "b_c_mis","b_l_trial", "b_bias:c_mis"), prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(limits=rev,
                   labels = c(
                     "b_bias:c_mis" = "Ungram. Bias\n    x   \nPlural Attractor", 
                     "b_l_trial" = "Trial No",
                     "b_c_mis" = "Plural Attractor",
                     "b_bias" = "Ungram. Bias" 
                   ),
                   expand = expand_scale(mult = c(.05, .15*1.3), 
                                         add = c(0, 0)
                   )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp3utbl$xmax+0.3, y = exp3utbl$coef,#_idx, 
                label = sprintf("[%s]", exp3utbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp3utbl$xmax[1]+0.5, y = length(unique(exp3utbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp3umodel.png",w=10,h=4)




#exp3.mresp.g



exp3gfit <- as.matrix(exp3.mresp.g)
exp3gtbl <- model_summary_(exp3.mresp.g)
exp3gtbl$xmax <- with(exp3gtbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' < -0.1)'))")
#exp3gtbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp3gcoefnames <- c(
  "bias:c_mis" = "b_bias:c_mis", 
  "l_trial" = "b_l_trial",
  "c_mis" = "b_c_mis",
  "bias" = "b_bias" 
)
for (i in seq_along(exp3gcoefnames)) {
  idx <- which(exp3gtbl$coef == names(exp3gcoefnames)[i])
  if (length(idx) > 0) {
    if (exp3gcoefnames[i] == "") {
      exp3gtbl <- exp3gtbl[-idx,]
    } else {
      exp3gtbl$coef[idx] <- exp3gcoefnames[i]
    }
  }
}

mcmc_intervals(exp3gfit, pars = c("b_bias", "b_c_mis","b_l_trial", "b_bias:c_mis"), prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(limits=rev,
                   labels = c(
                     "b_bias:c_mis" = "Ungram. Bias\n    x   \nPlural Attractor", 
                     "b_l_trial" = "Trial No",
                     "b_c_mis" = "Plural Attractor",
                     "b_bias" = "Ungram. Bias" 
                   ),
                   expand = expand_scale(mult = c(.05, .15*1.3), 
                                         add = c(0, 0)
                   )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp3gtbl$xmax+0.3, y = exp3gtbl$coef,#_idx, 
                label = sprintf("[%s]", exp3gtbl$PBelowZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp3gtbl$xmax[1]+0.4, y = length(unique(exp3gtbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp3gmodel.png",w=10.5,h=4)


# EXP 4 ===


exp4.gram.label <- c(
  gram = "Grammatical\n(Singular Verb)",
  ungram = "Ungrammatical\n(Plural Verb)"
)

exp4.resp.avg.plot <- exp4.avg %>%
  ggplot(aes(resp.register, resp.M, 
             color = resp.attractor_num, 
             group = resp.attractor_num)) + 
  geom_point() + geom_line() + 
  facet_wrap(~resp.grammatical, 
             labeller = labeller(resp.grammatical = exp3.gram.label),
             scales = "free_y") + 
  geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
  xlab("Register") + 
  ylab("Percentage 'acceptable'") + 
  scale_y_continuous(labels=scales::percent) + 
  scale_x_discrete(name = "Register", 
                   labels = c("Formal", "Informal")) +
  scale_color_lancet(name = "Attractor Number", 
                     labels = c("Plural", "Singular")) + 
  theme_classic() +
  theme(text=element_text(size=16, family="Helvetica Neue"))+
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom')


smp("exp4.png")


exp4fit <- as.matrix(exp4.mresp)
exp4tbl <- model_summary_(exp4.mresp)
exp4tbl$xmax <- with(exp4tbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
#exp4tbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp4coefnames <- c(
  "c_formal:c_ung:c_att" = "b_c_formal:c_ung:c_att",
  "c_ung:c_att" = "b_c_ung:c_att", 
  "c_formal:c_att" = "b_c_formal:c_att",
  "c_formal:c_ung" = "b_c_formal:c_ung",
  "l_trial" = "b_l_trial",
  "c_att" = "b_c_att",
  "c_formal" = "b_c_formal",
  "c_ung" = "b_c_ung" 
)
for (i in seq_along(exp4coefnames)) {
  idx <- which(exp4tbl$coef == names(exp4coefnames)[i])
  if (length(idx) > 0) {
    if (exp4coefnames[i] == "") {
      exp4tbl <- exp4tbl[-idx,]
    } else {
      exp4tbl$coef[idx] <- exp4coefnames[i]
    }
  }
}

mcmc_intervals(exp4fit, exp4coefnames, prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(
    labels = c(
      "b_c_formal:c_ung:c_att" = "Formal x \nGrammaticality Illusion",
      "b_c_formal:c_att" = "Formal x   \nPlural Attractor",
      "b_c_formal:c_ung" = "Formal x \nUngrammaticality",
      "b_c_ung:c_att" = "Ungrammaticality x   \nPlural Attractor\n(Grammaticality Illusion)", 
      "b_c_formal" = "Formal",
      "b_l_trial" = "Trial No",
      "b_c_att" = "Plural Attractor",
      "b_c_ung" = "Ungrammaticality" 
    ),
    expand = expand_scale(mult = c(.05, .15*1.3), 
                          add = c(0, 0)
    )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp4tbl$xmax+0.3, y = exp4tbl$coef,#_idx, 
                label = sprintf("[%s]", exp4tbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp4tbl$xmax[1]+0.5, y = length(unique(exp4tbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp4M1.png",w=12,h=8)



exp4Ffit <- as.matrix(exp4F.mresp)
exp4Ftbl <- model_summary_(exp4F.mresp)
exp4Ftbl$xmax <- with(exp4Ftbl, max(c(Estimate, Q2.5, Q97.5)))
label <- parse(text = "underline(paste('P(', beta, ' > 0.1)'))")
#exp4Ftbl %<>% subset(!coef %in% c("c_form:c_ung", "c_form:c_mis"))
exp4Fcoefnames <- c(
  "c_ung:c_att" = "b_c_ung:c_att", 
  "l_trial" = "b_l_trial",
  "c_att" = "b_c_att",
  "c_ung" = "b_c_ung" 
)
for (i in seq_along(exp4Fcoefnames)) {
  idx <- which(exp4Ftbl$coef == names(exp4Fcoefnames)[i])
  if (length(idx) > 0) {
    if (exp4Fcoefnames[i] == "") {
      exp4Ftbl <- exp4Ftbl[-idx,]
    } else {
      exp4Ftbl$coef[idx] <- exp4Fcoefnames[i]
    }
  }
}

mcmc_intervals(exp4Ffit, exp4Fcoefnames, prob = 0.5) +
  vline_at(c(0.1,-0.1), linetype = 3, size = 1, color = "red") + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_y_discrete(
    labels = c(
      "b_c_ung:c_att" = "Ungrammaticality x   \nPlural Attractor\n(Grammaticality Illusion)", 
      "b_l_trial" = "Trial No",
      "b_c_att" = "Plural Attractor",
      "b_c_ung" = "Ungrammaticality" 
    ),
    expand = expand_scale(mult = c(.05, .15*1.3), 
                          add = c(0, 0)
    )) +
  scale_x_continuous(expand = expand_scale(mult = c(.05, .15*1.3), 
                                           add = c(0, 0)),
                     breaks = ggplot2::waiver(), 
                     minor_breaks = ggplot2::waiver()) +
  geom_text(aes(x = exp4Ftbl$xmax+0.3, y = exp4Ftbl$coef,#_idx, 
                label = sprintf("[%s]", exp4Ftbl$PAboveZeroStr)), 
            family = "mono", hjust = "left", size=4.5) +
  geom_text(x = exp4Ftbl$xmax[1]+0.5, y = length(unique(exp4Ftbl$coef))+0.3, 
            label = label,
            family = "mono", , size=5) + 
  labs(
    title = "Posterior distributions",
    subtitle = parse(text = "paste('with medians and 90% (',bold(paste('50%')),') intervals')")
  ) +
  xlab("Estimate (Probit)") + 
  theme(text=element_text(size=16, family="Helvetica Neue"))



smp("exp4M2.png",w=10.5,h=4)


# BIAS ===


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
  results.subtitle = T,
  package = "ggsci",
  palette = "lanonc_lancet"
) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(text=element_text(size=16, family="Helvetica Neue"), legend.position = "none") +
  scale_x_discrete(
    labels = c("gram" = "Grammaticality", "ungram" = "Ungrammaticality")
  ) +
  scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1))

smp("exp3_bias_our.png", w = 8, h =  5)

A <- 
  ggstatsplot::ggbetweenstats(
    data = hammerlybias_exp,
    x = exp,
    y = bias,
    xlab = "",
    ylab = "Estimated Bias",
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
    results.subtitle = T,
    centrality.plotting = FALSE,
    package = "ggsci",
    palette = "lanonc_lancet"
  ) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(text=element_text(size=16, 
                          family="Helvetica Neue"), 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_x_discrete(
    labels = c("1" = "Experiment 1", "3" = "Experiment 3")
  ) +
  scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) + ggtitle("Bias Calc. with Experimental Items")



B <- 
  ggstatsplot::ggbetweenstats(
    data = hammerlybias_fill,
    x = exp,
    y = bias,
    xlab = "",
    ylab = "Estimated Bias",
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
    results.subtitle = T,
    centrality.plotting = FALSE,
    package = "ggsci",
    palette = "lanonc_lancet"
  ) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(text=element_text(size=16, 
                          family="Helvetica Neue"), 
        legend.position = "none") +
  scale_x_discrete(
    labels = c("1" = "Experiment 1", "3" = "Experiment 3")
  ) +
  scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) +
  ggtitle("Bias Calc. with Filler Items")

A / B 

smp("hammer_bias.png", w = 10, h =  12)


A <- 
  ggstatsplot::ggbetweenstats(
    data = hammerlybias_exp,
    x = exp,
    y = bias,
    xlab = "",
    ylab = "Estimated Bias",
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
    results.subtitle = F,
    centrality.plotting = FALSE,
    package = "ggsci",
    palette = "lanonc_lancet"
  ) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(text=element_text(size=16, 
                          family="Helvetica Neue"), 
        legend.position = "none") +
  scale_x_discrete(
    labels = c("1" = "Experiment 1", "3" = "Experiment 3")
  ) +
  scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) + ggtitle("Using Experimental Items")



B <- 
  ggstatsplot::ggbetweenstats(
    data = hammerlybias_fill,
    x = exp,
    y = bias,
    xlab = "",
    ylab = "Estimated Bias",
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
    results.subtitle = F,
    centrality.plotting = FALSE,
    package = "ggsci",
    palette = "lanonc_lancet"
  ) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(text=element_text(size=16, 
                          family="Helvetica Neue"), 
        legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  scale_x_discrete(
    labels = c("1" = "Experiment 1", "3" = "Experiment 3")
  ) +
  scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) +
  ggtitle("Using Filler Items")

A + B 

smp("defense_hammer_bias.png", w = 8, h =  4)
