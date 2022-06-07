

library(dplyr)
library()

ddm_acceptable <- function(z_bias = .5, v_drift=1) {
    # values 
    alpha <- 2 
    t_nondecision <- 1
    # generate data
    df <- RWiener::rwiener(
        n = 1e3, 
        alpha = alpha, 
        beta = z_bias, 
        delta = v_drift, 
        tau = t_nondecision
    )
    # computes the number of acceptable and unacceptable responses
    response_table <- table(df$resp) %>% data.frame
    
    acceptable_responses <- response_table %>%
        filter(Var1 == "upper") %>% pull(Freq) %>%
        as.numeric %>%
        divide_by(.,1e3) * 100
        
    acceptable_responses
}

bias_list <- c(.5, .7)
mm_drift_list <- c(.8, 1.3, -.8, -1.3)
cue_drift_list <- c(1.3,1.3, -0.8, -1.3)
df_ddm_sim <- data.frame()

for (z in bias_list) {
    for (v in mm_drift_list) {
        temp <- ddm_acceptable(z_bias = z, v_drift = v)
        temp_df <- data.frame(
            account = "Marking & Morphing",
            bias = ifelse(
                z == 0.5, "No Bias", 
                ifelse(
                    z >= 0.5, "Grammaticality Bias", 
                    "Ungrammaticality Bias"
                )
            ),
            drift = v,
            p_yes = temp
        )
        df_ddm_sim <- rbind(df_ddm_sim, temp_df)
    }
}   

for (z in bias_list) {
    for (v in cue_drift_list) {
        temp <- ddm_acceptable(z_bias = z, v_drift = v)
        temp_df <- data.frame(
            account = "Cue-based Retrieval",
            bias = ifelse(
                z == 0.5, "No Bias", 
                ifelse(
                    z >= 0.5, "Grammaticality Bias", 
                    "Ungrammaticality Bias"
                )
            ),
            drift = v,
            p_yes = temp
        )
        df_ddm_sim <- rbind(df_ddm_sim, temp_df)
    }
}

df_ddm_sim %<>% mutate(
    attpl = case_when(
        account == "Marking & Morphing" & drift == .8 ~ "pl",
        account == "Marking & Morphing" & drift == -.8 ~ "pl",
        account == "Cue-based Retrieval" & drift == -.8 ~ "pl",
        account == "Cue-based Retrieval" & drift == 1.1 ~ "pl",
        TRUE ~ "sg"
    ),
    verbpl = case_when(
        drift >= 0 ~ "sg",
        TRUE ~ "pl"
    )
) %>% as.data.table()
df_ddm_sim$attpl[13] = "pl"
df_ddm_sim$attpl[9] = "pl"
df_ddm_sim %<>% mutate(perc_yes = p_yes/100)

# graph
p_ddm_simulation <- df_ddm_sim %>% 
                        ggplot(
                            aes(
                                x = reorder(
                                    verbpl, 
                                    dplyr::desc(verbpl)
                                ), 
                                y = perc_yes, 
                                linetype = attpl, 
                                group = attpl
                            )
                        ) + 
                        geom_point() + 
                        geom_line() +
                        facet_wrap(account ~ bias, scales="free_y") +
                        theme(strip.background = element_rect(fill="white")) +
                        xlab("") + ylab("Percentage 'acceptable'") + 
                        scale_y_continuous(labels=scales::percent) + 
                        scale_linetype_discrete(name = "Attractor Number", labels = c("Plural", "Singular")) + 
                        scale_x_discrete(labels = c("Grammatical\n(Singular Verb)", "Ungrammatical\n(Plural Verb)")) +
                        theme_minimal() + 
                        theme(legend.position = 'bottom')


# SIMULATION AVERAGES
# (S)imulation - BIAS - VERBNUM - mean
SGPM <- mean(df_ddm_sim[bias == "Grammaticality Bias" & verbpl == "pl"]$p_yes) %>% round(.,2)
SNPM <- mean(df_ddm_sim[bias == "No Bias" & verbpl == "pl"]$p_yes) %>% round(.,2)

DELTA_gramasym_mm <-  diff(
    c(
        df_ddm_sim[account == "Marking & Morphing" & bias == "No Bias" & verbpl == "pl"]$p_yes %>% diff() %>% abs(),
        df_ddm_sim[account == "Marking & Morphing" & bias == "No Bias" & verbpl == "sg"]$p_yes %>% diff() %>% abs()
    )
) %>% abs()

DELTA_gramasym_cue <-  diff(
    c(
        df_ddm_sim[account == "Cue-based Retrieval" & bias == "No Bias" & verbpl == "pl"]$p_yes %>% diff() %>% abs(),
        df_ddm_sim[account == "Cue-based Retrieval" & bias == "No Bias" & verbpl == "sg"]$p_yes %>% diff() %>% abs()
    )
) %>% abs()