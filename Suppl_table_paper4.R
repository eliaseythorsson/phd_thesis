# Additional analysis for paper IV: table censored at 30 months of age rather than 60 months

chi_lg <- 
    ror_lg_surv %>%
    filter(proc == 1, age >= 36) %>%
    count(group, cum_presc) %>%
    right_join(
        ror_lg_surv %>%
            filter(proc == 1, age >= 36) %>%
            count(group, cum_presc) %>%
            expand(group, cum_presc)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_presc_agg = case_when(
            cum_presc == 0 ~ "0",
            cum_presc == 1 ~ "1",
            cum_presc == 2 ~ "2",
            cum_presc >= 3 & cum_presc <= 4 ~ "3-4",
            cum_presc >= 5 & cum_presc <= 7 ~ "5-7",
            cum_presc >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_presc_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_presc_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_presc_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = prop.test, conf.level = 0.95) %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_presc_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    mutate(
        `estimate.prop 1` = `estimate.prop 1`*100,
        `estimate.prop 2` = `estimate.prop 2`*100,
        conf.int1 = conf.int1*100, 
        conf.int2 = conf.int2*100,
        diff = `estimate.prop 1` - `estimate.prop 2`,
        cum_presc = as.numeric(cum_presc_agg)
    ) %>%
    arrange(cum_presc_agg) %>%
    transmute(
        cumulative = cum_presc_agg,
        presc_ARD = paste0(
            format(signif(diff, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(conf.int1, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(conf.int2, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

rr_lg <- 
    ror_lg_surv %>%
    filter(proc == 1, age >= 36) %>%
    count(group, cum_presc) %>%
    right_join(
        ror_lg_surv %>%
            filter(proc == 1, age >= 36) %>%
            count(group, cum_presc) %>%
            expand(group, cum_presc)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_presc_agg = case_when(
            cum_presc == 0 ~ "0",
            cum_presc == 1 ~ "1",
            cum_presc == 2 ~ "2",
            cum_presc >= 3 & cum_presc <= 4 ~ "3-4",
            cum_presc >= 5 & cum_presc <= 7 ~ "5-7",
            cum_presc >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_presc_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_presc_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_presc_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = riskratio, conf.level = 0.95, rev = "both") %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_presc_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    transmute(
        cumulative = cum_presc_agg,
        presc_VNEC = paste0(
            format(signif(data4/data7 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data4, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        presc_VEC = paste0(
            format(signif(data5/data8 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data5, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        presc_RR = paste0(
            format(signif(measure4, digits = 3), digits = 2, trim = T,  nsmall = 2),
            " (",
            format(signif(measure2, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(measure6, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

chi_lsh <- 
    ror_hg_lsh_surv %>%
    filter(proc == 1, age >= 36) %>%
    count(group, cum_visit) %>%
    right_join(
        ror_hg_lsh_surv %>%
            filter(proc == 1, age >= 36) %>%
            count(group, cum_visit) %>%
            expand(group, cum_visit)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_visit_agg = case_when(
            cum_visit == 0 ~ "0",
            cum_visit == 1 ~ "1",
            cum_visit == 2 ~ "2",
            cum_visit >= 3 & cum_visit <= 4 ~ "3-4",
            cum_visit >= 5 & cum_visit <= 7 ~ "5-7",
            cum_visit >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_visit_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_visit_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_visit_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = prop.test, conf.level = 0.95) %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_visit_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    mutate(
        `estimate.prop 1` = `estimate.prop 1`*100,
        `estimate.prop 2` = `estimate.prop 2`*100,
        conf.int1 = conf.int1*100, 
        conf.int2 = conf.int2*100,
        diff = `estimate.prop 1` - `estimate.prop 2`,
        cum_visit = as.numeric(cum_visit_agg)
    ) %>%
    arrange(cum_visit_agg) %>%
    transmute(
        cumulative = cum_visit_agg,
        visit_ARD = paste0(
            format(signif(diff, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(conf.int1, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(conf.int2, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

rr_lsh <- 
    ror_hg_lsh_surv %>%
    filter(proc == 1, age >= 36) %>%
    count(group, cum_visit) %>%
    right_join(
        ror_hg_lsh_surv %>%
            filter(proc == 1, age >= 36) %>%
            count(group, cum_visit) %>%
            expand(group, cum_visit)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_visit_agg = case_when(
            cum_visit == 0 ~ "0",
            cum_visit == 1 ~ "1",
            cum_visit == 2 ~ "2",
            cum_visit >= 3 & cum_visit <= 4 ~ "3-4",
            cum_visit >= 5 & cum_visit <= 7 ~ "5-7",
            cum_visit >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_visit_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_visit_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_visit_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = riskratio, conf.level = 0.95, rev = "both") %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_visit_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    transmute(
        cumulative = cum_visit_agg,
        visit_VNEC = paste0(
            format(signif(data4/data7 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data4, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        visit_VEC = paste0(
            format(signif(data5/data8 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data5, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        visit_RR = paste0(
            format(signif(measure4, digits = 3), digits = 2, trim = T,  nsmall = 2),
            " (",
            format(signif(measure2, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(measure6, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

save.image(file = paste0("_analyses/paper_4/", Sys.Date(), "-04-2-results-suppl-table-paper4", ".RData"))
