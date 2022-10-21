get.PK_ncas <- function(dat, mode = "tac", lvl, ...) {
  if (mode == "tac") {
    lst <- plyr::dlply(dat, .(group, id, organ))
    out <- lapply(lst, get.PK_nca)
    df <- bind_rows(out, .id = "var", ) %>%
      separate(var,
        into = c("group", "id", "organ"),
        sep = "\\."
      ) %>%
      `rownames<-`(c())
  } else {
    dat <- dat %>%
      group_by(t, organ, group, id) %>%
      dplyr::summarise(
        conc = mean(conc, na.rm = TRUE),
      ) %>%
      arrange(group, organ, t)

    zero_pad <- data.frame(
      t = rep(0, time = length(unique(dat$id))),
      id = dat$id %>% unique(),
      conc = rep(0, time = length(unique(dat$id)))
    ) %>%
      separate(id,
        into = c("group", "organ"),
        sep = "_", remove = FALSE
      ) %>%
      mutate(organ = stringr::str_to_title(organ)) %>% # I HAVE A NAMING PROBLEM
      arrange(group, organ, t)

    ###
    zero_pad$organ <- fct_recode(
      zero_pad$organ,
      UB = "Bladder",
      "L Intestine" = "Large Intestine",
      "S Intestine" = "Small Intestine",
      "ALN" = "Axillary Ln",
      "CLN" = "Cervical Ln",
      "ILN" = "Inguinal Ln",
      "MLN" = "Mesenteric Ln"
    )
    ###

    dat <- bind_rows(zero_pad, dat) %>%
      arrange(t, organ, group, id, conc)

    lst <- plyr::dlply(dat, .(group, id, organ))
    out <- lapply(lst, get.PK_nca)
    df <- bind_rows(out, .id = "var", ) %>% #what is this blank?
      separate(var,
        into = c("group", "id", "organ"),
        sep = "\\."
      ) %>%
      `rownames<-`(c())
  }

  sz.f <- function(x) {
    sum(!is.na(x))
  }
  sd.f <- function(x) {
    sd(x, na.rm = T)
  }
  se.f <- function(x) {
    sd(x, na.rm = T) / sqrt(sz.f(x))
  }
  mn.f <- function(x) {
    mean(x, na.rm = T)
  }

  mn.df <- df %>%
    group_by(group, organ) %>%
    dplyr::summarise_all(mn.f) %>%
    mutate(stats = "mean") %>%
    select(-c(id))

  sd.df <- df %>%
    group_by(group, organ) %>%
    dplyr::summarise_all(sd.f) %>%
    mutate(stats = "SD") %>%
    select(-c(id))

  se.df <- df %>%
    group_by(group, organ) %>%
    dplyr::summarise_all(se.f) %>%
    mutate(stats = "SEM") %>%
    select(-c(id))

  sz.df <- df %>%
    group_by(group, organ) %>%
    dplyr::summarise_all(sz.f) %>%
    mutate(stats = "n") %>%
    select(-c(id))

  tbl <- rbind(mn.df, sd.df, se.df, sz.df) %>%
    relocate(stats, .before = hl) %>%
    mutate(stats = ordered(stats,
      levels = c("mean", "SD", "SEM", "n")
    )) %>%
    arrange(group, organ, stats) %>%
    mutate(organ = factor(organ)) %>%
    mutate(organ = fct_relevel(organ, lvl)) %>%
    arrange(group, organ)

  if (mode != "tac") {
    tbl <- tbl %>%
      filter(stats == "mean") %>%
      select(-c("stats"))
    return(tbl)
  } else {
    return(tbl)
  }
}