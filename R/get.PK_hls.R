get.PK_hls <- function(df) {
  tmax <- df$t[which.max(df$conc)]
  # exponential by PK::biexp
  exp_hl_out <-
    tryCatch(PK::biexp(
      time = df$t[df$t >= tmax],
      conc = df$conc[df$t >= tmax]
    ),
    error = function(e) {
      return(NA)
    }
    )
  exp_hl_xy <- get.PK_hl(exp_hl_out) %>%
    mutate(
      method = "biexp"
    ) %>%
    as.data.frame()
  
  # linear by PK::lee
  lin_hl_out <-
    tryCatch(PK::lee(
      time = df$t[df$t >= tmax],
      conc = df$conc[df$t >= tmax]
    ),
    error = function(e) {
      return(NA)
    }
    )
  lin_hl_xy <- get.PK_hl(lin_hl_out) %>%
    mutate(
      method = "lin"
    ) %>%
    as.data.frame()
  
  out_hl <- list("biexp" = exp_hl_out, "lin" = lin_hl_out)
  out_xy <- rbind(exp_hl_xy, lin_hl_xy)
  return(list("hls" = out_hl, "curves" = out_xy))
}