get.PK_nca <- function(df) {
  pk_df <- df

  hl_out <- list()
  hl <- list()
  cmax <- list()
  tmax <- list()
  tlast <- list()
  clast <- list()
  auc <- list()
  auc_inf <- list()
  exp.hl.initial <- list()
  exp.hl.terminal <- list()
  exp.kel <- list()
  lin.hl.initial <- list()
  lin.hl.terminal <- list()
  lin.kel <- list()
  auc_t2inf.exp <- list()
  auc_t2inf.lin <- list()
  auc_inf.exp <- list()
  auc_inf.lin <- list()

  for (subj in c("1")) {
    pk_df_subject <- pk_df

    hl_out[[subj]] <- pk.calc.half.life(
      time = pk_df_subject$t,
      conc = pk_df_subject$conc,
      min.hl.points = 3, allow.tmax.in.half.life = TRUE
    )

    hl[subj] <- hl_out[[subj]]$half.life

    cmax[subj] <- pk.calc.cmax(conc = pk_df_subject$conc)

    tmax[subj] <- pk.calc.tmax(
      time = pk_df_subject$t,
      conc = pk_df_subject$conc
    )

    exp.hl.initial[subj] <-
      tryCatch(PK::biexp(
        time = pk_df_subject$t[pk_df_subject$t >= tmax[subj]],
        conc = pk_df_subject$conc[pk_df_subject$t >= tmax[subj]]
      )$parms$initial[1],
      error = function(e) {
        return(NA)
      }
      )

    exp.hl.terminal[subj] <-
      tryCatch(PK::biexp(
        time = pk_df_subject$t[pk_df_subject$t >= tmax[subj]],
        conc = pk_df_subject$conc[pk_df_subject$t >= tmax[subj]]
      )$parms$terminal[1],
      error = function(e) {
        return(NA)
      }
      )

    exp.kel[subj] <-
      tryCatch(0.693 / as.numeric(exp.hl.terminal[subj]),
        error = function(e) {
          return(NA)
        }
      )

    lin.hl.initial[subj] <-
      tryCatch(PK::lee(
        time = pk_df_subject$t[pk_df_subject$t >= tmax[subj]],
        conc = pk_df_subject$conc[pk_df_subject$t >= tmax[subj]]
      )$parms$initial[1],
      error = function(e) {
        return(NA)
      }
      )

    lin.hl.terminal[subj] <-
      tryCatch(PK::lee(
        time = pk_df_subject$t[pk_df_subject$t >= tmax[subj]],
        conc = pk_df_subject$conc[pk_df_subject$t >= tmax[subj]]
      )$parms$terminal[1],
      error = function(e) {
        return(NA)
      }
      )

    lin.kel[subj] <-
      tryCatch(0.693 / as.numeric(lin.hl.terminal[subj]),
        error = function(e) {
          return(NA)
        }
      )

    tlast[subj] <- pk.calc.tlast(
      time = pk_df_subject$t,
      conc = pk_df_subject$conc
    )

    clast[subj] <- pk_df_subject$conc[pk_df_subject$t == tlast[subj]]

    auc[subj] <- pk.calc.auc(
      time = pk_df_subject$t,
      conc = pk_df_subject$conc
    )

    auc_inf[subj] <- pk.calc.auc.inf(
      time = pk_df_subject$t,
      conc = pk_df_subject$conc, lambda.z = hl_out[[subj]]$lambda.z
    )

    auc_t2inf.exp[subj] <- as.numeric(clast[subj]) / as.numeric(exp.kel[subj])
    auc_t2inf.lin[subj] <- as.numeric(clast[subj]) / as.numeric(lin.kel[subj])

    auc_inf.exp[subj] <- as.numeric(auc[subj]) + as.numeric(auc_t2inf.exp[subj])
    auc_inf.lin[subj] <- as.numeric(auc[subj]) + as.numeric(auc_t2inf.lin[subj])

    pk_parms_per_subject <- data.frame(
      hl = unlist(hl),
      exp.hl.initial = unlist(exp.hl.initial),
      exp.hl.terminal = unlist(exp.hl.terminal),
      lin.hl.initial = unlist(lin.hl.initial),
      lin.hl.terminal = unlist(lin.hl.terminal),
      cmax = unlist(cmax),
      tmax = unlist(tmax),
      tlast = unlist(tlast),
      clast = unlist(clast),
      exp.kel = unlist(exp.kel),
      lin.kel = unlist(lin.kel),
      auc = unlist(auc),
      auc_inf = unlist(auc_inf),
      auc_inf.exp = unlist(auc_inf.exp),
      auc_inf.lin = unlist(auc_inf.lin)
    )
    # print(subj)
  }
  return(pk_parms_per_subject)
}