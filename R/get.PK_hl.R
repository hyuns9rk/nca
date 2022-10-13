get.PK_hl <- function(x) {
  b1 <- x$parms[2, 1]
  a1 <- x$parms[3, 1]
  b2 <- x$parms[2, 2]
  a2 <- x$parms[3, 2]

  switch(x$method,
    lee = {
      if (is.na(x$chgpt)) {
        x$chgpt <- min(x$time)

        t <- approx(x$time)$y
        conc <- 10^(b1 * t + a1)

        out <- data.frame(t = t, conc = conc)
      } else {
        fast.t <- approx(x$time[x$time < x$chgpt])$y
        slow.t <- approx(x$time[x$time > x$chgpt])$y
        fast.conc <- 10^(b1 * fast.t + a1)
        slow.conc <- 10^(b2 * slow.t + a2)

        out <- data.frame(
          t = c(fast.t, slow.t),
          conc = c(fast.conc, slow.conc)
        )
      }
    },
    biexp = {
      if (b1 == b2) {
        t <- approx(x$time)$y
        conc <- a1 * exp(-b1 * t)

        out <- data.frame(t = t, conc = conc)
      }
      if (b1 != b2) {
        t <- approx(x$time)$y
        conc <- a1 * exp(-b1 * t) + a2 * exp(-b2 * t)

        out <- data.frame(t = t, conc = conc)
      }
    }
  )
  return(out)
}