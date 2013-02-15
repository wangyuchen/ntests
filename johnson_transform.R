johnsonTransform <- function(x, test="shapiro.test") {
  res <- data.frame()
  for (z in seq(from=0.26, to=1.25, by=0.01)) {
    q <- getQuantiles(x, z)
    
    # fit sl distribution
    trans <- fitSL(x, q)
    if (length(trans) != 1) {
      # fit succeeds
      for (t in test) {
        p.value <- do.call(t, list(trans))$p.value
        d <- data.frame(test=t, p.value, z, type="SL")
        res <- rbind(res, d)
      }
    }
    
    if (q$QR <= 1) {
      trans <- fitSB(x, q)
      if (length(trans) != 1) {
        for (t in test) {
          p.value <- do.call(t, list(trans))$p.value
          d <- data.frame(test=t, p.value, z, type="SB")
          res <- rbind(res, d)
        }
      }
    } else {
      trans <- fitSU(x, q)
      if (length(trans) != 1) {
        for (t in test) {
          p.value <- do.call(t, list(trans))$p.value
          d <- data.frame(test=t, p.value, z, type="SU")
          res <- rbind(res, d)
        }
      }
    }
  }
  
  # collect max p.value
  RVAL <- do.call(rbind, lapply(split(res, res$test), 
                                function(x) x[which.max(x$p.value), ]))
  return(RVAL)
}

getQuantiles <- function(x, z, s=3) {
  x.q <- quantile(x, probs=pnorm(c(-s * z, -z, z, s * z)))
  xl <- x.q[2]-x.q[1]
  xm <- x.q[3]-x.q[2]
  xu <- x.q[4]-x.q[3]
  QR <- xu * xl / xm^2
  q <- list(xl, xm, xu, z, QR, x.q[2], x.q[3])
  names(q) <- c("xl", "xm", "xu", "z", "QR", "x2", "x3")
  return(q)
}

arsinh <- function(x) {
  return(log(x + sqrt(x^2 + 1)))  
}

arcosh <- function(x) {
  if (x >= 1) return(log(x + sqrt(x^2 - 1)))
}

fitSB <- function(x, q) {
  eta <- q$z / arcosh(.5 * sqrt((1 + q$xm / q$xu) * (1 + q$xm / q$xl)))  
  gamma <- eta * arsinh((q$xm / q$xl - q$xm / q$xu) * 
                          sqrt((1 + q$xm / q$xu) * (1 + q$xm / q$xl) - 4) / 
                          (2 * (q$xm^2 / q$xl / q$xu - 1))) 
  lambda <- (q$xm * sqrt(((1 + q$xm / q$xu) * (1 + q$xm / q$xl) - 2)^2 - 4) / 
               (q$xm^2 / q$xl / q$xu - 1))
  epsilon <- .5 * (q$x2 + q$x3 - lambda + 
                     q$xm * (q$xm / q$xl - q$xm / q$xu) / 
                     (q$xm^2 / q$xl / q$xu - 1))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0) return(NA)
  
  if (all(x > epsilon) & all(x < epsilon + lambda)) {
    return(gamma + eta *  log((x - epsilon) / (lambda + epsilon - x)))
  } else return(NA)
}

fitSL <- function(x, q) {
  if (q$xu/q$xm <= 1) return(NA)
  
  eta <- 2 * q$z / log(q$xu / q$xm)
  gamma <- eta * log((q$xu / q$xm - 1) / sqrt(q$xu * q$xm))
  epsilon <- .5 * (q$x2 + q$x3 - q$xm * (q$xu / q$xm + 1) / 
                     (q$xu / q$xm - 1))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0) return(NA)
  
  if (all(x > epsilon)) {
    return(gamma + eta * log(x - epsilon))
  } else return(NA)
}

fitSU <- function (x, q) {
  eta <- 2 * q$z / arcosh(.5 * (q$xu / q$xm + q$xl / q$xm))
  gamma <- eta * arsinh((q$xl / q$xm - q$xu / q$xm) / 
                          (2 * sqrt(q$xu * q$xl / q$xm^2 - 1)))
  lambda <- (2 * q$xm * sqrt(q$xu * q$xl / q$xm^2 - 1) / 
               (q$xu / q$xm + q$xl / q$xm - 2) / 
               sqrt(q$xu / q$xm + q$xl / q$xm + 2))
  epsilon <- .5 * (q$x2 + q$x3 + q$xm * (q$xl / q$xm - q$xu / q$xm) / 
                     (q$xu / q$xm + q$xl / q$xm - 2))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0) return(NA)
  
  return(gamma + eta * arsinh((x - epsilon) / lambda))
}
