shapiro.test <- function(x)
{
  DNAME <- deparse(substitute(x))
  stopifnot(is.numeric(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if(is.na(n) || n < 3L || n > 5000L)
    stop("sample size must be between 3 and 5000")
  rng <- x[n] - x[1L]
  if(rng == 0) stop("all 'x' values are identical")
  if(rng < 1e-10) x <- x/rng # rescale to avoid ifault=6 with single version.
  res <- .Call(C_SWilk, x)
  RVAL <- list(statistic = c(W = res[1]), p.value = res[2],
               method = "Тест Шапиро-Уилка на нормальность"
               #, data.name = DNAME
               )
  class(RVAL) <- "htest"
  return(RVAL)
}

