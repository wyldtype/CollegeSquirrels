updateFreq <- function(.N, .p) {
  new_alleles <- rbinom(n = length(.p), size = 2*.N, prob = .p)
  new_alleles/(2*.N)
}

wrightFisher <- function(.N, .p, .X = NULL, .g = 500, .g_now = 1) {
  .X_new <- cbind(.X, .p, deparse.level = 0)
  if (.g_now >= .g) {
    return(.X_new)
  }
  .p_new <- updateFreq(.N = .N, .p = .p)
  return(wrightFisher(.N = .N, .p = .p_new, .X = .X_new, 
                      .g = .g, .g_now = .g_now + 1))
}