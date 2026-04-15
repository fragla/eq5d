make_long_profile <- function(pre, post) {
  n <- length(pre)
  data.frame(
    id    = rep(seq_len(n), 2),
    visit = rep(c("pre","post"), each = n),
    profile = c(pre, post),
    stringsAsFactors = FALSE
  )
}

make_long_dimensions <- function(pre, post, version="3L") {
  pre.df  <- get_dimensions_from_health_states(pre,  version)
  post.df <- get_dimensions_from_health_states(post, version)
  n <- nrow(pre.df)
  data.frame(
    id    = rep(seq_len(n), 2),
    visit = rep(c("pre","post"), each=n),
    rbind(pre.df, post.df),
    stringsAsFactors = FALSE
  )
}
