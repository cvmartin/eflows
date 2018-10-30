
all_flex_mtx = function(l) {
  all(sapply(l, function(x){inherits(x, "flex_mtx")}))
}
on_failure(all_flex_mtx) <- function(call, env) {
  "All objects in flex must inherit from class flex_mtx."
}