# get2 function mimicking get
get2 <- function(x, envir = NULL) {
  checkmate::assert_character(x, any.missing = FALSE, len = 1L)
  checkmate::assert_environment(envir)
  eval(as.name(x), envir = envir)
}
# check if this works
get("%o%")
get2("%o%", envir = as.environment("package:base"))

# assign2 function mimicking assign
assign2 <- function(x, value = NULL, envir = NULL) {
  checkmate::assert_character(x, any.missing = FALSE, len = 1L)
  checkmate::assert_environment(envir)
  eval(substitute(x_ <- value_, list(x_ = as.name(x), value_ = value)),
    envir = envir)
}
# check if this works
test <- new.env(hash = TRUE, parent = parent.frame(), size = 1L)
assign("test_x", "Hello World", envir = test)
get("test_x", envir = test)
assign2("test_x", "Bye", envir = test)
get2("test_x", envir = test)

# function g for which defaults should be changed
g <- function(x = 20, y) {
  x + y
}
# change the defaults
formals(g)$x <- substitute()
formals(g)$y <- 10

# function to find all terms of a formula that belong to interactions
find_terms_in_interactions <- function(your_formula) {
  checkmate::assert_formula(your_formula)
  interactions <- grep(":", x = attr(terms(your_formula), "term.labels"),
    value = TRUE)
  unique(unlist(strsplit(interactions, split = ":")))
}
# test this
find_terms_in_interactions(
  a ~ 1 + a + b * c + poly(d, 3) + (g + h)^2 + e:f:b:fnargl
)
