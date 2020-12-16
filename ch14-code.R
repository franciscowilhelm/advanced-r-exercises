Deck <- R6Class("Deck", list(
  cards = cards,
  draw = function(n) {
    self$drawn <- sample(self$cards, n)
    self$cards <- setdiff(self$cards, self$drawn)
    invisible(self)
  }
))
