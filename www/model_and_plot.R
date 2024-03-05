model_and_plot <- function(data, tilde, label="") {
  newdat <- data[ , all.vars(tilde)]
  names(newdat) <- c("y", "one", "two")[1:length(newdat)]

  if (length(all.vars(tilde)) == 1) newtilde <- y ~ 1
  if (length(all.vars(tilde)) == 2) newtilde <- y ~ one
  if (length(all.vars(tilde)) == 3) {
    newtilde <- tilde
    newtilde[[2]] <- as.name("y")
    newtilde[[3]][[2]] <- as.name("one")
    newtilde[[3]][[3]] <- as.name("two")
  }
  mod <- newdat |> model_train(newtilde)
  model_plot(mod, nlevels=11) |>
    add_plot_labels(title=label)
}
