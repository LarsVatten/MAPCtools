devsetup <- function() {
  source("dev/dependencies.R")
}

attachdata <- function() {
  source("dev/testing/attach_data.R")
}

reload <- function() {
  unlink("NAMESPACE")
  devtools::document()
  devtools::load_all()
}
