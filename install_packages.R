packages <- c("shiny", "dplyr", "writexl", "DT")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, repos = "https://cran.r-project.org")
  }
}