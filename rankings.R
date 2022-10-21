library(googlesheets4)
library(dplyr)
library(magrittr)

url <- "https://docs.google.com/spreadsheets/d/1u-J0lPO-6Iz63dGgtguZ1snat8RANoyqiXVt9C9CnVQ/edit#gid=1293285841"

ppl <-
  googlesheets4::read_sheet(url, sheet = 2)

ppl <-
  c(
    "Aki",
    "CC",
    "Dobby",
    "HB",
    "Iris",
    "Jane",
    "Joyce",
    "Julie",
    "Kayla",
    "KSpa",
    "Kyla",
    "LWoods",
    "Marié",
    "Nancy",
    "Pepi",
    "Sophia",
    "Squid",
    "Wei"
  )


build_rankings <- function(ppl) {
  out <- tibble()

  for (p in ppl) {
    this <-
      tibble(
        proposer = p,
        proposee = ppl[-which(ppl == p)],
        ranking = sample(length(ppl) - 1, length(ppl) - 1)
      )

    out %<>%
      bind_rows(
        this
      )
  }
  out
}

rankings <-
  build_rankings(ppl) %>%
  arrange(proposer, ranking)
