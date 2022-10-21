
proposee_rankings <- 
  rankings %>% 
  rename(
    proposer = proposee,
    proposee = proposer,
    proposee_ranking = ranking
  )
  
proposer_propose <- function(tbl) {
  tbl %>% 
    group_by(proposer) %>% 
    arrange(ranking) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename(
      proposer_ranking = ranking
    )
}

proposee_accept_or_reject <- function(tbl) {
  tbl %<>% 
    count(proposee)
  
  one_proposal <- 
    tbl %>% 
    filter(n == 1)
  
  multiple_proposals <- 
    tbl %>% 
    filter(n > 1) 
  
  no_proposal <- 
    anti_join(
      rankings %>% 
        distinct(proposer) %>% 
        rename(
          proposee = proposer
        ),
      bind_rows(
        one_matches,
        multiples
      )
    ) %>% 
    select(proposee)
  
  multiples_weeded <- 
    multiple_proposals %>% 
    inner_join(proposee_rankings) %>% 
    group_by(proposee) %>% 
    arrange(proposee_ranking) %>% 
    slice(1) %>% 
    ungroup() 
  
  bind_rows(
    one_proposal,
    multiples_weeded,
    no_proposal
  )
}

solve <- function(tbl) {
  n_rounds <- 
    tbl %>% 
    distinct(proposer) %>% 
    nrow()
  
  message("Starting round 1")
  
  # First round
  slate <- 
    tbl %>% 
    proposer_propose() %>% 
    proposee_accept_or_reject()
  
  for (r in 2:n_rounds) {
    message(glue::glue("On round {r} of {n_rounds}"))
    
    # Take out people we've already proposed to
    slate <- 
      tbl %>% 
      anti_join(slate) %>% 
      proposer_propose() %>% 
      proposee_accept_or_reject()
  }
  
  slate
}

solve(rankings)

debugonce(solve)

