library(gdata)

get_xg_probs <- function (country, gender, season_end_year, tier, competition_name) {
    matches <- worldfootballR::fb_match_urls(country, gender, season_end_year, tier)
    probs <- NULL

    for (match in matches) {
        Sys.sleep(5)
        shots <- worldfootballR::fb_match_shooting(match)

        home_shots <- shots |>
            tidytable::filter(
                Home_Away == "Home"
            )
        home_team <- unique(home_shots$Squad)
        home_probs <- poibin::dpoibin(seq.int(0, length(home_shots$xG)), home_shots$xG)

        away_shots <- shots |>
            tidytable::filter(
                Home_Away == "Away"
            )
        away_team <- unique(away_shots$Squad)
        away_probs <- poibin::dpoibin(seq.int(0, length(away_shots$xG)), away_shots$xG)

        outer_prod <- outer(home_probs, away_probs)
        hprob <- sum(lowerTriangle(outer_prod))
        aprob <- sum(upperTriangle(outer_prod))
        dprob <- sum(diag(outer_prod))

        match_probs <- tidytable::tidytable(
            team = c(home_team, away_team),
            competition = rep(competition_name, 2),
            season = rep(season_end_year, 2),
            wprob = c(hprob, aprob),
            dprob = c(dprob, dprob),
            lprob = c(aprob, hprob)
        )

        if (is.null(probs)) {
            probs <- match_probs
        } else {
            probs <- tidytable::bind_rows(list(probs, match_probs))
        }
    }

    write.csv(probs, "data/probs_xg.csv")
    saveRDS(probs, file = "data/probs_xg.rds")

    return (probs)
}
