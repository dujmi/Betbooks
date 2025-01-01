get_points_distribution <- function (probs, team_name, competition_name, season_year) {
    team_probs <- probs |>
        tidytable::filter(
            team == team_name,
            competition == competition_name,
            season == season_year
        )

    n <- nrow(team_probs)
    max_points <- 3 * n

    pd <- numeric(max_points + 1)
    pd[1] <- 1

    for (i in 1:n) {
        for (j in (3 * i + 1):1) {
            pd[j] <- (
                (if (j > 3) pd[j - 3] * team_probs$wprob[i] else 0) +
                    (if (j > 1) pd[j - 1] * team_probs$dprob[i] else 0) +
                    pd[j] * team_probs$lprob[i]
            )
        }
    }

    return (
        tidytable::tidytable(
            points = 0:max_points,
            prob = pd
        )
    )
}
