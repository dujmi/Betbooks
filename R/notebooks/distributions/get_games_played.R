get_games_played <- function(games, team_name, competition_name, season_year) {
    team_games <- games |>
        tidytable::filter(
            team == team_name,
            competition == competition_name,
            season == season_year
        )

    return (nrow(team_games))
}
