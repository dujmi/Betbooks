source("R/notebooks/distributions/get_points_distribution.R")
source("R/notebooks/distributions/get_games_played.R")
source("R/notebooks/distributions/get_xg_probs.R")
source("R/theme/theme.R")

teams <- list(
    list(
        team = "Bournemouth",
        competition = "Premier League",
        season = 24,
        actual_points = 30
    ),
    list(
        team = "Fulham",
        competition = "Premier League",
        season = 24,
        actual_points = 29
    ),
    list(
        team = "Nott'm Forest",
        competition = "Premier League",
        season = 24,
        actual_points = 37
    )
)

probs <- readRDS("data/probs.rds")
teams_prob_dist <- NULL

for (i in (1:length(teams))) {
    team_games_played <- get_games_played(probs, teams[[i]]$team, teams[[i]]$competition, teams[[i]]$season)
    team_prob_dist <- get_points_distribution(probs, teams[[i]]$team, teams[[i]]$competition, teams[[i]]$season) |>
        tidytable::mutate(
            team = teams[[i]]$team, 
            n = team_games_played,
            actual_points = teams[[i]]$actual_points
    )

    if (is.null(teams_prob_dist)) {
        teams_prob_dist <- team_prob_dist
    } else {
        teams_prob_dist <- tidytable::bind_rows(list(teams_prob_dist, team_prob_dist))
    }
}

for (i in (1:length(teams))) {
    team_games_played <- get_games_played(probs, teams[[i]]$team, teams[[i]]$competition, teams[[i]]$season)
    team_prob_dist <- get_points_distribution(probs, teams[[i]]$team, teams[[i]]$competition, teams[[i]]$season) |>
        tidytable::mutate(
            team = teams[[i]]$team, 
            n = team_games_played
    )

    if (is.null(teams_prob_dist)) {
        teams_prob_dist <- team_prob_dist
    } else {
        teams_prob_dist <- tidytable::bind_rows(list(teams_prob_dist, team_prob_dist))
    }
}

load_fonts()
phi <- (1 + sqrt(5)) / 2
integer_percent <- function(x) {
    paste0(round(x * 100), "%")
}

ggplot2::ggplot(teams_prob_dist, ggplot2::aes(x = points, y = prob, color = team)) +
    ggplot2::geom_vline(linewidth = 0.35, linetype = "dashed", ggplot2::aes(xintercept = actual_points, color = team), show.legend = FALSE) + 
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_continuous(
        limits = c(0, max(teams_prob_dist$n) * 3),
        breaks = seq(0, max(teams_prob_dist$n) * 3, 10)
    ) +
    ggplot2::scale_y_continuous(
        labels = integer_percent,
        limits = c(0, max(teams_prob_dist$prob) * phi)
    ) +
    ggplot2::labs(
        title = "Vjerojatnost trenutnog bodovnog učinka premierligaša",
        subtitle = "Raspodjela temeljena na prosječnim tržišnim kvotama za svaku utakmicu",
        x = "Bodovi",
        y = "Vjerojatnost",
        caption = "Mislav Dujak, izvor: football-data.co.uk"
    ) +
    ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.725),
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(
            family = "Fira Medium",
            size = 20,
            color = "#111213"
        )
    ) +
    load_seven_points_theme()

ggplot2::ggsave(
    "figures/bournemouth_odds.png",
    width = 1080 * phi,
    height = 1080,
    unit = "px",
    dpi = "retina"
)
