library(showtext)

load_fonts <- function() {
    showtext_auto()

    font_add(
        "Commit Mono Medium",
        "fonts/commitmono/CommitMono-600-Regular.otf"
    )
    font_add(
        "Commit Mono",
        "fonts/commitmono/CommitMono-400-Regular.otf"
    )
    font_add(
        "Fira Black",
        "fonts/firasans/FiraSans-ExtraBold.ttf"
    )
    font_add(
        "Fira Medium",
        "fonts/firasans/FiraSans-Medium.ttf"
    )
    font_add(
        "Fira",
        "fonts/firasans/FiraSans-Regular.ttf"
    )
}

load_seven_points_theme <- function() {
    ggplot2::theme(
        plot.title = ggplot2::element_text(
            family = "Fira Black",
            size = 40,
            color = "#111213",
            hjust = 0.5,
            vjust = 0.5,
            margin = ggplot2::margin(b = 4)
        ),
        plot.title.position = "plot",
        plot.subtitle = ggplot2::element_text(
            family = "Fira",
            size = 28,
            color = "#494C50",
            hjust = 0.5,
            vjust = 0.5,
            margin = ggplot2::margin(b = 8)
        ),
        plot.caption = ggplot2::element_text(
            family = "Commit Mono",
            size = 18,
            hjust = 1,
            vjust = 0.5,
            color = "#494C50"
        ),
        axis.title = ggplot2::element_text(
            family = "Fira Medium",
            size = 22,
            color = "#111213",
            hjust = 0.5,
            vjust = 0.5,
        ),
        axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 6)
        ),
        axis.title.y = ggplot2::element_text(
            margin = ggplot2::margin(r = 6)
        ),
        axis.text = ggplot2::element_text(
            family = "Commit Mono Medium",
            size = 20,
            color = "#111213",
            face = "bold"
        ),
        plot.margin = ggplot2::margin(t = 8, r = 12, b = 8, l = 12),
        axis.ticks = ggplot2::element_blank(),
        #axis.line = ggplot2::element_line(
        #    linewidth = 0.25,
        #    color = "#111213"
        #),
        axis.line = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_line(
            linewidth = 0.2,
            color = "#DBDCDF"
        ),
        panel.grid.major = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
    )
}
