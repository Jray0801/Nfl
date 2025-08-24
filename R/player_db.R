#' Build player database
#'
#' Downloads player metadata and statistics and stores them in a SQLite database.
#'
#' @param db_path Path where the database should be created.
#' @param seasons Seasons to include; defaults to all available seasons.
#'
#' @return Invisibly returns the path to the created database.
#' @export
#'
#' @examples
#' \dontrun{
#' nflverse_build_player_db("players.db")
#' }

nflverse_build_player_db <- function(db_path = "player_stats.db",
                                     seasons = NULL) {
  players <- nflreadr::load_players()
  stats <- nflreadr::load_player_stats(seasons)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "players", players, overwrite = TRUE)
  DBI::dbWriteTable(con, "player_stats", stats, overwrite = TRUE)
  invisible(db_path)
}

#' Plot player counts by position
#'
#' Generates a bar chart of the number of players at each position using data in
#' the player database.
#'
#' @param db_path Path to the player database.
#'
#' @return A ggplot object showing player counts per position.
#' @export
#'
#' @examples
#' \dontrun{
#' nflverse_build_player_db()
#' nflverse_plot_position_counts("player_stats.db")
#' }
nflverse_plot_position_counts <- function(db_path = "player_stats.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  players <- DBI::dbReadTable(con, "players")
  counts <- as.data.frame(table(players$position))
  ggplot2::ggplot(counts, ggplot2::aes(x = Var1, y = Freq)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Position", y = "Number of Players")
}

