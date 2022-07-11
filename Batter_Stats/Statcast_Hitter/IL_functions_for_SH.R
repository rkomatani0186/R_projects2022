connect_db <- function(db) {
  con <- dbConnect(MySQL(),
                   user = "mbaadmin",
                   password = "gUZSaQAAwgDRhqZPPCqa",
                   dbname = db,
                   host = "prodbaseballmysqlohio.ciurqw3b29ox.us-east-2.rds.amazonaws.com")
  return(con)
}
field_lines = function(g) {
  g = g +
    geom_segment(aes(x = 0, y = 0, xend = -233, yend = 233), color = "black") +
    geom_segment(aes(x = 0, y = 0, xend = 233, yend = 233), color = "black") +
    geom_segment(aes(x = 0, y = 0, xend = -100, yend = 194), color = "black") +
    geom_segment(aes(x = 0, y = 0, xend = -35, yend = 223), color = "lightgray") +
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 194), color = "black") +
    geom_segment(aes(x = 0, y = 0, xend = 35, yend = 223), color = "lightgray") +
    geom_segment(aes(x = -80, y = 204, xend = -150, yend = 322), color = "black") +
    geom_segment(aes(x = -35, y = 222, xend = -75, yend = 358), color = "lightgray") +
    geom_segment(aes(x = 35, y = 222, xend = 75, yend = 358), color = "lightgray") +
    geom_segment(aes(x = 80, y = 204, xend = 150, yend = 322), color = "black") +
    geom_curve(aes(x = -150, y = 150, xend = 150, yend = 150), color = "black", curvature = -0.5) +
    geom_curve(aes(x = -233, y = 233, xend = 233, yend = 233), color = "black", curvature = -0.6) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, vjust=1),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    coord_fixed()
  
  return(g)
}

