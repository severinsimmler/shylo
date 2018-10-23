library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(ggdendro)

function(distance.table, include.dendrogram, label.size){
  hc <- hclust(dist(distance.table), "ward.D")
  dhr <- as.dendrogram(hc)
  order_r <- rownames(distance.table)[hc$order]

  hc <- hclust(dist(t(distance.table)), "ward.D")
  dhc <- as.dendrogram(hc)
  order_c <- colnames(distance.table)[hc$order]

  expr_set <- bind_cols(
    data_frame(rowvar = rownames(distance.table)),
    as.data.frame(distance.table)
  )

  expr_set <- gather(expr_set, colvar, measure, -rowvar)
  expr_set$rowvar <- factor(expr_set$rowvar, levels = order_r)
  expr_set$colvar <- factor(expr_set$colvar, levels = order_c)
  expr_set <- arrange(expr_set, rowvar, colvar)

  data_c <- dendro_data(dhc, type = "rectangle")
  data_c <- segment(data_c) %>% mutate(
    y = y + length(order_r) + .5,
    yend = yend + length(order_r) + .5
  )

  data_r <- dendro_data(dhr, type = "rectangle")
  data_r <- segment(data_r)
  data_r <- data_r %>%
    mutate(x_ = y + length(order_c) + .5,
           xend_ = yend + length(order_c) + .5,
           y_ = x,
           yend_ = xend)

  expr_set <- expr_set %>%
    mutate(
      tooltip = sprintf("%s<br/>%s<br/>Delta Score: %.02f</span>",
                        rowvar, colvar, measure),
      data_id = sprintf("%s_%s", rowvar, colvar)
    )

  p <- ggplot(data = expr_set, aes(x = colvar, y = rowvar)) +
    geom_tile_interactive(aes(fill = measure, tooltip = tooltip, data_id = data_id), colour = "white") +
    scale_fill_gradient2(low="#FFFFFF", high="#FF0000", name="Delta Score")
  if (include.dendrogram){
    p <- p +
    geom_segment(
      data = data_c,
      mapping = aes(x = x, y = yend, xend = xend, yend = y),
      colour = "gray20", size = .4) +
    geom_segment(
      data = data_r,
      mapping = aes(x = x_, y = y_, xend = xend_, yend = yend_),
      colour = "gray20", size = .4) +
    coord_equal()}

  p <- p + theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_line(color = "transparent"),
      panel.grid.major = element_line(color = "transparent"),
      axis.ticks.length   = unit(2, units = "mm"),
      axis.text.y = element_text(hjust = 1, size = label.size, colour = "gray40"),
      axis.text.x = element_text(angle = 90, hjust = 1, size = label.size, colour = "gray40"),
      legend.title=element_text(face = "bold", hjust = 0.5, size=10),
      legend.text=element_text(size=10)
    ) +
    labs(x = "", y = "")

  return(ggiraph(ggobj = p, selection_type="none", tooltip_extra_css = "font-family: Sans; font-size: 12px; background-color: #F3F3F3; padding: 5px 5px 5px 5px;"))}
