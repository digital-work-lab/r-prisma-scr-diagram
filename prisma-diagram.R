library(tidyverse)

search_db_1 = 10
search_db_2 = 20
search_toc = 30

total_results_from_search = search_db_1 + search_db_2 + search_toc

duplicates_removed = 10
records_screened = total_results_from_search - duplicates_removed

exclusion1 = 20
full_text_articles_retrieved = records_screened - exclusion1

exclusion2 = 5
articles_indluded_in_synthesis = full_text_articles_retrieved - exclusion2

format_statistics <- function(statistic){
  return(format(round(statistic), nsmall = 0, big.mark = ","))
}

data <- tibble(x = 1:100, y = 1:100)
data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() ->
  p

p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 70, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 85, label = 'Identification', size = 4, angle = 90)

p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 25, ymax = 68, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 46, label = 'Study Selection', size = 4, angle = 90)


p <- p +
  geom_rect(xmin = 10, xmax = 30, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 20, y = 95, label = paste('Database 1\n (n=',
                                                 format_statistics(search_db_1),
                                                 ')'),
                                                 size = 4)

p <- p +
  geom_rect(xmin = 40, xmax = 60, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 50, y = 95, label = paste('Database 2\n (n=',
                                                 format_statistics(search_db_2),
                                                 ')'),
                                                 size = 4)

p <- p +
  geom_rect(xmin = 70, xmax = 90, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 80, y = 95, label = paste('Table-of-content scan\n (n=',
                                                 format_statistics(search_toc),
                                                 ')'),
                                                 size = 4)

p <- p +
  geom_segment(
    x = 20, xend = 20, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 50, xend = 50, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 80, xend = 80, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 20, xend = 80, y = 85, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 35, xend = 35, y = 85, yend = 80,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 70, ymax = 80, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 75, label = paste('Total results from search\n (n=',
                                                 format_statistics(total_results_from_search),
                                                  ')'),
                                                  size = 4)

p <- p +
  geom_segment(
    x = 35, xend = 55, y = 67.5, yend = 67.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 70, yend = 65,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 72.5, ymax = 62.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 67.5, label = paste('Duplicates removed\n (n=',
                                                   format_statistics(duplicates_removed),
                                                   ')'),
                                                   size = 4)

p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 55, ymax = 65, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 60, label = paste('Records screened\n (n=',
                                                 format_statistics(records_screened),
                                                 ')'),
                                                 size = 4)

p <- p +
  geom_segment(
    x = 35, xend = 55, y = 52.5, yend = 52.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 55, yend = 50,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 57.5, ymax = 47.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 52.5, label = paste('Articles excluded (Title or Abstract)\n (n=',
                                                   format_statistics(exclusion1),
                                                   ')'),
                                                   size = 4)

p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 40, ymax = 50, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 45, label = paste('Full-text articles retrieved\n (n=',
                                                 format_statistics(full_text_articles_retrieved),
                                                 ')'),
                                                 size = 4)

p <- p +
  geom_segment(
    x = 35, xend = 55, y = 37.5, yend = 37.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 40, yend = 35,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 42.5, ymax = 32.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 37.5, label = paste('Articles excluded (Full-text)\n (n=',
                                                   format_statistics(exclusion2),
                                                   ')'),
                                                   size = 4)

p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 35, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 30, label = paste('Articles included in the synthesis\n (n=',
                                                 format_statistics(articles_indluded_in_synthesis),
                                                 ')'),
                                                 size = 4)

p <- p + theme_void()

jpeg('prisma-diagram.jpg', width = 1000, height = 600)
plot(p)
dev.off()
