
bulletGraph <- function(measures, markers=NA, ranges=NA) {
  #   http://bl.ocks.org/mbostock/4061961
  require(assertive)
  assert_is_vector(measures)
  assert_is_numeric(measures)
  assert_all_are_in_range(length(measures), 1, 2)
  assert_is_vector(markers)
  if (is_not_na(markers)) {
    assert_is_numeric(markers)
    assert_all_are_in_range(length(markers), 1, 2)
  }
  assert_is_vector(ranges)
  if (is_not_na(ranges)) {
    assert_is_numeric(ranges)
    assert_all_are_in_range(length(ranges), 2, 5)
  }
  kWidthRanges <- 0.6
  kWidthMeasures <- kWidthRanges / 3
  kWidthMarkers <- kWidthMeasures * 2
  kWidthMarkers <- kWidthRanges * 0.9
  
  # mandatory data frame for ggplot2
  df <- data.frame(xdummy=1, ranges)
  
  # plot ranges
  # note, ranges are reversed because of overplotting (instead of stacking differences)
  p <- ggplot(df, aes(x=xdummy, y=rev(ranges), fill=rev(factor(ranges))) ) 
  p <- p + geom_bar(stat="identity", 
                    position="identity", 
                    width=kWidthRanges)
  p <- p + scale_fill_manual(values=bulletRangeColours(length(ranges)))
  
  # plot markers
  p <- p + geom_errorbar(aes(y = markers[1], 
                             ymin = markers[1], 
                             ymax = markers[1]), 
                         width = kWidthMarkers, size=1.2, colour="black") 
  if (length(markers)==2) {
    p <- p + geom_errorbar(aes(y = markers[2], 
                               ymin = markers[2], 
                               ymax = markers[2]), 
                           width = kWidthMarkers, size=1, colour="grey25") 
  }
  
  # plot measures
  p <- p + geom_bar(aes(y=measures), stat="identity", position="dodge", fill="black", 
                    width=kWidthMeasures) 
  
  # adjust look
  p <- p + theme_tufte() + 
    theme(title=element_blank(), panel.grid=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
          legend.position="none")
  
  # plot horizontally
  p <- p + coord_flip() 
  return(p)
}

require(ggplot2)
require(ggthemes)

ranges <- c(75, 85, 100)
measures <- 78
markers <- c(38, 88)
bulletGraph(measures, markers, ranges)
