grid.bulletGraph <- 
  function(qualitativeLimits,      ## 2 to 5 values for qualitative ranges; 
                                   ## max is upper limit of quantitative scale,
                                   ## min is assumed to be zero
           target = NA,            ## goal at end of period
           value,                  ## actual value
           projected = NA,         ## projected value
           ToDate = NA,            ## optional actual to-date value
           ppToDate = NA,          ## optioinal prior period to-date value
           main,                   ## main text label
           sub = "",               ## optional sub label
           col.qual = bulletGraphGrays(length(qualitiveLimits)),  ## background bar colors
           col.bullet = brewer.pal(9, "Blues")[c(9, 7)],          ## bullet colors
           labelWidth = 0.3        ## proportion total width for text label
  ) {}

bulletRangeColours <- function(n) {
#   From Stephen Few, Perceptual Edge
#   Bullet Graph Design Specification 
#   Last Revision: March 12, 2010 
#   http://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pdf
#   The Qualitative Ranges
#   Formatting Defaults:
#   Color. Rather than using distinct hues, which might not be distinguishable by those who are colorblind, encode these ranges as distinct intensities from dark to light of a single hue. Use the darker color intensities for the poor states and the lighter color intensities for the favorable states. Use these intensities for the following numbers of ranges:
#   Two: 35% and 10% black
#   Three: 40%, 25%, and 10% black
#   Four: 50%, 35%, 20%, and 10% black
#   Five: 50%, 35%, 20%, 10%, and 3% black
#   Border. None
  
#   note: grey0 = black, grey100 = white
  
#   require(futile.logger)
#   if (!is.numeric(n)) {
#     flog.fatal("n is not numeric")
#     stop()
#   }
  require(assertive)
  assert_is_a_number(n)
  assert_all_numbers_are_whole_numbers(n)
  assert_all_are_in_range(n, 2, 5)
  QualRangesBlack <- list(c(35, 10),
                         c(40, 25, 10),
                         c(50, 35, 20, 10),
                         c(50, 35, 20, 10, 3))
  colrs <-  paste0("grey", 100-QualRangesBlack[[n-1]])
  return(colrs)
}
