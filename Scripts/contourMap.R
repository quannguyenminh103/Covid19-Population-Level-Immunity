contourMap <- function(){
  # Simulate the data for plotting
  x <- seq(from = 0, to = 1, by = 0.1)
  y <- seq(from = 0, to = 1, by = 0.1)
  # immunity level
  z1 <- outer(X = x, Y = y, FUN = function(x, y) x + (1-x)*y) # data for surface plot
  fig <- plot_ly(
    x = x,
    y = y,
    z = 100*z1,
    type = "contour",
    autocontour = FALSE,
    contours = list(
      start = 0,
      end = 90,
      size = 10,
      showlabels = TRUE,
      showlines = TRUE,
      coloring = "heatmap"
    ), hovertemplate = paste("<extra></extra>"),
    line = list(smoothing = 0,
                width = 1,
                dash = 'dash',
                color = "black"
                )
    
  ) %>% add_contour(ncontours = 1, contours = list(start = 95, end = 95,  showlabels = TRUE,
                                                   showlines = TRUE,
                                                   coloring = "none"), showlegend = F) %>%
    layout(
      title = list(text = "Population level immunity (%)",
                   font = list(size = 21), yref = 'paper', xref = 'paper',
                   y=1.5),
      xaxis = list(
        range = c(-0.02,1),
        title = list(text = 'Fraction of people infected (%)',
                     font = list(size = 20)
                     ),
        # linewidth = 0,
        # linecolor = toRGB("white"),
        mirror = TRUE,
        tickformat = "%",
        tickfont = list(size = 15),
        ticklen = 5
      ),
      yaxis = list(
        range = c(-0.01,1),
        title = list(text = 'Fraction of people vaccinated (%)',
                     font = list(size = 20)
                     ),
        # linewidth = 0,
        # linecolor = toRGB("white"),
        mirror = TRUE,
        tickformat = "%",
        tickfont = list(size = 15)
      ),
      margin = list(l = 0,
                    r = 0,
                    b = 0,
                    t = 50,
                    pad = 1)
    ) %>% hide_colorbar()
  # default point
  base <- fig %>% add_trace(x = 0.3, y = 0, type = "scatter", size = 20, 
                            hovertemplate = paste0('<b>Estimated Population Recovered</b><br>',
                                                  'Infected: %{x}<br>',
                                                  'Vaccinated (at least one dose): %{y}<br><extra></extra>' ),
                            marker = list(color='black'),
                            line = list(width = 0), showlegend = F)
  
  return(base)

}
