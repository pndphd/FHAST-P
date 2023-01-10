display_plot <- function(plot) {
  if (print_plots) {
    X11()
    print(plot)
  }
}

display_plot <- function(plot, height, width) {
  if (print_plots) {
    X11(height = height, width = width)
    print(plot)
  }
}
