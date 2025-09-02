# Figure 1: Simple blank plot for x-axis ticks ---------------------------------

plot(1, type = "n", 
     xlim = c(1850, 2025), 
     ylim = c(0, 1),
     xlab = "", 
     ylab = "",
     axes = FALSE)


axis(1, at = c(1854, 1921, 1984, 2002), 
     labels = c("1854", "1921", "1984", "2002"))



box()

# Then, the rest of the image was drawn on Powerpoint for publication