# Supplementary figure: bird and plants used from each period -------------

table(birdsForest$Period)
table(birdsForest$Species)

table(plantsRangitāhua$Period)
table(plantsRangitāhua$Species)


plant_plot <- ggplot(plantsRangitāhua, aes(x = Period, fill = Species)) +
  geom_bar() +
  labs(title = "(A)",
       x = "",
       y = "Number of Samples",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust=0.5),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol =3)) +
  scale_fill_manual(values = plantsSpeciesColors)

bird_plot <- ggplot(birdsForest, aes(x = Period, fill = Species)) +
  geom_bar() +
  labs(title = "(B)",
       x = "",
       y = "",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust=0.5),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2)) +
  scale_fill_manual(values = birdsSpeciesColors)



# Display the plots
print(bird_plot)
print(plant_plot)

grid.arrange(plant_plot, bird_plot, ncol= 2)


# Order by first appearance
# For plants
plantsRangitāhua$Species <- factor(plantsRangitāhua$Species, 
                                   levels = plantsRangitāhua %>% 
                                     group_by(Species) %>% 
                                     summarise(first_year = max(CollectionYear)) %>% 
                                     arrange(first_year) %>% 
                                     pull(Species))

# For birds
birdsForest$Species <- factor(birdsForest$Species, 
                              levels = birdsForest %>% 
                                group_by(Species) %>% 
                                summarise(first_year = max(CollectionYear)) %>% 
                                arrange(first_year) %>% 
                                pull(Species))


plant_plot2 <- ggplot(plantsRangitāhua, aes(x = CollectionYear, y = Species, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "(A)",
       x = "",
       y = "",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "italic"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust=0.5),
        legend.position = "none") +
  scale_x_continuous(limits = c(1836, 2023), breaks = c(seq(1836, 2023, 10), 2023)) +
  #guides(fill = guide_legend(ncol =3)) +
  scale_color_manual(values = plantsSpeciesColors)

bird_plot2 <- ggplot(birdsForest, aes(x = CollectionYear, y = Species, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "(B)",
       x = "",
       y = "",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "italic"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust=0.5),
        legend.position = "none") +
  scale_x_continuous(limits = c(1836, 2023), breaks = c(seq(1836, 2023, 10), 2023)) +
  #guides(fill = guide_legend(ncol = 2)) +
  scale_color_manual(values = birdsSpeciesColors)



# Display the plots
print(plant_plot2)
print(bird_plot2)


combined_plot <- plant_plot2 / bird_plot2
print(combined_plot)
