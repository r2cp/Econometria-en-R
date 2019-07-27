pca <- prcomp(Admissions %>% filter(University.Rating == 2) %>% select(-University.Rating), scale. = TRUE)
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Varianza explicada
pca$sdev^2
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:7),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

# Otro
pca <- prcomp(Admissions.2, scale. = TRUE)
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:24),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")