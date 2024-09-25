# Lee_Schoneman_unit1.1_BIOL672.r

# create a random set of 5000 points following a poisson distribution
set.seed(100)
random_set_1 <- rpois(5000, 20)
random_mean <- mean(random_set_1)
random_sd <- sd(random_set_1)
random_mean
random_sd

#print mean, standard deviation, and summary to file
sink(file = '/Users/leesch/BIOL672_Unit_1/desc.txt')
print(random_mean)
print(random_sd)
print(summary(random_set_1))
sink() 

plot_1 <- ggplot(NULL, aes(x = random_set_1, y = after_stat(density))) +
  geom_histogram(fill = "lightblue", colour = "grey") + geom_density() + xlim(0, 50)
print(plot_1)