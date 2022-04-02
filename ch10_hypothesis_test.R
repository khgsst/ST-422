#### User parameters ####
n_sim <- 250
p_H0 <- 0.5
p_true <- 0.5
alpha <- 0.05
n_samp <- 30
# Include left tail (n=0, y=1)
tail_l <- 1
# Include right tail (n=0, y=1)
tail_r <- 1


#### Probability computations ####
phat <- rep(0.0, n_sim)
reject <- rep(FALSE, n_sim)
z_alpha <- qnorm(alpha / (tail_l + tail_r), lower.tail = FALSE)
var_H0 <- p_H0 * (1 - p_H0) / n_samp
var_true <- p_true * (1 - p_true) / n_samp
rr <- c(p_H0 - z_alpha * sqrt(var_H0), p_H0 + z_alpha * sqrt(var_H0))

for (i in 1:n_sim) {
    x <- rbinom(n = 1, size = n_samp, prob = p_true)
    phat[i] <- x / n_samp
    rej_l <- ((phat[i] - p_H0) / sqrt(var_H0) < -z_alpha) & tail_l
    rej_r <- ((phat[i] - p_H0) / sqrt(var_H0) > z_alpha) & tail_r
    reject[i] <- rej_l | rej_r
}

# for beta
lb <- tail_l * pnorm((rr[1] - p_true) / sqrt(var_true))
rb <- tail_r * pnorm((rr[2] - p_true) / sqrt(var_true), lower.tail = FALSE)


#### Simulation Results ####
# Proportion of sample for which H0 rejected
sum(reject) / n_sim
# Type I error (under assumption H0 true)
alpha
# Type II error (under assumption H0 false)
1 - (lb + rb)
