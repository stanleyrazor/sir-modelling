# Set parameters
beta <- 2 # Infection rate
gamma <- 13 # Recovery rate
contact_rate <- 0.08 # Fraction of population each person is connected to
init_infect <- 10 # Number of people initially infected
N <- 200 # Total population size
maxtime <- 20 # How long to simulate for
npts <- 150 # Number of time points during the simulation
dt <- .2 # maxtime / npts # Timestep length

actual_infected <- c(
    6, 11, 14, 15, 17, 20, 25, 31, 40, 47, 52, 57, 66,
    75, 85, 93, 99, 100, 107, 111, 112, 118, 123, 117, 120, 122,
    122, 123, 123, 125, 122, 120, 119, 110, 112, 117, 115, 118, 117,
    113, 115, 112, 111, 107, 105, 103, 101, 99, 100, 99, 98, 97,
    94, 93, 92, 91, 89, 88, 88, 87, 86, 83, 81, 80, 78,
    76, 76, 73, 72, 71, 71, 71, 69, 68, 67, 65, 61, 58,
    57, 56, 56, 54, 52, 51, 51, 49, 49, 47, 46, 46, 46,
    46, 45, 45, 45, 43, 43, 40, 37, 36
)

require(purrr)
require(dplyr)
require(ggplot2)

# initialiation of the three (S I R) states
initstate <- function() list(s = T, i = F, r = F)
recover <- function() list(s = F, i = F, r = T)
infect <- function() list(s = F, i = T, r = F)

# function for assigning a state to an agent based on the letter given
make_state <- function(letter) {
    # letter can either be 's', 'i', 'r'
    if (letter == "s") state <- initstate() 
    else if (letter == 'i') state <- infect()
    else state <- recover()
    return(state)
}

# function for attempting to infect a susceptible person
check_infection <- function(person_state, ...) {
    if (runif(1) < beta * dt) {
        person_state <- infect()
    }
    return(person_state)
}

# function for attempting to recover infected individuals
check_recovery <- function(person_state, ...) {
    if (runif(1) < gamma * dt) {
        person_state <- recover()
    }
    return(person_state)
}

# Function for actually assigning a state to an agent at any time t
sir_assign <- function(person_state, beta, gamma, dt, action) {
    if (action == "infect") person_state <- check_infection(person_state)
    if (action == "recover") person_state <- check_recovery(person_state)

    return(person_state)
}

# Applying the SIR model to a population and checking how the states and individuals evolve over time
population_evolve <- function(
    num = 200, time = 100, init_infect,
    beta = 1.22, gamma = .5, dt = .2) {

    # creating the states of the people
    people <- replicate(num, initstate() |> unlist(), FALSE)
    start_infected <- sample(num, init_infect)
    for (i in start_infected) people[[i]] <- infect() |> unlist()

    # storing the first initial run
    person_states_in_time <- matrix(NA, nrow = time, ncol = num)
    person_states_in_time[1, ] <- people |>
        lapply(FUN = \(x) which(x == T) |> names()) |>
        unlist()

    # Looping across time and individuals
    for (t in 2:time) {
        person_states <- vector()
        for (p in 1:num) {
            previous_state <- person_states_in_time[t - 1, p]
            person <- make_state(previous_state)

            if (person$s) {
                person.status <- sir_assign(person, beta, gamma, dt, action = "infect") |> unlist()
            } else if (person$i) {
                person.status <- sir_assign(person, beta, gamma, dt, action = "recover") |> unlist()
            } else {
                person.status <- c("s" = F, "i" = F, "r" = T)
            }

            current_state <- which(person.status == 1)
            person_states <- c(person_states, names(current_state))
        }
        person_states_in_time[t, ] <- person_states
    }
    return(person_states_in_time)
}



num_pop <- 200
time_pop <- 20
npts = time_pop / dt
ee <- population_evolve(num_pop, time_pop, dt = .2, init_infect=5)
sir_pop <- data.frame(
    t = seq(from = .2, to = time_pop, by = dt),
    actual_infected = actual_infected,
    s = 1:num_pop |>
        map(\(i) {
            x <- ee[i, ]
            y <- ifelse(x == "s", 1, 0)
            z <- sum(y)
            z
        }) |>
        unlist(),
    i = 1:num_pop |>
        map(\(i) {
            x <- ee[i, ]
            y <- ifelse(x == "i", 1, 0)
            z <- sum(y)
            z
        }) |>
        unlist(),
    r = 1:num_pop |>
        map(\(i) {
            x <- ee[i, ]
            y <- ifelse(x == "r", 1, 0)
            z <- sum(y)
            z
        }) |>
        unlist()
)

sir_pop |>
    ggplot() +
    geom_point(aes(x = t, y = actual_infected), col = "green") +
    geom_line(aes(x = t, y = s), col = "black") +
    geom_line(aes(x = t, y = r), col = "blue") +
    geom_line(aes(x = t, y = i), col = "red") +
    theme_classic()



e.grid <- expand.grid(
    beta_par = seq(from = 0, to = 3, length = 50),
    gamma_par = seq(from = 0, to = 3, length = 50),
    mae = NA
)

for (e in 1:nrow(e.grid)) {
    if ((e %% 10) == 0) print(e)

    num_pop <- 200
    time_pop <- 100
    ee <- population_evolve(num_pop, time_pop)

    i <- 1:time_pop |>
        map(\(i) {
            x <- ee[i, ]
            y <- ifelse(x == "i", 1, 0)
            z <- sum(y)
            z
        }) |>
        unlist()

    e.grid[e, 3] <- ModelMetrics::mae(actual = actual_infected, predicted = i)
}
