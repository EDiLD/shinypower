Power Simulations of ocunt data in one factorial designs
===================

### Data
Count data are simulated froma negative binomial distribution ($NB(\mu, \kappa)$) with mean = $\mu$ and variance = $\mu + \mu^2 / \kappa$.

$\kappa$ is a dispersion parameter: 

* if $\kappa \rightarrow 0$ overdispersion increases
* if $\kappa \rightarrow \infty$ data become Poisson distributed (no overdispersion)

The summary in the *Simulation-Design* tab gives mean and variances per treatment.

### Design 
Data are simulated for a one factorial design with 4 treatments and a step effect between treatments 2 and 3:

Abundance in in treatments 1 + 2 are draw from $NB(\mu_c, \kappa)$, whereas the mean abundance in treatments 3 + 4 are reduced by factor $r$: $NB(r \times \mu_c, \kappa)$. 
Therefore, the LOEC is at treatment 3 and NOEC at treatment 2.

Note, that $\kappa$ is equal between all treatments.

$\mu_c$, $\kappa$ and $r$ can be controlled by the sliders (*Effects*).
The `Simulation-Design` tab on the right gives a graphical representation of the design.
For demonstrational purposes 1000 data points per replicate are dawn from the specified design.


### Models

Two type of models are currently implemented:

1. Linear model on transformed data
2. Quasi-Poisson model

Both are compuationally feasible and give correct Type I errors.


### Inference
The global treatment effect is assesed using a F-test.
LOEC is determined via Dunnett contrasts.
The type of hypothesis (one-sided / two-sided) can be specified.

### Simulations
Power is calculated using simulations.
The numer of simulations can be specfied.
Up to 5 sample sizes (displayed on the x-axis) can be entered.
Press the *Run simulation* Butto to start the simulations.


