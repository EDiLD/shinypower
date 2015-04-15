Power Simulations of ocunt data in one factorial designs
===================

### Data + Design
Count data are simulated froma negative binomial distribution ($NB(\mu, \kappa)$) with mean = $\mu$ and variance = $\mu + \mu^2 / \kappa$.
$\kappa$ is a dispersion parameter: 

* if $\kappa \rightarrow 0$ overdispersion increases
* if $\kappa \rightarrow \infty$ data become Poisson distributed (no overdispersion)


Data are simulated for a one factorial design with 4 treatments and a step effect between treatments 2 and 3:
Abundance in in treatments 1 + 2 are draw from $NB(\mu_c, \kappa)$, whereas the mean abundance in treatments 3 + 4 are reduced by factor $r$: $NB(r \times \mu_c, \kappa)$. 
Therefore, the LOEC is at treatment 3 and NOEC at treatment 2.
Note, that $\kappa$ is equal between treatments.

$\mu_c$, $\kappa$ and $r$ can be controlled by the sliders (under effects).
The `Simulation-Design` tab on the right gives a graphical representation of the design.
For demonstrational purposes this with a sample size per replicate of 1000 is drawn.



