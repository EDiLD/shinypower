# Design

## Description
Data are simulated from a one factorial design with 4 treatments.

Abundances are drawn from a negative binomial distribution ($NB(\mu, \kappa)$) with mean = $\mu$ and variance = $\mu + \mu^2 / \kappa$.

Abundance in treatments 1 + 2 are draw from $NB(\mu_c, \kappa)$.
The abundances in treatments 3 + 4 are reduced by the factor r and are drawn from $NB(r \times \mu_c, \kappa)$).
$\kappa$ is equal between treatments.
Therefore, the LOEC is at treatment 3 and NOEC a treatment 2.


## Controls
$\mu_c$, $\kappa$ and r can be controlled by the sliders on the left.



## Output

The summary tab gives a graphical representation (assuming a sample size of 1000) and also mean and variances per treatment.
