---
title: Interpolation of the magnetic field for indoor tracking
---

In this report, I will introduce one of my attempts at modeling the magnetic 
field of a room using a continuous representation. The first goal of this work 
is to assist the tracking of the orientation of a device by taking into account 
the multiple distortions of the field observed indoor: this is a step toward a 
positioning algorithm that takes advantage of these anomalies.

## Maxwell's equations and the magnetic scalar potential

In order to be plausible, our approximation of the magnetic field should obey 
some properties well known by physicists. The magnetic field is denoted by $B$ 
in two of the [Maxwell's 
equations](https://en.wikipedia.org/wiki/Maxwell%27s_equations#Formulation_in_SI_units_convention):

$$
  \nabla \cdot B = 0
$$

$$
  \nabla \times B = \mu_0 \left(J + \epsilon_0 \frac{\partial E}{\partial t}\right)
$$

where $\mu_0$ and $\epsilon_0$ are respectively the permeability and 
permittivity of vacuum, $J$ is the current density and $E$ the electric field.

We are assuming that there is no free current in the air and no time-dependent 
effects due to moving charges, thus the Ampère's circuital law reduces to:

$$
  \nabla \times B = 0
$$

In other words, the magnetic field we are modeling must be *divergence-free* and 
*irrotational*: these two constraint drastically reduces the search space of 
possible vector fields.

An irrotational vector field can be fully described as the gradient of a scalar 
field, called a scalar potential:

$$
  B = - \nabla \psi
$$

Thus, by modeling $\psi$ in place of $B$ directly, we are implicitly modeling a 
vector field that follows the second law.[^wahlstrom][^solin] The first law will 
be added as a regularization in the optimization procedure.

[^wahlstrom]: Niklas Wahlström et al., "Modeling magnetic fields using Gaussian 
  Processes", *2013 International Conference on Acoustics, Speech and Signal 
  Processing (ICASSP)*

[^solin]: Arno Solin et al., "Modeling and interpolation of the ambient magnetic 
  field by Gaussian processes", *arXiv:1509.04634*

## Differentiable interpolation

The map is represented by a set of anchor points $M = \{(c_k, w_k) | k \in [1, 
K]\}$, where $c_k$ is the position of the anchor in $\mathbb{R}^2$ and $w_k$ its 
associated value – a scalar potential in this case.

In order to have a model differentiable with respect to the position and the 
elements of the map $M$, the [interpolation function](https://en.wikipedia.org/wiki/Multivariate_interpolation) 
is defined to be in the form:

$$
  \psi(x) = \sum_{k=1}^K w_k \phi(x, c_k)
$$

where $x \in \mathbb{R}^2$ is a point in space, $w_k$ the value of the map at 
the anchor $k$ located in position $c_k$, and $\phi \in (\mathbb{R}^2, 
\mathbb{R}^2) \mapsto \mathbb{R}$ is a differentiable radial basis function: it 
gives a weight to the anchors depending on their distance to the point $x$.  In 
essence, this approach is similar to the attention mechanism in deep 
learning.[^attention]

With this definition of the scalar potential function $psi$, the estimated 
magnetic field $B$ is:

$$
  B(x) = \nabla_x \psi(x) = \sum_{k=1}^K w_k \nabla_x \phi(x, c_k)
$$

<span style="font-size: smaller">
We dropped the negative sign to simplify the equations even though this not 
standard among mathematicians and physicists, however the true value of the 
potential is of little interest to our application.
</span>

[^attention]: Olah & Carter, "Attention and Augmented Recurrent Neural 
  Networks", *Distill, 2016*. http://distill.pub/2016/augmented-rnns/

### Radial basis functions

A radial basis function satisfies the property $\phi(x) = \phi(||x||)$: its 
value depends only on the distance from the origin – or another point called the 
center. Some commonly used RBF are shown bellow.

![](/images/rbf.svg){width=350px}

## Learning the map parameters

Given a set of known points $S^\star = \{ (x_i^\star, y_i^\star) | i \in [1..N] 
\}$ where $x_i^\star \in \mathbb{R}^2$ is a position in space and $y_i^\star \in 
\mathbb{R}^2$ the observed value of the $B$ field at this position, we are 
looking for a differentiable function $\psi \in \mathbb{R}^2 \mapsto \mathbb{R}$ 
that minimizes the loss

$$
  \mathcal{L}_\psi = \sum_{i=1}^N R(\nabla \psi(x_i^\star), y_i^\star)
$$

where $R \in (\mathbb{R}^2, \mathbb{R}^2) \mapsto \mathbb{R}$ is a measurement 
of the error between the output of the model and the expected value.

![](/images/magnetic-field-data.svg){width=400px}

### Stochastic gradient descent

We consider the positions $c_k$ of the anchors to be fixed, the parameters of 
the model are then $\theta = (w_1, \cdots, w_k)^\top$. Minimize the loss 
$\mathcal{L}_\psi$ using a stochastic gradient descent comes down to update the 
parameters iteratively using

$$
\theta \leftarrow \theta - \epsilon \nabla_\theta R(\nabla_x \psi(x^\star), 
y^\star)
$$

where $\epsilon$ is a constant controling the learning rate and $(x^\star, 
y^\star)$ is an element of $S^\star$ chosen at random. This is the simplest form 
of SGD, in practice there is [many 
improvements](http://sebastianruder.com/optimizing-gradient-descent/) to make it 
much more effective.

## Initial tests and results

We chose $\phi$ to be a in the form of a Gaussian function such that

$$
  \phi(x, c_k) = e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

where $\sigma$ is an hyperparameter related to the [full width at half 
maximum](https://en.wikipedia.org/wiki/Full_width_at_half_maximum) – it gives 
the *spread* of the weighting. Its gradient w.r.t the position $x$ is

$$
  \nabla_x \phi(x, c_k) = \frac{c_k - x}{\sigma^2}
                          e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

Therefore, we have

$$
B(x) = \sum_{k=1}^K w_k \nabla_x \phi(x, c_k)
     = \sum_{k=1}^K w_k \frac{c_k - x}{\sigma^2} e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

In the following experiement, the model was trained for 10 epochs on a set of 64 
samples taken at random from a ground truth made of 380 samples:

![](/images/radial-basis-map.svg){width=400px}

## Future work

### Sum of hot spots

Instead of viewing this problem as an interpolation between control point, we 
could try to model the sources of anomalies in the magnetic field: the control 
points would then be objects such as a furniture made of metal, or a strong 
magnets – in a speaker for instance. We would thus try to optimize their 
positions and the strength of their influence on the magnetic potential.