---
title: Magnetic field interpolation
draft: true
---

## Maxwell's equations and the magnetic scalar potential

Our primary interest is the magnetic field, denoted by $B$ in two of the 
[Maxwell's 
equations](https://en.wikipedia.org/wiki/Maxwell%27s_equations#Formulation_in_SI_units_convention):

$$
  grad * B = 0
$$

$$
  grad xx B = mu_0 (J + epsilon_0 (del E) / (del t))
$$

where $mu_0$ and $epsilon_0$ are respectively the permeability and permittivity 
of vacuum, $J$ is the current density and $E$ the electric field.

We are assuming that there is no free current in the air and no time-dependent 
effects due to moving charges, thus the Ampère's circuital law reduces to:

$$
  grad xx B = 0
$$

In order to be plausible, our approximation of the magnetic field should obey 
these two laws. In other words, it must be *divergence-free* and *irrotational*.  

An irrotational vector field can be fully described as the gradient of a scalar 
field, called a scalar potential:

$$
  B = - grad psi
$$

Thus, by modeling $psi$ in place of $B$ directly, we are implicitly modeling a 
vector field that follows the second law. The first law will be added as a 
regularization in the optimization procedure.

## Differentiable interpolation

The map is represented by a set of anchor points $M = {(c_k, w_k) | k in [1, 
K]}$, where $c_k$ is the position of the anchor in $RR^2$ and $w_k$ its 
associated value – a scalar potential in this case.

In order to have a model differentiable with respect to the position and the 
elements of the map $M$, the [interpolation function](https://en.wikipedia.org/wiki/Multivariate_interpolation) 
is defined to be in the form:

$$
  psi(x) = sum_(k=1)^K w_k phi(x, c_k)
$$

where $x in RR^2$ is a point in space, $w_k$ the value of the map at the anchor 
$k$ located in position $c_k$, and $phi in (RR^2, RR^2) |-> RR$ is a 
differentiable radial basis function: it gives a weight to the anchors depending 
on their distance to the point $x$. In essence, this approach is similar to [the 
attention mechanism](http://distill.pub/2016/augmented-rnns/) in deep learning.

With this definition of the scalar potential function $psi$, the estimated 
magnetic field $B$ is:

$$
  B(x) = grad_x psi(x) = sum_(k=1)^K w_k grad_x phi(x, c_k)
$$

<span style="font-size: smaller">
We dropped the negative sign to simplify the equations even though this not 
standard among mathematicians and physicists, the true value of the potential is 
of little interest of our application.
</span>

*[[TODO mention Barnes interpolation]]*

### Radial basis functions

*[[TODO plot exp(-x^2), 1/(1+x^2), 1/sqrt(1+x^2)]]*

## Learning the map parameters

Given a set of known points $S^{***} = { (x_i^{***}, y_i^{***}) | i in [1..N] }$ 
where $x_i^{***} in RR^2$ is a position in space and $y_i^{***} in RR^2$ the 
observed value of the $B$ field at this position, we are looking for a 
differentiable function $psi in RR^2 |-> RR$ that minimizes the loss

$$
  L_psi = sum_(i=1)^N R(grad psi(x_i^{***}), y_i^{***})
$$

where $R in (RR^2, RR^2) |-> RR$ is a measurement of the error between the 
output of the model and the expected value.

### Stochastic gradient descent

We consider the positions $c_k$ of the anchors to be fixed, the parameters of 
the model are then $theta = ((w_1, ..., w_k))^TT$. Minimize the loss $L_psi$ 
using a stochastic gradient descent comes down to update the parameters 
iteratively using

$$
theta larr theta - epsilon grad_theta R(grad_x psi(x^{***}), y^{***})
$$

where $epsilon$ is a constant controling the learning rate and $(x^{***}, 
y^{***})$ is an element of $S^{***}$ chosen at random. This is the simplest form 
of SGD, in practice there is [many improvements](http://sebastianruder.com/optimizing-gradient-descent/) 
to make it much more effective.

## Initial tests and results

We chose $phi$ to be a in the form of a Gaussian function such that

$$
  phi(x, c_k) = e^(-||x - c_k||^2 / (2 sigma^2))
$$

where $sigma$ is an hyperparameter related to the [full width at half 
maximum](https://en.wikipedia.org/wiki/Full_width_at_half_maximum) (it gives the 
"spread" of the weighting). Its gradient w.r.t the position $x$ is

$$
  grad_x phi(x, c_k) = (c_k - x)/(sigma^2) e^(-||x - c_k||^2 / (2 sigma^2)
$$

Therefore, we have

$$
B(x) = sum_(k=1)^K w_k grad_x phi(x, c_k)
     = sum_(k=1)^K w_k (c_k - x)/(sigma^2) e^(-||x - c_k||^2 / (2 sigma^2)
$$

In the following experiement, the model was trained for 10 epochs on a set of 64 
samples taken at random from a ground truth made of 380 samples:

![](/images/gaussian-map-model.svg){width=100%}

## Future work
### Sum of hot spots
*[[TODO explain the similar form, highlight the very different semantic]]*
