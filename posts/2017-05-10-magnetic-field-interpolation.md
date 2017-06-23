---
title: Real time interpolation of the magnetic field
---

This report introduces one of my attempts at modelling the magnetic field of a 
room using a continuous representation. The first goal of this work is to assist 
the tracking of the orientation of a device by taking into account the multiple 
distortions of the field observed indoor: this is a step toward a positioning 
algorithm that takes advantage of these anomalies.

## Maxwell's equations and the magnetic scalar potential

In order to be plausible, an approximation of the magnetic field should obey 
some properties well known by physicists. All electromagnetic phenomena are 
governed by [Maxwell's 
equations](https://en.wikipedia.org/wiki/Maxwell%27s_equations#Formulation_in_SI_units_convention), 
two of them involve the magnetic field:

$$
  \nabla \cdot B = 0
$$

$$
  \nabla \times B = \mu_0 \left(J + \epsilon_0 \frac{\partial E}{\partial t}\right)
$$

where $B \in \mathbb{R}^3 \mapsto \mathbb{R}^3$ is the magnetic field, $\mu_0$ 
and $\epsilon_0$ are respectively the permeability and permittivity of vacuum, 
$J$ is the current density and $E$ the electric field. 

The first of these two laws states that the magnetic field is divergence free, 
in other words, *magnetic monopoles do not exist*. We are assuming that there is 
no free current in the air and no time-dependent effects due to moving electric 
charges, thus the Ampère's circuital law reduces to:

$$
  \nabla \times B = 0
$$

In these conditions, the magnetic field is then said to be *irrotational*. An 
irrotational vector field can be fully described as the gradient of a scalar 
field, called a scalar potential:

$$
  B = - \nabla \psi
$$

Thus, by modelling $\psi$ in place of $B$ directly, we are implicitly modelling 
a vector field that follows the second law.[^wahlstrom][^solin] The first law 
will be added as a regularization in the optimization procedure.

[^wahlstrom]: Niklas Wahlström et al., "Modelling magnetic fields using Gaussian 
  Processes", *2013 International Conference on Acoustics, Speech and Signal 
  Processing (ICASSP)*

[^solin]: Arno Solin et al., "Modelling and interpolation of the ambient 
  magnetic field by Gaussian processes", *arXiv:1509.04634*

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

With this definition of the scalar potential function $\psi$, the estimated 
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
center. Some commonly used RBF are shown below.

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
the model are then $\theta = (w_1, \cdots, w_K)^\top$. Minimize the loss 
$\mathcal{L}_\psi$ using a stochastic gradient descent (SGD) comes down to 
updating the parameters iteratively using

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

We chose $\phi$ to be the form of a Gaussian function such that

$$
  \phi(x, c_k) = e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

where $\sigma$ is a hyperparameter related to the [full width at half 
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

In the following experiment, the model was trained for 10 epochs on a set of $N 
= 64$ samples taken at random from a ground truth made of 380 samples:

![](/images/radial-basis-map.svg){width=400px}

As for the number of control points, it has been chosen such as there is a point 
every 25cm on an area covering the entire scene.


## Implementation using a Kalman filter

<div style="font-size: smaller; font-style: italic">
Updated on June 27, 2017.
</div>

$$
  B(x) = \nabla_x \psi(x) = \sum_{k=1}^K w_k \nabla_x \phi(x, c_k)
$$

Going back to equation 2.2, we can take the $c_k$ as hyperparameters – i.e. 
fixing their value during optimization. We can now express $B(x)$ as a linear 
function of the parameters we are trying to estimate (the weights $w_k$) such 
that:

$$
  B = H \cdot w
$$

where $H$ is the $3 \times K$ matrix

$$
H =
\begin{bmatrix}
  \nabla_x \phi(x, c_1) & \cdots & \nabla_x \phi(x, c_K)
\end{bmatrix}
$$

and $w$ the column vector

$$
\begin{bmatrix}
  w_1 \\
  \vdots \\
  w_K
\end{bmatrix}
$$

When defined as a linear optimization problem, the research of the most likely 
field potential fits in the (original) [Kalman 
filter](https://en.wikipedia.org/wiki/Kalman_filter) framework – which is, under 
some conditions, well suited algorithm for real time estimation of unknown 
variables. In particular, we are making use of the *update* step of the 
algorithm during which observations are used to correct the estimation of the 
state of the system.

An update iteration of the estimated state $w$ given a measurement $z$ goes as 
follow:

<div class="algorithm">

1. Compute the $H$ matrix for the current position $x$
2. Compute the Kalman gain $K$ and residual $y$ between the true and estimated 
   measurements using

$$
\begin{aligned}
y &= z - H w \\
S &= R + H P H^\top \\
K &= P H^\top S^{-1} \\
\end{aligned}
$$

3. Update the state $w$ and its covariance $P$ with

$$
\begin{aligned}
w &\leftarrow w + K y \\
P &\leftarrow (I - K H) P
\end{aligned}
$$

</div>

In our case, the measurement $z$ is the output of the magnetometer in 
$\mathbb{R}^3$ (transformed to take into account the orientation of the device). 
Thus, $S$ is in $\mathcal{M}^{3\times3}$ and its inversion is easy to compute.

## Future work

### Regularization using the divergence

We used only one property of the magnetic field, the divergence-free property 
could be introduced in the system as a regularizer such that:

$$
\nabla \cdot B(x) = \nabla \cdot \nabla \psi(x)
= \sum_{k=1}^K w_k \nabla_x \cdot \nabla_x \phi(x, c_k)
$$

which can be rewritten using the scalar Laplacian:

$$
\nabla \cdot B(x) = \sum_{k=1}^K w_k \Delta_x \phi(x, c_k)
$$

### Sum of hot spots

Instead of viewing this problem as an interpolation between control points, we 
could try to model the sources of anomalies in the magnetic field: the control 
points would then be objects such as a furniture made of metal, or a strong 
magnets – in a speaker for instance. We would thus try to optimize their 
positions and the strength of their influence on the magnetic potential.
