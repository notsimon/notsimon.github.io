---
title: Interpolation of the magnetic field for indoor positioning
subtitle: Interpolation du champ magnétique pour la localisation en intérieur
author: Simon Guillot
draft: true
---

# Sevenhugs

<figure>
<iframe width="560" height="315"
        src="https://www.youtube.com/embed/jB7iuBKcfZw?iv_load_policy=3" frameborder="0"
        allowfullscreen></iframe>
</figure>


* To track its position and orientation in the room, the remote merges:
    * Measurements from a set of embedded sensors: a gyroscope, an accelerometer, a
magnetometer
    * The distances to three beacons and its charging base
* During setup, the user places its real and virtual objects using the remote

# Indoor usage of a magnetometer

![Variations in color represent variations in 
inclination](/images/map.svg){style="width: 70%; margin: -2.5em 0"}

* Important variations of the intensity and direction in the magnetic field
* The magnetometer must be recalibrated when exposed to strong fields

# Estimation of the magnetic field

![](/images/map_measurements.svg){width=80%}

The model learns from a set of readings $D^\star = \{ (x_i^\star, y_i^\star) | i 
\in [1..N] \}$ where

- $y^\star \in \mathbb{R}^3$ is the magnetometer reading transformed to be 
  independent of the orientation of the device
- $x^\star \in \mathbb{R}^3$ its associated position

We are looking for an approximation $B$ of the field such that
$$
B(x^\star) \approx y^\star
$$

# Estimation of the magnetic field

![](/images/map_ground_truth.svg){width=80%}

Given a (new) position, the model is expected to predict the value of the 
magnetic field.

<hr></hr>

**Difficulty:** How to generalize well enough to be accurate in unexplored areas 
?

# A priori knowledge

Electromagnetic phenomena are governed by the four Maxwell's equations, two 
involve the magnetic field.

<div class="next">
$$
  \nabla \cdot B = 0
$$

**The magnetic field is divergence-free**: magnetic monopoles do not exist.

</div>

<div class="next" style="margin-top: 3em">
$$
  \nabla \times B = \mu_0 \left(J + \epsilon_0 \frac{\partial E}{\partial t}\right)
$$

Around a closed loop, it depends on the electric current.
</div>

<div class="next">
We are assuming:

- no free current in the air
- no time-dependent effects due to moving charge

Ampere's circuital law reduces to:

$$
  \nabla \times B = 0
$$

**The magnetic field is irrotational.**
</div>

# The magnetic potential

An irrotational vector field can be represented as *the gradient of a scalar 
field*

$$
  B(x) = \nabla_x \psi(x)
$$

Our goal is now to find a function $\psi$

- in $\mathbb{R}^3 \mapsto \mathbb{R}$
- differentiable w.r.t the position $x$

[^wahlstrom]: Niklas Wahlström et al., "Modeling magnetic fields using Gaussian 
  Processes", *2013 International Conference on Acoustics, Speech and Signal 
  Processing (ICASSP)*

[^solin]: Arno Solin et al., "Modeling and interpolation of the ambient magnetic 
  field by Gaussian processes", *arXiv:1509.04634*

[[TODO figure with potential and its associated gradient]]

# Proposed model

[[TODO figure with potential and the grid of points]]

Interpolation from a set of $K$ anchor points $M = \{(c_k, w_k) | k \in [1, 
K]\}$ where

- $c_k$ is the position of the anchors in $\mathbb{R}^3$
- $w_k$ its associated scalar value

# Proposed model

We define $\psi$ as

$$
  \psi(x) = \sum_{k=1}^K w_k \phi(x, c_k)
$$

where $\phi$ gives a weight to the anchors depending on their distance.

<div class="next">
<hr></hr>
We chose $\phi$ to be a Gaussian:
$$
  \phi(x, c_k) = e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

![Example of radial basis functions suitable to be used as 
$\phi$](/images/rbf.svg){width=280px}
</div>

# Kalman filters

- Optimal for **linear optimization problems** involving Gaussian noises
- Estimate a state $w$ with its associated covariance matrix $P$

<div class="algorithm">
#### Algorithm outline

1. **Predict**
    - Use a "transition model" to forward the estimated state in time

2. **Update**
    - Project the estimated state into the measurement space using an 
      "observation model"
    - Compare to the actual measurement
    - Correct the estimation
</div>

Advantages

- Online estimation of the parameters, each measurement is observed only 
  **once**
- Fine-grained uncertainty on the measurements and estimation

# Reducing to a linear optimization problem

$$
\begin{aligned}
  \psi(x) &= \sum_{k=1}^K w_k \phi(x, c_k) \\\\
  B(x)    &= \nabla_x \psi(x) \\\\
          &= \sum_{k=1}^K w_k \nabla_x \phi(x, c_k)
\end{aligned}
$$

<hr></hr>

If we do not fit the positions $c_k$ of the anchors, $B$ can then be written

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

# Estimation of the potential using a Kalman filter

<div class="algorithm">
#### For each measurement $(x, y)$

1. Compute the $H$ matrix for the current position $x$
2. Compute the residual $z$ between the true and estimated measurements, its 
   associated covariance $S$ and the Kalman gain $K$ using

$$
\begin{aligned}
z &= y - H w \\
S &= R + H P H^\top \\
K &= P H^\top S^{-1} \\
\end{aligned}
$$

3. Update the state $w$ and its covariance $P$ with

$$
\begin{aligned}
w &\leftarrow w + K z \\
P &\leftarrow (I - K H) P
\end{aligned}
$$

</div>

- $S \in \mathcal{M}^{3\times3}$, its inverse is easy to compute
- $H$ grows linearly with the number of anchor points, $P$ quadratically

# Initial results

$c_k$ such that the points are spread of a grid covering a least the area of 
interest with a resolution of 25cm

# Non-linear version
## Optimization by stochastic gradient descent

We are looking for the function $\psi$ that minimizes the loss

$$
  \mathcal{L} = \sum_{i=1}^N \delta(\nabla \psi(x_i^\star), y_i^\star)
$$

where $\delta \in (\mathbb{R}^3, \mathbb{R}^3) \mapsto \mathbb{R}$ is a 
measurement of the error.

<hr></hr>

Let $\theta = (w_1, \cdots, w_K, c_1, \cdots, c_K)^\top$

To minimize $\mathcal{L}_\psi$, update iteratively $\theta$ using

$$
\theta \leftarrow \theta - \epsilon \nabla_\theta \delta(\nabla \psi(x^\star), 
y^\star)
$$

where

- $\epsilon$ is a constant controling the learning rate
- $(x^\star, y^\star)$ is an element of $S^\star$ chosen at random

## Contraint on the divergence

As a regularizer in the SGD

As an observation in the Kalman filter
