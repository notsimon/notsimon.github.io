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


# State estimation

![](/images/fusion_kalman.svg){width=60%}

The inputs are

- $\{\bar{r_i} | i \in [1, N]\}$ the (very noisy) distances to the beacons
- $\bar{a} \in \mathbb{R}^3$ the measurement extracted from the accelerometer
    - effect of gravity must be subtracted
    - then integrated twice to retrieve motion
- $\bar{\omega} \in \mathbb{R}^3$ the angular velocity given by the gyroscope
    - some calibration parameters must be estimated continuously
- $\bar{m} \in \mathbb{R}^3$ the magnetometer
    - must be recalibrated when exposed to strong magnetic fields
    - the magnetic field direction isn't always the magnetic north!

# State estimation

![](/images/fusion_map.svg){width=60%}

Pose estimation comes down to estimating

- the position $p$ in $\mathbb{R}^3$
- the orientation $q$ in $\mathbb{R}^3$, represented as a rotation quaternion for convenience

To which we add information on the dynamics of the body, for instance

- the velocity $v$
- the acceleration $a$
- the angular velocity $\omega$

The system's state to estimate is in $\mathbb{R}^{15}$.

# Model's inputs and outputs

![](/images/map_measurements.svg){width=80%}

The model input is a set of magnetometer readings with

- the associated absolute positions
- the orientation of the sensor substracted

# Model's inputs and outputs

![](/images/map_ground_truth.svg){width=80%}

Given a position, the model is expected to output the value of the magnetic 
field in $\mathbb{R}^3$.

# A priori knowledge

Electromagnetic phenomena are governed by the four Maxwell's equations, two 
involve the magnetic field.

$$
  \nabla \cdot B = 0
$$

[**The magnetic field is divergence-free**: magnetic monopoles do not 
exist.]{.next}

$$
  \nabla \times B = \mu_0 \left(J + \epsilon_0 \frac{\partial E}{\partial t}\right)
$$

[The magnetic field induced around a closed loop depends on the electric 
current.]{.next}

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
field*, called the field potential.

$$
  B(x) = - \nabla_x \psi(x)
$$

where $B \in \mathbb{R}^3 \mapsto \mathbb{R}^3$ and
$\psi$ is a *differentiable* function in $\mathbb{R}^3 \mapsto \mathbb{R}$

By modeling $\psi$ in place of $B$ directly, we are implicitly modeling a vector 
field that follows the second law.[^wahlstrom]

[^wahlstrom]: Niklas Wahlström et al., "Modeling magnetic fields using Gaussian 
  Processes", *2013 International Conference on Acoustics, Speech and Signal 
  Processing (ICASSP)*

[^solin]: Arno Solin et al., "Modeling and interpolation of the ambient magnetic 
  field by Gaussian processes", *arXiv:1509.04634*


# Proposed model

The map is represented by as set of $K$ anchor points $M = \{(c_k, w_k) | k \in 
[1, K]\}$, where

- $c_k$ is the position of the anchors in $\mathbb{R}^3$
- $w$ their associated value, a scalar in this case

In order to have a model differentiable w.r.t the position and elements of the 
map $M$, we define $\psi$ as

$$
  \psi(x) = \sum_{k=1}^K w_k \phi(x, c_k)
$$

where $\phi \in (\mathbb{R}^3, \mathbb{R}^3) \mapsto \mathbb{R}$ is a 
differentiable radial basis function: it gives a weight to the anchors depending 
on their distance.

![](/images/rbf.svg){width=300px}

# Learning the map parameters

Given a set of known points $S^\star = \{ (x_i^\star, y_i^\star) | i \in [1..N] 
\}$ where

- $x_i^\star \in \mathbb{R}^3$ is a position in space
- $y_i^\star \in \mathbb{R}^3$ the observed value of the $B$ field at this 
  position

We are looking for the function $\psi$ that minimizes the loss

$$
  \mathcal{L} = \sum_{i=1}^N R(\nabla \psi(x_i^\star), y_i^\star)
$$

where $R \in (\mathbb{R}^3, \mathbb{R}^3) \mapsto \mathbb{R}$ is a measurement 
of the error.

# Stochastic gradient descent

$\theta = (w_1, \cdots, w_K, c_1, \cdots, c_K)^\top$

Minimizing the loss $\mathcal{L}_\psi$ comes down to updating the parameters 
iteratively using

$$
\theta \leftarrow \theta - \epsilon \nabla_\theta R(\nabla_x \psi(x^\star), 
y^\star)
$$

$\epsilon$ is a constant controling the learning rate and $(x^\star, y^\star)$ 
is an element of $S^\star$ chosen at random

# Reducing to a linear optimization problem

By fixing the values of the $c_k$ – i.e. taking them as hyperparameters of the 
model – we can now express $B(x)$ as a linear function of the parameters we are 
trying to estimate (the weights $w_k$) such that:

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

When defined as a linear optimization problem, the quest for the most likely 
field potential fits in the (original) Kalman filter framework.

# 30 seconds intro to Kalman filtering

Iterative

- Predict
- Update

Measurement of uncertainty on the data and estimation.

# Estimation of the potential using a Kalman filter

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
Thus, $S$ is in $\mathcal{M}^{3\times3}$ and its inverse is easy to compute.

# Initial results

$c_k$ such that the points are spread of a grid covering a least the area of 
interest with a resolution of 25cm
