---
title: Real time interpolation of the magnetic field
subtitle: Interpolation du champ magnétique en temps réel
author: Simon Guillot
---

# Sevenhugs

![](/media/sevenhugs/pointing.gif){width="90%"}

* To track its position and orientation in the room, the remote merges:
    * Measurements from a set of embedded sensors: a gyroscope, an accelerometer, a
magnetometer
    * The distances to three beacons and its charging base
* During setup, the user places its real and virtual objects using the remote

# Indoor usage of a magnetometer

![Variations in color represent variations in 
inclination](/media/map.svg){style="width: 70%; margin: -2.5em 0"}

* Important variations of the intensity and direction in the magnetic field
* The magnetometer must be recalibrated when exposed to strong fields

# Estimation of the magnetic field

![](/media/magnetic-field-interpolation/map_measurements.svg){width=80%}

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

![](/media/magnetic-field-interpolation/map_ground_truth.svg){width=80%}

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
field* [^wahlstrom] [^solin]

$$
  B(x) = -\nabla_x \psi(x)
$$

<div class="next">
Our goal is now to find a function $\psi$

- in $\mathbb{R}^3 \mapsto \mathbb{R}$
- differentiable w.r.t the position $x$
</div>

[^wahlstrom]: Niklas Wahlström et al., "Modeling magnetic fields using Gaussian 
  Processes", *2013 International Conference on Acoustics, Speech and Signal 
  Processing (ICASSP)*

[^solin]: Arno Solin et al., "Modeling and interpolation of the ambient magnetic 
  field by Gaussian processes", *arXiv:1509.04634*

![A scalar field and its associated 
gradient](/media/magnetic-field-interpolation/potential_example.svg){width=80%}

# Proposed model

![The model is supported by a set of anchors 
points](/media/magnetic-field-interpolation/model_anchors.svg){width=45%}


Interpolation from a set of $K$ anchor points $M = \{(c_k, w_k) | k \in [1, 
K]\}$ where

- $c_k$ is the position of the anchors in $\mathbb{R}^3$
- $w_k$ its associated scalar value

# Proposed model

![The model is supported by a set of anchors 
points](/media/magnetic-field-interpolation/model_interpolation.svg){width=45%}

We define $\psi$ as

$$
  \psi(x) = \sum_{k=1}^K w_k \phi(x, c_k)
$$

where $\phi$ gives a weight to the anchors depending on their distance.

# Proposed model

$$
\begin{aligned}
  B(x)    &= \nabla_x \psi(x) \\\\
          &= \sum_{k=1}^K w_k \nabla_x \phi(x, c_k)
\end{aligned}
$$

We chose $\phi$ to be a Gaussian:
$$
  \phi(x, c_k) = e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
$$

its gradient is

$$
  \nabla_x \phi(x, c_k) = \frac{c_k - x}{\sigma^2}
                          e^{-\frac{||x - c_k||^2}{2 \sigma^2}}
  \quad \in \mathbb{R}^3
$$

<div class="next">
<hr style="margin-bottom: 1em"></hr>
![](/media/magnetic-field-interpolation/rbf.svg){width=240px}
</div>

# Reducing to a linear optimization problem

If we **do not fit the positions $c_k$ of the anchors**, $B$ can then be written

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
w =
\begin{bmatrix}
  w_1 \\
  \vdots \\
  w_K
\end{bmatrix}
$$

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

- Resolution and sigma of 25cm
- Addition of a bias – a linear function of the position when viewed as a 
  potential

<center>
<video width="72%" controls>
  <source src="/media/magnetic-field-interpolation/model_learning.mp4" 
  type="video/mp4">
  Your browser does not support the video tag.
</video>
</center>

# Initial results

- Resolution and sigma of 25cm
- Addition of a bias – a linear function of the position when viewed as a 
  potential

![Model confidence, using the anchors 
variance](/media/magnetic-field-interpolation/model_confidence.svg){width=90%}

# Conclusion

Summary:

- Modelisation of a magnetic potential instead of the vector field
- Continuous interpolation over a set of chosen points
- Inclusion of a second law of Maxwell using a regularization

Novelty of the method:

- Usage of an attention mechanism with a grid of potentials
- Implemention in a Kalman filter

# Non-linear version
## Optimization by stochastic gradient descent

We are looking for the function $\psi$ that minimizes the loss

$$
  \mathcal{L} = \sum_{i=1}^N \delta(\nabla \psi(x_i^\star), y_i^\star)
$$

where $\delta \in (\mathbb{R}^3, \mathbb{R}^3) \mapsto \mathbb{R}$ is a 
measurement of the error.

Let $\theta = (w_1, \cdots, w_K, c_1, \cdots, c_K)^\top$, to minimize 
$\mathcal{L}_\psi$, update iteratively $\theta$ using

$$
\theta \leftarrow \theta - \epsilon \nabla_\theta \delta(\nabla \psi(x^\star), 
y^\star)
$$

where

- $\epsilon$ is a constant controling the learning rate
- $(x^\star, y^\star)$ is an element of $S^\star$ chosen at random

# Constraint on the divergence

$$
\begin{aligned}
\nabla \cdot B(x) &= \nabla \cdot \nabla \psi(x) \\ \\
                  &= \sum_{k=1}^K w_k \nabla_x \cdot \nabla_x \phi(x, c_k)
\end{aligned}
$$

which can be rewritten using the scalar Laplacian:

$$
\nabla \cdot B(x) = \sum_{k=1}^K w_k \Delta_x \phi(x, c_k)
$$

* As a regularizer during a SGD, the loss becomes
    $$
    \mathcal{L} = \sum_{i=1}^N \delta(\nabla \psi(x_i^\star), y_i^\star)
    + mean_j\left(\nabla \cdot B(x_j)\right)
    $$
* As an observation in the Kalman filter: the model is expected to produce a of 
  zero for a number of random positions.

