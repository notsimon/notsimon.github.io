---
title: Interpolation of the magnetic field for indoor positioning
subtitle: Interpolation du champ magnétique pour la localisation en intérieur
author: Simon Guillot
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

# A priori informations

Maxwell's equations applied to magnetostatic:



