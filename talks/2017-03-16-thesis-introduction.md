---
title: Multisensors Localisation and Mapping
subtitle: Localisation et Cartographie Multicapteurs <br/> Thèse CIFRE en collaboration avec Sevenhugs
author: Simon Guillot
---

# Sevenhugs

<figure>
<iframe width="560" height="315"
        src="https://www.youtube.com/embed/jB7iuBKcfZw?iv_load_policy=3" frameborder="0"
        allowfullscreen></iframe>
</figure>

* Founded in 2014
* Seed funding of 1.4M€ in 2014, series A round of 14M€ in 2016
* R&D in Paris, marketing relocated in San Francisco


# Sevenhugs' remote

![](/images/sr_setup.gif){width=50%}

* To track its position and orientation in the room, the remote merges:
    * Measurements from a set of embedded sensors: a gyroscope, an accelerometer, a
magnetometer
    * The distances to three beacons and its charging base
* During setup, the user places its real and virtual objects using the remote


# State estimation

![](/images/fusion.svg){width=60%}

The inputs are

- $\{\bar{r_i} | i \in [1, N]\}$ the (very noisy) distances to the beacons
- $\bar{a}$ the measurement extracted from the accelerometer
    - effect of gravity must be subtracted
    - then integrated twice to retrieve motion
- $\bar{\omega}$ the angular velocity given by the gyroscope
- $\bar{m}$ the magnetometer
    - the magnetic field direction can be useless while being correct

$$ z = (\bar{r}, \bar{a}, \bar{\omega}, \bar{m})^\top, z \in \mathbb{R}^{N + 9} 
$$


# State estimation

![](/images/fusion.svg){width=60%}

The estimation of the targeted objects requires at least

* the position $p$
* the orientation $q$


# State estimation

![](/images/fusion_kalman.svg){width=60%}

Adding information on the dynamics of the body, for instance

- the velocity $v$
- the acceleration $a$
- the angular velocity $\omega$

leads to the usage of kinematics models, required by state-space estimators such
as **Kalman filters**.

The augmented state to estimate becomes:

$$ x = (p, v, a, q, \omega)^\top $$
$$ x \in \mathbb{R}^{15} $$


# Indoor usage of a magnetometer

![Direction of the magnetic field on a plane at one meter above the ground 
(vectors are normalized)](/images/map_normalized.svg){style="width: 70%; margin: 
-2.5em 0"}

* Some regions can be up to 45º off from the north direction


# Indoor usage of a magnetometer

![Variations in color represent variations in 
inclination](/images/map.svg){style="width: 70%; margin: -2.5em 0"}

* Important variations of the norm and inclination of the magnetic field
* Both can be measured without ambiguities
* These can provide hints on the position


# Thesis subject

Industrial goal: find ways of positioning a device indoor without modifications 
of the environment (eg. adding beacons or markers)

- How to apply _Simulatenous Localisation and Mapping_ techniques when the
dimensionality of the inputs is low and the odometry of poor quality ?
    - Mostly, how to properly close loops and eliminate ambiguities ?
- How to augment the system with informations extracted from electromagnetic
signals ? (Bluetooth, WiFi etc.)
- How to compress – due to hardware constraints – and recall the heterogeneous 
  data found in the environment ?


# Work overview

- Comparison of multiple adaptative filters for this non-linear problem
    - Extended Kalman Filter, Unscented KF, Recurrent Neural Nets
- Trajectories and sensors simulation
- Sensors calibration
    - Estimate the fixed transformations applied to the measurements
- Numerous heuristics based on the remote usage and environment, e.g.:
    - Absence of acceleration is more likely due to inactivity than 
    constant speed
    - Altitude can be approximated using the inclination
- Recently, the acquisition of a ground-truth using a set of infrared cameras

