---
title: Blind estimation of the potential of a magnetic field
draft: true
---

This mixture model use a set of Gaussian defined at anchor points such that
${(a^{(k)}, b^{(k)}, c^{(k)}) | k in [1, K]}$ where $a in RR$ is the height of 
the Gaussian, $b^{(k)} in RR^2$ is the position on the plane, and $c^{(k)} in 
RR$ which is related to the [full width at half 
maximum](https://en.wikipedia.org/wiki/Full_width_at_half_maximum).

Given a position $p = [x,y]^T$, the scalar potential at this point is governed 
by the following mixture model:

$$
  varphi(p) =
  sum_(k=1)^K (a^{(k)} * e^(-||p - b^{(k)}||^2 / (2 (c^{(k)})^2)))
$$

Its gradient w.r.t to $p$ is

$$
grad varphi(p) = [[del/(del x) varphi(p)],
                  [del/(del y) varphi(p)]]
$$

where

$$
del/(del x) varphi(p) = sum_(k=1)^K (a * (b_x^{(k)} - x) / (2 (c^{(k)})^2)
                          * e^(-||p - b^{(k)}||^2 / (2 (c^{(k)})^2)))
$$
$$
del/(del y) varphi(p) = sum_(k=1)^K (a * (b_y^{(k)} - y) / (2 (c^{(k)})^2)
                          * e^(-||p - b^{(k)}||^2 / (2 (c^{(k)})^2)))
$$

which can be simplified as

$$
f(p) = grad_p varphi(p)
     = sum_k (a^{(k)} (b^{(k)} - p) / (2 (c^{(k)})^2)
              * e^(-||p - b^{(k)}||^2 / (2 (c^{(k)})^2)))
$$

## Gradient descent

Given a set of $N$ known points ${(p^**, m^**)}$ where $m^** in RR^2$ is the 
measured magnetic vector value projected on a plane, we are minimizing the 
following mean square error:

$$
R(m^**, f_(a, b, c)(p^**)) = 1/N sum ||m^** - f(p^**)||^2
$$

We use the following update function:

$$
theta_(t+1) larr theta_t - epsilon grad_theta R
$$
