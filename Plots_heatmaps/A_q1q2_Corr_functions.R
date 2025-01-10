# On SB impact on correlation -------------------------------------------------

# Loading libraries -----------------------------------------------------------
library(tidyverse)
library(reshape2)
library(latex2exp)
library(patchwork)

## DP -------------------------------------------------------------------------

q1_dp <- function(a){
  1/(1+a) 
}
q2_dp <- function(b){
  (1+b)/(1+2*b)
}
corr_DP = function(a,b){
  1-(1-q1_dp(a))*(1-q2_dp(b))
}

## Pitman-Yor --------------------------------------------------------------

sum_zeta2 = function(sig,theta,l=1000){
  theta_tilde = theta/sig
  theta_hat = (theta+1)/sig
  ell = 1:l
  lo = 2*( lgamma(theta_tilde+ell) - lgamma(theta_hat+ell) )
  sum(exp(lo))
}
q2_py = function(sig, theta){
  theta_tilde =  theta/sig
  theta_hat   = (theta+1)/sig
  log_q2 =
    log((1-sig)) -2*log(sig) + log(1+theta) +
    2 * lgamma(theta_hat) - 2*lgamma(theta_tilde+1) +
    log(sum_zeta2(sig,theta))
  exp(log_q2)
}
q1_py = function(sig_d,theta_d){
  (1-sig_d)/(1+theta_d)
}
corr_PY = function(sig,theta,sig_d,theta_d){
  1-(1-q1_py(sig_d,theta_d))*(1-q2_py(sig,theta))
}

## 2PBP ---------------------------------------------------------------

q2_2p = function(a,b){
  1-2*b/((a+2*b)*(a+1))
}
q1_2p = function(a_d,b_d){
  (a_d+1)/(a_d+2*b_d+1)
}
corr_2p = function(a,b,a_d,b_d){
  1-(1-q1_2p(a_d,b_d))*(1-q2_2p(a,b) )
}

# Skip-breaking -----------------------------------------------------------

# q1 is the same as the 2PBP, as it p cancels out

q1_skb <- function(a_d,b_d,p){
  q1_2p(a_d,b_d)
}

q2_skb <- function(a,b,p){
  a*(1-p)*(a+2*b+1)/( (a+2*b+a*p)*(a+1))
}
corr_skb = function(a,b,p,a_d,b_d){
  1-(1-q1_2p(a_d,b_d))*(1- q2_skb(a,b,p))
}
