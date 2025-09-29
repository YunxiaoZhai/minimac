# minimac
## The Multi-Horizon Test for Market Frictions

This repo contains hypothesis testing algorithm developed by Z. Merrick Li and Xiye Yang (2025), "Multi-Horizon Test for Market Frictions". <DOI: to be filled>. This method tests for the presence of market frictions that induce transitory deviations of observed asset prices from the underlying efficient prices. The test is based on the joint inference of return covariances across multiple horizons. 

The minimum statistic is defined as

$$
H(Y;\mathbf{K}_\ell)_t^n:=\min_{k_i\in\mathbf{K}}H(Y;k_i)_t^n.
$$

where $\mathbf{K}_\ell=\{1,2,...,2^\ell\},\;\ell=0,1,2,3$ and the standardized single-horizon testing statistic

$$
H(Y;k)_t^n:=\frac{F(Y;k)_t^n}{\sqrt{\Phi_k\:Q(Y)_t^n}}
$$
is derived from
$$
F(Y;k)_t^n:=\frac{1}{2k}\sum_{i=k+1}^{n_t-k}f(Y;k)_i^n,\quad f(Y;k)_i^n:=(Y_i^n-Y_{i-k}^n)(Y_{i+k}^n-Y_i^n),\quad \Phi_k:=\frac16(k+\frac1{2k}), \quad Q(Y)_t^n:=\sum_{i=2}^{n_t-1}(f(Y;1)_i^n)^2
$$

## Getting Started

There are 3 folders in the directory:

- In the `./MATLAB` folder,  a function script `minimac.m` that depends on `cvs.mat` table and a test script `test.m` are provided. 
- Similarly, each of `./R` and `./Python` contains a corresponding function script that depends on `cvs.csv` table and a test script/ipynb. 

## Usage and Examples

- The callable function `stat=minimac(y,T)` employs uniform syntax across mentioned languages, where `y` is **log price** and `T` is **sample period of your dataset** (T=1/252 represents data covers 1 day's trading hours). 

- `summary_minimac(stat)` returns formal output for `stat=minimac(y,T)` testing result (also callable in all mentioned languages). Note that the function is in independent script for MATLAB, while for R and Python, it is integrated in corresponding function script of minimac.

- Before calling `minimac(y,T)`, put function script (e.g., `minimac.m`, `summary_minimac.m`) and critical value table (e.g., `cvs.mat`) into your current directory. 

- MATLAB:

  ```matlab
  % Import data
  data = readtable('../SPY_20141215_1s.csv'); % import your intraday data here
  y = log(data.PRICE);
  
  % Testing for Market Friction
  stat = minimac(y,1/252);
  summary_minimac(stat)
  ```

- R:

  ```R
  # Load function of minimac
  source("minimac.R")
  
  # Import data
  data <- read.csv("../SPY_20141215_1s.csv") # import your data here
  y <- log(data$PRICE)
  
  # Testing for Market Friction
  stat <- minimac(y,1/252)
  summary_minimac(stat)
  
  ```

- Python:

  ```python
  ## Import modules
  from minimac import minimac, summary_minimac
  import pandas as pd
  import numpy as np
  
  ## Load data and run minimac
  data = pd.read_csv('../SPY_20141215_1s.csv') # input your data here
  y = np.log(data['PRICE'])
  stat = minimac(y,1/252)
  summary_minimac(stat)
  ```

- All languages will return identical output

  ```
  ================ minimac testing result ================
  >> Testing Statistics:
          K_0      K_1      K_2      K_3
  H -0.683775 -1.61216 -3.90376 -3.90376
  >> Rejection Status:
                 K_0 K_1 K_2 K_3
  1%, one-sided    0   0   1   1
  1%, two-sided    0   0   1   1
  5%, one-sided    0   0   1   1
  5%, two-sided    0   0   1   1
  10%, one-sided   0   1   1   1
  10%, two-sided   0   0   1   1
  ```
