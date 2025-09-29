## Prepare modules
import os

try:
    import numpy as np
    import pandas as pd
    import scipy.io
except ImportError:
    print("Trying to Install required module...\n")
    os.system('python -m pip install numpy')
    os.system('python -m pip install pandas')
    os.system('python -m pip install scipy')
    
import numpy as np
import pandas as pd
import scipy.io


## Main function
def minimac(y, T):
    """
    minimac: Testing for Asset Pricing Errors
    
    This function calculates the minimum autocovariances over multiple
    horizons to test for the presence of pricing errors.
    
    Parameters:
        y - Log price of a financial asset (vector)
        T - Sample period (T=1/252 represents 1 day) (numeric)
    
    Returns:
        stat: Structure containing the following fields:
            - 'h': Testing statistics for kns = 1, 1:2, 1:4, and 1:8
            - 'sig_h': 6x4 matrix of significance results:
                       Columns correspond to the four groups of kns
                       Rows 1-2: 1%  level (1-sided, 2-sided)
                       Rows 3-4: 5%  level (1-sided, 2-sided)
                       Rows 5-6: 10% level (1-sided, 2-sided)
                       1 indicates rejection, 0 indicates acceptance

    Reference:
        Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Efficiency."
    """
    h = scovks(y, range(1, 9), T)
    hs = np.zeros(4)
    
    # Load the critical values
    cvs_df = pd.read_csv('cvs.csv',header=None)
    cvs = cvs_df.values
    sig_h = np.zeros((6, 4))
    
    for L in range(4):
        ks = range(2**L)
        list(ks)
        # Get the min stats
        mh = min(h[ks])
        # Store the min stats
        hs[L] = mh
        
        for j in range(3):
            sig_h[2*j, L] = mh < cvs[3*j, L]   # 1-sided test
            sig_h[2*j+1, L] = (mh > cvs[3*j+1, L]) | (mh < cvs[3*j+2, L])  # 2-sided test
    
    stat = {'h': hs, 'sig_h': sig_h}
    return stat


## Supplement function
def scovks(y, ks, T):
    """
    SCOVKS: Calculate standardized testing statistics for multiple horizons.

    This function computes the standardized testing statistics for multiple
    horizons specified by the vector ks.

    Parameters:
    y  - Log-prices vector
    ks - Vector of tuning parameters (horizons) for testing statistics
    T  -  Sample period (T=1/252 represents 1 day) (numeric)
    
    Returns:
    Hs - Standardized testing statistics with jumps truncated for each horizon.
    
    Reference:
        Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Efficiency."
    """
    # Number of horizons to process
    numKs = len(ks)

    # Preallocate arrays
    Hs = np.zeros(numKs)

    # Loop over each horizon
    for idx in range(numKs):
        kn = ks[idx]
        H = scovk(y, kn, T)
        Hs[idx] = H
    
    return Hs


## Supplement function
def scovk(y, kn, T):
    """
    SCOVK: Calculate standardized covariances over a single k-horizon.

    This function computes standardized testing statistics for log prices
    over a specified horizon.

    Parameters:
    y  - Log prices vector
    kn - Horizon parameter
    
    Returns:
    H - Standardized testing statistic
    
    Reference:
        Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Efficiency."
    """
    # Calculate log returns
    dy = np.diff(y)
    n = len(y)
    
    # Jumps truncations
    dn = 1 / n
    BPV = (np.pi / 2 / T) * np.sum(np.abs(dy[:-1]) * np.abs(dy[1:]))
    threshold = 3 * (dn*T)**0.48 * np.sqrt(BPV)
    Jidx = np.abs(dy) > threshold  # Identify jumps
    dyt = dy * (~Jidx)  # Remove jumps
    
    # Variance estimation
    var_cont4 = np.sum((dyt[:-1] * dyt[1:])**2) * kn / 2
    factor_kn = 1 / 3 + 1 / (6 * kn**2)  # Adjustment factor based on horizon
    U = factor_kn * var_cont4
    
    # F-bar statistic for truncated values
    y_trunc = np.cumsum(np.concatenate([[0], dyt]))  # Log prices with jumps removed
    dykn = y_trunc[kn:] - y_trunc[:-kn]
    Fbar = np.sum(dykn[kn:] * dykn[:-kn]) / (2 * kn)
    
    # Calculate the standardized testing statistic
    if Fbar == 0 or U == 0:
        H = 0
    else:
        H = Fbar / np.sqrt(U)
    
    return H


## Summary function
def summary_minimac(stat):
    """
    This function prints output formally for minimac function.
    
    Parameters:
        stat (dict): output from minimac(y,T) function
    """
    print('========== minimac testing result ==========')
    print('>> Testing Statistics:')
    h = pd.DataFrame([stat['h']], columns=['K_0', 'K_1', 'K_2', 'K_3'], index=['H'])
    print(h)
    print('>> Rejection Status:')
    sig_h = pd.DataFrame(
        stat['sig_h'].astype(int),
        columns=['K_0', 'K_1', 'K_2', 'K_3'],
        index=['1%, one-sided', '1%, two-sided', '5%, one-sided', '5%, two-sided', '10%, one-sided', '10%, two-sided']
    )
    print(sig_h)