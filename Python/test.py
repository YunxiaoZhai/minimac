## Prepare modules
import os
try:
    import pandas as pd
    import numpy as np
except ImportError:
    print("Trying to Install required module...\n")
    os.system('python -m pip install pandas')
    os.system('python -m pip install numpy')

## Import modules
from minimac import minimac, summary_minimac
import pandas as pd
import numpy as np

## Load data and run minimac
data = pd.read_csv('../SPY_20141215_1s.csv')
y = np.log(data['PRICE'])
stat = minimac(y,1/252)
summary_minimac(stat)