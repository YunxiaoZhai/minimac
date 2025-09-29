% Import data
data = readtable('../SPY_20141215_1s.csv'); % import your intraday data here
y = log(data.PRICE);

% Testing for Market Friction
stat = minimac(y,1/252);
summary_minimac(stat)