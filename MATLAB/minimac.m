function stat = minimac(y, T)
%% minimac: Minimum Multi‑Horizon Autocovariances
%
% This function calculates the minimum autocovariances over multiple
% horizons to test for the presence of market friction.
% Input:
%   y - Log price of a financial asset (vector)
%   T - Sample period (T=1/252 represents 1 day)
%
% Outputs:
%   stat - Structure containing the following fields:
%     .h - Testing statistics for kns = 1, 1:2, 1:4, and 1:8
%     .sig_h - 6x4 matrix of significance results:
%               Columns correspond to the four groups of kns
%               Rows 1-2: 1%  level (1-sided, 2-sided)
%               Rows 3-4: 5%  level (1-sided, 2-sided)
%               Rows 5-6: 10% level (1-sided, 2-sided)
%               1 indicates rejection, 0 indicates acceptance
%
% Reference:
%   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Frictions."
%
% Usage:
%   result = minimac(asset_log_prices);
%
h = scovks(y,1:8,T);
hs = zeros(4,1);
% load the critical values
cvs_struct = load('cvs.mat');
cvs = cvs_struct.cvs; 
sig_h = zeros(6,4);
for L = 0 : 3
    ks = 1 : 2^L;
    % get the min stats
    mh = min(h(ks));
    % store the min stats
    hs(L+1) = mh;
    %
    for j = 1 : 3
        sig_h(1 + 2*(j-1), L+1) = mh < cvs(1 + 3*(j-1), L+1);%1-sided test
        sig_h(2 + 2*(j-1), L+1) = mh > cvs(2 + 3*(j-1), L+1) ...
            | mh < cvs(3 + 3*(j-1), L+1);% 2-sided test
        % sig_h(2 + 2*(j-1), L+1) = mh > cvsR(j, L+1); % right tail test
    end
end
stat.h = hs';
stat.sig_h = sig_h;
end
%%
%%
%% Supplementary Functions
%%
%%
function Hs = scovks(y, ks, T)
%% SCOVKS Calculate standardized testing statistics for multiple horizons.
% This function computes the standardized testing statistics for multiple
% horizons specified by the vector ks. 
%
% Inputs:
%   y  - Log-prices vector.
%   ks - Vector of tuning parameters (horizons) for testing statistics.
%   T - Sample period (T=1/252 represents 1 day)
%
% Outputs:
%   stats0      - Standardized testing statistics for each horizon.
%   stats_trunc - Standardized testing statistics with jumps truncated for each horizon.
%
% Reference:
%   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Firctions."

% Number of horizons to process
numKs = length(ks);

% Preallocate arrays
Hs = zeros(numKs, 1); 

% Loop over each horizon
for idx = 1:numKs
    kn = ks(idx);
    H = scovk(y, kn, T);
    Hs(idx) = H;
end
end

function H = scovk(y, kn, T)
%% SCOVK Calculate standardized covariances over a single k-horizon.
% This function computes standardized testing statistics for log prices
% over a specified horizon.
%
% Inputs:
%   y  - Log prices vector
%   kn - Horizon parameter
%   T - Sample period (T=1/252 represents 1 day)
%
% Outputs:
%   H - Standardized testing statistic
%
% References:
%   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Frictions."
%
%%
% Calculate log returns
dy = diff(y);
n = length(y);

% Jumps truncations
dn = 1 / n;
BPV = (pi / 2 / T) * sum(abs(dy(1:end-1)) .* abs(dy(2:end)));
threshold = 3 * (T*dn)^(0.48) * sqrt(BPV);
Jidx = abs(dy) > threshold; % Identify jumps
dyt = dy .* (~Jidx);% Remove jumps

% Variance estimation
var_cont4 = sum((dyt(1:end-1).* dyt(2:end)).^2) * kn / 2; 
factor_kn = 1 / 3 + 1 / 6 / kn^2; % Adjustment factor based on horizon
U = factor_kn * var_cont4;

% F-bar statistic for truncated values

y_trunc = cumsum([0; dyt]);% Log prices with jumps removed
dykn = y_trunc((1+kn):end) - y_trunc(1:(end-kn));
Fbar = sum(dykn((1+kn):end) .* dykn(1:(end-kn))) / (2 * kn);

% Calculate the standardized testing statistic
if Fbar == 0 || U == 0
    H = 0;
else
    H = Fbar ./ sqrt(U);
end

end