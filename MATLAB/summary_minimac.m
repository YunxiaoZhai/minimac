function summary_minimac(stat)
% This function print output formally for minimac function.
%
% Inputs:
%   stat - output from minimac(y,T) function
%
%%
fprintf('================ minimac testing result ================\n');
fprintf('>> Testing Statistics:\n');
h = array2table(stat.h, 'VariableNames',{'K_0', 'K_1', 'K_2', 'K_3'});
h.Properties.RowNames = {'H'};
disp(h);
fprintf('>> Rejection Status:\n');
sig_h = array2table(stat.sig_h, 'VariableNames',{'K_0', 'K_1', 'K_2', 'K_3'});
sig_h.Properties.RowNames = {'1%, one-sided', '1%, two-sided', '5%, one-sided', '5%, two-sided', '10%, one-sided', '10%, two-sided'};
disp(sig_h);
end