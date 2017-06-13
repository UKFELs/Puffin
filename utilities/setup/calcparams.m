%
% This script takes common FEL parameters and works out the equivalent 
% scaled parameters used in Puffin.
%
% Author: Lawrence Campbell
%         University of Strathclyde
%         2013

clear all;

m_e = 9.109382e-31;
q_e = 1.602176e-19;
eps_0 = 8.854188e-12;
c = 2.997924e8;

Er = m_e * c^2 / q_e;  % Rest energy of electron
E = 233e6;             % Energy in ev
gamma = E / Er;           % Relativistic factor

aw = 0.716 * sqrt(2); %2.3335;           % rms undulator parameter (2.3335 rms, 3.3 peak)
ux = 1; uy = 0;        % Undulator polarization
aw_rms = aw * sqrt(ux^2 + uy^2) / sqrt(2);
emit = 1e-6 / gamma;   % Un-normalized emittance
lambda_w = 0.0275;      % Undulator period
N_w = 133;             % Number of wiggler periods
ff = sqrt(2);          % Focus factor
Q = 400 * sqrt(2*pi) * 250e-15;%5e-12;             % Charge

qFlatTopZ2 = 0;        % =1 if flat top, else gaussian.
qHardEdgeX = 0;        % =1 if disk (circle) in transverse plane, else gaussian.

sigz = 250e-15 * c;         % rms beam length in z (ct)

k_w = 2 * pi / lambda_w;                           % Get wiggler wavenumber
lambda_r = lambda_w / (2 * gamma^2) * (1 + aw_rms^2);  % Resonant wavelength

sigt = sigz / c;                      % Get sigma in t dimension

k_beta = aw * k_w / ( ff * gamma );   % Betatron wavelength
sigx = sqrt(emit / k_beta);          % rms radius in x
sigy=sigx;                           % For now....

%sigx = 1e-6;   sigy = 1e-6;           % Fix to vals supplied by Bernhard
sig_av = sqrt((sigx^2 + sigy^2) / 2); % rms radius

N = Q / q_e;                          % Number of real electrons in pulse


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Beam area and FEL parameter


if (qHardEdgeX == 1)
    r_av = sig_av;           % Hard edged circle in x and y
else
    r_av = sqrt(2) * sig_av; % Gaussian dist in x and y
end

tArea = pi * r_av^2;         % Tranverse beam area

if (qFlatTopZ2 == 1)
    lArea = sigz;            % longitudinal integral over charge dist (flat top)
else
    lArea = sqrt(2*pi) * sigz; % longitudinal integral over charge dist(gaussian)
end

n_p = N / (tArea * lArea);                % Electron number density
wp = sqrt(q_e^2 * n_p / (eps_0 * m_e) );  % plasma frequency

rho = 1 / gamma * (aw * wp / ( 4 * c * k_w ))^(2/3); % FEL parameter


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scaled parameters for Puffin


lambda_z2 = 4*pi*rho;             % Resonant wavelength in z2
Lg = lambda_w / lambda_z2;        % Gain length
Lc = lambda_r / lambda_z2;        % Cooperation length
zbarprop = N_w * lambda_z2;       % Length of undulator in zbar (gain lengths)
sigz2 = sigz / Lc;                % Length of pulse in z2 (cooperation lengths)

beta = sqrt(gamma^2 - 1 - aw^2)/gamma;        % Average velocity over c
eta = (1-beta)/beta;                          % Scaled average velocity

k_beta_bar = k_beta * Lg;                     % Scaled betatron wavenumber
emit_bar = emit / (rho * Lc);                 % Scaled emittance
Z_R = pi * r_av^2 / lambda_r;                 % Rayleigh range
Z_bar_R = r_av^2 / (Lg * Lc) / (4 * rho);     % Scaled Rayleigh Range
B = (2 * Z_bar_R)^(3/2);                      % Saldin parameter


NL = N/sigz2 * lambda_z2;                     % electrons per radiation period
Anoise = 6*sqrt(pi)*rho / (NL * sqrt(log(NL/rho))); % Spontaneous noise estimate (in scaled units)
Acse = 16 * rho^2;                            % CSE estimate for flat-top current


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sampling


elms_per_wave = 24;        % Elements per resonant wavelength
steps_per_per = 96;        % should be roughly 4-5* elms_per_wave

if (qFlatTopZ2 == 1)
    lez2     = sigz2;      % flat-top
    sigz2_in = 1e8;
else
    lez2     = 9*sigz2;    % gaussian
    sigz2_in = sigz2;
end

lwz2 = 25.0;               % Additional field length required beyond beam
lsys_z2 = lwz2 + lez2;     % Total length of sampled system in z2
lsys_z2 = 243;


dz2 = lambda_z2 / elms_per_wave;   % Node spacing in z2
NNodesZ2 = lsys_z2 / dz2 + 1;      % Number of radiation field nodes
NMElecsZ2 = lez2 / dz2;            % MINIMUM number of macroparticles
dz = lambda_z2 / steps_per_per;    % Step size in zbar
Nsteps = zbarprop / dz;            % Number of steps


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print results


fprintf('SUPPLIED PARAMETERS\n');
fprintf('===================\n');

fprintf('aw (peak)  = %.14e\n',aw);
fprintf('gamma      = %.14e\n',gamma);
fprintf('lambda_w   = %.14e\n',lambda_w);
fprintf('ux         = %.14e\n',ux);
fprintf('uy         = %.14e\n',uy);
fprintf('ff         = %.14e\n',ff);
fprintf('emit(unnorm) = %.14e\n',emit);
fprintf('emit_bar   = %.14e\n',emit_bar);
fprintf('Q          = %.14e\n',Q);
fprintf('Nw         = %.14e\n',N_w);

fprintf('\n');

fprintf('CALCULATED PARAMETERS\n');
fprintf('=====================\n');

fprintf('rho        = %.14e\n',rho);
fprintf('Lg         = %.14e\n',Lg);
fprintf('Lc         = %.14e\n',Lc);
fprintf('lambda_r   = %.14e\n',lambda_r);
fprintf('beta_av    = %.14e\n',beta);
fprintf('eta        = %.14e\n',eta);
fprintf('Emit_bar   = %.14e\n',emit_bar);
fprintf('k_beta_bar = %.14e\n',k_beta_bar);
fprintf('lambda_beta_bar = %.14e\n',2 * pi / k_beta_bar);
fprintf('Focus fac  = %.14e\n',ff);
fprintf('Sigz2      = %.14e\n',sigz2_in);
fprintf('SigX (m)   = %.14e\n',sigx);
fprintf('zR         = %.14e\n',Z_R);
fprintf('zbarR      = %.14e\n',Z_bar_R);
fprintf('B (Saldin) = %.14e\n',B);
fprintf('zbarprop   = %.14e\n',zbarprop);
fprintf('Initial noise estimate   = %.14e\n',Anoise);

fprintf('\n');

fprintf('NUMERICS\n');
fprintf('========\n');

fprintf('StepSize         = %.14e\n',dz);
fprintf('NSteps           = %.14e\n',Nsteps);
fprintf('sFieldLengthZ2   = %.14e\n',lwz2);
fprintf('NNodesZ2         = %.14e\n',NNodesZ2);
fprintf('sLenEPulseZ2     = %.14e\n',lez2);
fprintf('NMElecsZ2        = %.14e\n',NMElecsZ2);

fprintf('\n');

fprintf('ADDITIONAL\n');
fprintf('==========\n');

fprintf('Initial noise estimate                        = %.14e\n',Anoise);
fprintf('Initial CSE estimate (For hard edged beam)    = %.14e\n',Acse);
