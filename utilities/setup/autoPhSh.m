%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The undulator modules in Puffin include entry and exit tapers,
%%% which have a non-zero phase drift in the resonant phase.
%%% Furthermore, the drift sections are not auto-matched, also resulting
%%% in adding a non-zero offset to the beam in the resonant radiation 
%%% phase.
%%%
%%% Further complications arise from the broadband modelling in Puffin,
%%% since the user may wish to shift into a completely different frequency,
%%% undulator polarization and/or tuning in every module.
%%%
%%% This can be corrected by use of a chicane in the lattice file (e.g. 
%%% see the CLARA example input deck). The phase shift of the chicane is 
%%% described in units of the reference wavelength. This script helps to 
%%% calculate the phase shift required to knock the beam back into the 
%%% radiation phase.
%%%
%%%
%%% The script below calculates the drift required to correct the phase
%%% after leaving an undulator module, going through an inter-module free
%%% space drift, and entering the next module. The script first calculates
%%% the total shift resulting from the end of the first module, then
%%% calculates the total shift from the entry into the next wiggler, and
%%% then calculates the total shift from the drift between the modules.
%%%     
%%%       e.g. see the below illustration of the undulator peak field, and
%%%       which part of the script deals with which section.
%%%
%%%       _________________                         ______________
%%%  --> /                 \                       /              \   --->
%%%     /     Undulator     \_______ DRIFT _______/                \
%%%                                  
%%%                        ^           ^           ^      
%%%                      First       Third      Second
%%%                     section     Section     Section
%%%
%%%
%%% The script takes the cumulative shift from all sections, and calculates
%%% the additional shift required to correct the phase drift.
%%%
%%%% COMMON PARAMETERS:
%%%% (these describe the 'reference' undulator and beam)

rho = 0.005;
aw = 1.0121809;

gammaFr = 1.0;   % Fractional gammaFr = gamma / gamma_r

% reference rms wiggler

ux = 0;
uy = 1;
aw_rms = aw .* sqrt(ux.^2 + uy.^2) ./ sqrt(2); 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Back of undulator ramp down  (1st module)

% Module polarization
ux = 0;
uy = 1;

alpha = 1;   % Tuning of this module
npts = 1000;

lw = 4 * pi * rho;
lw2 = 2 * lw;  % length of front


zbar = linspace(0,lw2,npts);
dz = zbar(2);

% Analytic functional pperp
px = -uy .* alpha ./ 8 ./ pi ./ rho .* ((8.*pi.*rho-zbar) .* cos(zbar./2./rho) ...
    + 2.*rho.*sin(zbar./2./rho));

py = - ux .* alpha ./ 6 ./ pi ./ rho .* ( (zbar - 7.*pi.*rho) .* sin(zbar./2./rho) ...
    + 2.*rho.*cos(zbar./2./rho));

py(zbar<pi*rho) = ux .* alpha .* sin(zbar(zbar<pi*rho) ./2./rho);
py(zbar>7*pi*rho) = 0;



%pperp = - alpha .* sin(zbar./16./rho).^2 .* cos(zbar./2./rho);
pperp = px - 1j*py;

ppsq = abs(pperp).^2;

%figure; plot(zbar, px, zbar, py, zbar, ppsq);

p2 = (1./gammaFr).^2 ./ (1 + aw_rms.^2) .* (1 + (aw.^2 .* ppsq));

z2b = trapz(zbar, p2);   %  actual shift in z2 from end

z2nw = z2b ./ lw;         %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)

z2modab = z2modnw * 4 * pi * rho; % ...and in units of z2







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Front ramp up (2nd module)

% Module polarization
ux = 0;
uy = 1;

alpha = 1;  % Tuning of this module
npts = 1000;

lw = 4 * pi * rho;
lw2 = 2 * lw;  % length of front


zbar = linspace(0,lw2,npts);
dz = zbar(2);

% Analytic functional pperp
px = uy .* alpha ./ 8 ./ pi ./ rho .* (-zbar.*cos(zbar./2./rho) ...
    + 2.*rho.*sin(zbar./2./rho));

py = - ux .* alpha ./ 6 ./ pi ./ rho .* ( (zbar - pi.*rho) .*sin(zbar./2./rho) ...
    + 2.*rho.*cos(zbar./2./rho));

py(zbar<pi*rho) = 0;
py(zbar>7*pi*rho) = ux .* alpha .* sin(zbar(zbar<pi*rho) ./2./rho);


%pperp = - alpha .* sin(zbar./16./rho).^2 .* cos(zbar./2./rho);
pperp = px - 1j*py;

ppsq = abs(pperp).^2;


p2 = (1./gammaFr).^2 ./ (1 + aw_rms.^2) .* (1 + (aw.^2 .* ppsq));

z2a = trapz(zbar, p2);   %  actual shift in z2 from end

z2nw = z2a ./ lw;         %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)

z2modaf = z2modnw * 4 * pi * rho; % ...and in units of z2









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ...and drift

aw_rms = 0;
%alpha = 1; %not needed
npts = 1000;

lw = 4 * pi * rho;
lw2 = 16.4545455 * lw;  % length of front - SET TO DR * lw


zbar = linspace(0,lw2,npts);
dz = zbar(2);

% Analytic functional pperp
pperp = zeros(1,length(zbar));%- alpha .* cos(zbar./16./rho).^2 .* cos(zbar./2./rho);


ppsq = abs(pperp).^2;


p2 = (1./gammaFr).^2 ./ (1 + aw_rms.^2) .* (1 + (aw.^2 .* ppsq));

z2d = trapz(zbar, p2);   %  actual shift in z2 from end

z2nw = z2d ./ lw;         %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)

z2modad = z2modnw * 4 * pi * rho; % ...and in units of z2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%
%%%%%%%
%%% TOTAL SHIFT

%z2modT = z2modaf + z2modab + z2modad

%z2modTnw = z2modT / (4 * pi * rho)

lwfact = 1.0;                % Resonant wavelength to correct for, in units 
                             % of lambda_r / lambda_r0

z2modT = z2a + z2b + z2d;          % Total shift from both front and back (in z2)

z2nw = z2modT ./ (lw * lwfact);  %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)
z2modnw = z2modnw * lwfact;   % remainder to shift by (in units of lambda_r0)

fprintf('\n');
fprintf('Shift from the front in units of reference wavelength = %.14e\n', z2a/lw);
fprintf('Shift from the back in units of reference wavelength = %.14e\n', z2b/lw);
fprintf('Shift from the drift section = %.14e\n', z2d/lw);

fprintf('\n');

fprintf('Number of periods to phase shift by = %.14e\n', z2modnw);
fprintf('\n');

