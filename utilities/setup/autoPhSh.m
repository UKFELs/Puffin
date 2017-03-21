%%%%%%%
%%%
%%% For Puffin Plane-Pole...

%%% ...front...

rho = 0.005;
aw = 1.0121809;
aw_rms = aw ./ sqrt(2);  % if helical - should be same as aw...!!!

gammaFr = 1.03;   % Fractional gammaFr = gamma / gamma_r

alpha = 1;
npts = 1000;

lw = 4 * pi * rho;
lw2 = 2 * lw;  % length of front


zbar = linspace(0,lw2,npts);
dz = zbar(2);

% Analytic functional pperp
pperp = - alpha .* sin(zbar./16./rho).^2 .* cos(zbar./2./rho);


ppsq = abs(pperp).^2;


p2 = (1./gammaFr).^2 ./ (1 + aw_rms.^2) .* (1 + (aw.^2 .* ppsq));

z2a = trapz(zbar, p2);   %  actual shift in z2 from end

z2nw = z2a ./ lw;         %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)

z2modaf = z2modnw * 4 * pi * rho; % ...and in units of z2






%%% ...and back

rho = 0.005;
aw = 1;
aw_rms = aw ./ sqrt(2);

alpha = 1;
npts = 1000;

lw = 4 * pi * rho;
lw2 = 2 * lw;  % length of front


zbar = linspace(0,lw2,npts);
dz = zbar(2);

% Analytic functional pperp
pperp = - alpha .* cos(zbar./16./rho).^2 .* cos(zbar./2./rho);


ppsq = abs(pperp).^2;


p2 = (1./gammaFr).^2 ./ (1 + aw_rms.^2) .* (1 + (aw.^2 .* ppsq));

z2a = trapz(zbar, p2);   %  actual shift in z2 from end

z2nw = z2a ./ lw;         %  ...and as a fraction of resonant wavelength

z2modnw = ceil(z2nw) - z2nw;  % remainder to shift by (in units of lambda_r)

z2modab = z2modnw * 4 * pi * rho; % ...and in units of z2



%%%%%%%
%%%%%%%
%%% TOTAL SHIFT

z2modT = z2modaf + z2modab

z2modTnw = z2modT / (4 * pi * rho)




