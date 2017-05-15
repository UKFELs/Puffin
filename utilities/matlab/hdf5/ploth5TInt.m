function [ hf ] = ploth5TInt( fname , sz2 )

% Plots the transverse intensity from a field dump....



rho = hdf5read(fname,'/runInfo','rho');
nZ2 = hdf5read(fname,'/runInfo','nZ2');
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');
nX = hdf5read(fname,'/runInfo','nX');
sLengthOfElmX = hdf5read(fname,'/runInfo','sLengthOfElmX');
nY = hdf5read(fname,'/runInfo','nY');
sLengthOfElmY = hdf5read(fname,'/runInfo','sLengthOfElmY');

nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * (nZ2-1);

nX = double(nX);
lenX = sLengthOfElmX * (nX-1);
Xaxis = linspace(-lenX/2, lenX/2, nX);
nY = double(nY);
lenY = sLengthOfElmY * (nY-1);
Yaxis = linspace(-lenY/2, lenY/2, nY);




h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum



lw = 4*pi*rho;
npp = round(lw / sLengthOfElmZ2) + 1  % nodes per period



% nearest instantaneous slice to given z2

indz2 = round(sz2 / sLengthOfElmZ2) + 1;



% For single 'instantaneous' slice....
apc = h5read(fname,'/aperp',[1,1,indz2,1],[120,120,1,2]);
apcx = apc(:,:,1,1);
apcy = -apc(:,:,1,2);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plotting

figure;
hf = surf(Xaxis, Yaxis, abs(apcx).^2 + abs(apcy).^2);
title('Instantaneous');

set(hf,'edgecolor','none');

shading interp;

hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);




% For average over a wavelength 

apc = h5read(fname,'/aperp',[1,1,indz2,1],[120,120,npp,2]);
apcx = apc(:,:,:,1);
apcy = -apc(:,:,:,2);


apcx = reshape(apcx,[120,120,npp]);
apcy = reshape(apcy,[120,120,npp]);

avint = sum(abs(apcx).^2 + abs(apcy).^2, 3);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plotting

figure;
hf = surf(Xaxis,Yaxis,avint);
title('Average');

set(hf,'edgecolor','none');

shading interp;

hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);



end

