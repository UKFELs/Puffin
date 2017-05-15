function [ hf ] = ploth5MagPh( fname )
%PLOTH5SPEC Summary of this function goes here
%   Detailed explanation goes here


zbar = hdf5read(fname,'/aperp','zbarTotal');
rho = hdf5read(fname,'/runInfo','rho');
nZ2 = hdf5read(fname,'/runInfo','nZ2');
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');


nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * (nZ2-1);
Z2axis = linspace(0,lenZ2,nZ2);


h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum

NumUniquePts = ceil((nZ2+1)/2);
fs = (nZ2)/lenZ2; %sampling frequency




%apcz2 = h5read(fname,'/aperp',[60,60,1,1],[1,1,nZ2,2]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For 3D
%apcz2 = zeros(1,1,nZ2,1);

%for iz2 = 1:nZ2
%    apcz2(1,1,iz2,1) = h5read(fname,'/aperp',[61,67,iz2,1],[1,1,1,1]);
%end


%xf = apcz2(:,:,:,1);

%xf = reshape(xf,[nZ2,1]);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For 1D

apcz2 = h5read(fname,'/aperp',[1,1],[nZ2,2]);


xf = apcz2(:,1);
yf = -apcz2(:,2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

field = xf - 1j*yf;

[xf   yf] = FilterField(field,1.0,0.05,nZ2,sLengthOfElmZ2, rho, 1);

[magx phasex]=getStokesf(xf,nZ2,rho,lenZ2);

%[magy phasey]=getStokesf(yfield,nZ2,rho,lenZ2);









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plotting

figure;

% Field

subplot(3,1,1);
hf = plot(Z2axis,xf.^2);%,'LineWidth',2);
%xlim([0    lenZ2]);
xlim([0  150]);
%ylim([0   6e-3]);
title(strcat('zbar = ',num2str(zbar)));

xlabel('$$\bar{z}_2$$','interpreter','latex','fontsize',14);  
ylabel('x-field sqd','interpreter','latex','fontsize',14);


% Magnitude...

subplot(3,1,2);
hf = plot(Z2axis,magx.^2);%,'LineWidth',2);
%xlim([0    lenZ2]);
xlim([0  150]);
%xlim([190  210]);
%ylim([0   6e-3]);

xlabel('$$\bar{z}_2$$','interpreter','latex','fontsize',14);  
ylabel('Avg Mag x sq','interpreter','latex','fontsize',14);


% ... and phase
subplot(3,1,3);
hf = plot(Z2axis,phasex);%,'LineWidth',2);
%xlim([0    lenZ2]);
xlim([0  150]);
%xlim([190  210]);
%ylim([0   6e-3]);

xlabel('$$\bar{z}_2$$','interpreter','latex','fontsize',14);  
ylabel('Phase-x','interpreter','latex','fontsize',14);


end

