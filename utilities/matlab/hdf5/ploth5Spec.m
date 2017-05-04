function [ hf ] = ploth5Spec( fname )
%PLOTH5SPEC Summary of this function goes here
%   Detailed explanation goes here


zbar = hdf5read(fname,'/aperp','zbarTotal');
rho = hdf5read(fname,'/runInfo','rho');
nZ2 = hdf5read(fname,'/runInfo','nZ2');
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');
lambda_r = hdf5read(fname,'/runInfo','lambda_r');

nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * (nZ2-1);



h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum

NumUniquePts = ceil((nZ2+1)/2);
fs = (nZ2)/lenZ2; %sampling frequency




%apcz2 = h5read(fname,'/aperp',[60,60,1,1],[1,1,nZ2,2]);

%apcz2 = zeros(1,1,nZ2,1);

%for iz2 = 1:nZ2
%    apcz2(1,1,iz2,1) = h5read(fname,'/aperp',[61,61,iz2,1],[1,1,1,1]);
%end

apcz2 = h5read(fname,'/aperp',[1,1],[nZ2,2]);


xf = apcz2(:,1);
yf = -apcz2(:,2);




%xf = apcz2(:,:,:,1);
%yf = -apcz2(:,:,:,2);

xf = reshape(xf,[1,nZ2]);
%yf = reshape(yf,[1,nZ2]);


xff = fft(xf);

ftfieldtemp = xff(1:NumUniquePts); 

ftpower = abs(ftfieldtemp).^2;

if rem(nZ2,2)
    ftpower(2:end) = ftpower(2:end)*2;
else
    ftpower(2:end-1) = ftpower(2:end-1)*2;
end












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normalised frequency xaxis(1 is resonant frequency)

% ftxaxis = ((0:NumUniquePts-1)*(fs/nZ2))*(4*pi*rho);
% %sp_x_axis='$$\omega / \omega_r$$';
% sp_x_axis='$$\bar{\omega}$$';
% sp_title='Intensity Spectrum';


ftxaxis = c_0 ./ (((0:NumUniquePts-1)*(fs/nZ2)) / lambda_r * (4*pi*rho) * c_0); 
sp_x_axis='$$\lambda (m)$$';
sp_title='Intensity Spectrum';



figure; 
%hf = semilogy(ftxaxis, ftpower);
hf = plot(ftxaxis, ftpower);
%hf = plot((ftxaxis-1)/2/rho, ftpower);
xlabel(sp_x_axis, 'interpreter','latex');
%xlim([0.9 1.1]);
%xlim(([0.9 1.1] - 1)/2/rho);
xlim([0.9e-7 1.1e-7]);
%title(strcat(sp_title,' @ zbar = ',num2str(zbar)));



end

