function hp = wanls2(fname)







%parr = {'nX','nY','nZ2','sLengthOfElmX','sLengthOfElmY', ...
%'sLengthOfElmZ2','rho','eta','aw','gamma_r',...
%'iIntWriteNthSteps','sStepSize'};

rho = hdf5read(fname,'/runInfo','rho');
nZ2 = hdf5read(fname,'/runInfo','nZ2');
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');
lambda_r = hdf5read(fname,'/runInfo','lambda_r');
zbar = hdf5read(fname,'/aperp','zbarInter');


nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * (nZ2-1);

Z2axis = linspace(0,lenZ2,nZ2);

h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum

NumUniquePts = ceil((nZ2+1)/2);
fs = (nZ2)/lenZ2; %sampling frequency



dz2 = sLengthOfElmZ2;


apcz2 = h5read(fname,'/aperp',[1,1],[nZ2,2]);


xf = apcz2(:,1);
yf = -apcz2(:,2);




% choose subset to analyse (data is FAR too big to analyse all of
% it...)

% z2 limits

z2st = 70;
z2ed = 90;

%z2st = 200;
%z2ed = 220;


z2nst = round(z2st / dz2); %421760;
z2ned = round(z2ed / dz2); %425739;

Z2axis = Z2axis(z2nst:z2ned);
xf = xf(z2nst:z2ned);
yf = yf(z2nst:z2ned);
tf = xf - 1j*yf;
lenZ2 = Z2axis(end)-Z2axis(1);
nZ2 = max(size(xf));

[magx phasex]=getStokesf(xf,nZ2,rho,lenZ2);


%sig = {yf,dz2};
%figure; cwtS1 = cwtft(sig,'plot');




% Try again, with other scales and wavelets
%s0  = 6*dz2;  ds = 0.15;  NbSc = 50;
%%%%%%%%%%
%%%% WORKS
% s0  = 0.0007;  ds = 1.5e-2;  NbSc = 100;
% SCA = {s0,ds,NbSc};
% cwtS2 = cwtft(sig,'scales',SCA,'plot');
% 
% MorletFourierFactor = 4*pi/(6+sqrt(2+6^2));
% Scales = cwtS2.scales.*MorletFourierFactor;
% Freq = 1./Scales;
%%%%%
%%%%%%%%%%

% imagesc(Z2axis,[],abs(cwtS2.cfs));
% indices = get(gca,'ytick');
% set(gca,'yticklabel',Freq(indices));
% xlabel('Time'); ylabel('Hz');
% title('Time-Frequency Analysis with CWT');


% 
% scales = cwtS1.scales;
% MorletFourierFactor = 4*pi/(6+sqrt(2+6^2));
% freq = 1./(scales.*MorletFourierFactor);
% figure; contour(Z2axis,freq,real(cwtS1.cfs));
% xlabel('Seconds'); ylabel('Pseudo-frequency');
% axis([0 Z2axis(end) 0 15]);

%clear all;

%%%%%%%%%%%%
% WORKS
% Try again - now with more intuitively with frequncies
% labelled

% dt = dz2;
% figure;
% %s0  = 6*dt;  ds = 0.15;  NbSc = 50;
% s0  = 0.00075;  ds = 0.75e-2;  NbSc = 100;
% wname = 'bump';
% SCA = {s0,ds,NbSc};
% cwtsig = cwtft({yf,dt},'scales',SCA,'wavelet',wname);
% MorletFourierFactor = 4*pi/(6+sqrt(2+6^2));
% Scales = cwtsig.scales.*MorletFourierFactor;
% Freq = 1./Scales;
% imagesc(Z2axis,[],abs(cwtsig.cfs));
% indices = get(gca,'ytick');
% set(gca,'yticklabel',Freq(indices));
% xlabel('Time'); ylabel('Hz');
% title('Time-Frequency Analysis with CWT');
%%%%%%%%%%%%%


Fs = 1/(lenZ2/nZ2);
%figure;
subplot(3,1,1);
hf = plot(Z2axis, abs(xf).^2);
xlim([z2st   z2ed]);
title(strcat('zbar = ', num2str(zbar)));
% ylim([0   3]);

subplot(3,1,2);
hf = plot(Z2axis, phasex);
xlim([z2st   z2ed]);


subplot(3,1,3);
[sst,f] = wsst(xf,Fs,'bump');
f = ((f * 4 * pi * rho) - 1)/2/rho;
contour(Z2axis,f,abs(sst));
%figure; 
%hp = pcolor(Z2axis,f,log10(abs(sst)));
%hp = pcolor(Z2axis,f,abs(sst));
hp.EdgeColor = 'none';
title('Wavelet Synchrosqueezed Transform');
xlabel('Time'); ylabel('Hz');
%ylim([15  20]);
%ylim([15*4*pi*rho  20*4*pi*rho]);
ylim( (( [15  20]*4*pi*rho) - 1)/2/rho );