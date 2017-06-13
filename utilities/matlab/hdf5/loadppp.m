
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PHYSICAL CONSTANTS
%%%%

h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% READ IN FILE AND CONSTANTS
%%%%

%fname = 'SSS_aperp_6000.h5';
fname = 'test1_aperp_smallt_12180.h5';

%zbar = hdf5read(fname,'/aperp','zbarTotal');
rho = hdf5read(fname,'/runInfo','rho');

nZ2 = hdf5read(fname,'/runInfo','nZ2');
nX = hdf5read(fname,'/runInfo','nX');
nY = hdf5read(fname,'/runInfo','nY');
nX=40;  % ...if reduced set need to specify for now...
nY=40;  % ...if reduced set need to specify for now...
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');
sLengthOfElmX = hdf5read(fname,'/runInfo','sLengthOfElmX');
sLengthOfElmY = hdf5read(fname,'/runInfo','sLengthOfElmY');
%nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * double(nZ2-1);
Z2axis = linspace(0,lenZ2,nZ2);

NumUniquePts = ceil((nZ2+1)/2);
fs = (nZ2)/lenZ2; %sampling frequency

% Picking region to read

apcz2 = h5read(fname,'/aperp',[1,1,21169-100,1],[double(nX),double(nY),500,2]);
nZ2=500;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% DATA MANIPULATION
%%%%

inz2 = 109;

aperp = apcz2(:,:,:,1) + 1j.*apcz2(:,:,:,2);  % Form complex field

aperp = reshape(aperp,[nX,nY,nZ2]);   

% Filter around harmonic
[ax2  ay2] = FilterField(aperp,1,0.1,nZ2,sLengthOfElmZ2,rho,0);

figure; quiver(ax2(:,:,inz2), ay2(:,:,inz2));





figure; 
subplot(1,2,1);
hf = surf(abs(ax2(:,:,inz2)).^2 + abs(ay2(:,:,inz2)).^2);
%xlim([34  54]);
%ylim([34  54]);
set(hf,'edgecolor','none');
shading interp;
hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);



%figure; hf = surf(abs(my));
subplot(1,2,2); hf = quiver(ax2(:,:,inz2), ay2(:,:,inz2));
%xlim([34  54]);
%ylim([34  54]);



