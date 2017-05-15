function hp = plotElPhase(fname)






zbar = hdf5read(fname,'/electrons','zbarTotal');
rho = hdf5read(fname,'/runInfo','rho');
nZ2 = hdf5read(fname,'/runInfo','nZ2');
sLengthOfElmZ2 = hdf5read(fname,'/runInfo','sLengthOfElmZ2');
gammar = hdf5read(fname,'/runInfo','gamma_r');


nZ2 = double(nZ2);
lenZ2 = sLengthOfElmZ2 * (nZ2-1);
Z2axis = linspace(0,lenZ2,nZ2);


h = 6.626e-34; % Planck constant
q_e = 1.60217646e-19; % Charge on electron
c_0 = 2.99792458e8; % Speed of light in vacuum

NumUniquePts = ceil((nZ2+1)/2);
fs = (nZ2)/lenZ2; %sampling frequency




%apcz2 = h5read(fname,'/aperp',[60,60,1,1],[1,1,nZ2,2]);


% For 3D
%apcz2 = zeros(1,1,nZ2,1);

%for iz2 = 1:nZ2
%    apcz2(1,1,iz2,1) = h5read(fname,'/aperp',[61,67,iz2,1],[1,1,1,1]);
%end


%xf = apcz2(:,:,:,1);

%xf = reshape(xf,[nZ2,1]);



% For 1D

% Get size of dataset
els = h5info(fname);

numParts = els.Datasets.Dataspace.Size;
numParts = numParts(2);

%els = h5read(fname,'/electrons',[1,1],[1,numParts]);

sElZ2 = h5read(fname,'/electrons',[3,1],[1,numParts]);

sElGamma = h5read(fname,'/electrons',[6,1],[1,numParts]);


figure; hp = plot(sElZ2, sElGamma, '.');




