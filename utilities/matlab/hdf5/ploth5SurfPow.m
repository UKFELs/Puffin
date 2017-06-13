function hp = ploth5En(fname)

fname = 'f1_main_aperp_30000.h5';
%pwsInfo = h5info(fname, '/power');

%numPZ2 = pwsInfo.Dataspace.Size;


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



dstep = 1500;
nsteps = 60000;

steps = 0:dstep:nsteps;

% Get size of dataset



%powers = zeros(size(steps));

ind = 0;
for step = steps

    %hf = plotPow2(step,1);
    ind = ind+1;
    apcz2 = h5read(strcat('f1_main_aperp_', int2str(step), '.h5'), ...
        '/aperp');

    
    xf = apcz2(:,1);
    yf = -apcz2(:,2);

    power(ind,:) = abs(xf).^2 + abs(yf).^2;

    
%    [magx phasex]=getStokesf(xf,nZ2,rho,lenZ2);

%    power(ind,:) = magx;
    power(ind,:) = power(ind,:) ./ max(power(ind,:));

    %En(ind) = sum(power);
    %zbar(ind) = hdf5read(strcat('f1_main_integrated_', int2str(step), '.h5'), ...
    %    '/power','zbarTotal');
    
end 
    
figure; hf = surf(power);
%xlabel('zbar');
%ylabel('Energy (arb units)');

set(hf,'edgecolor','none');

shading interp;

hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);
