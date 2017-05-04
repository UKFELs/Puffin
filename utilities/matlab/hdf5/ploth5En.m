function hp = ploth5En(fname)



dstep = 1500;
nsteps = 60000;

steps = 0:dstep:nsteps;

% Get size of dataset

fname = 'f1_main_integrated_30000.h5';
pwsInfo = h5info(fname, '/power');

numPZ2 = pwsInfo.Dataspace.Size;


ind = 0;
for step = steps

    %hf = plotPow2(step,1);
    power = h5read(strcat('f1_main_integrated_', int2str(step), '.h5'), ...
        '/power');

    ind = ind+1;
    En(ind) = sum(power);
    zbar(ind) = hdf5read(strcat('f1_main_integrated_', int2str(step), '.h5'), ...
        '/power','zbarTotal');
    
end 
    

figure; semilogy(zbar,En);
xlabel('zbar');
ylabel('Energy (arb units)');

