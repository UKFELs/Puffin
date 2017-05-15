function hf = makMov2(nsteps, dstep)

% Create video of synchro-squeezed wavelet analysis at steps specified by
% inputs.


%vid = VideoWriter('peaks.mp4','MPEG-4');
vid = VideoWriter('fig4-mov2.avi');
vid.FrameRate = 4;
open(vid);

steps = 0:dstep:nsteps;
%steps(1) = 0;

hf = figure;
figp = get(hf,'OuterPosition');
%figp(3:4) = figp(3:4) * 1.2;
set(hf,'OuterPosition',figp);


for step = steps

    %hf = plotPow2(step,1);
    ht = wanls2(step);
    ht = get(hf,'parent');
    frame = getframe(hf);
    
    writeVideo(vid,frame);
    clf;
end

close(vid);