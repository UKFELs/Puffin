
% Create video of synchro-squeezed wavelet analysis at steps specified by
% inputs.


%vid = VideoWriter('peaks.mp4','MPEG-4');
vid = VideoWriter('fig3-mov.avi');
vid.FrameRate = 4;
open(vid);

hf = figure;
figp = get(hf,'OuterPosition');
%figp(3:4) = figp(3:4) * 1.2;
set(hf,'OuterPosition',figp);


for ij = 3000:3050

    hf = quiver(ax2(:,:,ij), ay2(:,:,ij));
    xlim([34 54]);
    ylim([34 54]);
    
    hf = get(hf,'parent');
    frame = getframe(hf);
    
    writeVideo(vid,frame);
    clf;
    
end


close(vid);

