

%  (going from 34 : 54)

nZ2t = 51;

rhoT = rho;

syslen = (nZ2t - 1) * sLengthOfElmZ2;


for ix = 1:40
    for iy = 1:40
        
        [magxrms phasex]=getStokesf(reshape(ax2(ix,iy,100:150),[nZ2t,1]),nZ2t,rhoT,syslen);
        [magyrms phasey]=getStokesf(reshape(ay2(ix,iy,100:150),[nZ2t,1]),nZ2t,rhoT,syslen);

        
        mx(ix,iy) = magxrms(30);
        my(ix,iy) = magyrms(30);
        
        phx(ix,iy) = phasex(30);
        phy(ix,iy) = phasey(30);
    
        if (ix == 42)
            if (iy == 42)
                phsv = phasex(1:nZ2t);
                mgsv = magxrms(1:nZ2t);
            end
        end 
    end
end

figure; 
subplot(1,2,1);
hf = surf(phx);
%xlim([34  54]);
%ylim([34  54]);
set(hf,'edgecolor','none');
shading interp;
hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);



%figure; hf = surf(abs(my));
%subplot(1,2,2); hf = surf(abs(my));
subplot(1,2,2); hf = surf(  sum(abs(ax2(:,:,100:113)).^2 + abs(ay2(:,:,100:113)).^2,3) );
%xlim([34  54]);
%ylim([34  54]);
set(hf,'edgecolor','none');
shading interp;
hf = get(get(hf,'parent'),'parent');
set(hf,'renderermode','man');
set(hf,'renderer','zbuffer');
view([0 90]);
