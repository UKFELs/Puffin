

numUniquePtsZ2 =real( ceil((nZ2+1)/2));
numUniquePtsX = real( ceil((nX+1)/2));
numUniquePtsY = real(ceil((nY+1)/2));



%kx = 2.*pi .* (0:numUniquePtsX) ./ (sLengthOfElmX .* nX);
%ky = 2.*pi .* (0:numUniquePtsY) ./ (sLengthOfElmY .* nY);
%kz2 = 2.*pi .* (0:numUniquePtsZ2) ./ (sLengthOfElmZ2 .* nZ2);

nZ2 = double(nZ2);
nX = double(nX);
nY = double(nY);

if rem(nZ2,2)
    kz2 = [double(0:numUniquePtsZ2)  double(-numUniquePtsZ2:-1)] .* 2.0 .* pi ./ (sLengthOfElmZ2 .* nZ2);
else
    kz2 = [double(0:numUniquePtsZ2)  double(-numUniquePtsZ2+1:-1)] .* 2.0 .* pi ./ (sLengthOfElmZ2 .* nZ2);
end




if rem(nX,2)
    kx = double([(0:numUniquePtsX)  (-numUniquePtsX:-1)]) .* 2 .* pi ./ (sLengthOfElmX .* nX);
else
    kx = double([(0:numUniquePtsX)  (-numUniquePtsX+1:-1)]) .* 2 .* pi ./ (sLengthOfElmX .* nX);
end



if rem(nY,2)
    ky = double([(0:numUniquePtsY)  (-numUniquePtsY:-1)]) .* 2 .* pi ./ (sLengthOfElmY .* nY);
else
    ky = double([(0:numUniquePtsY)  (-numUniquePtsY+1:-1)]) .* 2 .* pi ./ (sLengthOfElmY .* nY);
end







dzbar = 6.0;


ax2FT = fftn(aperp);


for ix = 1:nX

    for iy = 1:nY
        
        for iz2 = 1:nZ2

            if (kz2(iz2) ~= 0)
                
            
                ax2FT(ix,iy,iz2) = ax2FT(ix,iy,iz2) .* exp(1j .* dzbar .* ...
                    ( kx(ix).^2 + ky(iy).^2 ) ...
                      ./ (2.*kz2(iz2)) );
            end
                           
        end 
    end
end



apf = ifftn(ax2FT);
%clear ax2FT;


