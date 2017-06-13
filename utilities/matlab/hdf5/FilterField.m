function [xfield   yfield] = FilterField(field,crfr,distfr,nZ2,sLengthOfElmZ2, rho, q1d)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter - HARD FILTER

%crfr=1.4167;
%distfr=0.2;
 
nn = round(sLengthOfElmZ2 * nZ2 * crfr / (4*pi*rho));
nns = round(sLengthOfElmZ2 * nZ2 * distfr / (4*pi*rho));

if (q1d == 1)

%%%%%    1D    %%%%%%%

  ftfield = fft(field);

  ftfield(1:(nn-nns)) = 0;
  ftfield((nn+nns):ceil(nZ2/2)) = 0;

  ftfield(ceil(nZ2/2) + 1:nZ2-(nn+nns)+2) = 0;
  ftfield((nZ2 - (nn-nns) + 2 ) : nZ2) = 0;

  field = ifft(ftfield);

  xfield = real(field);
  yfield = -imag(field);

else
  
%%%%%    3D    %%%%%%%

  ftfield = fft(field,[],3);

  ftfield(:,:,1:(nn-nns)) = 0;
  ftfield(:,:,(nn+nns):ceil(nZ2/2)) = 0;

  ftfield(:,:,ceil(nZ2/2) + 1:nZ2-(nn+nns)+2) = 0;
  ftfield(:,:,(nZ2 - (nn-nns) + 2 ) : nZ2) = 0;

  field = ifft(ftfield,[],3);

  xfield = real(field);
  yfield = -imag(field);

end

%%%%%%%%%%%%%%%%%%%%%%