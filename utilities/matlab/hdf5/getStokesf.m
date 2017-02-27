function [magxrms phasex]=getStokesf(Ex,nZ2,rho,syslen)
%clear all;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Script to get the phase and magnitude of a wave.
%It uses the method outlined in the thesis, "Physics
%of a 4th Generation Light Source", by LT Campbell. 
%Author: Lawrence Campbell
%        Dept of Physics
%        University of Strathclyde
%        Glasgow
%Date:   4th July, 2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Vars:-
%rho = 0.1;
%syslen = 10*4*pi*rho;
%nZ2 = 1000;
wavel=4*pi*rho;

ax=1;
thix=-1;

xaxis = linspace(0,syslen,nZ2)'; % xaxis for real space
%Ex = ax*cos(xaxis/(2*rho)+thix); %Define x field

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%MAGNITUDE:
%2*df is the number of nodes spanning a resonant wavelength
%de is the number of nodes to the left to average from
%dg is the number of nodes to the right
%so average around node_index-de:node_index+dg
%first of all, find the number of nodes corresponding to one lamda_r,nnl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lr=wavel;
nsp=syslen/(nZ2-1);
nel=round(lr/nsp);
nnl=nel+1;

df=floor((nnl)/2);
dg=df;
if (mod(nnl,2)==1)
    de=df;
else    
    de=df-1;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Specify a segment around the current node
%and average around it over a wavelength
%intxrms is rms square root of the mean intensity, or
%the rms absolute field.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intxrms=zeros(nZ2,1);
intyrms=zeros(nZ2,1);

for m=1:nZ2
    lo=m-de; hi=lo+(nnl-2);%hi=m+dg;
    if lo<1 
        lo=1;
    end
    if hi>nZ2
        hi=nZ2;
    end
    intxrms(m)=sqrt(mean(Ex(lo:hi).^2));
end

magxrms=sqrt(2)*intxrms; %magxrms is the rms magnitude of the wave

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PHASE:
%First create the "reference" wave, cos(\omega t)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
z2cyclic=xaxis-(4*pi*rho*floor(xaxis/(4*pi*rho)));
z2cyclic=z2cyclic/(2*rho);

slopy=-sin(2*pi*xaxis/wavel);

invcosx=acos(Ex./magxrms);

for a=1:nZ2
  if (abs(Ex(a))>=magxrms(a))
      if (Ex(a)>0)
          invcosx(a)=0;
      elseif (Ex(a)<0)
          invcosx(a)=pi;
      end
  end 
end

for a=1:nZ2-1
    if Ex(a)<Ex(a+1) %best way to do this may be taking 
         %%%% gradient of Ex. If negative, then shift
         %%%% to 3rd and 4th quadrant....
         %invcosx(a)=invcosx(a)*(-1);
        invcosx(a)=(2*pi)-invcosx(a);
    end
end
phasex=invcosx-z2cyclic;

for a=1:nZ2
    if phasex(a)<0
        phasex(a)=2*pi-abs(phasex(a));
    end
end

%plot(xaxis,magxrms);
%hold on;
%plot(xaxis,phasex,'r');
%hold off;