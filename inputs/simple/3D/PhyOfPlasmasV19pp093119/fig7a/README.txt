This example uses similar parameters to the example in ../fig7.

The difference is in the beam, and in some sampling parameters.
Since the Puffin paper was published, there have been some 
changes in the code, and a greater understanding of the 3D bband
cases have been developed. 

For example, in the case of the 3D CSE from a beam with a flat-top
current profile, the sudden sharp drop in current is NOT possible 
to model correctly in the 3D case, since we solve the diffraction in
Fourier space. The diffraction of the field arising from this 
discontinuity causes numerical artifacts which we do not believe are 
physical. So we have included an alternative input file here,
which does not include the sharp edges in the current profile,
and instead 'rounds off' the current profile at the edges,
giving a smoother radiation emission profile.

